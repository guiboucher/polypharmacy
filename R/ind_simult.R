#' Simultaneous indicator
#'
#' Description
#'
#' **individual_stats**, **stats**:\cr
#'
#'
#' @param processed_tab Table created by `data_process()` function.
#' @param individual_stats Statistics to calculate for each drug user. See *Details* for possible values.
#' @param stats Statistics to calculate for each `individual_stats`. See *Details* for possible values.
#' @param calendar `TRUE` or `FALSE`. Create a table indicating the number of drugs consumed for each day (`FALSE` by default).
#'
#' @import data.table
#' @importFrom lubridate as_date
#' @importFrom stringr str_detect str_remove
#' @return if `calendar` is `FALSE`:
#' * data.table` indicating each `stats` (columns) for each `individual_stats` (rows).
#'
#' if `calendar` is `TRUE`, a list of two (2) elements:
#' * indicators: Table described above.
#' * calendar: Table indicating the number of drugs consumed for each day.
#'   + Iden
#' @export
ind_simult <- function(
  processed_tab, individual_stats = c("mean", "min", "median", "max"),
  stats = c("mean", "sd", "min", "p5", "p10", "q1", "median", "q3", "p90", "p95", "max"),
  calendar = FALSE
) {

# Internal FCTS -----------------------------------------------------------

  stat_chr_to_fct <- function(x) {
    ### Return function considering a word (character string)
    if (x == "mean") {
      return(mean)
    } else if (x == "min") {
      return(min)
    } else if (x == "median") {
      return(median)
    } else if (x == "max") {
      return(max)
    } else if (x == "sd") {
      return(sd)
    }
  }
  stat_quantile_prob <- function(x) {
    ### If we want a quantile, we need to determine the probability
    if (str_detect(x, "q")) {
      if (x == "q1") {
        x <- "p25"
      } else if (x == "q2") {
        x <- "p50"
      } else {
        x <- "p75"
      }
    }
    return(as.numeric(str_remove(x, "p")))
  }

# Core FCT ----------------------------------------------------------------

  ### Initial Variables
  # Columns name
  rx_cols <- attr(processed_tab, "cols")

  # processed_tab should be a data.table (if created by data_process())
  if (is.data.table(processed_tab)) {
    processed_tab <- copy(processed_tab)
  } else {
    processed_tab <- as.data.table(processed_tab)
  }
  setnames(processed_tab, unlist(rx_cols), c("id", "drug_code"))

  # Convert dates to integer
  if (!is.integer(processed_tab$tx_start)) {
    processed_tab[, tx_start := as.integer(tx_start)]
  }
  if (!is.integer(processed_tab$tx_end)) {
    processed_tab[, tx_end := as.integer(tx_end)]
  }

  # Indicate for each day if there is a consumption of drug -> calendar
  for (dy in min(processed_tab$tx_star):max(processed_tab$tx_end)) {
    processed_tab[, paste(dy) := FALSE]
    processed_tab[tx_start <= dy & dy <= tx_end, paste(dy) := TRUE]
  }
  processed_tab[, `:=` (tx_start = NULL, tx_end = NULL)]
  processed_tab <- melt(  # columns to rows: faster calculation for stats
    processed_tab, id.vars = c("id", "drug_code"),
    variable.name = "date_drug", value.name = "cons",
    variable.factor = TRUE  # faster to work with factor than CHR
  )
  processed_tab[, date_drug := as.integer(levels(date_drug))[date_drug]]  # convert factor as integer (dates)
  processed_tab <- processed_tab[, .(n_drugs = sum(cons)), keyby = .(id, date_drug)]  # number of drug cunsomption per day

  tab_calendar <- processed_tab[, .(id, consumtion_date = as_date(date_drug), n_drugs)]

  # Stats calculation for each id
  for (col in individual_stats) {
    if (col %in% c("mean", "sd", "min", "median", "max")) {
      processed_tab[, (col) := stat_chr_to_fct(col)(n_drugs), .(id)]
    } else {
      prob <- stat_quantile_prob(col)
      processed_tab[, (col) := quantile(n_drugs, probs = prob / 100)]
    }
  }
  # Keep 1 line per id: individual stats
  cols <- c("id", individual_stats)  # cols to keep
  processed_tab <- processed_tab[processed_tab[, .I[1], .(id)]$V1][, ..cols]  # select 1st row is faster than unique()


  # Table indicate stats for each individual stats
  tab_result <- data.table()  # final table where stats will be displayed
  for (ind_st in individual_stats) {
    stats_to_add <- data.table()
    stats_to_add[, ind_stat := ind_st]  # indicate what is the individual stat that we are calculating
    for (st in stats) {  # indicate statistics for individual stats
      if (st %in% c("mean", "sd", "min", "median", "max")) {
        # Stats that are not quantile (percentil)
        stats_to_add[, (st) := stat_chr_to_fct(st)(processed_tab[[ind_st]])]
      } else {
        # If quantile (percentile) include 'q{1,2,3}' values
        prob <- stat_quantile_prob(st)  # extract prob value from 'pX' where X is a percentage
        stats_to_add[, (st) := quantile(processed_tab[[ind_st]], probs = prob/100)]
      }
    }
    tab_result <- rbind(tab_result, stats_to_add)
  }

  # Return results depending if calendar is wanted
  if (calendar) {
    return(list(indicators = tab_result,
                calendar = tab_calendar))
  } else {
    return(tab_result)
  }

}
