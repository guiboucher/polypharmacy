#' Simultaneous indicator
#'
#' Description
#'
#' Using two (2) attributes from
#'
#' @param processed_tab Table created by \code{\link{data_process}} function.
#' @param individual_stats Statistics to calculate for each drug user. See *Details* for possible values.
#' @param stats Statistics to calculate for each `individual_stats`. See *Details* for possible values.
#' @param calendar `TRUE` or `FALSE`. Create a table indicating the number of drugs consumed for each day (`FALSE` by default).
#'
#' @import data.table
#' @importFrom lubridate as_date
#' @importFrom stringr str_detect str_remove
#' @return if `calendar` is `FALSE`:
#' * data.table` indicating each `stats` (columns) for each `individual_stats` (rows).\cr\cr
#' if `calendar` is `TRUE`, a list of two (2) elements:
#' * indicators: Table described above.
#' * calendar: Table indicating the number of drugs consumed for each day.
#' @export
ind_simult <- function(
  processed_tab,
  individual_stats = c("mean", "min", "median", "max"),
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
      } else if (x == "q3") {
        x <- "p75"
      } else {
        stop("ind_simult.stat_quantile_prob(): wrong value.")
      }
    }
    return(as.numeric(str_remove(x, "p")))
  }

# Core FCT ----------------------------------------------------------------

  # Extract attributes
  rx_cols <- attr(processed_tab, "cols")  # initial columns name
  cohort <- attr(processed_tab, "Cohort")  # cohort ids vector

  # processed_tab should be a data.table (if created by data_process())
  if (is.data.table(processed_tab)) {
    processed_tab <- copy(processed_tab)
  } else {
    processed_tab <- as.data.table(processed_tab)
  }
  setnames(processed_tab, unlist(rx_cols), c("id", "drug_code"))  # rename columns

  # Indicate for each day if there is a consumption of drug
  for (dy in min(processed_tab$tx_star):max(processed_tab$tx_end)) {
    processed_tab[, paste(dy) := 0L]  # 0 for no consumption
    processed_tab[tx_start <= dy & dy <= tx_end, paste(dy) := 1L]  # 1 if there is a consumption
  }
  processed_tab[, `:=` (tx_start = NULL, tx_end = NULL)]
  processed_tab <- melt(  # columns to rows: faster calculation for stats
    processed_tab, id.vars = c("id", "drug_code"),
    variable.name = "date_drug", value.name = "cons",
    variable.factor = TRUE  # faster to work with factor than CHR
  )
  processed_tab[, date_drug := as.integer(levels(date_drug))[date_drug]]  # convert factor to integer (dates)
  processed_tab <- processed_tab[, .(n_drugs = sum(cons)), keyby = .(id, date_drug)]  # number of drug consumption per day

  # Create a calendar if wanted
  if (calendar) {
    tab_calendar <- dcast(processed_tab, id ~ date_drug, value.var = "n_drugs")  # each column is a date indicating the number of meds
    if (!all(cohort %in% tab_calendar$id)) { # add missing ids to the calendar if necessary
      tab_calendar <- rbind(tab_calendar, data.table(id = cohort[!cohort %in% tab_calendar$id]),
                            fill = TRUE)
      for (j in names(tab_calendar)) { # replace NAs by 0
        set(tab_calendar, which(is.na(tab_calendar[[j]])), j, 0L)
      }
    }
    setkey(tab_calendar, id)
    setnames(tab_calendar, "id", rx_cols$Rx_id)  # keep initial id column name
    names(tab_calendar)[2:ncol(tab_calendar)] <-  # colnames should be dates, not integer
      as.character(as_date(as.integer(names(tab_calendar)[2:ncol(tab_calendar)])))
  }

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
