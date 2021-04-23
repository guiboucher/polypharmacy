#' Assess polypharmacy based on the number of medications that is consumed both during the initial and the final period of the study period
#'
#' Calculates the number of distinct medications that are consumed both during the initial and the final period of the overall study period by every individual of the study cohort and provides cohort descriptive statistics on this indicator.
#'
#' \strong{stats:} Possible values are
#' * `'mean'`, `'min'`, `'median'`, `'max'`, `'sd'`;
#' * `'pX'` where *X* is an integer value in ]0, 100];
#' * `'q1'`=`'p25'`, `'q2'`=`'p50'`=`'median'`, `q3`=`'p75'`.
#'
#' @param processed_tab Table of individual drug treatments over the study period. Created by \code{\link{data_process}} function.
#' @param pdays Duration (in days) of the initial and final periods of time . The initial period = \[min; min+`pdays`\] and the final period = \[max-`pdays`; max\], where *min* and *max* are the very first and last date, respectively, found in `processed_tab`.
#' @param stats Cohort descriptive statistics to calculate on the polypharmacy indicator. See *Details* for possible values.
#'
#' @return `list`:
#' * `indic`: `data.table` indicating each `stats` (columns).
#' * `stats_id`: `data.table` indicating the number of drugs use for each individual (all cohort).
#' @import data.table
#' @export
#' @encoding UTF-8
ind_stdcontinuous <- function(
  processed_tab, pdays,
  stats = c('mean', 'sd', 'min', 'p5', 'p10', 'p25', 'median', 'p75', 'p90', 'p95', 'max')
) {

  ### Extract attributes
  rx_cols <- attr(processed_tab, "cols")  # initial columns name
  cohort <- attr(processed_tab, "Cohort")  # cohort ids vector
  study_dates <- attr(processed_tab, "study_dates")  # study period
  if (is.null(study_dates$start)) {
    study_dates$start <- min(processed_tab$tx_start)
  }
  if (is.null(study_dates$end)) {
    study_dates$end <- max(processed_tab$tx_end)
  }

  ### Arrange data
  if (!is.data.table(processed_tab)) {
    setDT(processed_tab)  # convert data.table
  }
  processed_tab <- processed_tab[, c(unlist(rx_cols), "tx_start", "tx_end"), with = FALSE]  # cols selection
  setnames(processed_tab, unlist(rx_cols), c("id", "drug_code"))

  ### Arrange arguments
  # pdays
  if (!is.integer(pdays)) {
    pdays <- as.integer(pdays)
  }
  # min and max dates
  min_dy <- lubridate::as_date(study_dates$start)
  max_dy <- lubridate::as_date(study_dates$end)
  # stats
  stats <- vapply(stats, function(x) {  # convert quarter to percentile
    if (x == "q1") {
      x <- "p25"
    } else if (x == "q2") {
      x <- "p50"
    } else if (x == "q3") {
      x <- "p75"
    }
    return(x)
  }, character(1), USE.NAMES = FALSE)

  ### Drugs Consumption per period
  processed_tab[, `:=` (P1_date = min_dy + pdays - 1,
                        P2_date = max_dy - pdays + 1,
                        P1 = 0L, P2 = 0L)]
  processed_tab[tx_start <= P1_date & tx_end >= min_dy, P1 := 1L]
  processed_tab[tx_start <= max_dy & tx_end >= P2_date, P2 := 1L]
  processed_tab <- processed_tab[, .(P1 = max(P1), P2 = max(P2)), .(id, drug_code)]
  processed_tab <- processed_tab[P1 == 1 & P2 == 1]  # keep only those who have consumption in both periods

  ### nRx
  processed_tab <- processed_tab[, .(nRx = sum(P1)), .(id)]

  ### Add cohort without consumption
  ids2add <- data.table(id = cohort[!cohort %in% processed_tab$id], nRx = 0L)
  if (nrow(ids2add)) {
    processed_tab <- rbind(processed_tab, ids2add)
  }
  setkey(processed_tab, id)

  ### stats
  tab_stat <- data.table()
  for (stt in stats) {
    if (stt %in% c("mean", "min", "median", "max", "sd")) {
      tab_stat[, (stt) := get(stt)(processed_tab$nRx)]
    } else {
      tab_stat[
        , (stt) := quantile(processed_tab$nRx,
                            probs = stat_quantile_prob(stt) / 100)
      ]
    }
  }
  tab_stat[, cohort := length(cohort)]  # nbr people

  return(list(
    indic = tab_stat,
    stats_ids = processed_tab
  ))

}
