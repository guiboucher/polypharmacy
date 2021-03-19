#' Indicator: Weight Cumulative
#'
#' Description
#'
#' \strong{\code{stats}}: Possible values are
#' * `'mean'`, `'min'`, `'median'`, `'max'`, `'sd'`;
#' * `'pX'` where *X* is a value in ]0, 100];
#' * `'q1'` = `'p25'`, `'q2'` = `'p50'` = `'median'`, `q3` = `'p75'`.
#'
#' @param processed_tab Table created by \code{\link{data_process}} function.
#' @param stats Statistics to calculate on the drug consumption. See *Details* for possible values.
#'
#' @return `list`:
#' * `indic`: `data.table` indicating each `stats` (columns).
#' * `stats_id`: `data.table` indicating the number of drugs use for each individual (all cohort).
#' @import data.table
#' @export
#' @encoding UTF-8
ind_wcumul <- function(
  processed_tab,
  stats = c('mean', 'sd', 'min', 'p5', 'p10', 'p25', 'median', 'p75', 'p90', 'p95', 'max')
) {

  ### Extract attributes
  rx_cols <- attr(processed_tab, "cols")  # initial columns name
  cohort <- attr(processed_tab, "Cohort")  # cohort ids vector
  study_dates <- attr(processed_tab, "study_dates")  # study period

  ### Arrange data
  if (!is.data.table(processed_tab)) {
    setDT(processed_tab)  # convert data.table
  }
  processed_tab <- processed_tab[, c(unlist(rx_cols), "tx_start", "tx_end"), with = FALSE]  # cols selection
  setnames(processed_tab, unlist(rx_cols), c("id", "drug_code"))

  ### Arrange arguments
  # ndays - number of days in the study period
  ndays_tot <- as.integer(lubridate::as_date(study_dates$end) - lubridate::as_date(study_dates$start) + 1)
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

  ### nRx
  processed_tab[, ndays := as.integer(tx_end - tx_start + 1)]  # nbr of days consumption
  processed_tab <- processed_tab[, .(ndays = sum(ndays)), .(id, drug_code)]  # total nbr days per drug
  processed_tab[, ratios := ndays / ndays_tot]
  processed_tab <- processed_tab[, .(nRx = sum(ratios)), .(id)]

  ### Add cohort without consumption
  ids2add <- data.table(id = cohort[!cohort %in% processed_tab$id], nRx = 0L)  # ids not in processed_tab
  if (nrow(ids2add)) {
    processed_tab <- rbind(processed_tab, ids2add)  # combine datas
  }
  setkey(processed_tab, id)

  ### Stats
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
