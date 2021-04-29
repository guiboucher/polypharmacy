#' Assess polypharmacy based on the number of distinct medications consumed weighted to their respective duration of consumption
#'
#' Calculates the number of distinct medications weighted by the duration of consumption that are consumed by every individual and provides cohort descriptive statistics on this indicator.
#'
#' \strong{stats:} Possible values are
#' * `'mean'`, `'min'`, `'median'`, `'max'`, `'sd'`;
#' * `'pX'` where *X* is an integer value in ]0, 100];
#' * `'q1'`=`'p25'`, `'q2'`=`'p50'`=`'median'`, `q3`=`'p75'`.
#'
#' @param processed_tab Table of individual drug treatments over the study period. Created by \code{\link{data_process}} function.
#' @param stats Cohort descriptive statistics to calculate on the polypharmacy indicator. See *Details* for possible values.
#'
#' @return `list`:
#' * `indic`: `data.table` indicating each `stats` (columns).
#' * `stats_id`: `data.table` indicating the number of drugs use for each individual (all cohort).
#' @import data.table
#' @export
#' @encoding UTF-8
#' @examples
#' rx1 <- data.frame(id = c(1, 1, 1, 2),
#'                   code = c("A", "B", "C", "A"),
#'                   date = c("2000-01-01", "2000-01-01", "2000-01-26", "2000-01-17"),
#'                   duration = c(30, 5, 5, 10))
#' cohort1 <- data.frame(id = as.numeric(1:3),
#'                       age = c(45, 12, 89),
#'                       sex = c("F", "F", "M"))
#' rx_proc1 <- data_process(Rx_deliv = rx1, Rx_id = "id", Rx_drug_code = "code",
#'                          Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'                          Cohort = cohort1, Cohort_id = "id",
#'                          study_start = "2000-01-01", study_end = "2000-01-30",
#'                          cores = 1)
#' dt_ind_wcumul <- ind_wcumul(processed_tab = rx_proc1)
ind_wcumul <- function(
  processed_tab,
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
  ids2add <- data.table(id = cohort[!cohort %in% processed_tab$id])  # ids not in processed_tab
  if (nrow(ids2add)) {
    ids2add[, nRx := 0L]
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
