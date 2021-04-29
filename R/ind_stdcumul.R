#' Assess polypharmacy based on the average number of distinct medications consumed over successive periods of time of equal length
#'
#' Averages the number of distinct medications that are consumed by every individual during successive periods of time of equal length and provides cohort descriptive statistics on this indicator.
#'
#' \strong{stats:} Possible values are
#' * `'mean'`, `'min'`, `'median'`, `'max'`, `'sd'`;
#' * `'pX'` where *X* is an integer value in ]0, 100];
#' * `'q1'`=`'p25'`, `'q2'`=`'p50'`=`'median'`, `q3`=`'p75'`.
#'
#' @param processed_tab Table of individual drug treatments over the study period. Created by \code{\link{data_process}} function.
#' @param nPeriod Number of subperiods of equal time length in which the study period will be subdivided: Integer value greater or equal to 1 and lesser or equal to the total number of days in the study period. If `nPeriod` is greater than 1, the study period is divided in `nPeriod` subperiods and the number of medications consumed in each subperiod is averaged over the number of subperiods.
#' @param stats Cohort descriptive statistics to calculate on the polypharmacy indicator. See *Details* for possible values.
#'
#' @return `list`:
#' * `indic`: `data.table` indicating each `stats` (columns).
#' * `stats_id`: `data.table`. For each individual (all cohort), indicate the number of drug use per period (`perX` where `X` is a number between 1 and `nPeriod`) and the mean of the periods (`nRx`).
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
#' # 1 period
#' dt_ind_stdcumul_per1 <- ind_stdcumul(processed_tab = rx_proc1, nPeriod = 1)
#' # 3 periods
#' dt_ind_stdcumul_per3 <- ind_stdcumul(processed_tab = rx_proc1, nPeriod = 3)
ind_stdcumul <- function(
  processed_tab, nPeriod = 1,
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
  # nPeriod
  date_vec <- seq(lubridate::as_date(study_dates$start), lubridate::as_date(study_dates$end), 1)
  if (nPeriod == 1) {
    date_per <- list(date_vec)
  } else {
    # Split date_vec in nPeriod
    date_per <- parallel::splitIndices(length(date_vec), nPeriod)
    date_per <- lapply(date_per, function(x) date_vec[x])
  }
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

  ### Drugs consumption per period
  for (i in 1:length(date_per)) {
    # Indicate if there is a consumption in the period (at least 1 day)
    #   0 for no consumption
    #   1 for a consumption
    processed_tab[, paste0("per",i) := 0L]
    processed_tab[
      tx_start <= max(date_per[[i]]) & tx_end >= min(date_per[[i]]),
      paste0("per",i) := 1L
    ]
    processed_tab[, paste0("per",i) := sum(get(paste0("per",i))), .(id)]
  }

  ### nRx: drug consumption for the period (mean if there is more than 1 period)
  processed_tab <- processed_tab[processed_tab[, .I[1], .(id)]$V1]  # keep only 1 row per id
  per_cols <- names(processed_tab)[stringr::str_detect(names(processed_tab), "^per")]  # cols nbr drugs per period
  processed_tab <- processed_tab[, c("id", per_cols), with = FALSE]  # cols selection
  # Mean of drugs consumption per period
  if (length(per_cols) > 1) {
    processed_tab[, nRx := apply(processed_tab[, !"id"], 1, mean)]
  } else {
    processed_tab[, nRx := per1]
  }

  ### Add cohort without consumption
  ids2add <- data.table(id = cohort[!cohort %in% processed_tab$id])  # ids not in processed_tab
  if (nrow(ids2add)) {
    for (col in c(per_cols, "nRx")) {  # add each column with zeros
      ids2add[, (col) := 0L]
    }
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
  tab_stat[, nPeriod := (nPeriod)]  # nbr periods
  tab_stat[, cohort := length(cohort)]  # nbr people

  return(list(
    indic = tab_stat,
    stats_ids = processed_tab
  ))

}
