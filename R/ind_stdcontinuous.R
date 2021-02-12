#' Indicator: Continuous
#'
#' Description
#'
#' \strong{\code{stats}}: Possible values are
#' * `'mean'`, `'min'`, `'median'`, `'max'`, `'sd'`;
#' * `'pX'` where *X* is a value in ]0, 100];
#' * `'q1'` = `'p25'`, `'q2'` = `'p50'` = `'median'`, `q3` = `'p75'`.
#'
#' @param processed_tab Table created by \code{\link{data_process}} function.
#' @param pdays Number of days to create intervals `[min; min+pdays]` and `[max-pdays; max]` where a drug should be consumed to be counted.
#' @param stats Statistics to calculate on the drug consumption. See *Details* for possible values.
#'
#' @return `data.table` indicating each `stats` (columns).
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
  stats <- sapply(stats, function(x) {  # convert quarter to percentile
    if (x == "q1") {
      x <- "p25"
    } else if (x == "q2") {
      x <- "p50"
    } else if (x == "q3") {
      x <- "p75"
    }
    return(x)
  }, USE.NAMES = FALSE)

  ### Drugs Consumption per period
  processed_tab[, `:=` (P1_date = min_dy + pdays - 1,
                        P2_date = max_dy - pdays + 1,
                        P1 = FALSE, P2 = FALSE)]
  processed_tab[tx_start <= P1_date & tx_end >= min_dy, P1 := TRUE]
  processed_tab[tx_start <= max_dy & tx_end >= P2_date, P2 := TRUE]
  processed_tab <- processed_tab[
    , .(P1 = max(P1), P2 = max(P2)),
    .(id, drug_code)
  ]
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
  tab_stat[, Cohort := length(cohort)]  # nbr people

  ### Atttributes
  attr(tab_stat, "nRx") <- processed_tab

  return(tab_stat)

}
