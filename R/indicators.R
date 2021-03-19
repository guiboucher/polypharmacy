#' Indicators: All selected
#'
#' Wrapper function for all *Indicator* functions.
#'
#' \strong{\code{stats}} & \strong{\code{simult_ind_stats}}: Possible values are
#' * `'mean'`, `'min'`, `'median'`, `'max'`, `'sd'`;
#' * `'pX'` where *X* is a value in ]0, 100];
#' * `'q1'` = `'p25'`, `'q2'` = `'p50'` = `'median'`, `q3` = `'p75'`.
#'
#' @param processed_tab Table created by \code{\link{data_process}} function.
#' @param stats Statistics to calculate on the drug consumption. See *Details* for possible values.
#' @param method Indicator functions name to use.
#' @param stdconti_pdays *stdcontinuous* method: Number of days to create intervals `[min; min+pdays]` and `[max-pdays; max]` where a drug should be consumed to be counted.
#' @param simult_ind_stats *simult* method: Statistics to calculate for each drug user.
#' @param simult_calendar *simul* method: `TRUE` or `FALSE`. Create a table indicating the number of drugs consumed for each day for each user (`FALSE` by default).
#' @param stdcumul_nPeriod *std_cumul* method: Integer value greater or equal to 1 and lesser or equal to the total number of days in the study period. If `nPeriod` is greater than 1, the study period is divide in `nPeriod` subperiod and the total number of drugs consumption would be the average of the periods.
#' @param cores The number of cores to use when executing `ind_simult()`. See \code{\link[parallel]{parallel::detectCores}}.
#'
#' @return `list` of all indicators
#' @export
#' @encoding UTF-8
indicators <- function(
  processed_tab,
  stats = c('mean', 'sd', 'min', 'p5', 'p10', 'p25', 'median', 'p75', 'p90', 'p95', 'max'),
  method = c('ind_simult', 'ind_stdcumul', 'ind_wcumul', 'ind_stdcontinuous', 'ind_ucontinuous'),
  # stdcontinuous
  stdconti_pdays,
  # simult
  simult_ind_stats = c('mean', 'min', 'median', 'max'),
  simult_calendar = FALSE,
  # stdcumul
  stdcumul_nPeriod = 1,

  cores = parallel::detectCores()
) {

  ll <- vector("list", length(method))
  i <- 1L

  if ("ind_simult" %in% method) {
    ll[[i]] <- ind_simult(
      processed_tab, individual_stats = simult_ind_stats, stats = stats,
      calendar = simult_calendar, cores = cores
    )
    i <- i + 1L
  }

  if ("ind_stdcumul" %in% method) {
    ll[[i]] <- ind_stdcumul(processed_tab, stdcumul_nPeriod, stats)
    i <- i + 1L
  }

  if ("ind_wcumul" %in% method) {
    ll[[i]] <- ind_wcumul(processed_tab, stats)
    i <- i + 1L
  }

  if ("ind_stdcontinuous" %in% method) {
    ll[[i]] <- ind_stdcontinuous(processed_tab, stdconti_pdays, stats)
    i <- i + 1L
  }

  if ("ind_ucontinuous" %in% method) {
    ll[[i]] <- ind_ucontinuous(processed_tab, stats)
  }

  names(ll) <- method

  return(ll)

}
