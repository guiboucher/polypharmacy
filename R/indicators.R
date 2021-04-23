#' Provide several polypharmacy indicators at once
#'
#' Wrapper function to run sequentially various polypharmacy functions on a single set of data. Each function corresponds to a different definition of polypharmacy.
#'
#' \strong{stats & simult_ind_stats:} Possible values are
#' * `'mean'`, `'min'`, `'median'`, `'max'`, `'sd'`;
#' * `'pX'` where *X* is an integer value in ]0, 100];
#' * `'q1'`=`'p25'`, `'q2'`=`'p50'`=`'median'`, `q3`=`'p75'`.
#'
#' \strong{method:} Possible values are
#' * `'ind_simult'` to assess polypharmacy based on the daily simultaneous consumption of medication.
#' * `'ind_stdcumul` to assess polypharmacy based on the cumulative number of distinct medications consumed over a given period of time (i.e. the standard definition).
#' * `'ind_wcumul'` to assess polypharmacy based on the cumulative number of distinct medication consumed over a given period of time, weighted by the duration of consumption of each medication.
#' * `'ind_stdcontinuous'` to assess polypharmacy based on the number of medications that are consumed both during the initial and the final period of the study period.
#' * `'ind_ucontinuous'` to assess polypharmacy based on the uninterrupted consumption of distinct medications over the study period.
#'
#' @param processed_tab Name of the table of individual drug treatments to analyze. Created by the \code{\link{data_process}} function.
#' @param stats Polypharmacy cohort descriptive statistics to calculate on every polypharmacy indicator requested. See *Details* for possible values.
#' @param method Names of the functions corresponding to each of the polypharmacy indicators to be calculated.. See *Details* for possible values.
#' @param stdconti_pdays `pdays` argument of the \code{\link{ind_stdcontinuous}} function.
#' @param simult_ind_stats `stats` argument of the \code{\link{ind_simult}} function.
#' @param simult_calendar `TRUE` or `FALSE`. `calendar` argument of the \code{\link{ind_simult}} function.
#' @param stdcumul_nPeriod `nPeriod` argument of the \code{\link{ind_stdcumul}} function.
#' @param cores The number of CPU cores to use when executing \code{\link{ind_simult}}. See \code{\link[parallel]{detectCores}}.
#'
#' @return `list` of the values returned by every function listed in the `method` argument.
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
