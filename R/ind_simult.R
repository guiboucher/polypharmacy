#' Simultaneous indicator
#'
#' Descriptive statistics on daily consumption.
#'
#' \strong{\code{individual_stats}} & \strong{\code{stats}}: Possible values are
#' * `'mean'`, `'min'`, `'median'`, `'max'`, `'sd'`;
#' * `'pX'` where *X* is a value in ]0, 100];
#' * `'q1'` = `'p25'`, `'q2'` = `'p50'` = `'median'`, `q3` = `'p75'`.
#'
#' @param processed_tab Table created by \code{\link{data_process}} function.
#' @param individual_stats Statistics to calculate for each drug user. See *Details* for possible values.
#' @param stats Statistics to calculate for each `individual_stats`. See *Details* for possible values.
#' @param calendar `TRUE` or `FALSE`. Create a table indicating the number of drugs consumed for each day for each user (`FALSE` by default).
#' @param cores The number of cores to use when executing `ind_simult()`. See \code{\link[parallel]{parallel::detectCores}}.
#'
#' @import data.table
#' @return if `calendar` is `FALSE`:
#' * `data.table` indicating each `stats` (columns) for each `individual_stats` (rows).
#'
#' if `calendar` is `TRUE`, a list of two (2) elements:
#' * `indicators`: Table described above.
#' * `calendar`: Table indicating the number of drugs consumed for each day (only for those who has at least 1 day with 1 drug consumption).
#' @encoding UTF-8
#' @export
#' @examples
#' dt_process <- data_process(
#'   Rx_deliv = data.frame(
#'     ID = c(1, 1, 1, 2, 2), Code = c('A', 'B', 'C', 'D', 'E'),
#'     Date = c('2020-01-01', '2020-01-05', '2020-01-10', '2020-01-15', '2020-01-26'),
#'     Duration = c(20, 15, 10, 5, 3)
#'   ), Rx_id = 'ID', Rx_drug_code = 'Code', Rx_drug_deliv = 'Date', Rx_deliv_dur = 'Duration',
#'   cores = 1
#' )
#' dt_simult <- ind_simult(dt_process, cores = 1)
#' dt_calendar <- ind_simult(dt_process, calendar = TRUE, cores = 1)
ind_simult <- function(
  processed_tab,
  individual_stats = c('mean', 'min', 'median', 'max'),
  stats = c('mean', 'sd', 'min', 'p5', 'p10', 'p25', 'median', 'p75', 'p90', 'p95', 'max'),
  calendar = FALSE,
  cores = parallel::detectCores(logical = FALSE)
) {

  ### Arrange arguments
  # cores
  if (!is.integer(cores)) {
    cores <- as.integer(round(cores))
  }
  if (cores < 1) {
    cores <- 1
  } else if (cores > parallel::detectCores()) {
    cores <- parallel::detectCores()
  }
  if (cores > 1) {  # register clusters for multiprocessing
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
  }
  # individual stats
  individual_stats <- sapply(individual_stats, function(x) {  # convert quarter to percentile
    if (x == "q1") {
      x <- "p25"
    } else if (x == "q2") {
      x <- "p50"
    } else if (x == "q3") {
      x <- "p75"
    }
    return(x)
  }, USE.NAMES = FALSE)
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

  ### Extract attributes
  rx_cols <- attr(processed_tab, "cols")  # initial columns name
  cohort <- attr(processed_tab, "Cohort")  # cohort ids vector

  ### processed_tab should be a data.table (if created by data_process())
  if (!is.data.table(processed_tab)) {
    setDT(processed_tab)
  }
  processed_tab <- processed_tab[, c(unlist(rx_cols), "tx_start", "tx_end"), with = FALSE]
  setnames(processed_tab, unlist(rx_cols), c("id", "drug_code"))  # rename columns

  ### Nbr consumption for each day
  if (cores == 1) {
    for (dy in as.character(lubridate::as_date(min(processed_tab$tx_star):max(processed_tab$tx_end)))) {
      processed_tab[tx_start <= dy & dy <= tx_end, (dy) := 1L]  # 1 if there is a consumption
      processed_tab[, (dy) := sum(get(dy), na.rm = TRUE), .(id)]  # indicate total drugs for the day
    }
    processed_tab[, `:=` (drug_code = NULL, tx_start = NULL, tx_end = NULL)]  # delete cols
    processed_tab <- processed_tab[processed_tab[, .I[1], .(id)]$V1]  # keep 1st row (faster than unique)
  } else {
    processed_tab <- foreach(
      ids = itertools::isplitVector(sunique(processed_tab$id), chunks = cores),
      .combine = rbind, .packages = "data.table"
    ) %dopar% {
      SD <- processed_tab[id %in% ids]  # subset data
      min_date <- min(processed_tab$tx_start)
      max_date <- max(processed_tab$tx_end)
      for (dy in as.character(lubridate::as_date(min_date:max_date))) {
        SD[tx_start <= dy & dy <= tx_end, (dy) := 1L]  # 1 if there is a consumption
        SD[, (dy) := sum(get(dy), na.rm = TRUE), .(id)]  # indicate total drugs for the day
      }
      SD[, `:=` (drug_code = NULL, tx_start = NULL, tx_end = NULL)]  # delete cols
      SD <- SD[SD[, .I[1], .(id)]$V1]  # keep 1st row (faster than unique)
      SD  # return value
    }
  }

  ### Statistics for each id
  if (cores == 1) {
    stats_ids <- processed_tab[, .(id)]
    for (stt in individual_stats) {
      if (stt %in% c("mean", "min", "median", "max", "sd")) {
        stats_ids[, (stt) := apply(processed_tab[, 2:ncol(processed_tab)], 1, get(stt))]  # calculate stats
      } else {
        stats_ids[  # quantile stats
          , (stt) := apply(processed_tab[, 2:ncol(processed_tab)], 1,
                           quantile, probs = stat_quantile_prob(stt)/100)
        ]
      }
    }
  } else {
    stats_ids <- foreach(
      ids = itertools::isplitVector(sunique(processed_tab$id), chunks = cores),
      .combine = rbind, .packages = c("data.table", "polypharmacy")
    ) %dopar% {
      SD <- processed_tab[id %in% ids]  # subset data
      SD_stats_ids <- SD[, .(id)]
      for (stt in individual_stats) {
        if (stt %in% c("mean", "min", "median", "max", "sd")) {
          SD_stats_ids[, (stt) := apply(SD[, 2:ncol(SD)], 1, get(stt))]
        } else {
          SD_stats_ids[
            , (stt) := apply(SD[, 2:ncol(SD)], 1,
                             quantile, probs = stat_quantile_prob(stt)/100)
          ]
        }
      }
      SD_stats_ids  # return value
    }
  }

  ### Add people in cohort but not in stats_id -> people without drug consumption
  ids2add <- data.table(id = cohort[!cohort %in% stats_ids$id])  # user to add
  for (col in names(stats_ids)[names(stats_ids) != "id"]) {  # zeros for each cols
    ids2add[, (col) := 0]
  }
  stats_ids <- rbind(stats_ids, ids2add)  # combine datas to have all users

  ### Stats for individual stats
  tab_stats <- vector("list", length(individual_stats))
  i <- 1L
  for (stt in individual_stats) {
    tab_stats[[i]] = data.table(individual_stats = stt)
    for (stt_ind in stats) {
      if (stt_ind %in% c("mean", "min", "median", "max", "sd")) {
        tab_stats[[i]][, (stt_ind) := get(stt_ind)(stats_ids[[stt]])]
      } else {
        tab_stats[[i]][, (stt_ind) := quantile(stats_ids[[stt]], probs = stat_quantile_prob(stt_ind)/100)]
      }
    }
    i <- i + 1L
  }
  tab_stats <- rbindlist(tab_stats)

  ### Close multicores clusters
  if (cores > 1) {
    parallel::stopCluster(cl)
  }

  ### Return values
  if (calendar) {
    return(list(
      indicators = tab_stats,
      calendar = processed_tab
    ))
  } else {
    return(tab_stats)
  }

}
