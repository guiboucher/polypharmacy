#' Assess polypharmacy based on the daily simultaneous consumption of medications
#'
#' Calculates various metrics measuring the number of distinct medications consumed daily for every individual of the study cohort over the study period and provides cohort descriptive statistics on those metrics.
#'
#' \strong{individual_stats & stats:} Possible values are
#' * `'mean'`, `'min'`, `'median'`, `'max'`, `'sd'`;
#' * `'pX'` where *X* is an integer value in ]0, 100];
#' * `'q1'`=`'p25'`, `'q2'`=`'p50'`=`'median'`, `q3`=`'p75'`.
#'
#' @param processed_tab Table of individual drug treatments over the study period. Created by \code{\link{data_process}} function.
#' @param individual_stats Descriptive statistics of daily consumption over the study period to calculate for every individual. See *Details* for possible values.
#' @param stats Cohort descriptive statistics to calculate on the polypharmacy indicator. See *Details* for possible values.
#' @param calendar `TRUE` or `FALSE`. Create a table of the number of drugs consumed everyday by every individual (`FALSE` by default).
#' @param cores The number of CPU cores to use. See \code{\link[parallel]{detectCores}}.
#'
#' @import data.table
#' @import foreach
#' @return `list`:
#' * `indic`: `data.table` indicating each `stats` (columns) for each `individual_stats` (rows).
#' * `stats_id`: `data.table` indicating each `individual_stats` for each individuals (all cohort).
#' * `min_conso`: `data.table` indicating each `stats` for the number of days where an individual consume at least `X` drugs.
#' * `calendar`: If `calendar=TRUE`, `data.table` indicating the number of drugs consumed for each day (only for individuals who has at least 1 day with 1 drug consumption).
#' @encoding UTF-8
#' @export
#' @examples
#' rx1 <- data.frame(id = c(1, 1, 2),
#'                   code = c("A", "B", "A"),
#'                   date = c("2000-01-01", "2000-01-04", "2000-01-08"),
#'                   duration = c(5, 7, 5))
#' cohort1 <- data.frame(id = as.numeric(1:3),
#'                       age = c(45, 12, 89),
#'                       sex = c("F", "F", "M"))
#' rx_proc1 <- data_process(Rx_deliv = rx1, Rx_id = "id", Rx_drug_code = "code",
#'                          Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'                          Cohort = cohort1, Cohort_id = "id",
#'                          study_start = "2000-01-01", study_end = "2000-01-15",
#'                          cores = 1)
#' dt_ind_simult <- ind_simult(rx_proc1, calendar = TRUE, cores = 1)
ind_simult <- function(
  processed_tab,
  individual_stats = c('mean', 'min', 'median', 'max'),
  stats = c('mean', 'sd', 'min', 'p5', 'p10', 'p25', 'median', 'p75', 'p90', 'p95', 'max'),
  calendar = FALSE,
  cores = parallel::detectCores()
) {

  if (is.null(processed_tab)) {
    return(NULL)
  } else {
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
    individual_stats <- vapply(individual_stats, function(x) {  # convert quarter to percentile
      if (x == "q1") {
        x <- "p25"
      } else if (x == "q2") {
        x <- "p50"
      } else if (x == "q3") {
        x <- "p75"
      }
      return(x)
    }, character(1), USE.NAMES = FALSE)
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

    ### processed_tab should be a data.table (if created by data_process())
    if (!is.data.table(processed_tab)) {
      setDT(processed_tab)
    }
    processed_tab <- processed_tab[, c(unlist(rx_cols), "tx_start", "tx_end"), with = FALSE]
    setnames(processed_tab, unlist(rx_cols), c("id", "drug_code"))  # rename columns

    ### Nbr consumption for each day
    if (cores == 1) {
      for (dy in as.character(seq(lubridate::as_date(study_dates$start), lubridate::as_date(study_dates$end), 1))) {
        processed_tab[tx_start <= dy & dy <= tx_end, (dy) := 1L]  # 1 if there is a consumption
        processed_tab[, (dy) := sum(get(dy), na.rm = TRUE), .(id)]  # total drugs for the day
      }
      processed_tab[, `:=` (drug_code = NULL, tx_start = NULL, tx_end = NULL)]  # delete cols
      processed_tab <- processed_tab[processed_tab[, .I[1], .(id)]$V1]  # keep 1st row -> faster than unique)
    } else {
      processed_tab <- foreach(
        i_D_s_ = itertools::isplitVector(sunique(processed_tab$id), chunks = cores),
        .combine = rbind, .packages = "data.table"
      ) %dopar% {
        SD <- processed_tab[id %in% i_D_s_]  # subset data
        for (dy in as.character(seq(lubridate::as_date(study_dates$start), lubridate::as_date(study_dates$end), 1))) {
          SD[tx_start <= dy & dy <= tx_end, (dy) := 1L]  # 1 if there is a consumption
          SD[, (dy) := sum(get(dy), na.rm = TRUE), .(id)]  # indicate total drugs for the day
        }
        SD[, `:=` (drug_code = NULL, tx_start = NULL, tx_end = NULL)]  # delete cols
        SD <- SD[SD[, .I[1], .(id)]$V1]  # keep 1st row (faster than unique)
        SD  # return value
      }
    }

    ### Add people that are in cohort but not in processed_tab -> no consumption
    ids2add <- data.table(id = cohort[!cohort %in% processed_tab$id])  # user to add
    if (nrow(ids2add)) {
      processed_tab <- rbind(processed_tab, ids2add, fill = TRUE)
      setkey(processed_tab, id)
      for (col in names(processed_tab)[names(processed_tab) != "id"]) {
        set(processed_tab, which(is.na(processed_tab[[col]])), col, 0L)
      }
    }

    ### Minimal consumption
    max_conso <- max(apply(processed_tab[, !"id", with = FALSE], 1, max))
    min_conso <- data.table()
    for (i in 1:max_conso) {
      min_conso_SD <- data.table(min_conso = paste0(">= ", i))
      n_conso <- apply(processed_tab[, !"id", with = FALSE], 1, function(d) {sum(d >= i)})
      for (stt in stats) {
        if (stt %in% c("mean", "min", "median", "max", "sd")) {
          min_conso_SD[, (stt) := get(stt)(n_conso)]  # calculate stats
        } else {
          min_conso_SD[, (stt) := quantile(n_conso, probs = stat_quantile_prob(stt)/100)]
        }
      }
      min_conso <- rbind(min_conso, min_conso_SD)
    }
    min_conso[, cohort := length(cohort)]

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
        i_D_s_ = itertools::isplitVector(sunique(processed_tab$id), chunks = cores),
        .combine = rbind, .packages = c("data.table", "polypharmacy")
      ) %dopar% {
        SD <- processed_tab[id %in% i_D_s_]  # subset data
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

    ### Stats for individual stats
    tab_stat <- vector("list", length(individual_stats))
    i <- 1L
    for (stt in individual_stats) {
      tab_stat[[i]] = data.table(individual_stats = stt)
      for (stt_ind in stats) {
        if (stt_ind %in% c("mean", "min", "median", "max", "sd")) {
          tab_stat[[i]][, (stt_ind) := get(stt_ind)(stats_ids[[stt]])]
        } else {
          tab_stat[[i]][
            , (stt_ind) := quantile(stats_ids[[stt]],
                                    probs = stat_quantile_prob(stt_ind) / 100)
          ]
        }
      }
      i <- i + 1L
    }
    tab_stat <- rbindlist(tab_stat)
    tab_stat[, cohort := length(cohort)]  # nbr people

    ### Close multicores clusters
    if (cores > 1) {
      parallel::stopCluster(cl)
    }

    ### Return values
    retur <- list(
      indic = tab_stat,
      stats_ids = stats_ids,
      min_conso = min_conso
    )
    if (calendar) {
      retur[["calendar"]] <- processed_tab
    }

    return(retur)

  }

}
