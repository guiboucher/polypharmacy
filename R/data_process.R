#' Create the table of the drug treatments
#'
#' Reads a table of successive drug delivery records (usually extracted from a pharmacy or a health insurance information system) and creates the table required for the calculation of the polypharmacy indicators by applying various user-defined arguments, incorporating hospital stays into the treatment periods and reconstruct continuous treatment periods by merging quasi continuous and/or overlapping drugs deliveries.
#'
#' **Variables**:
#' * `Rx_id`, `Cohort_id` and `Hosp_id` columns must be of the same class (integer, numeric, character, ...).
#' * `Rx_drug_deliv`, `Hosp_admis` and `Hosp_discharge` can be 1) `as.Date('yyyy-mm-dd')`, 2) `as.character('yyyy-mm-dd')` or 3) `as.integer()` where 0 is January 1\ifelse{html}{\out{<sup>st</sup>}}{\eqn{^{st}}}, 1970.
#'
#' **Arguments**:
#' * `study_start` and `study_end` can be 1) `as.Date('yyyy-mm-dd')`, 2) `as.character('yyyy-mm-dd')` or 3) `as.integer()` where 0 is January 1\ifelse{html}{\out{<sup>st</sup>}}{\eqn{^{st}}}, 1970.
#'
#' **Hospital stays**:\cr
#' Drug availability is assumed to continue during the hospital stay as it is on the day prior admission. The patient is assumed to resume the consumption of the drugs delivered by community pharmacists (as recorded in `Rx_deliv`) the day after `hosp_discharge`.\cr
#' Grace period is always zero (0) for hospital stays.
#'
#' **Run-in period**:\cr
#' A run-in period is necessary to account for the medications that are available to the individuals on the day of `study_start`. It is recommended to include a run-in period of about 6 months (e.g. 7 months to account for possible delays) as some drugs are delivered for up to 6 months at once.
#'
#' **Grace period**:\cr
#' The grace period is used to determine if two successive deliveries can be considered as a continuous treatment even if there is a gap of several days for which no treatment is apparently available. Two successive deliveries of an identical drug are considered part of a single continuous treatment if the next delivery doesn’t occur more than `grace_cst` + (`grace_fctr` × `Rx_deliv_dur`) days after the end of the latest drug delivery. The availability of extra drugs accumulated over the successive deliveries is accounted for prior to evaluating the duration of the gap between deliveries.
#'
#' **Performance**\cr
#' For better performance, date columns are converted to integer numbers.
#'
#' **...**\cr
#' `verif_cols=FALSE` : For better performance, you can avoid columns class checking with `verif_cols=FALSE`. **Not recommended**.
#'
#' @param Rx_deliv Name of the table listing all prescription drugs deliveries including the run-in period. See *Details*.
#' @param Rx_id Column name of `Rx_deliv` containing individual unique identifier (any format).
#' @param Rx_drug_code Column name of `Rx_deliv` that contains the drug unique identifier (any format).
#' @param Rx_drug_deliv Column name of `Rx_deliv` that contains the dates of the drug delivery (Date format, see *Details*).
#' @param Rx_deliv_dur Column name of `Rx_deliv` that contains the duration of the delivery (integer number).
#' @param Cohort Name of the table providing the unique identifiers of the study cohort. Only the ids listed in both the `Cohort` and the `Rx_deliv` tables will be returned. if `Cohort = NULL`, all ids of the `Rx_deliv` table will be returned.
#' @param Cohort_id Column name of `Cohort` containing individual’s unique identifiers (same format as `Rx_id`). If `Cohort` is not `NULL` and `Cohort_id` is `NULL`, `Cohort_id` will take the same value as `Rx_id`.
#' @param Hosp_stays Name of the table listing all hospital stays. (see *Details* for possible format).
#' @param Hosp_id Column name of `Hosp_stays` containing individual’s unique identifier (same format as `Rx_id`). If `Hosp_stays` is not `NULL` and `Hosp_id` is `NULL`, `Hosp_id` will take the same value as `Rx_id`.
#' @param Hosp_admis Column name of `Hosp_stays` that contains the date of admission in hospital (Date format, see *Details*).
#' @param Hosp_discharge Column name of Hosp_stays that contains the date of discharge from hospital (Date format, see *Details*).
#' @param study_start,study_end Defines the first and last day of the study period for which the polypharmacy indicator(s) need to be calculated. All treatment periods prior to `study_start` and past `study_end` are not transcribed into the result table (Date format, see *Details*).
#' @param grace_fctr,grace_cst Numbers \eqn{\ge} 0. Two types of grace periods can be applied. One is proportional to the treatment duration of the latest delivery (`grace_fctr`) and the other is a constant number of days (`grace_cst`).
#' @param max_reserve An integer number \eqn{\ge} 0 or `NULL`. Longest treatment duration, in days, that can be stored from successive overlapping deliveries. When `max_reserve = NULL` no limit is applied. When `max_reserve = 0` no accumulation of extra treatment duration is accounted for.
#' @param cores The number of cores to use when executing `data_process()`. See \code{\link[parallel]{detectCores}}.
#' @param ... Additional arguments. See *Details*. Should not be used.
#'
#' @return `data.table` with four (4) variables:
#' * The individual unique identifier which name is defined by `Rx_id`.
#' * The drug unique identifier which name is defined by `Rx_drug_code`.
#' * `tx_start`: The date of initiation of the reconstructed continued treatment (format as date).
#' * `tx_end`: The date of the last day of the reconstructed continued treatment (format as date).
#' @import data.table
#' @import foreach
#' @encoding UTF-8
#' @export
#' @examples
#' ### Standard evaluation
#' data_process(
#'   Rx_deliv = sample_Rx_unprocessed, Rx_id = "id", Rx_drug_code = "code",
#'   Rx_drug_deliv = "start", Rx_deliv_dur = "duration",
#'   cores = 1L
#' )
#'
#' ### Hospitalisation stays
#' rx1 <- data.frame(
#'   id = c(1L, 3:8),
#'   code = LETTERS[c(1, 3:8)],
#'   date = as.Date(c("2001-01-15", "2003-03-15", "2004-04-15", "2005-05-15",
#'                    "2006-06-15", "2007-07-15", "2008-08-15")),
#'   duration = 10L
#' )
#' hosp1 <- data.frame(
#'   ID = 3:8,
#'   ADM = as.Date(c("2003-03-10", "2004-04-25", "2005-05-12",
#'                   "2006-06-20", "2007-07-26", "2008-08-01")),
#'   DEP = as.Date(c("2003-03-14", "2004-04-30", "2005-05-17",
#'                   "2006-06-30", "2007-07-30", "2008-08-13"))
#' )
#' data_process(
#'   Rx_deliv = rx1, Rx_id = "id", Rx_drug_code = "code",
#'   Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'   Hosp_stays = hosp1, Hosp_id = "ID", Hosp_admis = "ADM", Hosp_discharge = "DEP",
#'   study_start = "2001-01-01", study_end = "2008-12-31",
#'   cores = 1L
#' )
#' # Many drug codes
#' rx2 <- data.frame(
#'   id = 1L,
#'   code = c(111L, 222L, 222L, 333L, 444L),
#'   date = as.Date(c("2001-01-15", "2002-02-15", "2002-03-01", "2004-04-07", "2004-05-05")),
#'   duration = as.integer(c(10, 10, 10, 30, 10))
#' )
#' hosp2 <- data.frame(
#'   id = 1L,
#'   adm = as.Date(c("2000-01-01", "2000-01-15", "2001-01-01", "2002-02-23", "2004-04-15")),
#'   dep = as.Date(c("2000-01-31", "2000-01-31", "2001-01-10", "2002-02-28", "2004-05-15"))
#' )
#' data_process(
#'   Rx_deliv = rx2, Rx_id = "id", Rx_drug_code = "code",
#'   Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'   Hosp_stays = hosp2, Hosp_id = "id", Hosp_admis = "adm", Hosp_discharge = "dep",
#'   study_start = "2001-01-01", study_end = "2008-12-31",
#'   cores = 1L
#' )
#'
#' ### Study dates - start and end
#' rx3 <- data.frame(id = 1:3,
#'                   code = "A",
#'                   date = as.Date(c("2020-01-01", "2020-06-06", "2020-12-22")),
#'                   duration = 10L)
#' # NULLs
#' data_process(Rx_deliv = rx3, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              study_start = NULL, study_end = NULL,
#'              cores = 1)
#' # Not NULLs
#' data_process(Rx_deliv = rx3, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              study_start = "2020-06-10", study_end = NULL,
#'              cores = 1)
#' data_process(Rx_deliv = rx3, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              study_start = NULL, study_end = "2020-06-10",
#'              cores = 1)
#' data_process(Rx_deliv = rx3, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              study_start = "2020-01-05", study_end = "2020-12-25",
#'              cores = 1)
#'
#' ### Grace factor
#' rx4 <- data.frame(id = c(rep(1, 3), rep(2, 3)),
#'                   code = "A",
#'                   date = as.Date(c("2000-01-01", "2000-01-17", "2000-01-31",
#'                                    "2000-06-01", "2000-06-23", "2000-07-16")),
#'                   duration = as.integer(c(10, 10, 10, 15, 15, 15)))
#' # 50% of duration
#' data_process(Rx_deliv = rx4, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              grace_fctr = 0.5,
#'              cores = 1)
#' # 0% of duration
#' data_process(Rx_deliv = rx4, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              grace_fctr = 0,
#'              cores = 1)
#'
#' ### Grace constant
#' rx5 <- data.frame(id = 1,
#'                   code = "A",
#'                   date = as.Date(c("2000-01-01", "2000-01-14", "2000-01-25")),
#'                   duration = as.integer(c(10, 10, 6)))
#' # 2 days
#' data_process(Rx_deliv = rx5, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              grace_fctr = 0, grace_cst = 2,
#'              cores = 1)
#' # 3 days
#' data_process(Rx_deliv = rx5, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              grace_fctr = 0, grace_cst = 3,
#'              cores = 1)
#'
#' ### Max reserve
#' rx6 <- data.frame(id = as.integer(c(1, 1, 3, 3, 3, 5, 5)),
#'                   code = "A",
#'                   date = as.Date(c("2000-01-01", "2000-01-31",
#'                                    "2000-03-03", "2000-03-15", "2000-03-30",
#'                                    "2000-05-05", "2000-05-05")),
#'                   duration = as.integer(c(30, 30,
#'                                           30, 30, 30,
#'                                           90, 90)))
#' # 0 days
#' data_process(Rx_deliv = rx6, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              study_start = NULL, study_end = "2000-12-31",
#'              grace_fctr = 0, grace_cst = 0,
#'              max_reserve = 0,
#'              cores = 1)
#' # 60 days
#' data_process(Rx_deliv = rx6, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              study_start = NULL, study_end = "2000-12-31",
#'              grace_fctr = 0, grace_cst = 0,
#'              max_reserve = 60,
#'              cores = 1)
#' # Inf days
#' data_process(Rx_deliv = rx6, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              study_start = NULL, study_end = "2000-12-31",
#'              grace_fctr = 0, grace_cst = 0,
#'              max_reserve = NULL,
#'              cores = 1)
#'
#' ### Combine Hospital stays and Grace factor
#' rx7 <- data.frame(id = c(1L, 1L, 1L, 2L),
#'                   code = "A",
#'                   date = c("2000-01-01", "2000-02-20", "2000-04-11", "2002-02-02"),
#'                   duration = as.integer(c(30, 30, 30, 15)))
#' hosp7 <- data.frame(id = 1L,
#'                     adm = c("2000-01-11", "2000-02-21"),
#'                     dep = c("2000-01-15", "2000-02-25"))
#' data_process(Rx_deliv = rx7, Rx_id = "id", Rx_drug_code = "code",
#'              Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
#'              Hosp_stays = hosp7, Hosp_id = "id",
#'              Hosp_admis = "adm", Hosp_discharge = "dep",
#'              study_start = "2000-01-01", study_end = "2002-12-31",
#'              grace_fctr = 0.5, grace_cst = 0, max_reserve = NULL,
#'              cores = 1)
data_process <- function(
  Rx_deliv, Rx_id, Rx_drug_code, Rx_drug_deliv, Rx_deliv_dur,
  Cohort = NULL, Cohort_id = NULL,
  Hosp_stays = NULL, Hosp_id = NULL, Hosp_admis = NULL, Hosp_discharge = NULL,
  study_start = NULL, study_end = NULL,
  grace_fctr = 0.5, grace_cst = 0, max_reserve = NULL,
  cores = parallel::detectCores(logical = FALSE),
  ...
) {

  ### Arrange arguments
  dot_args <- list(...)
  if ("verif_cols" %in% names(dot_args)) {
    verif_cols <- dot_args$verif_cols
  } else {
    verif_cols <- TRUE
  }

  ### Argument check - stop if any error
  data_process.verif_args(
    Rx_deliv, Rx_id, Rx_drug_code, Rx_drug_deliv, Rx_deliv_dur,
    Cohort, Cohort_id,
    Hosp_stays, Hosp_id, Hosp_admis, Hosp_discharge,
    study_start, study_end,
    grace_fctr, grace_cst, max_reserve,
    verif_cols, cores
  )

  ### Arrange datas
  # Rx_deliv
  if (!is.data.table(Rx_deliv)) {
    setDT(Rx_deliv)  # convert to data.table
  }
  Rx_deliv <- Rx_deliv[, c(Rx_id, Rx_drug_code, Rx_drug_deliv, Rx_deliv_dur), with = FALSE]  #cols selection
  # Cohort_id & Hosp_id = Rx_id if NULL
  if (!is.null(Cohort) && is.null(Cohort_id)) {
    Cohort_id <- Rx_id
  }
  if (!is.null(Hosp_stays) && is.null(Hosp_id)) {
    Hosp_id <- Rx_id
  }

  ### Arrange values
  # cores
  if (!is.integer(cores)) {
    cores <- as.integer(round(cores))
  }
  if (cores < 1) {
    cores <- 1
  } else if (cores > parallel::detectCores()) {
    cores <- parallel::detectCores()
  }
  if (uniqueN(Rx_deliv[[Rx_id]]) < cores) {
    cores <- uniqueN(Rx_deliv[[Rx_id]])
  }

  ### Create Cohort if necessary
  if (is.null(Cohort)) {
    cohort_chunk <- sunique(Rx_deliv[[Rx_id]])
  } else {
    cohort_chunk <- sunique(Cohort[[Cohort_id]])
  }

  ### Apply 1 core or multicores data_process function
  if (cores == 1) {
    dt <- data_process.1_core(
      Rx_deliv, Rx_id, Rx_drug_code, Rx_drug_deliv, Rx_deliv_dur,
      Cohort, Cohort_id,
      Hosp_stays, Hosp_id, Hosp_admis, Hosp_discharge,
      study_start, study_end,
      grace_fctr, grace_cst, max_reserve
    )
  } else {
    if (is.null(study_start)) {
      study_start_multicore <- min(Rx_deliv[[Rx_drug_deliv]])
    } else {
      study_start_multicore <- study_start
    }
    if (is.null(study_end)) {
      study_end_multicore <- max(Rx_deliv[[Rx_drug_deliv]] + Rx_deliv[[Rx_deliv_dur]] - 1)
    } else {
      study_end_multicore <- study_end
    }
    # Apply multi cores function
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    dt <- foreach(
      i_D_s_ = itertools::isplitVector(sunique(Rx_deliv[[Rx_id]]), chunks = cores),
      .packages = c("data.table", "polypharmacy"),
      .combine = rbind
    ) %dopar% {
      SD <- Rx_deliv[get(Rx_id) %in% i_D_s_]
      SD <- data_process.1_core(
        SD, Rx_id, Rx_drug_code, Rx_drug_deliv, Rx_deliv_dur,
        Cohort, Cohort_id,
        Hosp_stays, Hosp_id, Hosp_admis, Hosp_discharge,
        study_start_multicore, study_end_multicore,
        grace_fctr, grace_cst, max_reserve
      )
      SD  # value to return
    }
    parallel::stopCluster(cl)
  }

  if (!is.null(dt) && nrow(dt)) {
    ### Attributes
    attr(dt, "cols") <- list(Rx_id = Rx_id, Rx_drug_code = Rx_drug_code)  # initial column names
    attr(dt, "Cohort") <- cohort_chunk  # vector with ids number
    attr(dt, "study_dates") <- list(start = study_start, end = study_end)

    setkeyv(dt, c(Rx_id, Rx_drug_code, "tx_start"))  # order
    return(dt)

  } else {
    return(NULL)
  }

}


#' @title Verification
#' @description Arguments verification for \code{\link{data_process}}.
#' @inheritParams data_process
#' @return Messages, warnings or errors
#' @keywords internal
#' @encoding UTF-8
data_process.verif_args <- function(
  Rx_deliv, Rx_id, Rx_drug_code, Rx_drug_deliv, Rx_deliv_dur, Cohort, Cohort_id,
  Hosp_stays, Hosp_id, Hosp_admis, Hosp_discharge, study_start, study_end,
  grace_fctr, grace_cst, max_reserve, verif_cols, cores
) {

  check <- newArgCheck()

  ### 1) Class of arguments

  # Rx_deliv
  if (!is.data.frame(Rx_deliv)) {
    addError("Rx_deliv must be a data.frame.", check)
  }
  # Rx_id, Rx_drug_code, Rx_drug_deliv, Rx_deliv_dur
  for (var in c("Rx_id", "Rx_drug_code", "Rx_drug_deliv", "Rx_deliv_dur")) {
    if (!is.character(get(var))) {
      addError(paste0(var," must be a character value."), check)
    }
    if (length(unique(get(var))) != 1) {
      addError(paste0(var," must be a single value."), check)
    }
  }
  # Cohort
  if (!is.null(Cohort) && !is.data.frame(Cohort)) {
    addError("Cohort must be a data.frame if not NULL.", check)
  }
  # Cohort_id
  if (!is.null(Cohort)) {
    if (!is.character(Cohort_id)) {
      addError("Cohort_id must be a character value if Cohort is not NULL.", check)
    }
    if (length(unique(Cohort_id)) != 1) {
      addError("Cohort_id must be a single value.", check)
    }
  }
  # Hosp_stays
  if (!is.null(Hosp_stays) && !is.data.frame(Hosp_stays)) {
    addError("Hosp_stays must be a data.frame if not NULL.", check)
  }
  # Hosp_id, Hosp_admis, Hosp_discharge
  if (!is.null(Hosp_stays)) {
    for (var in c("Hosp_id", "Hosp_admis", "Hosp_discharge")) {
      if (!is.character(get(var))) {
        addError(paste0(var," must be a character value."), check)
      }
      if (length(unique(get(var))) != 1) {
        addError(paste0(var," must be a single value."), check)
      }
    }
  }
  # study_start, study_end
  if (!is.null(study_start)) {
    if (is.na(lubridate::as_date(study_start))) {
      addError(paste0(
        "study_start must be a character value ('yyyy-mm-dd'), a Date value (as.Date('yyyy-mm-dd')) or a numeric value where 1970-01-01=0."),
        check)
    }
    if (length(unique(study_start)) != 1) {
      addError(paste0("study_start must be a single value."), check)
    }
  }
  if (!is.null(study_end)) {
    if (is.na(lubridate::as_date(study_end))) {
      addError(paste0(
        "study_end must be a character value ('yyyy-mm-dd'), a Date value (as.Date('yyyy-mm-dd')) or a numeric value where 1970-01-01=0."),
        check)
    }
    if (length(unique(study_end)) != 1) {
      addError(paste0("study_end must be a single value."), check)
    }
  }
  # grace_fctr, grace_cst
  for (arg in c("grace_fctr", "grace_cst")) {
    if (is.numeric(get(arg)) && get(arg) < 0) {
      addError(paste0(arg," must be greater or equal than zero (0)."), check)
    } else if (!is.numeric(get(arg))) {
      addError(paste0(arg," must be a numeric value."), check)
    }
  }
  # max_reserve
  if (!is.null(max_reserve)) {
    if (!is.numeric(max_reserve)) {
      addError("max_reserve must be a numeric value.", check)
    } else {
      if (max_reserve < 0) {
        addError("max_reserve must be greater or equal to zero (0).", check)
      }
    }
    if (length(unique(max_reserve)) != 1) {
      addError("max_reserve must be a single value.", check)
    }
  }
  # verif_cols
  if (!is.logical(verif_cols)) {
    addError("verif_cols must be a logical value.", check)
  }
  if (length(unique(verif_cols)) != 1) {
    addError("verif_cols must be a single value.", check)
  }
  # cores
  if (!is.numeric(cores)) {
    addError("cores must be a numeric value.", check)
  }

  finishArgCheck(check)

  ### ***Rx_id NEVER!!! = "i_D_s_": problem with data.table where the column can't
  ### have the same name of a variable
  if (Rx_id == "i_D_s_") {
    addError("Rx_id can't be equal to 'i_D_s_'", check)
  }

  ### 2) Are the columns exist?

  # Rx_id, Rx_drug_code, Rx_drug_deliv, Rx_deliv_dur
  for (col in c("Rx_id", "Rx_drug_code", "Rx_drug_deliv", "Rx_deliv_dur")) {
    if (!get(col) %in% names(Rx_deliv)) {
      addError(paste0(get(col)," (",col,") is not a column in Rx_deliv."),
               check)
    }
  }
  # Cohort_id
  if (!is.null(Cohort) && !Cohort_id %in% names(Cohort)) {
    addError(paste0(Cohort_id," (Cohort_id) is not a column in Cohort"),
             check)
  }
  # Hosp_id, Hosp_admis, Hosp_discharge
  if (!is.null(Hosp_stays)) {
    for (col in c("Hosp_id", "Hosp_admis", "Hosp_discharge")) {
      if (!get(col) %in% names(Hosp_stays)) {
        addError(paste0(get(col)," (",col,") is not a column in Hosp_stays."), check)
      }
    }
  }

  finishArgCheck(check)


  ### 3) Cohort ids unique?
  if (!is.null(Cohort) && nrow(Cohort) != uniqueN(Cohort[[Cohort_id]])) {
    addError(paste0("Cohort must be a unique list of identification codes:\n",
                    "nrow(Cohort)               : ",nrow(Cohort),"\n",
                    "uniqueN(Cohort[[Cohort_id]]: ",uniqueN(Cohort[[Cohort_id]])),
             check)
  }
  ### 3) Column classes & not supported values
  if (verif_cols) {  # can take some time... so optional

    # Rx_deliv
    # Rx_id
    if (anyNA(Rx_deliv[[Rx_id]])) {
      addError(paste0(Rx_id," can't contains NAs."), check)
    }
    # Rx_drug_code
    if (anyNA(Rx_deliv[[Rx_drug_code]])) {
      addError(paste0(Rx_drug_code," can't contains NAs."), check)
    }
    # Rx_drug_deliv
    nNAs <- sum(is.na(Rx_deliv[[Rx_drug_deliv]]))  # are there NAs?
    if (nNAs) {
      addError(paste0(Rx_drug_deliv," column (Rx_drug_deliv) can't contains NAs."), check)
    }
    if (nNAs != sum(is.na(lubridate::as_date(Rx_deliv[[Rx_drug_deliv]])))) {  # is it a possible date format?
      addError(paste0(
        Rx_drug_deliv," column (Rx_drug_deliv) must be character ('yyyy-mm-dd'), Date (as.Date('yyyy-mm-dd')) or numeric where 1970-01-01 = 0."),
        check)
    }
    # Rx_deliv_dur
    if (anyNA(Rx_deliv[[Rx_deliv_dur]])) {
      addError(paste0(Rx_deliv_dur," column (Rx_deliv_dur) can't contains NAs."), check)
    }
    if (!is.numeric(Rx_deliv[[Rx_deliv_dur]])) {
      addError(paste0(Rx_deliv_dur," column (Rx_deliv_dur) must be numeric."), check)
    }
    # Cohort
    if (!is.null(Cohort)) {
      # Cohort_id
      if (anyNA(Cohort[[Cohort_id]])) {
        addError(paste0(Cohort_id," column (Cohort_id) can't contains NAs."), check)
      }
      if (class(Cohort[[Cohort_id]]) != class(Rx_deliv[[Rx_id]])) {
        addError(paste0(
          Cohort_id," column (Cohort_id, class: ",class(Cohort[[Cohort_id]]),") ",
          "must have the same class as ",
          Rx_id," column (Rx_id, class: ",class(Rx_deliv[[Rx_id]]),")."
        ), check)
      }
    }
    # Hosp_stays
    if (!is.null(Hosp_stays)) {
      # Hosp_id
      if (anyNA(Hosp_stays[[Hosp_id]])) {
        addError(paste0(Hosp_id," column (Hosp_id) can't contains NAs."), check)
      }
      if (class(Hosp_stays[[Hosp_id]]) != class(Rx_deliv[[Rx_id]])) {
        addError(paste0(
          Hosp_id," column (Hosp_id, class: ",class(Hosp_stays[[Hosp_id]]),") ",
          "must have the same class as ",
          Rx_id," column (Rx_id, class: ",class(Rx_deliv[[Rx_id]]),")."
        ), check)
      }
      # Hosp_admis, Hosp_discharge
      for (col in c("Hosp_admis", "Hosp_discharge")) {
        nNAs <- sum(is.na(Hosp_stays[[get(col)]]))  # are there NAs?
        if (nNAs) {
          addError(paste0(get(col)," column (",col,") can't contains NAs."), check)
        }
        if (nNAs != sum(is.na(lubridate::as_date(Hosp_stays[[get(col)]])))) {  # is it a possible date format?
          addError(paste0(
            get(col)," column (",col,") must be character ('yyyy-mm-dd'), Date (as.Date('yyyy-mm-dd')) or numeric where 1970-01-01 = 0."),
            check)
        }
      }
    }

    finishArgCheck(check)

  }

}

#' @title Data Process
#' @description \code{\link{data_process}} but with only 1 core. To use in the multicores process.
#' @inheritParams data_process
#' @inherit data_process return
#' @keywords internal
#' @import data.table
#' @encoding UTF-8
data_process.1_core <- function(
  Rx_deliv, Rx_id, Rx_drug_code, Rx_drug_deliv, Rx_deliv_dur,
  Cohort = NULL, Cohort_id = NULL,
  Hosp_stays = NULL, Hosp_id = NULL, Hosp_admis = NULL, Hosp_discharge = NULL,
  study_start = NULL, study_end = NULL,
  grace_fctr = 0.5, grace_cst = 0, max_reserve = NULL
) {

  ## Initial arguments & arrange them
  # Initial names
  rx_names <- c(Rx_id, Rx_drug_code)
  # Cohort & Hosp_stays
  if (!is.null(Cohort) && is.null(Cohort_id)) {
    Cohort_id <- Rx_id
  }
  if (!is.null(Hosp_stays) && is.null(Hosp_id)) {
    Hosp_id <- Rx_id
  }


  ## Arrange datas
  # Cohort - keep id vector only. Future use: filter Rx_deliv
  if (!is.null(Cohort)) {
    Cohort <- sort(Cohort[[Cohort_id]])  # cohort study
  }

  # Rx_deliv
  if (!is.data.table(Rx_deliv)) {
    setDT(Rx_deliv)  # convert to data.table
  }
  # Cols selection
  Rx_deliv <- Rx_deliv[, c(Rx_id, Rx_drug_code, Rx_drug_deliv, Rx_deliv_dur), with = FALSE]  #cols selection
  setnames(  # rename cols
    Rx_deliv, c(Rx_id, Rx_drug_code, Rx_drug_deliv, Rx_deliv_dur),
    c("id", "drug_code", "tx_start", "drug_duration")
  )


  if (!is.null(Cohort)) {
    Rx_deliv <- Rx_deliv[id %in% Cohort]  # select ids in Cohort
    if (!nrow(Rx_deliv)) {
      # Return NULL if nrow(Rx_deliv)=0
      message("Rx_deliv[Rx_id %in% Cohort[[Cohort_id]]] return no observations.")
      return(NULL)
    }
  }

  # Convert drug_date and drug_duration as integer for better performances
  if (is.character(Rx_deliv$tx_start)) {
    Rx_deliv[, tx_start := lubridate::as_date(tx_start)]
  }
  if (!is.integer(Rx_deliv$tx_start)) {
    Rx_deliv[, tx_start := as.integer(tx_start)]
  }
  if (!is.integer(Rx_deliv$drug_duration)) {
    Rx_deliv[, drug_duration := as.integer(drug_duration)]
  }
  # Create tx_end
  Rx_deliv[, tx_end := tx_start + drug_duration - 1L]

  # Convert study dates as integer for better performances
  for (var in c("study_start", "study_end")) {
    if (is.null(get(var))) {
      if (var == "study_start") {
        assign(var, as.integer(lubridate::as_date(min(Rx_deliv$tx_start))))
      } else {
        assign(var, as.integer(lubridate::as_date(max(Rx_deliv$tx_end))))
      }
    } else {
      if (is.character(get(var))) {  # convert to Date
        assign(var, lubridate::as_date(get(var)))
      }
      if (!is.integer(get(var))) {  # convert to integer
        assign(var, as.integer(get(var)))
      }
    }
  }
  # Remove tx_start > study_end, will not be considered in study
  Rx_deliv <- Rx_deliv[tx_start <= study_end]

  if (nrow(Rx_deliv)) {

    setkey(Rx_deliv, id, drug_code, tx_start)  # order

    ### Grace period
    Rx_deliv[, grace_per := grace_fctr * drug_duration + grace_cst]

    ### Hosp_stays
    if (!is.null(Hosp_stays)) {
      if (!is.data.table(Hosp_stays)) {
        setDT(Hosp_stays)
      }
      Hosp_stays <- Hosp_stays[, c(Hosp_id, Hosp_admis, Hosp_discharge), with = FALSE]  # cols selection
      setnames(Hosp_stays, names(Hosp_stays), c("id", "tx_start", "tx_end"))
      if (!is.null(Cohort)) {
        Hosp_stays <- Hosp_stays[id %in% Cohort]  # select ids in Cohort
        if (!nrow(Hosp_stays)) {
          # Send message indicating that no observations after filter
          message("Hosp_stays[Hops_id %in% Cohort[[Cohort_id]]] return no observations.")
        }
      }
      # Convert dates as integer for better performances
      for (col in c("tx_start", "tx_end")) {
        if (is.character(Hosp_stays[[col]])) {
          Hosp_stays[, (col) := lubridate::as_date(get(col))]
        }
        if (!is.integer(Hosp_stays[[col]])) {
          Hosp_stays[, (col) := as.integer(get(col))]
        }
      }
      setkey(Hosp_stays, id, tx_start)

      Rx_deliv[, hosp := FALSE]  # indicate that rows are not hosp stays

      ### Keep Hospit stays that are contiguous or overlap a Rx period
      # Combine time periods that overlap or are contiguous to other hosp stays
      idx <- rmNA(Hosp_stays[, .I[.N > 1], .(id)]$V1)
      if (length(idx)) {
        Hosp_stays[idx, diff := tx_start - shift(tx_end), .(id)]  # days difference between start[i] and end[i-1]
        Hosp_stays[is.na(diff), diff := 0L]
        Hosp_stays[, per := 0L][diff > 1, per := 1L]  # 0: same time period, 1: new time period
        Hosp_stays[, per := cumsum(per) + 1L, .(id)]  # time period from 1 to n
        Hosp_stays <- Hosp_stays[  # combine all same time period number
          , .(tx_start = min(tx_start),
              tx_end = max(tx_end)),
          .(id, per)
        ][, per := NULL]  # delete col time period number
      }
      Hosp_stays[, drug_duration := tx_end - tx_start + 1L]  # need drug duration for same format as Rx_deliv
      Rx_days <- Rx_deliv[, .(id, drug_code, tx_start, tx_end)]
      Rx_days[, `:=` (tx_start = tx_start - 1L,
                      tx_end = tx_end + 1L)]
      Rx_days[, rx_date := lapply(1:nrow(Rx_days), function(x) {
        Rx_days[[x, "tx_start"]]:Rx_days[[x, "tx_end"]]
      })]
      Rx_days <- Rx_days[, .(rx_date = list(unique(unlist(rx_date)))), .(id, drug_code)]
      Hosp_stays <- Rx_days[Hosp_stays, on = .(id), allow.cartesian = TRUE]
      Hosp_stays[, hosp_date := lapply(1:nrow(Hosp_stays), function(x) {
        Hosp_stays[[x, "tx_start"]]:Hosp_stays[[x, "tx_end"]]
      })]
      Hosp_stays[, is_present := lapply(1:nrow(Hosp_stays), function(x) {
        any(Hosp_stays[[x, "hosp_date"]] %in% Hosp_stays[[x, "rx_date"]])
      })]
      Hosp_stays <- Hosp_stays[
        is_present == TRUE,
        .(id, drug_code, tx_start, tx_end, drug_duration, grace_per = 0L, hosp = TRUE)
      ]

      Rx_deliv <- rbind(Rx_deliv, Hosp_stays)
      setkey(Rx_deliv, id, drug_code, tx_start, hosp)

      ### Insert hosp=TRUE in Rx->hosp=FALSE
      eval_again <- TRUE
      while (eval_again) {
        eval_again <- FALSE

        # Insert the 1st row hosp==TRUE to the next one, hosp==FALSE
        idx <- intersect(  # detect first row where hosp = TRUE
          Rx_deliv[, .I [1], .(id, drug_code)]$V1,
          Rx_deliv[, .I[hosp == TRUE]]
        )
        if (length(idx)) {
          Rx_deliv[
            sunique(c(idx, idx + 1)),
            by_hosp := cumsum(hosp),
            .(id, drug_code)
          ]
          Rx_deliv[
            sort(c(idx, idx + 1)),
            `:=` (tx_start = min(tx_start),
                  drug_duration = sum(drug_duration),
                  grace_per = max(grace_per)),
            .(id, drug_code, by_hosp)
          ]
          Rx_deliv <- Rx_deliv[!idx]
          Rx_deliv[
            , `:=` (tx_end = tx_start + drug_duration - 1L,
                    by_hosp = NULL)
          ]
        }

        # Insert hosp=TRUE in the previous row, hosp=FALSE
        Rx_deliv[
          Rx_deliv[, .I[.N > 1], .(id, drug_code)]$V1,
          diff := tx_start - shift(tx_end),
          .(id, drug_code)
        ][is.na(diff), diff := 0L]
        idx <- rmNA(Rx_deliv[, .I[hosp == TRUE & shift(hosp) == FALSE & diff <= 1], .(id, drug_code)]$V1)
        if (length(idx)) {
          while (length(idx)) {
            Rx_deliv[
              sort(c(idx, idx - 1)),
              by_hospit := rep(1:(length(idx)), each = 2)
            ]
            Rx_deliv[
              sort(c(idx, idx - 1)),
              `:=` (tx_start = min(tx_start),
                    drug_duration = sum(drug_duration),
                    grace_per = max(grace_per)),
              .(by_hospit)
            ]
            Rx_deliv <- Rx_deliv[!idx]
            Rx_deliv[
              , `:=` (tx_end = tx_start + drug_duration - 1L,
                      by_hospit = NULL)
            ]
            Rx_deliv[
              Rx_deliv[, .I[.N > 1], .(id, drug_code)]$V1,
              diff := tx_start - shift(tx_end),
              .(id, drug_code)
            ][is.na(diff), diff := 0L]
            idx <- rmNA(Rx_deliv[, .I[hosp == TRUE & shift(hosp) == FALSE & diff <= 1], .(id, drug_code)]$V1)
          }
        }

        # Insert hosp=TRUE with the next row hosp=FALSE
        Rx_deliv[
          Rx_deliv[, .I[.N > 1], .(id, drug_code)]$V1,
          diff := tx_start - shift(tx_end),
          .(id, drug_code)
        ][is.na(diff), diff := 0L]
        idx <- rmNA(Rx_deliv[, .I[
          hosp == TRUE & shift(hosp, -1) == FALSE & shift(diff, -1) <= 1
        ], .(id, drug_code)]$V1)
        if (length(idx)) {
          while (length(idx)) {
            Rx_deliv[
              sort(c(idx, idx + 1)),
              by_hospit := rep(1:(length(idx)), each = 2)
            ]
            Rx_deliv[
              sort(c(idx, idx + 1)),
              `:=` (tx_start = min(tx_start),
                    drug_duration = sum(drug_duration),
                    grace_per = max(grace_per)),
              .(by_hospit)
            ]
            Rx_deliv <- Rx_deliv[!idx]
            Rx_deliv[
              , `:=` (tx_end = tx_start + drug_duration - 1L,
                      by_hospit = NULL)
            ]
            Rx_deliv[
              Rx_deliv[, .I[.N > 1], .(id, drug_code)]$V1,
              diff := tx_start - shift(tx_end),
              .(id, drug_code)
            ][is.na(diff), diff := 0L]
            idx <- rmNA(Rx_deliv[, .I[
              hosp == TRUE & shift(hosp, -1) == FALSE & shift(diff, -1) <= 1
            ], .(id, drug_code)]$V1)
          }
        }

        # Check if the previous manipulations created cases that did not exist previously
        idx <- intersect(  # detect first row where hosp = TRUE
          Rx_deliv[, .I [1], .(id, drug_code)]$V1,
          Rx_deliv[, .I[hosp == TRUE]]
        )
        if (length(idx) && !eval_again) {
          eval_again <- TRUE
        }
        Rx_deliv[
          Rx_deliv[, .I[.N > 1], .(id, drug_code)]$V1,
          diff := tx_start - shift(tx_end),
          .(id, drug_code)
        ][is.na(diff), diff := 0L]
        idx <- rmNA(Rx_deliv[, .I[hosp == TRUE & shift(hosp) == FALSE & diff <= 1], .(id, drug_code)]$V1)
        if (length(idx) && !eval_again) {
          eval_again <- TRUE
        }
        idx <- rmNA(Rx_deliv[, .I[
          hosp == TRUE & shift(hosp, -1) == FALSE & shift(diff, -1) <= 1
        ], .(id, drug_code)]$V1)
        if (length(idx) && !eval_again) {
          eval_again <- TRUE
        }

      }

      Rx_deliv[
        , `:=` (hosp = NULL,
                drug_duration = NULL)
      ]

    }


    ## Ajust tx_end date
    # Is there a reserve? Ajust if there is.
    Rx_deliv[
      Rx_deliv[, .I[.N > 1], .(id, drug_code)]$V1,
      diff := tx_start - shift(tx_end) - 1L,
      .(id, drug_code)
    ][is.na(diff), diff := 0L]
    Rx_deliv[
      , duration_ajust := Reduce(function(x, y) {
        z <- x + y
        if (z > 0) {
          z <- 0L
        } else if (!is.null(max_reserve) && z < -max_reserve) {
          z <- -max_reserve
        }
        return(as.integer(z))
      }, x = diff, accumulate = TRUE),
      .(id, drug_code)
    ]
    Rx_deliv[, tx_end_ajust := tx_end - duration_ajust]
    Rx_deliv[, tx_end_grace := as.integer(tx_end_ajust + grace_per)]
    Rx_deliv[
      Rx_deliv[, .I[.N > 1], .(id, drug_code)]$V1,
      diff_grace := tx_start - shift(tx_end_grace),
      .(id, drug_code)
    ][is.na(diff_grace), diff_grace := 0L]
    Rx_deliv[, per := 0L][diff_grace > 1, per := 1L]
    Rx_deliv[, per := cumsum(per) + 1L, .(id, drug_code)]
    Rx_deliv <- Rx_deliv[
      , .(tx_start = min(tx_start),
          tx_end = max(tx_end_ajust)),
      .(id, drug_code, per)
    ][, per := NULL]


    # Filter study dates
    Rx_deliv[tx_start < study_start, tx_start := study_start]
    Rx_deliv[tx_end > study_end, tx_end := study_end]
    Rx_deliv <- Rx_deliv[tx_start <= tx_end]

    if (nrow(Rx_deliv)) {
      ## Final touch on data: columns classes + columns name
      # start and end should be as Date?
      Rx_deliv[
        , `:=` (tx_start = lubridate::as_date(tx_start),
                tx_end = lubridate::as_date(tx_end))
      ]

      # Rename columns as initially
      setnames(Rx_deliv, c("id", "drug_code"), rx_names)
      return(Rx_deliv)
    } else {
      return(NULL)
    }

  } else {
    return(NULL)
  }

}
