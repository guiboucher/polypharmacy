#' Preparing polypharmacy indicators calculation
#'
#' Prepares `Rx_deliveries` for the calculation of the polypharmacy indicators by applying various user-defined arguments, incorporating hospital stays into the treatment periods and merging quasi continuous and overlapping treatment periods of identical drugs.
#'
#' **Columns**:
#' * `Rx_id`, `Cohort_id` and `Hosp_id` must have the same class (integer, numeric, character, ...).
#' * `Rx_date`, `Hosp_date_admis` and `Hosp_date_discharge` can be 1) `as.Date("yyyy-mm-dd")`, 2) `as.character("yyyy-mm-dd")` or 3) `as.integer()` where 0 is January 1\ifelse{html}{\out{<sup>st</sup>}}{\eqn{^{st}}}, 1970.
#'
#' **Arguments**:
#' * `study_start` and `study_end` can be 1) `as.Date("yyyy-mm-dd")`, 2) `as.character("yyyy-mm-dd")` or 3) `as.integer()` where 0 is January 1\ifelse{html}{\out{<sup>st</sup>}}{\eqn{^{st}}}, 1970.
#'
#' **Grace period**:\cr
#' We use the grace period to determine if a period should be continuous even if there are few days without consomption. For example, if an individual consumes a drug Monday through Friday every week, we might want to consider consumption without interruption despite Saturdays and Sundays, as the drug might still be active during these days.\cr
#' The grace period is determined by multiplying the duration by `grace_factor` and adding `grace_fix` (`grace_factor` \ifelse{html}{\out{&times;}}{\eqn{\times}} duration + `grace_fix`).
#'
#' **Performance**\cr
#' For better performance, date columns are converted to integer numbers.
#'
#' @param Rx_deliveries Table listing all prescription drug deliveries to be analyzed.
#' @param Rx_id Individual unique identifier (i.e. drug recipient).
#' @param Rx_drug_code Drug unique identifier.
#' @param Rx_date Date the drug was delivered.
#' @param Rx_duration Duration of treatment delivered in days (integer).
#' @param Cohort Table providing the unique identifiers of the study cohort.
#' @param Cohort_id Individual unique identifier.
#' @param Hosp_stays Table listing all hospital stays.
#' @param Hosp_id Individual unique identifier.
#' @param Hosp_date_admis Date of admission.
#' @param Hosp_date_discharge Date of discharge.
#' @param study_start,study_end `'yyyy-mm-dd'`. When the study should start and when it should end. `Rx_deliveries` will be modified if `study_start` or `study_end` are not `NULL`: `start = study_start` if `start < study_start` and `end = study_end` if `end > study_end`. Could be number values where 1970-01-01 = 0. Exemple: `as.integer(as.Date('2020-01-01')) = 18262`.
#' @param grace_factor,grace_fix A number \eqn{\ge} 0. See *Details*.
#' @param max_reserve A number \eqn{\ge} 0 or `NULL`. Maximum number of medication a user can store. `NULL` implies no limit.
#' @param final_names Vector of two (2) values indicating the name of the first and last date of continued drug use. See *Value*.
#' @param final_as_date Indicates if `start` and `end` columns should be returned `as_date` (`TRUE`). Else, dates are returned as character (`FALSE`). `TRUE` by default.
#' @param verif_cols Indicates if the program should verify `start` and `end` column format (safe). `TRUE` by default.
#'
#' @return `data.table` with four (4) variables:
#' * `Rx_id`: Individual unique identifier (i.e drug recipient).
#' * `Rx_drug_code`: Drug unique identifier.
#' * `final_names[1]`: First date of continued drug use.
#' * `final_names[2]`: Last date of continued drug use.
#' @import data.table
#' @importFrom lubridate is.Date as_date
#' @export
data_process <- function(
  Rx_deliveries, Rx_id, Rx_drug_code, Rx_date, Rx_duration,
  Cohort = NULL, Cohort_id = NULL,
  Hosp_stays = NULL, Hosp_id = NULL, Hosp_date_admis = NULL, Hosp_date_discharge = NULL,
  study_start = NULL, study_end = NULL,
  grace_factor = 0.5, grace_fix = 0, max_reserve = NULL,
  final_names = c("drug_start", "drug_end"), final_as_date = TRUE,
  verif_cols = TRUE
) {

  # Internal Fonctions ------------------------------------------------------

  verif_args <- function(
    Rx_deliveries, Rx_id, Rx_drug_code, Rx_date, Rx_duration, Cohort, Cohort_id,
    Hosp_stays, Hosp_id, Hosp_date_admis, Hosp_date_discharge, study_start, study_end,
    grace_factor, grace_fix, max_reserve, final_names, final_as_date, verif_cols
  ) {
    ### Initial verification, are there errors...
    ### 1) Arguments permitted values.
    ### 2) - columns exists?
    ###    - Argument name can't be its value: Rx_id != "Rx_id" (data.table package issue)
    ### 3) - Cohort unique ids?
    ###    - if verif_cols=TRUE: permitted values in columns.

    check <- newArgCheck()

    ## 1) Class of arguments
    # Rx_deliveries
    if (!is.data.frame(Rx_deliveries))
      addError("Rx_deliveries must be a data.frame.", check)
    # Rx_id, Rx_drug_code, Rx_date, Rx_duration
    for (var in c("Rx_id", "Rx_drug_code", "Rx_date", "Rx_duration")) {
      if (!is.character(get(var)))
        addError(paste0(var," must be a character value."), check)
      if (length(unique(get(var))) != 1)
        addError(paste0(var," must be a single value."), check)
    }
    # Cohort
    if (!is.null(Cohort) && !is.data.frame(Cohort))
      addError("Cohort must be a data.frame if not NULL.", check)
    # Cohort_id
    if (!is.null(Cohort)) {
      if (!is.character(Cohort_id))
        addError("Cohort_id must be a character value if Cohort is not NULL.", check)
      if (length(unique(Cohort_id)) != 1)
        addError("Cohort_id must be a single value.", check)
    }
    # Hosp_stays
    if (!is.null(Hosp_stays) && !is.data.frame(Hosp_stays))
      addError("Hosp_stays must be a data.frame if not NULL.", check)
    # Hosp_id, Hosp_date_admis, Hosp_date_discharge
    if (!is.null(Hosp_stays)) {
      for (var in c("Hosp_id", "Hosp_date_admis", "Hosp_date_discharge")) {
        if (!is.character(get(var)))
          addError(paste0(var," must be a character value."), check)
        if (length(unique(get(var))) != 1)
          addError(paste0(var," must be a single value."), check)
      }
    }
    # study_start, study_end
    for (var in c("study_start", "study_end")) {
      if (is.na(as_date(get(var))))
        addError(paste0(
          var," must be a character value ('yyyy-mm-dd'), a Date value (as.Date('yyyy-mm-dd')) or a numeric value where 1970-01-01 = 0."),
          check)
      if (length(unique(get(var))) != 1)
        addError(paste0(var," must be a single value."), check)
    }
    # grace_factor, grace_fix
    for (var in c("grace_factor", "grace_fix")) {
      if (is.numeric(get(var)) && get(var) < 0) {
        addError(paste0(var," must be greater or equal than zero (0)."), check)
      } else {
        addError(paste0(var," must be a numeric value."), check)
      }
    }
    # max_reserve
    if (!is.null(max_reserve)) {
      if (!is.numeric(max_reserve)) {
        addError("max_reserve must be a numeric value.", check)
      } else {
        if (max_reserve < 0)
          addError("max_reserve must be greater or equal to zero (0).", check)
      }
      if (length(unique(max_reserve)) != 1)
        addError("max_reserve must be a single value.", check)
    }
    # final_name
    if (!is.character(final_names))
      addError("final_name must be a character vector.", check)
    if (length(unique(final_names)) != 2)
      addError("final_name must have two (2) values.", check)
    # final_as_date, verif_cols
    if (!is.logical(final_as_date))
      addError("final_as_date must be a logical value.", check)
    if (length(unique(final_as_date)) != 1)
      addError("final_as_date must be a single value.", check)
    if (!is.logical(verif_cols))
      addError("verif_cols must be a logical value.", check)
    if (length(unique(verif_cols)) != 1)
      addError("verif_cols must be a single value.", check)

    finishArgCheck(check)


    ## 2) Are the columns exist?
    # Rx_id, Rx_drug_code, Rx_date, Rx_duration
    for (col in c("Rx_id", "Rx_drug_code", "Rx_date", "Rx_duration")) {
      if (!get(col) %in% names(Rx_deliveries))
        addError(paste0(get(col)," (",col,") is not a column in Rx_deliveries."),
                 check)
    }
    # Cohort_id
    if (!is.null(Cohort) && !Cohort_id %in% names(Cohort))
      addError(paste0(Cohort_id," (Cohort_id) is not a column in Rx_deliveries."),
               check)
    # Hosp_id, Hosp_date_admis, Hosp_date_discharge
    if (!is.null(Hosp_stays)) {
      for (col in c("Hosp_id", "Hosp_date_admis", "Hosp_date_discharge")) {
        if (!get(col) %in% names(Hosp_stays))
          addError(paste0(get(col)," (",col,") is not a column in Hosp_stays."), check)
      }
    }
    ## 2) Arg != "Arg"
    # Rx_deliveries
    for (arg in c("Rx_id", "Rx_drug_code", "Rx_date", "Rx_duration"))
      if (arg == get(arg))
        addError(paste0(arg," can't be equal to '",arg,"'. Please modify value."),
                 check)
    # Cohort
    if (!is.null(Cohort) && Cohort_id == "Cohort_id")
      addError("Cohort_id can't be equal to 'Cohort_id'. Please modify value.", check)
    # Hosp_stays
    if (!is.null(Hosp_stays))
      for (arg in c("Hosp_id", "Hosp_date_admis", "Hosp_date_discharge"))
        if (arg == get(arg))
          addError(paste0(arg," can't be equal to '",arg,"'. Please modify value."),
                   check)
    finishArgCheck(check)



    ## 3) final_names != Rx_id + Rx_drug_code
    if (any(final_names %in% c(Rx_id, Rx_drug_code)) || length(unique(c(final_as_date, Rx_id, Rx_drug_code))) != 4)
      addError("Rx_id, Rx_drug_code and final_names need four (4) distinct values.", check)
    ## 3) Cohort ids unique?
    if (!is.null(Cohort) && nrow(Cohort) != uniqueN(Cohort[[Cohort_id]]))
      addError(paste0("Cohort must be a unique list of identification codes:\n",
                      "nrow(Cohort)               : ",nrow(Cohort),"\n",
                      "uniqueN(Cohort[[Cohort_id]]: ",uniqueN(Cohort[[Cohort_id]])),
               check)
    ## 3) Column classes & not supported values
    if (verif_cols) {  # can take some time... so optional

      # Rx_deliveries
      # Rx_id
      if (anyNA(Rx_deliveries[[Rx_id]]))
        addError(paste0(Rx_id," can't contains NAs."), check)
      # Rx_drug_code
      if (anyNA(Rx_deliveries[[Rx_drug_code]]))
        addError(paste0(Rx_drug_code," can't contains NAs."), check)
      # Rx_date
      nNAs <- sum(is.na(Rx_deliveries[[Rx_date]]))  # are there NAs?
      if (nNAs)
        addError(paste0(Rx_date," column (Rx_date) can't contains NAs."), check)
      if (nNAs != sum(is.na(as_date(Rx_deliveries[[Rx_date]]))))  # is it a possible date format?
        addError(paste0(
          Rx_date," column (Rx_date) must be character ('yyyy-mm-dd'), Date (as.Date('yyyy-mm-dd')) or numeric where 1970-01-01 = 0."),
          check)
      # Rx_duration
      if (anyNA(Rx_deliveries[[Rx_duration]]))
        addError(paste0(Rx_duration," column (Rx_duration) can't contains NAs."), check)
      if (!is.numeric(Rx_deliveries[[Rx_duration]]))
        addError(paste0(Rx_duration," column (Rx_duration) must be numeric."), check)

      # Cohort
      if (!is.null(Cohort)) {
        # Cohort_id
        if (anyNA(Cohort[[Cohort_id]]))
          addError(paste0(Cohort_id," column (Cohort_id) can't contains NAs."), check)
        if (class(Cohort[[Cohort_id]]) != class(Rx_deliveries[[Rx_id]]))
          addError(paste0(
            Cohort_id," column (Cohort_id, class: ",class(Cohort[[Cohort_id]]),") ",
            "must have the same class as ",
            Rx_id," column (Rx_id, class: ",class(Rx_deliveries[[Rx_id]]),")."
          ), check)
      }

      # Hosp_stays
      if (!is.null(Hosp_stays)) {
        # Hosp_id
        if (anyNA(Hosp_stays[[Hosp_id]]))
          addError(paste0(Hosp_id," column (Hosp_id) can't contains NAs."), check)
        if (class(Hosp_stays[[Hosp_id]]) != class(Rx_deliveries[[Rx_id]]))
          addError(paste0(
            Hosp_id," column (Hosp_id, class: ",class(Hosp_stays[[Hosp_id]]),") ",
            "must have the same class as ",
            Rx_id," column (Rx_id, class: ",class(Rx_deliveries[[Rx_id]]),")."
          ), check)
        # Hosp_date_admis, Hosp_date_discharge
        for (col in c("Hosp_date_admis", "Hosp_date_discharge")) {
          nNAs <- sum(is.na(Hosp_stays[[get(col)]]))  # are there NAs?
          if (nNAs)
            addError(paste0(get(col)," column (",col,") can't contains NAs."), check)
          if (nNAs != sum(is.na(as_date(Hosp_stays[[get(col)]]))))  # is it a possible date format?
            addError(paste0(
              get(col)," column (",col,") must be character ('yyyy-mm-dd'), Date (as.Date('yyyy-mm-dd')) or numeric where 1970-01-01 = 0."),
              check)
        }
      }
      finishArgCheck(check)

    }

  }


  # Core Fonction -----------------------------------------------------------

  # Argument verification - stop if any error
  verif_args(Rx_deliveries, Rx_id, Rx_drug_code, Rx_date, Rx_duration,
             Cohort, Cohort_id,
             Hosp_stays, Hosp_id, Hosp_date_admis, Hosp_date_discharge,
             study_start, study_end,
             grace_factor, grace_fix, max_reserve,
             final_names, final_as_date,
             verif_cols)


  ## Initial arguments & arrange them
  # Convert study dates as integer for better performances
  for (var in c("study_start", "study_end")) {
    if (is.character(get(var)))
      assign(var, as_date(get(var)))
    if (!is.integer(get(var)))
      assign(var, as.integer(get(var)))
  }
  # Initial names
  rx_names <- c(Rx_id, Rx_drug_code)


  ## Arrange datas
  # Cohort - keep id vector only. Future use: filter Rx_deliveries
  if (!is.null(Cohort)) Cohort <- sort(Cohort[[Cohort_id]])

  # Rx_deliveries
  if (!is.data.table(Rx_deliveries)) setDT(Rx_deliveries)  # convert as.data.table
  # Cols selection
  Rx_deliveries <- Rx_deliveries[
    , .(id = get(Rx_id),
        drug_code = get(Rx_drug_code),
        drug_start = get(Rx_date),
        drug_duration = get(Rx_duration))
  ]
  if (!is.null(Cohort)) {
    Rx_deliveries <- Rx_deliveries[id %in% Cohort]  # select ids in Cohort
    if (!nrow(Rx_deliveries)) {
      # Return NULL if nrow(Rx_deliveries)=0
      message("Rx_deliveries[Rx_id %in% Cohort[[Cohort_id]]] return no observations.")
      return(NULL)
    }
  }
  # Convert drug_date and drig_duration as integer for better performances
  if (is.character(Rx_deliveries$drug_start))
    Rx_deliveries[, drug_start := as_date(drug_start)]
  if (!is.integer(Rx_deliveries$drug_start))
    Rx_deliveries[, drug_start := as.integer(drug_start)]
  if (!is.integer(Rx_deliveries$drug_duration))
    Rx_deliveries[, drug_duration := as.integer(drug_duration)]
  # Create drug_end
  Rx_deliveries[, drug_end := drug_start + drug_duration - 1L]
  # Filter study dates
  if (!is.null(study_start))
    Rx_deliveries[drug_start < study_start, drug_start := study_start]
  if (!is.null(study_end))
    Rx_deliveries[drug_end > study_end, drug_end := study_end]
  if (!is.null(study_start) || !is.null(study_end)) {
    Rx_deliveries <- Rx_deliveries[study_start <= study_end]
    if (!nrow(Rx_deliveries)) {
      message("Rx_deliveries: no rows after filtering study dates.")
      return(NULL)
    }
  }
  setkey(Rx_deliveries, id, drug_code, drug_start)

  # Hosp_stays
  if (!is.null(Hosp_stays)) {
    if (!is.data.table(Hosp_stays)) setDT(Hosp_stays)  # convert as.data.table
    # cols selection
    Hosp_stays <- Hosp_stays[
      , .(id = get(Hosp_id),
          # Choose colnames for future merge with Rx_deliveries
          drug_start = get(Hosp_date_admis),
          drug_end = get(Hosp_date_discharge))
    ]
    if (!is.null(Cohort)) {
      Hosp_stays <- Hosp_stays[id %in% Cohort]  # select ids in Cohort
      if (!nrow(Hosp_stays)) {
        # Send message indicating that no observations after filter
        message("Hosp_stays[Hops_id %in% Cohort[[Cohort_id]]] return no observations.")
      }
    }
    # Convert dates as integer for better performances
    for (col in c("drug_start", "drug_end")) {
      if (is.character(Hosp_stays[[col]]))
        Hosp_stays[, (col) := as_date(get(col))]
      if (!is.integer(Hosp_stays[[col]]))
        Hosp_stays[, (col) := as.integer(get(col))]
    }

    setkey(Hosp_stays, id, drug_start)
  }


  ## Add hospit to Rx_deliveries
  ## To be added, Hosp_stays (hosp=TRUE) must overlap or be contiguous to
  ## Rx_deliveries (hosp=FALSE)
  if (!is.null(Hosp_stays)) {
    Rx_deliveries[, hosp := FALSE]  # indicate that rows are not hosp stays
    # Combine time periods that overlap or are contiguous to other hosp stays
    Hosp_stays[, diff := drug_start - shift(drug_end), .(id)]  # days difference between start[i] and end[i-1]
    Hosp_stays[is.na(diff), diff := 0L]
    Hosp_stays[, per := 0L][diff > 1, per := 1L]  # 0: same time period, 1: new time period
    Hosp_stays[, per := cumsum(per) + 1L, .(id)]  # time period from 1 to n
    Hosp_stays <- Hosp_stays[  # combine all same time period number
      , .(drug_start = min(drug_start),
          drug_end = max(drug_end)),
      .(id, per)
    ][, per := NULL]  # delete col time period number

    # Combine hosp stays to drug_codes and add rows to Rx_deliveries
    hosp_add <- unique(Rx_deliveries[, .(id, drug_code)])  # combination id+drug_code
    hosp_add <- hosp_add[Hosp_stays, on = .(id), allow.cartesian = TRUE]  # create data where each hosp stays is associated to a drug code
    hosp_add[, hosp := TRUE]  # rows are hosp stays
    Rx_deliveries <- rbind(Rx_deliveries, hosp_add)  # add hosp stays to Rx deliveries


    ## Delete rows hosp=TRUE that are not overlapping or contiguous to hosp=FALSE
    # Delete hosp rows that are first with other hosp after (hosp==TRUE)
    idx <- intersect(
      Rx_deliveries[, .I[1], .(id, drug_code)]$V1,  # 1st value of each group
      rmNA(Rx_deliveries[, .I[hosp == TRUE & shift(hosp, -1) == TRUE], .(id, drug_code)]$V1)  # hosp followed by hosp
    )
    if (length(idx)) {
      # delete identified rows and repeat process until no rows are detected
      while(length(idx)) {
        Rx_deliveries <- Rx_deliveries[!idx]
        idx <- intersect(
          Rx_deliveries[, .I[1], .(id, drug_code)]$V1,  # 1st value of each group
          rmNA(Rx_deliveries[, .I[hosp == TRUE & shift(hosp, -1) == TRUE], .(id, drug_code)]$V1)  # hosp followed by hosp
        )
      }
    }
    # Delete hosp=TRUE that are between 2 hosp=TRUE
    idx <- rmNA(Rx_deliveries[, .I[
      hosp == TRUE & shift(hosp) == TRUE & shift(hosp, -1) == TRUE
    ], .(id, drug_code)]$V1)
    if (length(idx)) {
      while(length(idx)) {
        Rx_deliveries <- Rx_deliveries[!idx]
        idx <- rmNA(Rx_deliveries[, .I[
          hosp == TRUE & shift(hosp) == TRUE & shift(hosp, -1) == TRUE
        ], .(id, drug_code)]$V1)
      }
    }
    # Delete first hosp=TRUE that don't overlap with next row (hosp=FALSE)
    idx <- intersect(
      Rx_deliveries[, .I[1], .(id, drug_code)]$V1,
      rmNA(Rx_deliveries[, .I[
        hosp == TRUE & shift(hosp, -1) == FALSE &
          drug_end < shift(drug_start, -1) - 1
      ], .(id, drug_code)]$V1)
    )
    if (length(idx)) Rx_deliveries <- Rx_deliveries[!idx]
    # Delete hosp=TRUE that doesnt overlap with a hosp=FALSE
    # * No overlap for hosp=TRUE between them, so maximum 2 hosp=TRUE one after
    #   the other -> don't need to specify hosp=FALSE before or after.
    idx <- rmNA(Rx_deliveries[, .I[
      hosp == TRUE &
        drug_start > shift(drug_end) + 1 &
        drug_end < shift(drug_start, -1) - 1
    ], .(id, drug_code)]$V1)
    if (length(idx)) Rx_deliveries <- Rx_deliveries[!idx]
  }


  ## Grace time periods
  Rx_deliveries[, grace_per := grace_factor * drug_duration + grace_fix]
  if (!is.null(Hosp_stays))
    Rx_deliveries[hosp == TRUE, grace_per := 0]


  ## Insert hosp=TRUE in hosp=FALSE
  # Insert the 1st rwo hosp==TRUE to the next one, hosp==FALSE
  idx <- intersect(
    Rx_deliveries[, .I [1], .(id, drug_code)]$V1,
    Rx_deliveries[, .I[hosp == TRUE]]
  )
  if (length(idx)) {
    Rx_deliveries[
      sort(c(idx, idx + 1)),
      by_hosp := cumsum(hosp),
      .(id, drug_code)
    ]
    Rx_deliveries[
      sort(c(idx, idx + 1)),
      `:=` (drug_start = min(drug_start),
            drug_duration = sum(drug_duration),
            grace_per = max(grace_per)),
      .(id, drug_code, by_hosp)
    ]
    Rx_deliveries <- Rx_deliveries[!idx]
    Rx_deliveries[
      , `:=` (drug_end = drug_start + drug_duration - 1L,
              by_hosp = NULL)
    ]
  }
  # Insert hosp=TRUE in the previous row, hosp=FALSE
  Rx_deliveries[
    Rx_deliveries[, .I[.N > 1], .(id, drug_code)]$V1,
    diff := drug_start - shift(drug_end),
    .(id, drug_code)
  ][is.na(diff), diff := 0L]
  idx <- Rx_deliveries[, .I[hosp == TRUE & diff <= 1], .(id, drug_code)]$V1
  if (length(idx)) {
    while (length(idx)) {
      Rx_deliveries[
        sort(c(idx, idx - 1)),
        by_hospit := rep(1:(length(idx)), each = 2)
      ]
      Rx_deliveries[
        sort(c(idx, idx - 1)),
        `:=` (start = min(start),
              duration = sum(duration),
              grace_per = max(grace_per)),
        .(by_hospit)
      ]
      Rx_deliveries <- Rx[!idx]
      Rx_deliveries[
        , `:=` (drug_end = drug_start + drug_duration - 1L,
                by_hospit = NULL)
      ]
      Rx_deliveries[
        Rx_deliveries[, .I[.N > 1], .(id, drug_code)]$V1,
        diff := drug_start - shift(drug_end),
        .(id, drug_code)
      ][is.na(diff), diff := 0L]
      idx <- Rx_deliveries[, .I[hosp == TRUE & diff <= 1], .(id, drug_code)]$V1
    }
  }
  # Insert hosp=TRUE with the next row hosp=FALSE
  Rx_deliveries[
    Rx_deliveries[, .I[.N > 1], .(id, drug_code)]$V1,
    diff := drug_start - shift(drug_end),
    .(id, drug_code)
  ][is.na(diff), diff := 0L]
  idx <- Rx_deliveries[, .I[
    hosp == TRUE & shift(diff, -1) <= 1
  ], .(id, drug_code)]$V1
  if (length(idx)) {
    while (length(idx)) {
      Rx_deliveries[
        sort(c(idx, idx + 1)),
        by_hospit := rep(1:(length(idx)), each = 2)
      ]
      Rx_deliveries[
        sort(c(idx, idx + 1)),
        `:=` (start = min(start),
              duration = sum(duration),
              grace_per = max(grace_per)),
        .(by_hospit)
      ]
      Rx_deliveries <- Rx_deliveries[!idx]
      Rx_deliveries[
        , `:=` (drug_end = drug_start + drug_duration - 1L,
                by_hospit = NULL)
      ]
      Rx_deliveries[
        Rx_deliveries[, .I[.N > 1], .(id, drug_code)]$V1,
        diff := drug_start - shift(drug_end),
        .(id, drug_code)
      ][is.na(diff), diff := 0L]
      idx <- Rx_deliveries[, .I[
        hosp == TRUE & shift(diff, -1) <= 1
      ], .(id, drug_code)]$V1
    }
  }
  Rx_deliveries[
    , `:=` (hosp = NULL,
            drug_duration = NULL)
  ]


  ## Ajust drug_end date
  # Is there a reserve? Ajust if there is.
  Rx_deliveries[
    , duration_ajust := Reduce(function(x, y) {
      z <- x + y
      if (z > 0) {
        z <- 0L
      } else if (!is.null(max_reserve) && z < -max_reserve) {
        z <- -max_reserve
      }
      return(as.integer(z))
    }, diff, accumulate = TRUE),
    .(id, drug_code)
  ]
  Rx_deliveries[, drug_end_ajust := drug_end - duration_ajust]
  Rx_deliveries[, drug_end_grace := as.integer(drug_end_ajust + grace_per)]
  Rx_deliveries[
    Rx_deliveries[, .I[.N > 1], .(id, drug_code)]$V1,
    diff_grace := drug_start - shift(drug_end_grace),
    .(id, drug_code)
  ][is.na(diff_grace), diff_grace := 0L]
  Rx_deliveries[, per := 0L][diff_grace > 1, per := 1L]
  Rx_deliveries[, per := cumsum(per) + 1L, .(id, drug_code)]
  Rx_deliveries <- Rx_deliveries[
    , .(drug_start = min(drug_start),
        drug_end = max(drug_end_ajust)),
    .(id, drug_code, per)
  ][, per := NULL]


  ## Final touch on data: columns classes + columns name
  # start and end should be as Date?
  if (final_as_date) {
    Rx_deliveries[
      , `:=` (drug_start = as_date(drug_start),
              drug_end = as_date(drug_end))
    ]
  }
  # Rename columns as initially
  setnames(Rx_deliveries,
           c("id", "drug_code", "drug_start", "drug_end"),
           c(rx_names, final_names))

  return(Rx_deliveries)

}
