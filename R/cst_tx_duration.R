cst_tx_duration <- function(
  Rx_deliveries, Rx_drug_code, Rx_duration,
  Cst_Tx_duration, cst_drug_code, cst_tx_dur
) {

# Internal FCTS -----------------------------------------------------------

  verif_args <- function(Rx_deliveries, Rx_drug_code, Rx_duration,
                         Cst_Tx_duration, cst_drug_code, cst_duration) {
    ### Arguments verification
    ### 1) Argument classes
    ### 2) - Arg != "Arg"
    ###    - Columns exists?
    ### 3) - Columns must have same class
    ###    - No NAs
    ###    - duration as integer

    ## 1) Argument classes
    # Rx_deliveries
    if (!is.data.frame(Rx_deliveries)) {
      addError("Rx_deliveries must be a data.frame.", check)
    }
    # cst_tx_duration
    if (!is.data.frame(Cst_Tx_duration)) {
      addError("Cst_Tx_duration must be a data.frame.", check)
    }
    # Rx_drug_code, Rx_duration, cst_drug_code, cst_tx_dur
    for (var in c("Rx_drug_code", "Rx_duration", "cst_drug_code", "cst_tx_dur")) {
      if (!is.character(get(var))) {
        addError(paste0(var," must be a character vector."), check)
      }
      if (length(get(var)) != 1) {
        addError(paste0(var," must be a single value."), check)
      }
    }
    finishArgCheck(check)


    ## 2) Arg != "Arg"
    for (arg in c("Rx_drug_code", "Rx_duration", "cst_drug_code", "cst_tx_dur")) {
      if (arg == get(arg)) {
        addError(paste0(arg," can't be equal to '",arg,"'. Please modify value."),
                 check)
      }
    }
    ## 2) Columns exists?
    # Rx_deliveries
    for (col in c("Rx_drug_code", "Rx_duration")) {
      if (!get(col) %in% names(Rx_deliveries)) {
        addError(paste0(get(col)," (",col,") is not a column in Rx_deliveries."),
                 check)
      }
    }
    # Cst_Tx_duration
    for (col in c("cst_drug_code", "cst_tx_dur")) {
      if (!get(col) %in% names(Cst_Tx_duration)) {
        addError(paste0(get(col)," (",col,") is not a column in Cst_Tx_duration."),
                 check)
      }
    }
    finishArgCheck(check)

    ## 3) Columns must have the same class
    # drug code
    if (class(Rx_deliveries[[Rx_drug_code]]) != class(Cst_Tx_duration[[cst_drug_code]])) {
      addError(paste0(
        Rx_drug_code," column (Rx_drug_code, class: ",class(Rx_deliveries[[Rx_drug_code]]),") ",
        "must have the same class as ",
        cst_drug_code," column (cst_drug_code, class: ",class(Cst_Tx_duration[[cst_drug_code]]),")."
      ), check)
    }
    # duration
    if (class(Rx_deliveries[[Rx_duration]]) != class(Cst_Tx_duration[[cst_duration]])) {
      addError(paste0(
        Rx_duration," column (Rx_duration, class: ",class(Rx_deliveries[[Rx_duration]]),") ",
        "must have the same class as ",
        cst_duration," column (cst_duration, class: ",class(Cst_Tx_duration[[cst_duration]]),")."
      ), check)
    }
    ## 3) No NAs
    # Rx_deliveries
    for (col in c("Rx_drug_code", "Rx_duration")) {
      if (anyNA(Rx_deliveries[[get(col)]])) {
        addError(paste0(get(col)," column (",col,") can't contains NAs."), check)
      }
    }
    # Cst_Tx_duration
    for (col in c("cst_drug_code", "cst_duration")) {
      if (anyNA(Cst_Tx_duration[[get(col)]])) {
        addError(paste0(get(col)," column (",col,") can't contains NAs."), check)
      }
    }
    ## 3) duration as integer
    # Rx_duration
    if (!is.numeric(Rx_deliveries[[Rx_duration]])) {
      addError(paste0(Rx_duration," column (Rx_duration) must be numeric."), check)
    }
    if (!is.numeric(Cst_Tx_duration[[cst_tx_dur]])) {
      addError(paste0(cst_tx_dur," column (cst_tx_dur) must be numeric."), check)
    }
    finishArgCheck(check)

  }


# Code FCT ----------------------------------------------------------------

  ## Arrange datas
  # Rx_deliveries
  if (!is.data.table(Rx_deliveries)) {
    setDT(Rx_deliveries)
  }
  # Cst_Tx_duration
  if (!is.data.table(Cst_Tx_duration)) {
    setDT(Cst_Tx_duration)
  }


}





















