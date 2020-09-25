#' Translate combination drug deliveries into single active ingredients
#'
#' Replaces each combination drug into several deliveries of elementary active ingredients according to a user-provided correspondence table.
#'
#' @param Rx_deliv Name of the table listing all prescription drugs delivered.
#' @param Rx_drug_code Column name of `Rx_deliv` that contains the combination drugs’ unique identifiers (any format).
#' @param Combn_drugs Name of the correspondence table listing all elementary active ingredients that make up each combination drug.
#' @param Combn_drug_code Column name of `Combn_drugs` that contains the combination drugs’ unique identifiers (same format as `Rx_drug_code`).
#' @param Combn_act_code Column name of elementary active ingredients that is present in `Combn_drugs` (same format as `Rx_drug_code`).
#'
#' @return `data.table` of the same structure than `Rx_deliv` sorted by `Rx_drug_code`.
#' @import data.table
#' @export
#' @examples
#' Rx_dt <- data.frame(
#'   id = c(1, 1, 2, 2, 2),
#'   codeDrug = c(159, 753, 123, 456, 789)
#' )
#' SplitCode <- data.frame(
#'   code = c(159, 159, 456, 456, 456),
#'   split_code = c(1591, 1592, 4567, 4568, 4569)
#' )
#' Rx_split <- drug_bkdn(Rx_deliv = Rx_dt, Rx_drug_code = "codeDrug",
#'                       Combn_drugs = SplitCode, Combn_drug_code = "code",
#'                       Combn_act_code = "split_code")
drug_bkdn <- function(Rx_deliv, Rx_drug_code, Combn_drugs, Combn_drug_code, Combn_act_code) {


# Internal FCTS -----------------------------------------------------------

  verif_args <- function(Rx_deliv, Rx_drug_code, Combn_drugs, Combn_drug_code, Combn_act_code) {
    ### Argument verification
    ### 1) Arguments class + length
    ### 2) - Arg != "Arg"
    ###    - Cols exists?
    ### 3) - Cols have no NAs
    ###    - drug code cols have same class
    ###    - Combn_act_code no duplicate

    check <- newArgCheck()
    ## 1) Arguments class
    if (!is.data.frame(Rx_deliv)) {
      addError("Rx_deliv must be a data.frame.", check)
    }
    if (!is.data.frame(Combn_drugs)) {
      addError("Combn_drugs must be a data.frame.", check)
    }
    for (var in c("Rx_drug_code", "Combn_drug_code", "Combn_act_code")) {
      if (!is.character(get(var))) {
        addError(paste0(var," must be a character vector."), check)
      }
      if (length(get(var)) != 1) {
        addError(paste0(var," must be a single value."), check)
      }
    }
    finishArgCheck(check)

    ## 2) Arg != "Arg"
    for (arg in c("Rx_drug_code", "Combn_drug_code", "Combn_act_code")) {
      if (arg == get(arg)) {
        addError(paste0(arg," can't be equal to '",arg,"'. Please modify value."),
                 check)
      }
    }
    ## 2) Cols exists?
    if (!Rx_drug_code %in% names(Rx_deliv)) {
      addError(paste0(Rx_drug_code," (Rx_drug_code) is not a column in Rx_deliv."), check)
    }
    for (col in c("Combn_drug_code", "Combn_act_code")) {
      if (!get(col) %in% names(Combn_drugs)) {
        addError(paste0(get(col)," (",col,") is not a column in Combn_drugs."), check)
      }
    }
    finishArgCheck(check)


    ## 3) Any NAs?
    if (anyNA(Rx_deliv[[Rx_drug_code]])) {
      addError(paste0(Rx_drug_code," column (Rx_drug_code) can't contains NAs."), check)
    }
    for (col in c("Combn_drug_code", "Combn_act_code")) {
      if (anyNA(Combn_drugs[[get(col)]])) {
        addError(paste0(get(col)," column (",col,") can't contains NAs."), check)
      }
    }
    ## 3) drug code cols have same class
    if (class(Rx_deliv[[Rx_drug_code]]) != class(Combn_drugs[[Combn_drug_code]]) ||
        class(Rx_deliv[[Rx_drug_code]]) != class(Combn_drugs[[Combn_act_code]])) {
      addError(paste0(
        Combn_drug_code," column (Combn_drug_code, class: ",class(Combn_drugs[[Combn_drug_code]]),") and ",
        Combn_act_code," column (Combn_act_code, class: ",class(Combn_drugs[[Combn_act_code]]),") ",
        "must have the same class as ",
        Rx_drug_code," column (Rx_drug_code, class: ",class(Rx_deliv[[Rx_drug_code]]),")."
      ), check)
    }
    ## 3) no duplicate
    if (nrow(Combn_drugs) != uniqueN(Combn_drugs[[Combn_act_code]])) {
      addError(paste0(Combn_act_code," column (Combn_act_code) can't have duplicates."), check)
    }
    finishArgCheck(check)

  }


# Code FCT ----------------------------------------------------------------

  verif_args(Rx_deliv, Rx_drug_code, Combn_drugs, Combn_drug_code, Combn_act_code)

  ## Arrange datas
  # Rx_deliv
  # Rx_deliv
  if (!is.data.table(Rx_deliv)) {  # convert as data.table
    Rx_deliv <- as.data.table(Rx_deliv)
  } else {
    Rx_deliv <- copy(Rx_deliv)
  }
  colorder <- names(Rx_deliv)  # initial order columns
  setkeyv(Rx_deliv, Rx_drug_code)  # sort

  # Combn_drugs
  if (!is.data.table(Combn_drugs)) {  # convert as data.table
    Combn_drugs <- as.data.table(Combn_drugs)
  } else {
    Combn_drugs <- copy(Combn_drugs)
  }
  # Select cst_tx_dur essential cols
  cols <- c(Combn_drug_code, Combn_act_code)  # cols to select
  Combn_drugs <- Combn_drugs[, ..cols]  # selecting columns
  # Rename cst_tx_dur drug code as Rx_deliv
  setnames(Combn_drugs, Combn_drug_code, Rx_drug_code)  # rename drug code column
  setkeyv(Combn_drugs, Rx_drug_code)  # sort

  Rx_deliv <- Combn_drugs[Rx_deliv, on = Rx_drug_code]  # merge datas
  Rx_deliv[  # convert Rx_drug_code to Combn_act_code
    !is.na(get(Combn_act_code)), (Rx_drug_code) := get(Combn_act_code)
  ]
  # Keep initial structure
  Rx_deliv <- Rx_deliv[, ..colorder]

  return(Rx_deliv)

}
