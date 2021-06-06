#' Translate combination drug deliveries into several single active ingredients
#'
#' Replaces each combination drug into several deliveries of elementary active ingredients according to a user-provided correspondence table.
#'
#' @param Rx_deliv Name of the table listing all prescription drugs deliveries.
#' @param Rx_drug_code Column name of `Rx_deliv` that contains the combination drug unique identifiers (any format).
#' @param Combn_drugs Name of the correspondence table listing all elementary active ingredients that make up each combination drug.
#' @param Combn_drug_code Column name of `Combn_drugs` that contains the combination drug unique identifiers (same format as `Rx_drug_code`).
#' @param Combn_act_code Column name of elementary active ingredients that is present in `Combn_drugs` (same format as `Rx_drug_code`).
#'
#' @return `data.table` of the same structure as `Rx_deliv`.
#' @import data.table
#' @export
#' @examples
#' ### With matches
#' rx1 <- data.frame(id = c(1L, 1L, 2L, 2L, 2L),
#'                   code = c(159L, 753L, 123L, 456L, 789L))
#' split1 <- data.frame(code = c(159L, 159L, 456L, 456L, 456L),
#'                      splitcode = c(1591L, 1592L, 4567L, 4568L, 4569L))
#' drug_bkdn(Rx_deliv = rx1, Rx_drug_code = "code",
#'           Combn_drugs = split1, Combn_drug_code = "code", Combn_act_code = "splitcode")
#'
#' ### No matches
#' rx2 <- data.frame(id = c(1L, 1L, 2L, 2L, 2L),
#'                   code = c(159L, 753L, 123L, 456L, 789L))
#' split2 <- data.frame(CODE = c(147L, 147L, 963L, 963L, 963L),
#'                      SPLITCODE = c(1471L, 1472L, 9637L, 9638L, 9639L))
#' drug_bkdn(Rx_deliv = rx2, Rx_drug_code = "code",
#'           Combn_drugs = split2, Combn_drug_code = "CODE", Combn_act_code = "SPLITCODE")
drug_bkdn <- function(Rx_deliv, Rx_drug_code, Combn_drugs, Combn_drug_code, Combn_act_code) {

  ### Arguments check
  drug_bkdn.verif_args(Rx_deliv, Rx_drug_code, Combn_drugs, Combn_drug_code, Combn_act_code)

  ### Arrange datas
  # Rx_deliv
  if (!is.data.table(Rx_deliv)) {  # convert as data.table
    setDT(Rx_deliv)
  }
  colorder <- names(Rx_deliv)  # initial order columns
  # Combn_drugs
  if (!is.data.table(Combn_drugs)) {  # convert as data.table
    setDT(Combn_drugs)
  }

  ### Select cst_tx_dur essential cols
  Combn_drugs <- Combn_drugs[, c(Combn_drug_code, Combn_act_code), with = FALSE]  # selecting columns
  setnames(Combn_drugs, Combn_drug_code, Rx_drug_code)  # rename drug code column

  ### Final data
  Rx_deliv <- Combn_drugs[Rx_deliv, on = Rx_drug_code]  # merge datas
  Rx_deliv[!is.na(get(Combn_act_code)), (Rx_drug_code) := get(Combn_act_code)]  # convert Rx_drug_code to Combn_act_code
  Rx_deliv <- Rx_deliv[, c(colorder), with = FALSE]  # Keep initial structure

  return(Rx_deliv)

}


#' @title Verification
#' @description Arguments verification for \code{\link{drug_bkdn}}.
#' @inheritParams drug_bkdn
#' @return Messages, warnings or errors
#' @keywords internal
#' @encoding UTF-8
drug_bkdn.verif_args <- function(Rx_deliv, Rx_drug_code, Combn_drugs,
                                 Combn_drug_code, Combn_act_code) {
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
