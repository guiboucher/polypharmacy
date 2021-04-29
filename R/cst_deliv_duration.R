#' Constant delivery duration drugs
#'
#' Overwrites the recorded delivery durations of specific drugs with constant durations as specified in a user-provided table.
#'
#' @param Rx_deliv Name of the table listing all prescription drugs delivered.
#' @param Rx_drug_code Column name of `Rx_deliv` that contains the drug unique identifier.
#' @param Rx_deliv_dur Column name of the constant treatment duration in the `Rx_deliv` table.
#' @param Cst_deliv_dur Name of the table that contains the constant delivery durations that will overwrite that in the `Rx_deliv` table for the specified drug codes.
#' @param Cst_drug_code Column name of `Cst_deliv_dur` that contains the drug unique identifier (same format as `Rx_drug_code`).
#' @param Cst_duration Column name of the constant treatment duration in the `Cst_deliv_dur` table (same format as `Rx_deliv_dur`).
#'
#' @return `data.table` of the same structure as `Rx_deliv`.
#' @import data.table
#' @encoding UTF-8
#' @export
#' @examples
#' # With matches
#' rx1 <- data.frame(id = c(1, 1, 2, 2, 2), code = c("A", "B", "B", "C", "D"),
#'                   duration = as.integer(c(30, 15, 15, 7, 90)))
#' cst1 <- data.frame(CODES = c("B", "D"), DURATION = as.integer(c(45, 60)))
#' cst_deliv_duration(
#'   Rx_deliv = rx1, Rx_drug_code = "code", Rx_deliv_dur = "duration",
#'   Cst_deliv_dur = cst1, Cst_drug_code = "CODES", Cst_duration = "DURATION"
#' )
#'
#' # No matches
#' rx2 <- data.frame(id = c(1, 1, 2, 2, 2), code = c("A", "B", "B", "C", "D"),
#'                   duration = as.integer(c(30, 15, 15, 7, 90)))
#' cst2 <- data.frame(CODES = c("E", "F"), DURATION = as.integer(c(45, 60)))
#' cst_deliv_duration(
#'   Rx_deliv = rx2, Rx_drug_code = "code", Rx_deliv_dur = "duration",
#'   Cst_deliv_dur = cst2, Cst_drug_code = "CODES", Cst_duration = "DURATION"
#' )
cst_deliv_duration <- function(
  Rx_deliv, Rx_drug_code, Rx_deliv_dur,
  Cst_deliv_dur, Cst_drug_code, Cst_duration
) {

  ### Arguments check
  cst_deliv_duration.verif_args(Rx_deliv, Rx_drug_code, Rx_deliv_dur,
                                Cst_deliv_dur, Cst_drug_code, Cst_duration)

  ### Arrange datas
  # Rx_deliv
  if (!is.data.table(Rx_deliv)) {
    setDT(Rx_deliv)  # convert to data.table
  }
  colorder <- names(Rx_deliv)  # initial order columns
  # Cst_deliv_dur
  if (!is.data.table(Cst_deliv_dur)) {
    setDT(Cst_deliv_dur)  # convert to data.table
  }
  Cst_deliv_dur <- Cst_deliv_dur[, c(Cst_drug_code, Cst_duration), with = FALSE]  # cols selection
  setnames(Cst_deliv_dur, Cst_drug_code, Rx_drug_code)  # Rename Cst_deliv_dur drug code as Rx_deliv

  ### Final data
  # Merge Cst_deliv_dur to Rx_deliv
  Rx_deliv <- Cst_deliv_dur[Rx_deliv, on = Rx_drug_code]
  # Mutate to constant duration
  Rx_deliv[!is.na(get(Cst_duration)), (Rx_deliv_dur) := get(Cst_duration)]
  # Keep inital columns
  Rx_deliv <- Rx_deliv[, c(colorder), with = FALSE]

  return(Rx_deliv)

}

#' @title Verification
#' @description Arguments verification for \code{\link{cst_deliv_duration}}.
#' @keywords internal
#' @encoding UTF-8
cst_deliv_duration.verif_args <- function(Rx_deliv, Rx_drug_code, Rx_deliv_dur,
                                          Cst_deliv_dur, Cst_drug_code, Cst_duration) {
  ### Arguments verification
  ### 1) Argument classes + length
  ### 2) - Arg != "Arg"
  ###    - Columns exists?
  ### 3) - Columns must have same class
  ###    - No NAs
  ###    - duration as numeric values

  check <- newArgCheck()
  ## 1) Argument classes
  # Rx_deliv
  if (!is.data.frame(Rx_deliv)) {
    addError("Rx_deliv must be a data.frame.", check)
  }
  # Cst_deliv_dur
  if (!is.data.frame(Cst_deliv_dur)) {
    addError("Cst_deliv_dur must be a data.frame.", check)
  }
  # Rx_drug_code, Rx_deliv_dur, Cst_drug_code, Cst_duration
  for (var in c("Rx_drug_code", "Rx_deliv_dur", "Cst_drug_code", "Cst_duration")) {
    if (!is.character(get(var))) {
      addError(paste0(var," must be a character vector."), check)
    }
    if (length(get(var)) != 1) {
      addError(paste0(var," must be a single value."), check)
    }
  }
  finishArgCheck(check)


  ## 2) Arg != "Arg"
  for (arg in c("Rx_drug_code", "Rx_deliv_dur", "Cst_drug_code", "Cst_duration")) {
    if (arg == get(arg)) {
      addError(paste0(arg," can't be equal to '",arg,"'. Please modify value."),
               check)
    }
  }
  ## 2) Columns exists?
  # Rx_deliv
  for (col in c("Rx_drug_code", "Rx_deliv_dur")) {
    if (!get(col) %in% names(Rx_deliv)) {
      addError(paste0(get(col)," (",col,") is not a column in Rx_deliv."),
               check)
    }
  }
  # Cst_deliv_dur
  for (col in c("Cst_drug_code", "Cst_duration")) {
    if (!get(col) %in% names(Cst_deliv_dur)) {
      addError(paste0(get(col)," (",col,") is not a column in Cst_deliv_dur."),
               check)
    }
  }
  finishArgCheck(check)

  ## 3) Columns must have the same class
  # drug code
  if (class(Rx_deliv[[Rx_drug_code]]) != class(Cst_deliv_dur[[Cst_drug_code]])) {
    addError(paste0(
      Rx_drug_code," column (Rx_drug_code, class: ",class(Rx_deliv[[Rx_drug_code]]),") ",
      "must have the same class as ",
      Cst_drug_code," column (Cst_drug_code, class: ",class(Cst_deliv_dur[[Cst_drug_code]]),")."
    ), check)
  }
  # duration
  if (class(Rx_deliv[[Rx_deliv_dur]]) != class(Cst_deliv_dur[[Cst_duration]])) {
    addError(paste0(
      Rx_deliv_dur," column (Rx_deliv_dur, class: ",class(Rx_deliv[[Rx_deliv_dur]]),") ",
      "must have the same class as ",
      Cst_duration," column (Cst_duration, class: ",class(Cst_deliv_dur[[Cst_duration]]),")."
    ), check)
  }
  ## 3) No NAs
  # Rx_deliv
  for (col in c("Rx_drug_code", "Rx_deliv_dur")) {
    if (anyNA(Rx_deliv[[get(col)]])) {
      addError(paste0(get(col)," column (",col,") can't contains NAs."), check)
    }
  }
  # Cst_deliv_dur
  for (col in c("Cst_drug_code", "Cst_duration")) {
    if (anyNA(Cst_deliv_dur[[get(col)]])) {
      addError(paste0(get(col)," column (",col,") can't contains NAs."), check)
    }
  }
  ## 3) duration as numeric values
  # Rx_deliv_dur
  if (!is.numeric(Rx_deliv[[Rx_deliv_dur]])) {
    addError(paste0(Rx_deliv_dur," column (Rx_deliv_dur) must be numeric."), check)
  }
  if (!is.numeric(Cst_deliv_dur[[Cst_duration]])) {
    addError(paste0(Cst_duration," column (Cst_duration) must be numeric."), check)
  }
  finishArgCheck(check)

}
