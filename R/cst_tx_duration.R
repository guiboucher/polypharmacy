#' Constant treatment duration drugs
#'
#' Overwrites the treatment duration with constant durations for each drug code included in a user-provided table.
#'
#' @param Rx_deliv Name of the table listing all prescription drugs delivered.
#' @param Rx_drug_code Column name of `Rx_deliv` that contains the drug unique identifier.
#' @param Rx_duration Column name of the constant treatment duration in the `Rx_deliv` table.
#' @param cst_tx_dur Name of the table that contains the constant treatment durations that will overwrite that in the `Rx_deliv` table for the specified drug codes.
#' @param cst_drug_code Column name of `cst_tx_dur` that contains the drug unique identifier (same format as `Rx_drug_code`).
#' @param cst_duration Column name of the constant treatment duration in the `cst_tx_dur` table (same format as `Rx_duration`).
#'
#' @return `data.table` of the same structure than `Rx_deliv`, sorted by `Rx_drug_code`, listing all drugs in which a constant treatment duration replaces the original treatment duration.
#' @import data.table
#' @export
#' @examples
#' Rx_dt <- data.frame(id = c(rep(1, 3), rep(2, 2)),
#'                     code = c("A", "B", "C", "B", "D"),
#'                     duration = c(rep(15, 3), 15, 90))
#' cst_dt <- data.frame(codes = c("A", "C", "D"),
#'                      dur = c(50, 100, 45))
#' Rx_cst <- cst_tx_duration(Rx_deliv = Rx_dt,
#'                           Rx_drug_code = "code", Rx_duration = "duration",
#'                           cst_tx_dur = cst_dt,
#'                           cst_drug_code = "codes", cst_duration = "dur")
cst_tx_duration <- function(
  Rx_deliv, Rx_drug_code, Rx_duration,
  cst_tx_dur, cst_drug_code, cst_duration
) {

# Internal FCTS -----------------------------------------------------------

  verif_args <- function(Rx_deliv, Rx_drug_code, Rx_duration,
                         cst_tx_dur, cst_drug_code, cst_duration) {
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
    # cst_tx_dur
    if (!is.data.frame(cst_tx_dur)) {
      addError("cst_tx_dur must be a data.frame.", check)
    }
    # Rx_drug_code, Rx_duration, cst_drug_code, cst_duration
    for (var in c("Rx_drug_code", "Rx_duration", "cst_drug_code", "cst_duration")) {
      if (!is.character(get(var))) {
        addError(paste0(var," must be a character vector."), check)
      }
      if (length(get(var)) != 1) {
        addError(paste0(var," must be a single value."), check)
      }
    }
    finishArgCheck(check)


    ## 2) Arg != "Arg"
    for (arg in c("Rx_drug_code", "Rx_duration", "cst_drug_code", "cst_duration")) {
      if (arg == get(arg)) {
        addError(paste0(arg," can't be equal to '",arg,"'. Please modify value."),
                 check)
      }
    }
    ## 2) Columns exists?
    # Rx_deliv
    for (col in c("Rx_drug_code", "Rx_duration")) {
      if (!get(col) %in% names(Rx_deliv)) {
        addError(paste0(get(col)," (",col,") is not a column in Rx_deliv."),
                 check)
      }
    }
    # cst_tx_dur
    for (col in c("cst_drug_code", "cst_duration")) {
      if (!get(col) %in% names(cst_tx_dur)) {
        addError(paste0(get(col)," (",col,") is not a column in cst_tx_dur."),
                 check)
      }
    }
    finishArgCheck(check)

    ## 3) Columns must have the same class
    # drug code
    if (class(Rx_deliv[[Rx_drug_code]]) != class(cst_tx_dur[[cst_drug_code]])) {
      addError(paste0(
        Rx_drug_code," column (Rx_drug_code, class: ",class(Rx_deliv[[Rx_drug_code]]),") ",
        "must have the same class as ",
        cst_drug_code," column (cst_drug_code, class: ",class(cst_tx_dur[[cst_drug_code]]),")."
      ), check)
    }
    # duration
    if (class(Rx_deliv[[Rx_duration]]) != class(cst_tx_dur[[cst_duration]])) {
      addError(paste0(
        Rx_duration," column (Rx_duration, class: ",class(Rx_deliv[[Rx_duration]]),") ",
        "must have the same class as ",
        cst_duration," column (cst_duration, class: ",class(cst_tx_dur[[cst_duration]]),")."
      ), check)
    }
    ## 3) No NAs
    # Rx_deliv
    for (col in c("Rx_drug_code", "Rx_duration")) {
      if (anyNA(Rx_deliv[[get(col)]])) {
        addError(paste0(get(col)," column (",col,") can't contains NAs."), check)
      }
    }
    # cst_tx_dur
    for (col in c("cst_drug_code", "cst_duration")) {
      if (anyNA(cst_tx_dur[[get(col)]])) {
        addError(paste0(get(col)," column (",col,") can't contains NAs."), check)
      }
    }
    ## 3) duration as numeric values
    # Rx_duration
    if (!is.numeric(Rx_deliv[[Rx_duration]])) {
      addError(paste0(Rx_duration," column (Rx_duration) must be numeric."), check)
    }
    if (!is.numeric(cst_tx_dur[[cst_duration]])) {
      addError(paste0(cst_duration," column (cst_duration) must be numeric."), check)
    }
    finishArgCheck(check)

  }


# Code FCT ----------------------------------------------------------------

  ## Arrange datas
  # Rx_deliv
  if (!is.data.table(Rx_deliv)) {
    Rx_deliv <- as.data.table(Rx_deliv)
  } else {
    Rx_deliv <- copy(Rx_deliv)
  }
  colorder <- names(Rx_deliv)  # initial order columns
  setkeyv(Rx_deliv, Rx_drug_code)  # sort

  # cst_tx_dur
  if (!is.data.table(cst_tx_dur)) {
    cst_tx_dur <- as.data.table(cst_tx_dur)
  } else {
    cst_tx_dur <- copy(cst_tx_dur)
  }
  # Select cst_tx_dur essential cols
  cols <- c(cst_drug_code, cst_duration)
  cst_tx_dur <- cst_tx_dur[, ..cols]
  # Rename cst_tx_dur drug code as Rx_deliv
  setnames(cst_tx_dur, cst_drug_code, Rx_drug_code)
  setkeyv(cst_tx_dur, Rx_drug_code)  # sort

  # Merge cst_tx_dur to Rx_deliv
  Rx_deliv <- cst_tx_dur[Rx_deliv, on = Rx_drug_code]
  # Mutate to constant duration
  Rx_deliv[!is.na(get(cst_duration)), (Rx_duration) := get(cst_duration)]
  # Keep inital columns
  Rx_deliv <- Rx_deliv[, ..colorder]

  return(Rx_deliv)

}
