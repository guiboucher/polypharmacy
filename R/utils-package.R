#' Calculate several polypharmacy indicators
#'
#' This package analyse prescription drug deliveries to calculate several indicators of polypharmacy corresponding to the various definitions found in the literature.
#'
#' It is essential to know the concepts used to calculate the various polypharmacy indicators to adequately use this package.\cr
#' The core of the package is the `data_process()` function that creates the `data.table` of drug treatments by restructuring the drug delivery records (usually extracted from a pharmacy or a health insurance information system) into continuous periods of drug availability (called drug treatments), applying user-defined arguments such as the grace periods between renewals or the longest treatment duration that an individual may accumulate through the successive renewals.\cr\cr
#' Then, each polypharmacy indicator can be computed using the corresponding function (`ind_simult()`, `ind_stdcumul()`, `ind_wcumul()`, `ind_stdcontinuous()`, `ind_ucontinuous()`) or using the overall function `indicators()` to select the desired indicator(s) to be calculated at once.\cr\cr
#' Prior to running `data_process()` the user may need to pre-process the table of original drug delivery records to break down combination drug into their individual components (`drugs_bkdn()`) and/or to overwrite some delivery durations of specified drugs with constant durations (`cst_trt_dur()`).
#'
#' @docType package
"_PACKAGE"
