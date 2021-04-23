
# sample_Rx_processed --------------------------------------------------------

#' Table: Processed *unprocessed table*
#'
#' This table is provided to users of this package for training purposes. It is created by using \code{\link{data_process}} function on \code{\link{sample_Rx_unprocessed}} data.
#'
#' @format A `data.table` with 6792 obs and 4 variables:
#' \describe{
#'   \item{id}{Individual unique identifier.}
#'   \item{code}{Medication unique identifier.}
#'   \item{tx_start}{The date of initiation of the reconstructed continued treatment (format as date).}
#'   \item{tx_end}{The end date of the reconstructed continued treatment (format as date).}
#' }
#' @encoding UTF-8
"sample_Rx_processed"


# sample_Rx_unprocessed ------------------------------------------------------

#' Table: Prescription drugs deliveries
#'
#' A sample table of prescription drugs deliveries provided to users of this package for training purposes. It contains the raw information that leads to \code{\link{sample_Rx_processed}} when processes by the \code{\link{data_process}} function.
#'
#' @format A `data.table` with 17060 obs and 4 variables:
#' \describe{
#'   \item{id}{Individual unique identifier.}
#'   \item{code}{Medication unique identifier.}
#'   \item{start}{Date of the medication delivery.}
#'   \item{duration}{Treatment duration of the delivery.}
#' }
#' @encoding UTF-8
"sample_Rx_unprocessed"
