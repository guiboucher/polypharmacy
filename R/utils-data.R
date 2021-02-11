
# Rx_processed ------------------------------------------------------------

#' Table: Processed "unprocessed table"
#'
#' Table required for the calculation of the polypharmacy indicators. This table is created by using `data_process()` function on `Rx_unprocessed` data.
#'
#' @format A `data.table` with 6792 obs and 4 variables:
#' \describe{
#'   \item{id}{Individual unique identifier.}
#'   \item{code}{Drug unique identifier.}
#'   \item{tx_start}{The date of initiation of the reconstructed continued treatment (format as date).}
#'   \item{tx_end}{The date of the last day of the reconstructed continued treatment (format as date).}
#' }
#' @encoding UTF-8
"Rx_processed"


# Rx_unprocessed ----------------------------------------------------------

#' Table: Prescription drugs deliveries
#'
#' Table listing all prescription drugs deliveries.
#'
#' @format A `data.table` with 17060 obs and 4 variables:
#' \describe{
#'   \item{id}{Individual unique identifier}
#'   \item{code}{Drug unique identifier}
#'   \item{start}{Date of the drug delivery}
#'   \item{duration}{Duration of the delivery}
#' }
#' @encoding UTF-8
"Rx_unprocessed"
