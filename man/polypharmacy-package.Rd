% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-package.R
\docType{package}
\name{polypharmacy-package}
\alias{polypharmacy}
\alias{polypharmacy-package}
\title{Calculate several polypharmacy indicators}
\description{
This package analyses prescription drug deliveries to calculate several indicators of polypharmacy corresponding to the various definitions found in the literature.
}
\details{
It is essential to understand the underlying concepts used to calculate the various polypharmacy indicators to adequately use this package.\cr
The core of the package is the \code{data_process()} function that creates a \code{data.table} of drug treatments by restructuring the drug delivery records (usually extracted from a pharmacy or a health insurance information system) into continuous periods of drug availability (called drug treatments) for every individual over the whole period of the study period. This process relies on several user-defined arguments such as the grace periods between renewals or the longest treatment duration that an individual may accumulate through successive early renewals.\cr\cr
Then, each polypharmacy indicator can be computed using the corresponding function (\code{ind_simult()}, \code{ind_stdcumul()}, \code{ind_wcumul()}, \code{ind_stdcontinuous()}, \code{ind_ucontinuous()}) or using the wrapper function \code{indicators()} to select the desired indicator(s) to be calculated at once.\cr\cr
Prior to running \code{data_process()} the user may need to pre-process the table of original drug delivery records to break down combination drug into their individual components (\code{drugs_bkdn()}) and/or to overwrite the delivery durations of some specified drugs with constant user-defined durations (\code{cst_trt_dur()}).
}
\seealso{
Useful links:
\itemize{
  \item Report bugs at \url{https://github.com/guiboucher/polypharmacy/issues}
}

}
\author{
\strong{Maintainer}: Guillaume Boucher \email{guiboucher8@gmail.com}

Authors:
\itemize{
  \item Bernard Candas \email{bernard.candas.1@gmail.com}
  \item Houssem Missaoui \email{missaoui_houssem@hotmail.fr}
}

Other contributors:
\itemize{
  \item Caroline Sirois \email{caroline.sirois@pha.ulaval.ca} [scientific advisor]
  \item Marc Simard \email{marc.simard@inspq.qc.ca} [scientific advisor]
}

}
