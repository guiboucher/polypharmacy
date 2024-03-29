% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drug_bkdn.R
\name{drug_bkdn}
\alias{drug_bkdn}
\title{Translate combination drug deliveries into several single active ingredients}
\usage{
drug_bkdn(Rx_deliv, Rx_drug_code, Combn_drugs, Combn_drug_code, Combn_act_code)
}
\arguments{
\item{Rx_deliv}{Name of the table listing all prescription drugs deliveries.}

\item{Rx_drug_code}{Column name of \code{Rx_deliv} that contains the combination drug unique identifiers (any format).}

\item{Combn_drugs}{Name of the correspondence table listing all elementary active ingredients that make up each combination drug.}

\item{Combn_drug_code}{Column name of \code{Combn_drugs} that contains the combination drug unique identifiers (same format as \code{Rx_drug_code}).}

\item{Combn_act_code}{Column name of elementary active ingredients that is present in \code{Combn_drugs} (same format as \code{Rx_drug_code}).}
}
\value{
\code{data.table} of the same structure as \code{Rx_deliv}.
}
\description{
Replaces each combination drug into several deliveries of elementary active ingredients according to a user-provided correspondence table.
}
\examples{
### With matches
rx1 <- data.frame(id = c(1L, 1L, 2L, 2L, 2L),
                  code = c(159L, 753L, 123L, 456L, 789L))
split1 <- data.frame(code = c(159L, 159L, 456L, 456L, 456L),
                     splitcode = c(1591L, 1592L, 4567L, 4568L, 4569L))
drug_bkdn(Rx_deliv = rx1, Rx_drug_code = "code",
          Combn_drugs = split1, Combn_drug_code = "code", Combn_act_code = "splitcode")

### No matches
rx2 <- data.frame(id = c(1L, 1L, 2L, 2L, 2L),
                  code = c(159L, 753L, 123L, 456L, 789L))
split2 <- data.frame(CODE = c(147L, 147L, 963L, 963L, 963L),
                     SPLITCODE = c(1471L, 1472L, 9637L, 9638L, 9639L))
drug_bkdn(Rx_deliv = rx2, Rx_drug_code = "code",
          Combn_drugs = split2, Combn_drug_code = "CODE", Combn_act_code = "SPLITCODE")
}
