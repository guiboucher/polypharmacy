% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-functions.R
\encoding{UTF-8}
\name{stat_quantile_prob}
\alias{stat_quantile_prob}
\title{Statistic functions}
\usage{
stat_quantile_prob(x)
}
\arguments{
\item{x}{Character string, a quantile function, quarter values.}
}
\value{
Number {25, 50, 75}
}
\description{
Determine the percentile from a \emph{qX} value where \emph{X} is a number from 1 to 3.
}
\examples{
stat_quantile_prob("q1")
stat_quantile_prob("q2")
stat_quantile_prob("q3")
}
\keyword{internal}
