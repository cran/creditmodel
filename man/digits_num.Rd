% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_process_tools.R
\name{digits_num}
\alias{digits_num}
\title{Number of digits}
\usage{
digits_num(dat_x)
}
\arguments{
\item{dat_x}{A numeric variable.}
}
\value{
A number of digits
}
\description{
\code{digits_num} is for caculating optimal digits number for numeric variables.
}
\examples{
\dontrun{
digits_num(lendingclub[,"dti"])
# 7
}
}
