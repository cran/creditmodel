% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_process_tools.R
\name{max_min_norm}
\alias{max_min_norm}
\title{Max Min Normalization}
\usage{
max_min_norm(x)
}
\arguments{
\item{x}{Vector}
}
\value{
Normalized vector
}
\description{
\code{max_min_norm} is for normalizing each column vector of matrix 'x' using max_min normalization
}
\examples{
dat_s = apply(UCICreditCard[,12:14], 2, max_min_norm)
}
