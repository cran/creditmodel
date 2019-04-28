% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_derivation.R
\name{derived_ts_vars}
\alias{derived_ts_vars}
\alias{derived_ts}
\title{Derivation of Behavioral Variables}
\usage{
derived_ts_vars(dat, grx, td = 12, der = c("cvs", "sums", "means",
  "maxs", "max_mins", "time_intervals", "cnt_intervals", "total_pcts",
  "cum_pcts", "partial_acfs"), parallel = TRUE)

derived_ts(dat = dat, grx_x = NULL, td = 12, der = c("cvs", "sums",
  "means", "maxs", "max_mins", "time_intervals", "cnt_intervals",
  "total_pcts", "cum_pcts", "partial_acfs"))
}
\arguments{
\item{dat}{A data.frame contained only predict variables.}

\item{grx}{Regular expressions used to match variable names.}

\item{td}{Number of variables to derivate.}

\item{der}{Variables to derivate}

\item{parallel}{Logical, parallel computing. Default is FALSE.}

\item{grx_x}{Regular expression used to match a group of variable names.}
}
\description{
This function is used for derivating behavioral variables and is not intended to be used by end user.
}