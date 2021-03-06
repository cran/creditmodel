% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_anaylsis.R
\name{swap_analysis}
\alias{swap_analysis}
\title{Swap Out/Swap In Analysis}
\usage{
swap_analysis(
  dat,
  new_rules,
  old_rules,
  target = NULL,
  cross_type = "total_pct",
  value = NULL
)
}
\arguments{
\item{dat}{A data.frame with independent variables.}

\item{new_rules}{A list of new rules.}

\item{old_rules}{A list of old rules.}

\item{target}{The name of target variable.}

\item{cross_type}{Output form of the result of crosstable. Provide these four forms: "total_sum","total_pct","bad_sum","bad_pct" .}

\item{value}{The name of the variable to sum. When this parameter is NULL, the default statistics is to sum frequency.}
}
\value{
A cross table.
}
\description{
\code{swap_analysis} is for swap out/swap in analysis.
}
\examples{
swap_analysis(dat = UCICreditCard, new_rules = list("SEX == 'male' & AGE < 25"),
 old_rules = list("SEX == 'male' & AGE < 30"),
 target = "default.payment.next.month", cross_type = "bad_pct", value = "LIMIT_BAL")
}
