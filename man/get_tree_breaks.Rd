% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variable_binning.R
\name{get_tree_breaks}
\alias{get_tree_breaks}
\title{Getting the breaks for terminal nodes from decision tree}
\usage{
get_tree_breaks(
  dat,
  x,
  target,
  pos_flag = NULL,
  tree_control = list(p = 0.02, cp = 1e-06, xval = 5, maxdepth = 10),
  sp_values = NULL
)
}
\arguments{
\item{dat}{A data frame with x and target.}

\item{x}{name of variable to cut breaks by tree.}

\item{target}{The name of target variable.}

\item{pos_flag}{The value of positive class of target variable, default: "1".}

\item{tree_control}{the list of parameters to control cutting initial breaks by decision tree.
\itemize{
  \item \code{p} the minimum percent of observations in any terminal <leaf> node. 0 < p< 1; 0.01 to 0.1 usually work.
  \item \code{cp} complexity parameter. the larger, the more conservative the algorithm will be. 0 < cp< 1 ; 0.0001 to 0.0000001 usually work.
  \item \code{xval} number of cross-validations.Default: 5
  \item \code{max_depth} maximum depth of a tree. Default: 10
}}

\item{sp_values}{A list of special value. Default: NULL.}
}
\description{
\code{get_tree_breaks} is for generating initial braks by decision tree for a numerical or nominal variable.
The \code{get_breaks} function is a simpler wrapper for \code{get_tree_breaks}.
}
\examples{
#tree breaks
tree_control = list(p = 0.02, cp = 0.000001, xval = 5, maxdepth = 10)
tree_breaks = get_tree_breaks(dat = UCICreditCard, x = "MARRIAGE",
target = "default.payment.next.month", tree_control = tree_control)
}
\seealso{
\code{\link{get_breaks}}, \code{\link{get_breaks_all}}
}
