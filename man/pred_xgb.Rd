% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_training.R
\name{pred_xgb}
\alias{pred_xgb}
\title{pred_xgb}
\usage{
pred_xgb(
  xgb_model = NULL,
  dat,
  x_list = NULL,
  miss_values = NULL,
  model_name = NULL,
  model_path = getwd()
)
}
\arguments{
\item{xgb_model}{XGboost Model generated by \code{\link{training_model}}.}

\item{dat}{Dataframe of new data.}

\item{x_list}{Into the model variables.}

\item{miss_values}{missing values.}

\item{model_name}{Name of model}

\item{model_path}{dir_path of model.}
}
\value{
new prob.
}
\description{
\code{pred_xgb} is for using xgboost model to predict new data.
}
\seealso{
\code{\link{training_model}}, \code{\link{pred_score}}
}
