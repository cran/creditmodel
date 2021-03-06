% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_training.R
\name{train_xgb}
\alias{train_xgb}
\title{Training XGboost}
\usage{
train_xgb(
  seed_number = 1234,
  dtrain,
  nthread = 2,
  nfold = 1,
  watchlist = NULL,
  nrounds = 100,
  f_eval = "ks",
  early_stopping_rounds = 10,
  verbose = 0,
  params = NULL,
  ...
)
}
\arguments{
\item{seed_number}{Random number seed. Default is 1234.}

\item{dtrain}{train-data of xgb.DMatrix datasets.}

\item{nthread}{Number of threads}

\item{nfold}{Number of the cross validation of xgboost}

\item{watchlist}{named list of xgb.DMatrix datasets to use for evaluating model performance.generating by \code{\link{xgb_data}}}

\item{nrounds}{Max number of boosting iterations.}

\item{f_eval}{Custimized evaluation function,"ks" & "auc" are available.}

\item{early_stopping_rounds}{If NULL, the early stopping function is not triggered. If set to an integer k, training with a validation set will stop if the performance doesn't improve for k rounds.}

\item{verbose}{If 0, xgboost will stay silent. If 1, it will print information about performance.}

\item{params}{List of contains parameters of xgboost. The complete list of parameters is available at: \url{ http://xgboost.readthedocs.io/en/latest/parameter.html}}

\item{...}{Other parameters}
}
\description{
\code{train_xgb} is for training a xgb model using in \code{\link{training_model}}.
}
