% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_transformation.R
\name{gather_data}
\alias{gather_data}
\title{gather or aggregate data}
\usage{
gather_data(dat, x_list = NULL, ID = NULL, FUN = sum_x)
}
\arguments{
\item{dat}{A data.frame contained only predict variables.}

\item{x_list}{The names of variables to gather.}

\item{ID}{The name of ID of observations or key variable of data. Default is NULL.}

\item{FUN}{The function of gathering method.}
}
\description{
This function is used for gathering or aggregating data.
}
\details{
The key to creating a good model is not the power of a specific modelling technique, but the breadth and depth of derived variables that represent a higher level of knowledge about the phenomena under examination.
}
\examples{
dat = data.frame(id = c(1,1,1,2,2,3,3,3,4,4,4,4,4,5,5,6,7,7,
                            8,8,8,9,9,9,10,10,11,11,11,11,11,11),
                     terms = c('a','b','c','a','c','d','d','a',
                               'b','c','a','c','d','a','c',
                                  'd','a','e','f','b','c','f','b',
                               'c','h','h','i','c','d','g','k','k'),
                     time = c(8,3,1,9,6,1,4,9,1,3,4,8,2,7,1,
                              3,4,1,8,7,2,5,7,8,8,2,1,5,7,2,7,3))

gather_data(dat = dat, x_list = "time", ID = 'id', FUN = sum_x)
}
