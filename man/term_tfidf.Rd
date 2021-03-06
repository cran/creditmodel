% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_transformation.R
\name{term_tfidf}
\alias{term_tfidf}
\alias{term_idf}
\alias{term_filter}
\title{TF-IDF}
\usage{
term_tfidf(term_df, idf = NULL)

term_idf(term_df, n_total = NULL)

term_filter(term_df, low_freq = 0.01, stop_words = NULL)
}
\arguments{
\item{term_df}{A data.frame with id and term.}

\item{idf}{A data.frame with idf.}

\item{n_total}{Number of documents.}

\item{low_freq}{Use rate of terms or use numbers of terms.}

\item{stop_words}{Stop words.}
}
\value{
A data.frame
}
\description{
The \code{term_filter} is for filtering stop_words and low frequency words.
The \code{term_idf} is for computing idf(inverse documents frequency) of terms.
The \code{term_tfidf} is for computing tf-idf of documents.
}
\examples{
term_df = data.frame(id = c(1,1,1,2,2,3,3,3,4,4,4,4,4,5,5,6,7,7,
                            8,8,8,9,9,9,10,10,11,11,11,11,11,11),
terms = c('a','b','c','a','c','d','d','a','b','c','a','c','d','a','c',
          'd','a','e','f','b','c','f','b','c','h','h','i','c','d','g','k','k'))
term_df = term_filter(term_df = term_df, low_freq = 1)
idf = term_idf(term_df)
tf_idf = term_tfidf(term_df,idf = idf)
}
