% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_word.R
\name{multi_grepl_n}
\alias{multi_grepl_n}
\title{multi_grepl_n}
\usage{
multi_grepl_n(pattern, x, tolower = T)
}
\arguments{
\item{pattern}{vector of character string patterns}

\item{x}{a vector of strings to match with patterns}

\item{tolower}{a bool. Indicates whether or not to make each pattern and input vector lowercase}
}
\value{
A vector containing the number of times each pattern was matched
}
\description{
Returns the number of times grepl matched a pattern; vectorised.
}
