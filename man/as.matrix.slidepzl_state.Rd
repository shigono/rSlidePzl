% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class.R
\name{as.matrix.slidepzl_state}
\alias{as.matrix.slidepzl_state}
\title{convert a state into a character matrix}
\usage{
\method{as.matrix}{slidepzl_state}(state, setting, error_invalid = TRUE)
}
\arguments{
\item{state}{a object of 'slidepzl_state' class.}

\item{setting}{a object of 'slidepzl_setting' class.}

\item{error_invalid}{logical. Raise error if the state is invalid?}
}
\value{
a character matrix.
}
\description{
Internal. Convert a state into a character matrix.
Used to show verbose messages in \code{makeGraph} function.
}
