% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sum_normalize.R
\name{sum_normalize}
\alias{sum_normalize}
\title{sum_normalize}
\usage{
sum_normalize(data, refFunc = "sum", norm = T, na.rm = T)
}
\arguments{
\item{data}{lowest-level data frame}

\item{refFunc}{String that can be set as "sum" or "median" to define the reference function inside the calculation}

\item{norm}{Boolean that states whether the values are scaled}

\item{na.rm}{Boolean that tells whether NA values are removed inside the reference function}
}
\value{
Sum normalized lowest-level data frame.
}
\description{
Total sum normalization. This represents as an optional pre-processing step.
}
