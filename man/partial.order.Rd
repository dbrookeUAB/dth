% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partial_order.R
\name{partial.order}
\alias{partial.order}
\title{Partial Reordering}
\usage{
partial.order(
  x,
  exclude = NULL,
  where = "first",
  reorder_excluded = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{x}{vector to be reorder}

\item{exclude}{a vector of values to not be reordered. Default=NULL}

\item{where}{where to place the excluded values. Default="first"}

\item{reorder_excluded}{whether to reorder the exclude vector. Default=FALSE}

\item{verbose}{console displays the values excluded and where. Default=FALSE}
}
\description{
Partial Reordering
}
