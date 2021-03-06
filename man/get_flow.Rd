% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ui.R
\name{get_flow}
\alias{get_flow}
\title{Extract nodes and edges from R code}
\usage{
get_flow(path_to_file, ignore_source = NULL)
}
\arguments{
\item{path_to_file}{character: A file path string}

\item{ignore_source}{character: If not NULL, any file names matching those
specified will be ignored in any "source(filename)" within the scripts.}
}
\value{
Returns a list with 2 dataframes (a) nodes with columns id, assign,
member, function, code, and (b) edges with columns to and from
}
\description{
Extract nodes and edges from R code
}
