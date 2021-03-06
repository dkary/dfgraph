% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ui.R
\name{graph}
\alias{graph}
\title{Plot dataflow graph from R code}
\usage{
graph(
  path_to_file,
  ignore_source = NULL,
  interactive = FALSE,
  focus_node = NULL,
  prune_labels = NULL,
  prune_types = "function",
  label_option = "auto",
  hover_code = "node"
)
}
\arguments{
\item{path_to_file}{character: A file path string}

\item{ignore_source}{character: If not NULL, any file names matching those
specified will be ignored in any "source(filename)" within the scripts.}

\item{interactive}{logical: If TRUE, will use \code{\link[visNetwork]{visNetwork}}
for interactivity, otherwise \code{\link[DiagrammeR]{grViz}}}

\item{focus_node}{numeric: ID of a node to focus upon. Only the dependencies
of the specified node will be shown.}

\item{prune_labels}{character: If not NULL, any nodes with labels matching
those specified will be pruned (i.e., excluded from the plot) although their
dependencies will be cascaded.}

\item{prune_types}{character: Optionally prune nodes based on type (typically
"function" and/or "mutate"). Use NULL to override the default of "function".}

\item{label_option}{character: Either "both", "assign", "function" or
"auto" (which uses "assign" for input nodes and "function" for others).}

\item{hover_code}{character: Either "node", "network", or NULL. If "node", code
for the current node will be displayed on hover, all dependent code for
"network", and none if NULL.}
}
\value{
Returns a rendered data flow graph
}
\description{
Plot dataflow graph from R code
}
