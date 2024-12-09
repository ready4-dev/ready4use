% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_update.R
\name{update_column_names}
\alias{update_column_names}
\title{Update column names}
\usage{
update_column_names(
  X_Ready4useDyad,
  patterns_ls = list(c("[[:space:]]", "")),
  update_desc_1L_lgl = FALSE
)
}
\arguments{
\item{X_Ready4useDyad}{PARAM_DESCRIPTION}

\item{patterns_ls}{Patterns (a list), Default: list(c("[\link{:space:}]", ""))}

\item{update_desc_1L_lgl}{Update description (a logical vector of length one), Default: FALSE}
}
\value{
X (A dataset and data dictionary pair.)
}
\description{
update_column_names() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update column names. The function is called for its side effects and does not return a value.
}
