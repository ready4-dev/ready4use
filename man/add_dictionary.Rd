% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add.R
\name{add_dictionary}
\alias{add_dictionary}
\title{Add dictionary}
\usage{
add_dictionary(
  X_Ready4useDyad = Ready4useDyad(),
  new_cases_r3 = ready4use_dictionary(),
  var_ctg_chr = "Uncategorised",
  arrange_by_1L_chr = c("category", "name")
)
}
\arguments{
\item{X_Ready4useDyad}{PARAM_DESCRIPTION, Default: Ready4useDyad()}

\item{new_cases_r3}{New cases (a ready4 submodule), Default: ready4use_dictionary()}

\item{var_ctg_chr}{Variable category (a character vector), Default: 'Uncategorised'}

\item{arrange_by_1L_chr}{Arrange by (a character vector of length one), Default: c("category", "name")}
}
\value{
X (A dataset and data dictionary pair.)
}
\description{
add_dictionary() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add dictionary. The function is called for its side effects and does not return a value.
}
