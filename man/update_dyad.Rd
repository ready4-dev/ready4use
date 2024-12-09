% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_update.R
\name{update_dyad}
\alias{update_dyad}
\title{Update dyad}
\usage{
update_dyad(
  X_Ready4useDyad,
  arrange_1L_chr = c("var_ctg_chr, var_nm_chr", "category", "name", "both",
    "var_ctg_chr", "var_nm_chr"),
  categories_chr = character(0),
  dictionary_lups_ls = list(),
  dictionary_r3 = ready4use_dictionary(),
  exclude_chr = character(0),
  fn = NULL,
  fn_args_ls = NULL,
  lup_prototype_tb = NULL,
  match_var_nm_1L_chr = character(0),
  method_1L_chr = c("first", "sample"),
  names_chr = character(0),
  type_1L_chr = c("keep", "drop", "mutate", "update", "sequential", "batch", "self"),
  vars_chr = character(0),
  what_1L_chr = c("all", "dataset", "dictionary")
)
}
\arguments{
\item{X_Ready4useDyad}{PARAM_DESCRIPTION}

\item{arrange_1L_chr}{Arrange (a character vector of length one), Default: c("var_ctg_chr, var_nm_chr", "category", "name", "both", "var_ctg_chr",
"var_nm_chr")}

\item{categories_chr}{Categories (a character vector), Default: character(0)}

\item{dictionary_lups_ls}{Dictionary lookup tables (a list), Default: list()}

\item{dictionary_r3}{Dictionary (a ready4 submodule), Default: ready4use_dictionary()}

\item{exclude_chr}{Exclude (a character vector), Default: character(0)}

\item{fn}{Function (a function), Default: NULL}

\item{fn_args_ls}{Function arguments (a list), Default: NULL}

\item{lup_prototype_tb}{Lookup table prototype (a tibble), Default: NULL}

\item{match_var_nm_1L_chr}{Match variable name (a character vector of length one), Default: character(0)}

\item{method_1L_chr}{Method (a character vector of length one), Default: c("first", "sample")}

\item{names_chr}{Names (a character vector), Default: character(0)}

\item{type_1L_chr}{Type (a character vector of length one), Default: c("keep", "drop", "mutate", "update", "sequential", "batch",
"self")}

\item{vars_chr}{Variables (a character vector), Default: character(0)}

\item{what_1L_chr}{What (a character vector of length one), Default: c("all", "dataset", "dictionary")}
}
\value{
X (A dataset and data dictionary pair.)
}
\description{
update_dyad() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update dyad. The function is called for its side effects and does not return a value.
}
\keyword{internal}
