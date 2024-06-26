% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/C3_ready4use_mapes.R
\name{ready4use_mapes}
\alias{ready4use_mapes}
\alias{make_pt_ready4use_mapes}
\alias{is_ready4use_mapes}
\title{ready4 submodule class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.}
\usage{
ready4use_mapes(x = make_pt_ready4use_mapes())

make_pt_ready4use_mapes(
  param_name_chr = character(0),
  sex_age_band_chr = character(0),
  mape_05_yr_mde_dbl = numeric(0),
  mape_10_yr_mde_dbl = numeric(0),
  mape_15_yr_mde_dbl = numeric(0),
  mape_05_yr_min_dbl = numeric(0),
  mape_10_yr_min_dbl = numeric(0),
  mape_15_yr_min_dbl = numeric(0),
  mape_05_yr_max_dbl = numeric(0),
  mape_10_yr_max_dbl = numeric(0),
  mape_15_yr_max_dbl = numeric(0),
  mape_05_yr_shp_dbl = numeric(0),
  mape_10_yr_shp_dbl = numeric(0),
  mape_15_yr_shp_dbl = numeric(0)
)

is_ready4use_mapes(x)
}
\arguments{
\item{x}{An object of any type}

\item{param_name_chr}{Parameter name (a character vector), Default: character(0)}

\item{sex_age_band_chr}{Sex age band (a character vector), Default: character(0)}

\item{mape_05_yr_mde_dbl}{Mean absolute prediction error 05 year mode (a double vector), Default: numeric(0)}

\item{mape_10_yr_mde_dbl}{Mean absolute prediction error 10 year mode (a double vector), Default: numeric(0)}

\item{mape_15_yr_mde_dbl}{Mean absolute prediction error 15 year mode (a double vector), Default: numeric(0)}

\item{mape_05_yr_min_dbl}{Mean absolute prediction error 05 year minimum (a double vector), Default: numeric(0)}

\item{mape_10_yr_min_dbl}{Mean absolute prediction error 10 year minimum (a double vector), Default: numeric(0)}

\item{mape_15_yr_min_dbl}{Mean absolute prediction error 15 year minimum (a double vector), Default: numeric(0)}

\item{mape_05_yr_max_dbl}{Mean absolute prediction error 05 year maximum (a double vector), Default: numeric(0)}

\item{mape_10_yr_max_dbl}{Mean absolute prediction error 10 year maximum (a double vector), Default: numeric(0)}

\item{mape_15_yr_max_dbl}{Mean absolute prediction error 15 year maximum (a double vector), Default: numeric(0)}

\item{mape_05_yr_shp_dbl}{Mean absolute prediction error 05 year shape (a double vector), Default: numeric(0)}

\item{mape_10_yr_shp_dbl}{Mean absolute prediction error 10 year shape (a double vector), Default: numeric(0)}

\item{mape_15_yr_shp_dbl}{Mean absolute prediction error 15 year shape (a double vector), Default: numeric(0)}
}
\value{
A validated instance of the ready4 submodule class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.

A prototype for ready4 submodule class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.

A logical value, TRUE if a valid instance of the ready4 submodule class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
}
\description{
Create a new valid instance of the ready4 submodule class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
}
\details{
Tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
}
