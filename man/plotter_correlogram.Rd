% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotter_correlogram.R
\name{plotter_correlogram}
\alias{plotter_correlogram}
\title{Wrapper for Correlogram plots}
\usage{
plotter_correlogram(
  df,
  method_shape = "circle",
  na_handle = "pairwise.complete.obs",
  Site,
  vec_var,
  vec_var_newnames,
  show_x = TRUE,
  show_y = TRUE,
  legend = FALSE
)
}
\arguments{
\item{df}{dataframe containing variables to be used to compute the correlation coefficients}

\item{vec_var}{vector of variables for subsetting the dataframe df and used for computing the correlation coefficients}
}
\value{
a correlaogram
}
\description{
Wrapper for Correlogram plots
}
