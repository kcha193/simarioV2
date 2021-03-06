% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/support.r
\name{assign_values}
\alias{assign_values}
\title{Assign the values of variables (specifed by name in a source character vector)
to another set of variables (specifed by name in a dest character vector).
Will modify variables in enclosing environments.}
\usage{
assign_values(dest_var_names, src_var_names)
}
\arguments{
\item{dest_var_names}{a character vector of variable names to be assigned}

\item{src_var_names}{a parallel character vector of variable names supplying the values to}
}
\description{
Assign the values of variables (specifed by name in a source character vector)
to another set of variables (specifed by name in a dest character vector).
Will modify variables in enclosing environments.
}
