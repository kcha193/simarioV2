% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Simenv.r
\name{applyContAdjustmentToSimframe}
\alias{applyContAdjustmentToSimframe}
\title{Apply continuous adjustments to simframe.}
\usage{
applyContAdjustmentToSimframe(
  Simenv,
  varname,
  iteration,
  desiredProps,
  catToContModels,
  cont.binbreaks,
  propensities
)
}
\arguments{
\item{Simenv}{simenv receiving object. Simenv$simframe is modified.}

\item{catToContModels}{A list of models which will to used to convert the adjusted categorical variable back 
to continuous.}

\item{cont.binbreaks}{Binbreaks for the variable being adjusted if exist.}

\item{propensities}{named list of propensities for the cat.adjustments}

\item{varnames}{varname(s) of variable(s) to adjust.}

\item{iteration'}{the current iteration}

\item{desired_props}{a vector of desired proportions, eg: c(0.1, 0.1, 0.8).
Can have a "logisetexpr" attribute - if so, this is evaulated, and becomes a logical vector indicating which observations of "varname" to adjust.
       (i.e. the "logisetexpr" attribute gives which subset of the data the desired_props are intended for).}
}
\value{
NULL. simframe in receiving object is modified directly.
}
\description{
Apply continuous adjustments to simframe.
}
