% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Models.R
\name{predSimNormsSelectWithRounding}
\alias{predSimNormsSelectWithRounding}
\title{Predict and simulate value from n normal models with truncation/rounding to ensure simulated 
values stay within their category bounds.}
\usage{
predSimNormsSelectWithRounding(
  x.cat,
  models,
  cont.binbreaks,
  envir = parent.frame()
)
}
\arguments{
\item{x.cat}{a categorical vector}

\item{models}{a list of models with length equal to the number of categories in x.cat}

\item{cont.binbreaks}{the binbreaks of the categorical variable}

\item{envir}{environment in which to evaluate model variables.}
}
\value{
a continuous vector that when binned by cont.bonbreaks will be the same as x.cat
}
\description{
A function based on PredSimNormsSelect with the modification that if any simulated values are 
outside the binbreaks for the group, the simulated values are changed to be equal to the
boundary value.   Use when all the catToCont models for a variable are normal.
}
