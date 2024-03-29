% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tableBuilderNew.R
\name{tableBuilderNew}
\alias{tableBuilderNew}
\title{Returns a dataset for use in a table by the Table Builder GUI.}
\usage{
tableBuilderNew(
  env,
  statistic = c("frequencies", "means", "quantiles"),
  variableName,
  dict = env$dict,
  grpbyName = "",
  CI = TRUE,
  logisetexpr = "",
  envBase = NULL,
  basePop = FALSE,
  digits = 1
)
}
\arguments{
\item{statistic}{the summary measure to use in producing the dataset - frequencies, means, quantiles}

\item{variableName}{the variable to use in producing the dataset}

\item{dict}{Dictionary object.}

\item{grpbyName}{a subgroup by which to examine the variable}

\item{CI}{logical indicating whether 95\% confidence intervals should be generated}

\item{logisetexpr}{a character expression which defines the logiset variable}

\item{basePop}{logical which to allow the users to used the base population}

\item{digits}{integer indicating the number of decimal places}

\item{envName}{the environment to use - Base, Scenario etc.}

\item{binbreaks}{The binbreaks for the outcome variable.}

\item{env.base}{Base simulation results}
}
\value{
a summary table for the entire or subgroup of the variable of interest.
}
\description{
Currently cannot do freqs for final outcomes (can only do freqs for those vars with binbreaks)
Also currently can only use a true categorical variable as a groupby variable - this coould be 
changed.
}
\details{
If the user defined a logiset/subgroup expression inthe scenario weightings screen and then
goes to tableBuilder() and sets a grpbyName, then the results they get will be on the entire 
dataset, not just on their subgroup they defined earlier.  The user can not define a logiset
expression in tableBuilder - the logisetexpr parameter is there so it can be used to show the 
user, in the scenario weightings screen, the distributions of the variable of interest for 
their subgroup only so they can better choose the proportions for their subgroup scenario.
}
