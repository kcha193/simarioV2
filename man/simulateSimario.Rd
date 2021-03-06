% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Simenv.r
\name{simulateSimario}
\alias{simulateSimario}
\title{Perform a simulation of X runs using parallel computing.}
\usage{
simulateSimario(Simenv, total_runs = 1, simulateFun, parallel = TRUE)
}
\arguments{
\item{Simenv}{Simenv receiving object}

\item{total_runs}{total number of runs to simulate}

\item{simulateFun}{a function contains a set of actual simulation to be performed}

\item{parallel}{logical, which allows the user to decide on using parallel computing}
}
\value{
Simenv object with simulated results
}
\description{
NB: if it exists, uses propensities in global environment when doing adjustments for year 1
}
