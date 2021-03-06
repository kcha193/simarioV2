% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.r
\name{loadMergedFile}
\alias{loadMergedFile}
\title{Loads and merges a CSV/XLS file with the supplied values (keys). ie:
returns a dataframe (excluding key_column_name) for the supplied 
values that exist in key_column_name of the file}
\usage{
loadMergedFile(filedir, filename, key_column_name, selected_keys)
}
\arguments{
\item{filedir}{file directory, with or without trailing slash}

\item{filename}{file name. File type is determined from the file extension, eg: ".csv", ".xls", ".xlsx"}

\item{key_column_name}{a column in the propensity files to merge on, and select
those values that appear in selected_keys}

\item{selected_keys}{a vector of selected keys that are to be retained in the propensities}
}
\value{
a dataframe
}
\description{
Loads and merges a CSV/XLS file with the supplied values (keys). ie:
returns a dataframe (excluding key_column_name) for the supplied 
values that exist in key_column_name of the file
}
