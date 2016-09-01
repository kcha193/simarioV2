# TODO: Add comment
# 
# Author: oman002
###############################################################################


#' Returns the file extension, i.e: everything after the final dot.
#' 
#' @param x
#'  chacter vector
#' 
#' @return 
#'  file extension, eg: "csv"
#' 
#' @export
file_extension <- function(x) {
	matched <- regexpr("\\.([^\\.]+)$", x)
	substr(x, start=matched+1, stop=matched+attr(matched, "match.length")-1) 
}


#' Reads a CSV file.
#' 
#' @param filedir
#'  file directory, with or without trailing slash
#' 
#' @param filename
#'  file name
#' 
#' @param stringsAsFactors
#'  logical: should character vectors be converted to factors?
#' 
#' @param ...
#'  additional parameters to read.csv
#' 
#' @return 
#'  a data frame
#' 
#' @export
read_csv <- function (filedir, filename, stringsAsFactors = FALSE, ...) {
	filedir <- add_trailing_slash(filedir)		
	read.csv(paste(filedir, filename, sep=""), stringsAsFactors = stringsAsFactors, ...)
}


#' Read a file and return a dataframe.
#' 
#' @param filedir
#'  file directory, with or without trailing slash
#' 
#' @param filename
#'  file name 
#' 
#' @param filetype
#'  "csv", "xls", or "xlsx". Defaults to the extension of filename
#' 
#' @param stringsAsFactors
#'  logical: should character vectors be converted to factors?
#' 
#' @param ...
#'  additional parameters to read_csv or read.xlsx2
#' 
#' @return
#'  a data frame
#' 
#' @export
read_file <- function (filedir, filename, filetype = file_extension(filename), stringsAsFactors = FALSE, ...) {
	switch(filetype,
		csv = read_csv(filedir, filename, stringsAsFactors = stringsAsFactors, ...))
}

