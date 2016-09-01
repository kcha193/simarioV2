# String util functions
# 
# Author: oman002
###############################################################################

#' Adds a trailing slash, if required, to character vector
#' 
#' @param x
#'  character vector
#' @return
#'  x with a trailing slash, if required
#' 
#' @export
add_trailing_slash <- function(x) {
	if(grepl("/$", x)) {
		x
	} else {
		paste(x, "/", sep="")
	}
}


#' TRUE for elements that only contain alpha characters, ie: have no numeric component.
#' 
#' @param x
#'  character vector
#' 
#' @return 
#'  logical vector
#' 
#' @export
is.alpha.only <- function(x) {
	grepl("^\\D*$", x)
}


#' Remove any alpha from character vector, leaving only numbers.
#' 
#' @param x
#'  character vector
#' 
#' @return 
#' character vector without alpha
#' 
#' @export
strip.alpha <- function(x) {
	# strip any characters that are not . 0-9 or -
	gsub("[^\\.0-9-]", "", x)
}

#' Remove any numbers from character vector, leaving only alpha.
#' 
#' @param x
#'  character vector
#'  
#' @return 
#' character vector without numbers
#' 
#' @export
strip.numeric <- function(x) {
	# strip any characters that are . 0-9 or -
	gsub("[\\.0-9-]", "", x)
}

#' Remove any alpha from row and col names, leaving only numbers.
#' 
#' @param mx
#'  matrix 
#' 
#' @return 
#'  matrix after modify the row and col names
#' 
#' @export
strip.alpha.mx <- function(mx) {
	
	rownames(mx) <- strip.alpha(rownames(mx))
	colnames(mx) <- strip.alpha(colnames(mx))
	mx
}


#' Remove leading and trailing spaces from a string
#' 
#' @param string
#'  character vector
#' 
#' @return 
#'  character vector after remove spaces
#' 
#' @export
trim <- function (string) {
	#
	gsub("^\\s+|\\s+$", "", string)
}


