# Common functions for analysing data in microsimulation models.
# 
# Author: Oliver Mannion
###############################################################################

#' ROW constant = 1.
#' 
#' @export
ROW <- 1

#' COL constant = 2.
#'
#' @export
COL <- 2

#' ZDIM constant = 3.
#' 
#' @export
ZDIM <- 3

#' Assign the values of variables (specifed by name in a source character vector)
#' to another set of variables (specifed by name in a dest character vector).
#' Will modify variables in enclosing environments.
#'  
#' @param dest_var_names
#'  a character vector of variable names to be assigned
#' 
#' @param src_var_names
#'  a parallel character vector of variable names supplying the values to
#' 
#' @return 
#'  NULL
assign_values <- function(dest_var_names, src_var_names) {
	invisible(mapply(function(dest_var_name, src_var_name) {
						assign(dest_var_name, get(src_var_name), inherits=T)
					}, dest_var_names, src_var_names))
}
environment(assign_values) <- .GlobalEnv


#' Produces a csv string from x, returned as a length 1 chr vector.
#' 
#' @param x
#'  object
#' 
#' @param title
#'  optional title to prepend to output
#' 
#' @param row.names
#'  either a logical value indicating whether the row names of x are to be written along with x, 
#'  or a character vector of row names to be written
#' 
#' @return 
#'  a character vector
#'
#' @export 
as.csv.string <- function(x, title = NULL, row.names = T) {
	if (!(is.null(title))) title <- dQuote(title) 
	
	if (is.vector(x)) {
		#print as one line
		result <- paste(x,collapse=",")
	} else {
		result <- paste(c(title,capture.output(write.csv(x, row.names=row.names))),sep="",collapse="\n")
	}
	
	#add ending newline
	paste(result,"\n")
}


#' Apply to a list using the names from the list
#' 
#' @param xlist
#'  list
#' 
#' @param row.names
#'  either a logical value indicating whether the row names of x are to be written along with x, 
#'  or a character vector of row names to be written
#' 
#' @return 
#'  a list of character vectors
#' 
#' @export
as.csv.string.list <- function(xlist, row.names = T) {
	mapply(function (x, name) {
						as.csv.string(x,name, row.names = row.names)
					}, xlist, names(xlist))
}


#' Stops and displays indices of any element that is FALSE.
#' 
#' @param xvec
#'  logical vector
#' 
#' @return
#'  If there is FALSE, return error message.
#' 
#' @export
assert <- function(xvec) {
	if (!all(xvec)) {
		if (is.null(names(xvec))) names(xvec) <- paste("[",seq(length(xvec)),"]",sep="")
		#firstParamName <- as.character(sys.call())[2]
		stop(gettextf("failed for %s",  
						paste(names(xvec[!xvec]),collapse=", ")))
	}
}


#' Divides x into bins specified by breaks or a bin size
#' 
#' @param x
#' numeric vector to be binned
#' 
#' @param breaks
#' either a numeric vector of two or more cut 
#' points, or a single number giving the size of the bins in
#' which case x is divided in bins of size breaks.
#' NB: if cut points are specified, note that the cut point
#' value is not included in the bin (ie: include.lowest = FALSE)
#' Therefore the very first cut point must be less than min(x).
#' Subsequent cut points are the closed right bound,
#' and the final cut point should be max(x), eg: breaks of 
#'      c(0, 34, 35, 36, 37, 44)
#' will cut into the following bins
#'    (0,34] (34,35] (35,36] (36,37] (37,44] 
#' 
#' @param blabels
#' labels for the levels of the resulting category. 
#' If NULL, labels are constructed using "(a,b]" interval notation.
#' If specified, NAs are removed first. If breaks is specified,
#' the the first cut point must be less than min(x) and its name
#' (ie: the first value in blabels) should be NA.
#' If unspecified, names(breaks) is used.
#' 
#' @param breaklast 
#' if breaks is a bin size and breaklast is
#' specified then this is the position of the last break
#' 
#' @return 
#' the values of x factored into bins 
#' 
#' @export
bin <- function (x, breaks, blabels = names(breaks), breaklast=NULL) {
	if (length(breaks) == 1L) {
		# breaks is a binsize. calculate actual breaks
		binsize <- breaks
		
		breakmin <- (floor(min(x,na.rm=TRUE)/binsize)*binsize)
		
		# TODO: comparing doubles here, use all.equal instead?
		# depends what cut uses to compare I guess?
		if (min(x) == breakmin) {
			#min(x) == breakmin, so go down one binsize
			breakmin <- breakmin-binsize
		}
		
		breakmax <- ceiling(max(x,na.rm=TRUE)/binsize)*binsize
		
		if (is.null(breaklast)) {
			breaks <- seq(breakmin,breakmax,binsize)
		} else {
			breaks <- c(seq(breakmin,breaklast,binsize),breakmax)
		} 
	}
	if (!is.null(blabels)) blabels <- na.omit(blabels)
		
	factoredx <- cut(x, breaks=breaks, labels=blabels, include.lowest = FALSE)
	if (!is.null(dim(x))) {
		dim(factoredx) <- dim(x)
	}
	return(factoredx)
}


#' Combine levels specified in seperate binary level variables into one
#' variable.
#' 
#' @param ...
#'  a series of binary (0,1) vectors, specified as multiple arguments
#'  or as a list
#' @param levelvalues
#'  a vector indicating the level values to use. The first value of this vector is the value used
#'  for the first binary vector argument, the second  for the second binary vector argument etc.
#'  If unspecified, the defaults to (1,2,...n) where n is the number of binary vectors
#'  specified by ... 
#' 
#' @return
#'  a single vector
#' 
#' @export
binary.levels.combine  <- function (..., levelvalues = NULL) {
	
	binlevels <- if (is.list(c(...))) c(...) else list(...)
	
	if (is.null(levelvalues)) levelvalues <- seq(length(binlevels))
	
	mbinlevels <- as.matrixFromList(binlevels)
	
	#check we have one and only one binary value across each of the levels 
	if (any(colSums(mbinlevels) != 1)) {
		stop (c("invalid bin levels in position(s): ", paste(which(colSums(mbinlevels) != 1), collapse = ", ")))	
	}
	
	# multiply the binary levels by the corresponding level value
	binlevels.x.levelvalues <-  mbinlevels * levelvalues 
	
	# flatten into a single vector
	colSums(binlevels.x.levelvalues)
}


#' Create a list of binary levels from factors.
#' Compares a vector x against a set of factors and returns
#' a list for each factor which indicates whether x 
#' is equal to that factor or not.
#' 
#' @param x
#'  vector
#' 
#' @param f
#'  factors. Defaults to the unique set of values in x.
#' 
#' @return 
#'  list of binary levels
#' 
#' @export
binary.levels.split <- function(x, f=sort(unique(x))) {
	if (is.null(names(f))) names(f) <- f 
	lapply(f, function(fac) { as.integer(x == fac) } )
}


#' Increments x by factor
#' 
#' @param x 
#' numeric vector to be incremented
#' 
#' @param factoredx 
#' x factored
#' 
#' @param factorincrements 
#' amounts to increment each factor of factoredx by
#' 
#' @return 
#' x incremented by the factor increments
#' 
#' @export
incByFactor <- function(x, factoredx, factorincrements) {
	if (nlevels(factoredx) != length(factorincrements)) {
		stop("factor increments must be of length ", nlevels(factoredx))
	}
	
	# add factor increment to x based on its factoring (factoredx)
	x + factorincrements[factoredx]
}
		

#' Detach an environment and return it.
#' NB: the returned environment contains the contents of the attached environment but is
#' actually a newly created different environment object from the original.
#' 
#' @param envname
#'  name of the attached environment
#' 
#' @return 
#' an environment
#' 
#' @export
detachReturn <- function(envname) {
	#store modified env
	#env <- updatelist(env, as.list(as.environment(envname)))
	#env <- as.list(as.environment(envname))
	
	# gets the attached environment specified by envname
	# and returns it. Because an attached environment is
	# a copy of the original (see ?attach), this will return 
	# a different environment than the original
	# TODO: use proto or R.oo so we can avoid this problem
	# and have functions modify the original environment
	# rather than an attached copy 
	env <- as.environment(envname)
	
	#detach
	detach(envname, character.only = TRUE)
	
	#return modified env
	env
}


#' Calc the 95\% error from the t Distribution.
#' 
#' @param x
#'  vector
#' 
#' @return 
#'  the result calculated
#' 
#' @export
err <- function (x) {
	## see http://www.cyclismo.org/tutorial/R/confidence.html
	if (length(x) < 2) {
		return(NA)
	}
	qt(0.975,df=length(x)-1)*sd(x)/sqrt(length(x))	
}


#' Evaluate a list/vector of strings as expressions.
#' Errors if expressions cannot be evaluated.
#' 
#' @param exprlist 
#' list/vector of strings to evaluate
#' 
#' @param envir 
#' environment to evaluate in, defaults to global environment. 
#' 
#' @param enclos
#' Specifies the enclosure, i.e., where R looks for objects not found in envir.
#' Defaults to the caller's environment (parent.frame()) 
#' 
#' @param allowEmptyExpr 
#' allow expressions to return no value, defaults to FALSE
#' 
#' @return 
#' NULL
#' 
#' @export

eval.list <- function (exprlist, envir = .GlobalEnv, enclos = parent.frame(), allowEmptyExpr = FALSE) {
	
	#evaluate list of strings as expressions.
	#Any objects in the exprs must exist in envir or if not then in parent.frame()
	values <- lapply(exprlist,
			function (x) try(eval(parse(text=x), envir = envir, enclos = enclos), silent = TRUE))
	
	# if any parse/evaluation errors, fail
	tryerrs <- tryerrorMsgs(values)
	if (length(tryerrs) > 0) {
		firstParamName <- as.character(sys.call())[2]
		# concate errors together with names 
		msg <- paste(firstParamName, " ", names(tryerrs), ": ",tryerrs, sep="",collapse="")
		stop(msg)
	}
 	
	emptyexprs <- lapply(values,length)==0
	if (!allowEmptyExpr && any(emptyexprs)) {
		# expr evaluated to empty value, fail with error message
		firstParamName <- as.character(sys.call())[2]
		msg <- paste(firstParamName, " ",
				names(emptyexprs)[emptyexprs],
				" : expr '",
				exprlist[emptyexprs],
				"' returns no value\n",
				sep="",collapse="")
		stop(msg)
	}
	
	values
}


#' Save x into global variable, ie: top frame, not just this function
#' using the supplied varname
#' 
#' @param varname
#'  variable name
#' 
#' @param x
#'  value
#' 
#' @param pos
#'  environment to save into. Defaults to global environment.
#' 
#' @return
#' NULL
#' 
#' @export
globalNamed <- function (varname, x, pos = 1) {
	assign(varname, x, pos = pos)	
}


#' Save x into global variable as it's own name.
#' 
#' @param x
#'  value
#' 
#' @param pos
#'  environment to save into. Defaults to global environment.
#' 
#' @return
#' NULL
#' 
#' @export
global <- function (x, pos = 1) {
	
	param1Name <- as.character(sys.call())[2]
	globalNamed(param1Name, x, pos)
}


#' Returns whether x is a scalar (i.e. length 1)
#' and numeric.
#'
#' @param x
#'  object
#' 
#' @return 
#'  logical value
#' 
#' @export 
is_numeric_scalar <- function (x) {
	length(x) == 1 && is.numeric(x)
}


#' Return amount of memory, in bytes, used by each
#' element of a list
#' 
#' @param lx
#'  list
#' @return
#'  num vector of bytes per element
#' 
#' @export
mem.lx <- function(lx) {
	sapply(lx, object.size)
}


#' Add a meta attribute. Appends to existing meta attribute, if it exists, 
#' overwriting any meta elements with the same name.
#' 
#' @param x
#'  an object. May have an existing meta attribute.
#' 
#' @param new.meta
#'  a named vector to add to the meta attribute of x.
#'  if an element of new.meta already exists in the x's meta, 
#'  then the existing meta element will be replaced.
#'  if new.meta is NULL, nothing is done.
#'   
#' @return 
#'  x with "meta" attribute
#' 
#' @export
meta.add <- function(x, new.meta) {
	if (is.null(new.meta)) return(x)
	
	meta <- attr(x, "meta")
	keep.meta <- setdiff(names(meta), names(new.meta))
	
	result.meta <- c(meta[keep.meta], new.meta)
	
	structure(x, meta=result.meta)
}

#' Appends a "varname" meta element to the meta of each element
#' in a list.
#' 
#' @param xlist
#'  list of objects
#' @param varnames
#'  parallel vector of varnames. Defaults to names(xlist).
#'  If NULL, does nothing
#' 
#' @return 
#'  xlist with the corresponding varname append to the meta
#'  attribute of each xlist element.
#' 
#' @export
meta.add.list.varname <- function(xlist, varnames = names(xlist)) {
	if (is.null(varnames)) {
		xlist
	} else {
		mapply(function(x, varname) {
					meta.add(x, c(varname=varname))
					#lapply(x, `attr<-`, "varname.tag", varname)
				}, xlist, varnames, SIMPLIFY = FALSE)
	}
} 


#' Sort numerically. 
#' If x can be represented numerically, then
#' sort numerically otherwise sort normally. 
#' 
#' @param x
#'  object to sort
#' 
#' @param stripAlpha
#'  remove alpha characters before attempting sort, but retain them in the output
#' 
#' @param sortAlphaOnlySeparately
#'  if a value is purely alpha (ie: contains no numeric component) sort is separately
#'  from the alpha-numeric values, and return it after any numerically sorted values
#' 
#' @param ...
#'  additional arguments passed to sort
#' 
#' @return 
#'  x after sorted.
#' 
#' @seealso sort
#' 
#' @export
nsort <- function (x, stripAlpha = TRUE, sortAlphaOnlySeparately = TRUE, ...) {

	# remove any NAs
	index.nas <- is.na(x)
	if (any(index.nas)) {
		x <- x[!index.nas]
	}
	
	if (sortAlphaOnlySeparately) {
		is_alpha_only <- is.alpha.only(x)
		alpha_only <- x[is_alpha_only]
		x_to_sort_numerically <- x[!is_alpha_only]
	} else {
		x_to_sort_numerically <- x
	}
	
	if (stripAlpha) x_to_sort_numerically_striped <- strip.alpha(x_to_sort_numerically) else x_to_sort_numerically_striped <- x_to_sort_numerically
	
	# if can be converted via as.numeric without NAs, convert
	if (!any(is.na(suppressWarnings(as.numeric(x_to_sort_numerically_striped))))) {
		result <- x_to_sort_numerically[order(as.numeric(x_to_sort_numerically_striped))]
	} else {
		result <- sort(x_to_sort_numerically, ...)	
	} 
	
	# add back sorted alpha onlys
	if (length(alpha_only) > 0) {
		result <- c(result, sort(alpha_only, ...))
	}
	
	# add back any NAs to end
	if (any(index.nas)) {
		result <- c(result, rep(NA, sum(index.nas)))	
	}
	
	result
}


#' Remove all objects in global environment.
#' 
#' @param exceptions
#' 	names of vars not to remove
#' 
#' @return 
#' NULL 
#' 
#' @export
rmall <- function (exceptions = NULL) {
	vars <- ls(".GlobalEnv", all.names=TRUE)
	if (!is.null(exceptions)) {
		vars <- vars[!vars %in% exceptions]
	}
	rm(pos = ".GlobalEnv", list = vars)
}


#' Remove empty values from vector
#' 
#' @param xvec
#' 	a vector
#' 
#' @return 
#'  a vector without empty values
#' 
#' @export
stripEmpty <- function (xvec) {
	#remove empty values (NAs, empty string) from vector
	xvec <- xvec[!is.na(xvec)]	#remove NAs
	xvec <- xvec[xvec != ""]		#remove empty strings
	xvec
}


#' Remove class attribute from an object
#' 
#' @param x
#' 	object
#' 
#' @return 
#'  x without "class" attribute
#' 
#' @export
stripClass <- function (x) {
	#remove the class attribute
	`attr<-`(x, "class", NULL)
}


#' Remove meta attribute from an object
#' 
#' @param x
#'  object
#' 
#' @return
#'  x without "meta" attribute
#' 
#' @export
stripMeta <- function (x) {
	`attr<-`(x, "meta", NULL)
}


#' Remove meta attribute from a list
#' 
#' @param xlist
#'  list
#' 
#' @return
#'  xlist with elements without a "meta" attribute
#' 
#' @export 
stripMeta.list <- function (xlist) {
	lapply(xlist, stripMeta)	
}


#' Return the messages of any element that is a try-error
#' 
#' @param xlist
#'  list
#' 
#' @return 
#'  vector that contains all components in the list with "try-error" class
#' 
#' @export
tryerrorMsgs <- function (xlist) {
	unlist(sapply(xlist, function (x) if (class(x)=="try-error") { stripClass(x) }))
}


#' Same as within, but the expr executed is a function.
#' Unlike within, the func will be executed within data.
#' Functions will read variables from data, and if <<- is used 
#' then values will be assigned in data IF the variable already exists in data.
#' 
#' @param data 
#' data to use for constructing an environment.  Can be a list or a data frame.
#' 
#' @param func 
#' function to evaluate in data
#' 
#' @param ... 
#' arguments to pass to func
#' 
#' @return 
#' data modified
#' 
#' @export
withinfunc <- function(data, func, ...) {
	# create environment from base that has
	# parent environment that is the environment 
	# in which within.func was called
	parent <- parent.frame()
	dataenv <- evalq(environment(), data, parent)
	
	# set func environment to dataenv
	environment(func) <- dataenv
	
	args <- list(...) 
	do.call(func, args=args)
	
	# returned modified dataenv
	updatelist(data, as.list(dataenv))
}
