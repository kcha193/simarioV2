# Matrix and array util functions.
# 
# Author: oman002
###############################################################################

#' Align a list of matrices/vectors by row and column name, so all matrices have the 
#' same set of named rows and named cols. 
#' NAs are used to pad missing rows and cols. 
#' 
#' @param listmx
#'  a list of different sized matrices
#' 
#' @return
#'  the list of the matrices aligned to the maximal set of rows and cols.
#'  rows and cols are in numerical sort order.
#'
#' @export 
align.by.name.list.mx <- function(listmx) {
	
	listmx.is.vector <- sapply(listmx, is.vector)
	listmx.mx <- lapply(listmx, as.matrix)
	
	dims <-	sapply(listmx.mx, dim)
	all.colnames <- lapply(listmx.mx, colnames)
	all.rownames <- lapply(listmx.mx, rownames)
	
	nulls.colnames <- sapply(all.colnames, is.null)
	nulls.rownames <- sapply(all.rownames, is.null)
	
	if(any(nulls.colnames)) {
		# missing colnames, make sure all col dimensions are the same
		if (!all(dims[COL, ] == dims[COL, 1])) {
			stop(gettext("colnames not specified for a matrix and different number of columns"))
		}
	}
	
	if(any(nulls.rownames)) {
		# missing rownames, make sure all row dimensions are the same
		if (!all(dims[ROW, ] == dims[ROW, 1])) {
			stop(gettext("rownames not specified for a matrix and different number of rows"))
		}
	}
	
	# if rownames and colnames all the same just return listmx
	if (	all(sapply(all.colnames, identical, y=all.colnames[[1]])) && 
			all(sapply(all.rownames, identical, y=all.rownames[[1]]))) {
		return(listmx)			
	}
	
	# create the new matrix template, i.e: an ordered union of columns from all matrices
	# and rows from all matrices
	
	# NB: doesn't handle (yet) case where there is a NULL name but not all are NULL
	# NB: if NULL name then all matrices will be of the same dimension
	
	# if numeric names, sort numerically
	mt.rownames <- if (all(nulls.rownames)) NULL else nsort(unique(unlist(all.rownames)))
	mt.colnames <- if (all(nulls.colnames)) NULL else nsort(unique(unlist(all.colnames)))
	#mt.rownames <- if (all(nulls.rownames)) NULL else unique(unlist(all.rownames))
	#mt.colnames <- if (all(nulls.colnames)) NULL else unique(unlist(all.colnames))
	mt.dimnames <- list(mt.rownames, mt.colnames)
	
	# map existing matrix's columns and rows to new matrix template
	maps <- lapply(listmx.mx, function(mx) {
				#mx <- listmx.mx[[1]]
				
				#newcols specifies the source col for each col in matrix template
				#newcols specifies the source row for each row in matrix template
				newrows <- if (is.null(mt.rownames)) c(1:dim(mx)[ROW]) else match (mt.rownames, rownames(mx))
				newcols <- if (is.null(mt.colnames)) c(1:dim(mx)[COL]) else match (mt.colnames, colnames(mx)) 
				list(newrows=newrows, newcols=newcols)
			})
	
	
	result <- mapply(function(mx, map) {
				# mx <- listmx.mx[[1]] ; map <- maps[[1]]
				redim.mx(mx, map$newrows, map$newcols, mt.dimnames)
			}, listmx.mx, maps, SIMPLIFY = FALSE)
	
	# turn back into vectors if that's what we started with
	result[listmx.is.vector] <- lapply(result[listmx.is.vector], function(x) {
				#x <- result[[1]]
				vx <- as.vector(x)
				names(vx) <- rownames(x)
				vx
			})
	
	result
	
}

#' Append two lists of matrices by adding each element of listmx.src into 
#' the corresponding element of listmx.dest in the specified dimension.
#' 
#' @param listmx.dest
#' 	list of vector/matrix/arrays
#' 
#' @param listmx.src
#'  list of vector/matrix/arrays
#' 
#' @param into.dim
#'  the dimension in which to add each element of listmx.src.
#' 
#'  If into.dim = ZDIM (i.e: 3) then listmx.src elements will be added as an additional
#'  z matrix into each matrix/array element of listmx.dest. Existing elements of listmx.dest
#'  will be in the first z dim(s). 
#' 
#'  If into.dom = COL (i.e: 2) then listmx.src elements will be added as the 2nd
#'  col into each vector/matrix element of listmx.dest. Existing elements of listmx.dest
#'  will be in the first column(s).
#' 
#'  If into.dom = ROW (i.e: 1) then listmx.src elements will be added as the 2nd
#'  row into each vector/matrix element of listmx.dest. Existing elements of listmx.dest
#'  will be in the first row(s).  
#' 
#' @return 
#'  list of vector/matrix/arrays after appended.
#' 
#' @export 
append.list.mx <- function(listmx.dest, listmx.src, into.dim=ZDIM) {
	
	
	if (into.dim==ROW) {
		# abind doesn't maintain row seperation so use rbind instead
		# eg: rbind(listmx.dest[[1]], listmx.src[[1]])
		# eg: rbind(listmx.dest[[2]], listmx.src[[2]])
		combined <- mapply(rbind, listmx.dest, listmx.src, SIMPLIFY = FALSE)
	} else {
		# add sourcem in the specifide dimension to each element in listmx.dest
		# eg: abind(listmx.dest[[1]], listmx.src[[1]], along=ZDIM)
		combined <- mapply(abind, listmx.dest, listmx.src, MoreArgs=list(along=into.dim), SIMPLIFY = FALSE)
	}
	
	# add meta attribute back because abind kills it
	copyMeta.list(combined, listmx.src)
	
}


#' copy names of dimensions and meta attributes
#' from elements of list.src to list.dest
#' Called by append.list.mx().
#' 
#' @param list.dest
#' 	list of vector/matrix/arrays
#' 
#' @param list.src
#'  list of vector/matrix/arrays 
#' 
#' @return 
#'  list of vector/matrix/arrays after add names and attributes
#' 
#' @export 
copyMeta.list <- function(list.dest, list.src) {
	mapply(function(dest,source){
				#add back names of dimension 
				names(dimnames(dest)) <- names(dimnames(source))
				#add back meta attribute
				structure(dest, meta=attr(source, "meta"))
			}, list.dest, list.src, SIMPLIFY=FALSE )
}


#' convert list of vectors to an array.
#' Recycling rule will be applied.
#' 
#' @param mylist
#' 	list of vectors
#' 
#' @return 
#'  an array
#' 
#' @export 
as.arrayFromList <- function (mylist) {
	t(array(unlist(mylist), dim=c(length(mylist[[1]]),length(mylist))))
}


#' Convert a list of matrices into an array, with each matrix in the z dimension.
#' 
#' @param listmx
#'  list of matrices. Each must matrix the same dimensions.
#' 
#' @return
#'  array with dimnames and meta from listmx
#' 
#' @export 
as_array_list_mx <- function(listmx) {
	
	rows <- nrow(listmx[[1]]) ; rnames <- rownames(listmx[[1]])
	cols <- ncol(listmx[[1]]) ; cnames <- colnames(listmx[[1]])
	zdim <- length(listmx) ; znames <- names(listmx)
	structure( 
			array(	unlist(listmx), 
					dim = c(rows,cols,zdim),
					dimnames = list(rnames, cnames, znames)
			)
			, meta=attr(listmx[[1]], "meta"))
	
}


#' Converts a list of same length vectors to a matrix.
#' Unlike as.matrix.default, it unlists first and
#' uses names from the first list element in the result.
#' 
#' @param xlist
#'  a list of same length vectors
#' @param byrow
#'  if TRUE, each list element becomes a row in the matrix
#'  else, each list element becomes a column in the matrix
#' 
#' @return 
#' a matrix
#' 
#' @export 
as.matrixFromList <- function (xlist, byrow = TRUE) {
	if (byrow) {
	  do.call("rbind", xlist)
	} else {
	  do.call("cbind", xlist)
	}				
}


#' Push row/col headers (ie: names of the rownames and colnames) into the rownames and colnames.
#' 
#' @param mx
#'  matrix
#' 
#' @return 
#' matrix with prepended dimnames
#' 
#' @export 
dimnames_prepend_header <- function(mx) {
	
	dx <- dimnames(mx)[ROW]
	header <- if(is.na(names(dx))) "" else names(dx)
	new.row.names <- paste(header, unlist(dx))
	
	dx <- dimnames(mx)[COL]
	header <- if(is.na(names(dx))) "" else names(dx)
	new.col.names <- paste(header, unlist(dx))
	
	structure(mx, dimnames = list(new.row.names, new.col.names))
}


#' Convert a list of lists of matrices to an array, as follows:
#'  flatten each matrix into a single row,
#'  align the single rows within each list, then combine the rows into a single matrix per list
#'  align each single matrix and combine them into the z dimension of an array
#'  
#' Preserves names and meta attribute.
#' 
#' @param lol.mx
#'  a list of lists of matrices, eg:
#'  List of 2
#'  $ year1:List of 2
#'  ..$ 1: 'table' int [1:2, 1:3] 1 2 3 4 5 6
#'  ..$ 2: 'table' int [1:2, 1:3] 21 22 23 24 25 26
#'  $ year2:List of 2
#'  ..$ 1: 'table' int [1:2, 1:3] 31 32 33 34 35 36
#'  ..$ 2: 'table' int [1:2, 1:3] 41 42 43 44 45 46
#' 
#'  NB: matrices can have different dimensions and are aligned first within a list, and then between the lists.
#' 
#' @return 
#' an array
#' 
#' @seealso align.by.name.list.mx
#' 
#' @export
flatten_mxlists_to_array <- function(lol.mx) {
	# flatten each matrix into a single row then align and return each list of matrices as a single matrix
	lol.mx.flat <- lapply(lol.mx, flatten_mxs_to_single_mx)
	
	# align matrices
	lol.mx.flat.aligned <- align.by.name.list.mx(lol.mx.flat)
	
	# combine into an array, with each matrix in the z dimension
	lol.mx.flat.aligned.array <- as_array_list_mx (lol.mx.flat.aligned)
	
	# return with meta from first list of matrices
	structure(lol.mx.flat.aligned.array, meta=attr(lol.mx[[1]], "meta"))
	
}


#' Takes a list of matrices, flattens them into a single row, aligns them by column name
#' and then returns a single matrix.
#' 
#' @param listmx
#'  list of matrices to flatten. Can have different dimensions but must have similar names
#'  for alignment (@seealso listmx.flat.aligned)
#' 
#' @return 
#'  a single matrix with 
#'    colnames = the flattened row/col names of listmx[[1]]],
#'    meta = meta of listmx[[1]]
#'    rownames = names of listmx 
#' 
#' @export
flatten_mxs_to_single_mx <- function(listmx) {
	
	# flatten each matrix into a single row 
	listmx.flat  <- lapply(listmx, flatten_mx_to_row) 
	
	# align each row matrix 
	listmx.flat.aligned <- align.by.name.list.mx(listmx.flat)
	
	# collapse into a single matrix of all rows
	# keeps colnames and meta from first row matrix, use rownames from names of listmx.flat.aligned
	structure(
			matrix( unlist(listmx.flat.aligned), nrow=length(listmx.flat.aligned), byrow = T,
					dimnames=list(names(listmx.flat.aligned), colnames(listmx.flat.aligned[[1]])) )
			, meta=attr(listmx.flat.aligned[[1]], "meta") 
	)
	
}


#' Flatten a vector or matrix (r,c) into a single row matrix (1,c) 
#' creating new column names from a combination of old rownames 
#' and colnames. 
#' 
#' A single row matrix, unlike a vector, has the advantage that 
#' it can have a row name.
#'  
#' eg: A1, A2,
#'     B1, B2
#' 
#' @param mx
#'  matrix
#' @param row.names.first
#'  if TRUE, row.names will appear first in the names of the resultant columns, eg: becomes A1,B1,A2,B2
#'  else row.names will appear last, eg: 1A, 1B, 2A, 2B
#' 
#' @return
#'  a single row matrix (1,c)
#' 
#' @export
flatten_mx_to_row <- function (mx, row.names.first = FALSE) {
	# if already vector, return as single row matrix
	if (is.vector(mx)) {
		return ( matrix(mx, nrow=1, dimnames= list(NULL, names(mx))) )
	}
	
	# flatten into vector
	mx.flat <- structure(mx, .Dim=c(1,length(mx)))
	#mx.flat <- as.vector(mx)
	
	# create new column names, as a combination of the old row and col names
	rnames <- if (is.null(rownames(mx))) c("") else rownames(mx)
	cnames <- if (is.null(colnames(mx))) c("") else colnames(mx)
	cnames <- if (row.names.first ) 
										
				{trim(paste(rep(rnames,length(cnames)), sapply(cnames, rep, length(rnames))))
			} else {
				trim(paste(sapply(cnames, rep, length(rnames)), rep(rnames,length(cnames))))
			}
	
	#replace "NA" with NA
	cnames[cnames == "NA"] <- NA
	
	if (any(is.na(cnames)) || !cnames[1] == c("")) {
		colnames(mx.flat) <- cnames
	}
	
	mx.flat
}


#' Create a matrix of NA with specified col/row names/lengths.
#' 
#' @param rows 
#' row names, or a numeric scalar for the number of rows
#' @param cols 
#' columns names, or a numeric scalar for the number of cols
#' 
#' @return 
#' a named matrix of NA
#' 
#' @export
namedMatrix <- function (rows, cols) {
	nrows <- if (is_numeric_scalar(rows)) rows else length(rows)
	ncols <- if (is_numeric_scalar(cols)) cols else length(cols)
	rownames <- if (is_numeric_scalar(rows)) NULL else rows
	colnames <- if (is_numeric_scalar(cols)) NULL else cols
	matrix(nrow=nrows, ncol=ncols,
			dimnames=list(rownames,colnames))
}


#' Takes any number of 2D matrices, each with the same number of cols but with 
#' any set of named rows. Every col 1 of the matrices are combined into a new 
#' matrix, as are every col 2,3 etc.
#' @param ...
#'  matrices, or a list of matrices
#' 
#' @return 
#'  matrices, or a list of matrices after merged
#' 
#' @seealso merge_list_mx.by.rows
merge_list_mx.by.cols <- function(...) {
	xlistm <- if (nargs() > 1) list(...) else (...)
	merge_list_mx.by.rows(lapply(xlistm, t))
}


#' Takes any number of 2D matrices, each with the same number of rows but with 
#' any set of named cols. Every row 1 of the matrices are combined into a new 
#' matrix, as are every row 2,3 etc.
#'
#' @param ...
#'  matrices, or a list of matrices
#' 
#' @return 
#'  matrices, or a list of matrices after merged
#' 
#' @export
merge_list_mx.by.rows <- function(...) {
	xlistm <- if (nargs() > 1) list(...) else (...)

	# if colnames are all the same, then use them as is
	colnames1 <- colnames(xlistm[[1]])
	if (all(sapply(xlistm, function(mx) {
						#mx <- xlistm[[1]]
						identical(colnames(mx), colnames1)
			}))) {
		colset <- colnames1
	} else {
		# else get the unique set of colnames across each element in list xlistm
		colset <- nsort ( unique( as.vector( unlist(sapply(xlistm, colnames))) ) )
	}
	
	# for each row j in xlistm[[1]]
	result <- lapply(seq(nrow(xlistm[[1]])), function (j) {
				#j = 1
				#merge corresponding row j in each element of xlistm into colset
				newm <- sapply(xlistm, 
						function (mx)	{
							#mx <- xlistm[[1]]
							mx[j,][match(colset, colnames(mx))]
							#merge(colset, data.frame(colnames(mx), mx[j,]), by = 1, all.x=TRUE)$mx 
						}
				)
				
				colnames(newm) <- names(xlistm)
				rownames(newm) <- colset
				t(newm)
			})
	names(result) <- rownames(xlistm[[1]])
	result
}



#' Re-dimension a matrix.
#' Takes the matrix mx and transforms it into a length(newrows) x length(newcols) matrix
#' which has the rows from mx specified in newrows and the cols from mx specified in newcols.
#' 
#' @param mx
#'  source matrix
#' 
#' @param newrows
#'  a vector specifing the rows from the source matrix that will appear
#'  in the new matrix. The new matrix will have length(newrows) rows.
#'  The position a source row will have in the new matrix is their index in newrows. 
#'  A NA in this vector will create a row of NAs at the position in the new matrix.
#'  eg: 
#'  c(2,NA,1) will create a new matrix of 3 rows. 
#'  Row 1 of the new matrix will be row 2 of the source matrix
#'  Row 2 of the new matrix will be NAs.
#'  Row 3 of the new matrix will be row 1 of the source matrix.
#'  
#' @param newcols
#'  a vector specifing the cols from the source matrix that will appear
#'  in the new matrix. Works in the same manner as newrows but for columns.
#' 
#' @param dim.names
#'  A dimnames attribute for the matrix: NULL or a list of length 2 giving the row and column names respectively.
#'  An empty list is treated as NULL, and a list of length one as row names. 
#'  The list can be named, and the list names will be used as names for the dimensions.
#' 
#' @return 
#'  a new matrix of length(newrows) rows and length(newcols) cols with the
#'  source rows and cols as specified by newrows and newcols.
#'  If the source matrix is a table then will also return a table.
#' 
#' @export
redim.mx <- function(mx, newrows, newcols, dim.names) {
	numrows <- length(newrows)
	numcols <- length(newcols)
	rowindices <- rep(newrows, numcols)
	colindices <- rep(newcols, each=numrows)
	
	# indices into mx. 
	# the position of each index within mxindices, 
	# will be the new position in the final matrix, 
	# eg: the index at mxindices[1] specifies a value
	# that will end up in row 1 col 1 of the final matrix, 
	# the index at mxindices[2] specifies a value
	# that will end up in row 2 col 1 of the final matrix,
	# etc.
	mxindices <- rowindices + (colindices-1)*dim(mx)[1]
	newdata <- as.vector(mx)[mxindices]
	
	result <- matrix(newdata, nrow = numrows, ncol = numcols, dimnames = dim.names)
	
	if (is.table(mx)) {
		result <- as.table(result)
		dimnames(result) <- dim.names # because if colname is NULL don't what it to be "A"
	}
	
	# add back meta attribute
	structure(result, meta = attr(mx, "meta"))
}


#' Remove cols that contain all zeros.
#' 
#' @param mx
#'  matrix
#' 
#' @return 
#'  matrix after removed.
#' 
#' @export
remove.zero.cols <- function(mx) {
	col.is.non.zero <- !(colSums(mx) == 0)
	structure(mx[, col.is.non.zero, drop = FALSE], meta=attr(mx,"meta"))
}


#' Remove rows that contain all zeros.
#' 
#' @param mx
#'  matrix
#' 
#' @return 
#'  matrix after removed.
#'
#' @export
remove.zero.rows <- function(mx) {
	row.is.non.zero <- !(rowSums(mx) == 0)
	structure(mx[row.is.non.zero, , drop = FALSE], meta=attr(mx,"meta"))
}


#' Remove cols specified by indices.
#' 
#' @param mx
#'  matrix
#' @param indices
#'  indices to remove. If indices is length zero, then mx is return as is.
#' @return 
#'  the matrix mx with columns removed. Names and any "meta" attribute preserved.
#'  Always returns a matrix.
#' 
#' @export
remove.cols <- function(mx, indices) {
	if (length(indices)==0) return(mx)
	structure(mx[, -indices, drop=FALSE], meta=c(attr(mx, "meta")))
}


#' Remove columns that contain all NAs.
#' 
#' @param mx
#'  matrix
#' 
#' @return 
#'  matrix after removed.
#'
#' @export
remove.NA.cols <- function(mx) {
	col.is.na <- colSums(is.na(mx)) == nrow(mx)
	structure(mx[,!col.is.na, drop = FALSE], meta=attr(mx,"meta"))
}


#' Remove cols by name.
#' 
#' @param x
#'  matrix or dataframe
#' @param cnames
#'  vector of colnames
#' 
#' @return 
#'  matrix after removed.
#'
#' @export
remove.cols.named <- function(x, cnames) {
	matched <- match(cnames, colnames(x))
	if (is.na(matched)) {
		x
	} else {
		x[,-matched, drop = FALSE]
	}
}


#' Remove rows by name.
#' 
#' @param mx
#'  matrix or dataframe
#' @param rnames
#'  vector of rownames
#' 
#' @return 
#'  matrix after removed.
#'
#' @export
remove.rows.named <- function(mx, rnames) {
	matched <- match(rnames, rownames(mx))
	if (is.na(matched)) {
		mx
	} else {
		mx[-matched,, drop = FALSE]
	}
}


#' Select only the specified row from each matrix in a list,
#' and return a list of row vectors.
#' 
#' @param mxlist
#'  list of matrices
#' @param rownum
#'  row number to select
#' @param na.rm
#'  if TRUE, then do not return any selected row that contains a NA
#' 
#' @return
#' a list of selected row vectors
#' 
#' @export
select.row.list.mx <- function(mxlist, rownum, na.rm = T) {
	
	result <- lapply(mxlist, function (mx) {
				# mx <- mxlist[[1]]
				# mx <- mxlist[[2]]
				selected.row <- mx[rownum, ]
				if (na.rm && any(is.na(selected.row))) NA else selected.row	
			})
	
	if (na.rm) result <- result[!is.na(result)]
	result
	
}


#' Subset the first dimension, returning the same number of dimensions
#' 
#' @param x 
#' a vector, matrix or array
#' 
#' @param logiset 
#' vector to subset first dimension by
#' 
#' @return 
#' subset of the vector, matrix or array
#' 
#' @export 
subsetFirstDimension <- function (x, logiset) {
	
	matrixDims <- length(dim(x))
	
	if (matrixDims == 0) {
		#x is a vector
		
		if (length(x) != length(logiset)) {
			stop("logiset is not the same length as x")
		} else {
			x[logiset]
		}
		
	} else if (matrixDims == 2) {
		#x is a 2d matrix
		if (dim(x)[1] != length(logiset)) {
			stop("length of logiset is not the same as number of rows in x")
		} else {
			x[logiset, , drop = FALSE]
		}
		
		
	} else if (matrixDims == 3) {
		#x is a 3d matrix
		if (dim(x)[1] != length(logiset)) {
			stop("length of logiset is not the same as number of rows in x")
		} else {
			x[logiset, , , drop = FALSE]
		}
	}
	
}

