# TODO: Add comment
# 
# Author: oman002
###############################################################################


#' Merges (ie: join) on a key column and returns merged rows, minus
#' the key column. Merging means selecting rows from key_column_name
#' that are in selected_keys, erroring if a row cannot be found.  
#' 
#' @param df
#'  dataframe
#' @param key_column_name
#'  column to merge on
#' @param selected_keys
#'  values of keys in key_column_name to select
#'
#' @return
#' a dataframe after merged.
#' 
#' @export
mergeAndRemoveKeyColumn <- function(df, key_column_name, selected_keys) {
	if (!key_column_name %in% names(df)) {
		stop(gettextf("Cannot find key column %s", key_column_name))
	}
	
	selected_indices <- match(selected_keys, df[[key_column_name]])
	checkNAs(selected_indices)
	
	merged <- df[selected_indices,] 
	
	rownames(merged) <- merged[[key_column_name]]
	remove.cols.named(merged, key_column_name)
}

#' Remove rows specified by indices
#' 
#' @param x
#'  matrix or dataframe
#' @param indices
#'  integer vector of indices to remove
#' 
#' @return
#' a matrix or dataframe after remove rows
#' 
#' @export
remove_rows_by_index <- function(x, indices) {
	#create inverted logical array of nas
	invlogi <- rep(TRUE, dim(x)[1])
	invlogi[indices] = FALSE
	
	x[invlogi, ]
}
