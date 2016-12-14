
#' Return the category names for a vector of coded values.
#' If no category names, returns the list as is.
#'
#' @param .
#'  this 
#' @param x
#'  coded values
#' @param varname
#'  name of the category of x, or NULL
#' @return
#'  x, is there are no codings for varname, else a vector of category names
#'  corresponding to the values in x
#' 
#' @export
cmatch <- function (dict, x, varname) {
  if (is.null(varname)) return(x)
  
  xcodings <- dict$codings[[varname]]
  if (is.null(xcodings)) return(x)
  
  # match x in catcodings
  codings.indices <- match(x, xcodings)
  names(xcodings)[codings.indices]
  
}


#' Create a dictionary object.
#' 
#' @param dictionary_dataframe
#'   a dataframe with the variables:
#'   Varname = the variable name
#'   Description = a description of the variable.
#'   Varname = the name of the categorical variable
#'   Codings_Expr = an expression which generates the codings, eg:
#'  				"c("Other"=1, "Pacific"=2, "Maori"=3)"
#'  			   NB: Codings must be specified in numeric order
#' @return 
#' a dictionary object
#' Vector of variable desciptions.
#' names(descriptions) are the variable names.
#' List of codings.
#' names(codings) are the variable names.
#' Each element is a vector. The vector values are the category values (eg: 1, 2)
#' and the names of the vector are the category descriptions. 
#' Each element also contains the varname attribute.
#' 
#' Example of an element:
#'  
#' Professional     Clerical Semi-skilled 
#'        1            2            3 
#' attr(,"varname")
#' [1] "SESBTH"
#'  
#' @export
createDict <- function (dictionary_dataframe) {

  list(descriptions = createDescriptions(dictionary_dataframe),
       codings = createCodings(dictionary_dataframe), 
       age = createAgeRange(dictionary_dataframe) )	
}

#' Create the descriptions of the dictionary object. Called by new().
#' 
#' @param codings_dataframe
#'  
#' 
#' @return 
#' 
#' 
#' @export
createAgeRange <- function (descriptions_dataframe) {
  #remove empty variables, 
  #generally these are blank lines at the end of the file
  descriptions_dataframe <- subset(descriptions_dataframe, (Varname!=""))
  
  dict <- descriptions_dataframe$Age
  names(dict) <- descriptions_dataframe$Varname
  
  dict[dict != ""]
  
}	

#' Returns the category names for the vector of flattened codes.
#'
#' @param dict
#'  a dictionary object
#' @param x.flat
#'  a vector of flattened codes. A flattened code is in the form "0 1",
#'  where the first value is a grping code and the second a varname code.
#'  If grpby.tag is NULL or NA, then the flattened code will be in the form "0", 
#'  i.e: no grping codes only varname codes.
#'  
#' @param varname
#'  identifies the varname coding
#' @param grpby.tag
#'  identifies the grping coding, or NULL if no grouping coding.
#' 
#' @return 
#' 
#' 
#' @export
cmatchFlattened <- function (dict, x.flat, varname, grpby.tag) {
  
  if (is.null(grpby.tag) || is.na(grpby.tag)) {
    #no grping codes, only varname codes
    cmatch(dict, x.flat, varname)	
  } else {
    # add category and group by coding names, if any
    grping.match <- regexpr(".*\\s", x.flat)
    grping <- trim(regmatches(x.flat, grping.match))
    grpingNames <- cmatch(dict, grping, grpby.tag)
    
    cats.match <- regexpr("\\s.*", x.flat)
    cats <- trim(regmatches(x.flat, cats.match))
    catsNames <- cmatch(dict, cats, varname)
    
    structure(paste(grpingNames, catsNames), grpingNames=grpingNames)	
    
  }	
}

#' Returns the names of the codings for the supplied variables.
#' NB: Requires codings variable to have been set in global environment.
#' 
#' @param dict
#'  a dictionary object
#' @param vars
#'  vector of variable names
#' 
#' @return 
#'  list of coding names for the variables supplied
#' 
#' @export
cnamesLookup <- function(dict, vars) 
  lapply(dict$codings[vars], names)


#' Create the codings of the dictionary object. Called by new().
#' 
#' @param codings_dataframe
#'  
#' 
#' @return 
#' a list of category names for categorical variables.
#' 
#' @export

createCodings <- function (codings_dataframe) {
  #remove empty variables, 
  #these are blank lines at the end of the file or
  #variables without any coding expr
  codings_dataframe  <- subset(codings_dataframe, (Varname!=""))
  codings_dataframe  <- subset(codings_dataframe, (Codings_Expr!=""))
  
  #evaluate "Codings_Expr" column in global environment
  codings <- eval.list(codings_dataframe$Codings_Expr)
  
  names(codings) <- codings_dataframe$Varname
  
  #add "varname" attribute for use with mean_mx_cols.lbl function
  codings <- mapply(function(coding, name) {
    structure(coding, varname=name)
  }, codings, names(codings))
  
  codings
}


#' Create the descriptions of the dictionary object. Called by new().
#' 
#' @param codings_dataframe
#'  
#' 
#' @return 
#' 
#' 
#' @export

createDescriptions <- function (descriptions_dataframe) {
  #remove empty variables, 
  #generally these are blank lines at the end of the file
  descriptions_dataframe <- subset(descriptions_dataframe, (Varname!=""))
  
  dict <- descriptions_dataframe$Description
  names(dict) <- descriptions_dataframe$Varname
  
  dict
  
}	

#' Order the list by the results returned applying
#' dlookup to the list's elements
#' 
#' @param dict
#'  a dictionary object
#' 
#' @param ...
#'  objects to perform dlookup on
#' @seealso \code{\link{dlookup}}
#' 
#' @return 
#'  ... in order according to dlookup
#' 
#' @export
order_by_dlookup <- function (dict, ...) {
  xlist <- c(...)
  ordering <- sort.list(sapply(xlist, dlookup(dict)))
  xlist[ordering]
}

#' Lookup description of variable x in the dictionary
#' first determines the name of variable x, then does the lookup
#' 
#' @param dict
#'  a dictionary object
#' @param x
#'  
#' 
#' @return 
#' 
#' @export
dlookup <- function(dict, x) {
  
  name <- c()
  grouping <- c()
  set <- c()
  weighting <- c()
  meta <- attr(x, "meta")
  
  #get the variable name
  if (!is.null(meta)) {
    #use the meta attribute
    name <- meta["varname"]
    if (!is.na(meta["grouping"])) grouping <- paste(" by ", meta["grouping"], sep="")
    if (!is.na(meta["grpby.tag"])) {
      if (length(grep("sg.var", meta["grpby.tag"]))>=1) {
        grouping <- " by subgroup"	
      } else {
        grouping <- paste(" by ", dlookup(dict, meta["grpby.tag"]), sep="")
      }
    } 
    if (!is.na(meta["set"])) set <- paste(" (", meta["set"], ")", sep="")
    if (!is.na(meta["weighting"])) weighting <- meta["weighting"]
    
  } 
  
  # if no meta, or no name from meta
  if (is.null(name) || is.na(name)) {
    
    if (class(x) %in% c("matrix", "array", "table") && !is.null(names(dimnames(x)))) {
      #get name from names of dimensions
      namesdim <- names(dimnames(x))
      namesdim <- stripEmpty(namesdim) #remove NAs and empty strings
      
      # get last dim for name
      name <- namesdim[length(namesdim)]
      
    } else if (class(x) == "character") {
      #get name from first position of char vector
      name <- x[1]
      
    } else {
      #fail
      firstParamName <- as.character(sys.call())[2]
      stop(gettextf("cannot determine varname from %s: no meta or names", firstParamName))
    }
  }
  
  #lookup name in dictionary
  if (!name %in% names(dict$descriptions)) {
    stop(gettextf("'%s' does not exist in the data dictionary", name))
  }
  
  desc <- dict$descriptions[[name]]
  
  if (is.null(desc)) {
    stop(gettextf("variable named '%s' does not exist in data dictionary", name))
    name <- dname
  }
  
  #add grouping, weighting, and set descriptions (if any)
  weightdesc <- ifelse(weighting == "weightBase", "", " scenario")
  paste(desc, grouping, weightdesc, set, sep="")
}



#' A version of dlookup that can be use to see if a given expression is a variable in 
#' the dictionary or if it is not.  
#' Useful when users specifiy their own subgroup expression.  If the expression is not 
#' a name in the dictionary then one can assume that the user has specified their own 
#' expression to be used for subgrouping purposes.
#' 
#' @export
#'
dlookup_exists <- function(dict, x) {
  
  name <- c()
  grouping <- c()
  set <- c()
  weighting <- c()
  meta <- attr(x, "meta")
  
  #get the variable name
  if (!is.null(meta)) {
    #use the meta attribute
    name <- meta["varname"]
    if (!is.na(meta["grouping"])) grouping <- paste(" by ", meta["grouping"], sep="")
    if (!is.na(meta["grpby.tag"])) grouping <- paste(" by ", dlookup(dict, meta["grpby.tag"]), sep="")
    if (!is.na(meta["set"])) set <- paste(" (", meta["set"], ")", sep="")
    if (!is.na(meta["weighting"])) weighting <- meta["weighting"]
    
  } 
  
  # if no meta, or no name from meta
  if (is.null(name) || is.na(name)) {
    
    if (class(x) %in% c("matrix", "array", "table") && !is.null(names(dimnames(x)))) {
      #get name from names of dimensions
      namesdim <- names(dimnames(x))
      namesdim <- stripEmpty(namesdim) #remove NAs and empty strings
      
      # get last dim for name
      name <- namesdim[length(namesdim)]
      
    } else if (class(x) == "character") {
      #get name from first position of char vector
      name <- x[1]
      
    } else {
      #fail
      firstParamName <- as.character(sys.call())[2]
      #cat("Variable does not exist in the data dictionary \n")
      return(1)
      #stop(gettextf("cannot determine varname from %s: no meta or names", firstParamName))
    }
  }
  
  #lookup name in dictionary
  if (!name %in% names(dict$descriptions)) {
    #cat("Variable does not exist in the data dictionary \n")
    return(1)
    #stop(gettextf("'%s' does not exist in the data dictionary", name))
  }
  
  desc <- dict$descriptions[[name]]
  
  if (is.null(desc)) {
    stop(gettextf("variable named '%s' does not exist in data dictionary", name))
    name <- dname
  }
  
  #add grouping, weighting, and set descriptions (if any)
  weightdesc <- ifelse(weighting == "weightBase", "", " scenario")
  paste(desc, grouping, weightdesc, set, sep="")
}




