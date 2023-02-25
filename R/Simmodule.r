

#' Creates a Simulation module object.
#' 
#' A simulation module is really the core of a simulation. It contains the code and output for a distinct set 
#' of results generated, eg: health outcomes for years 1 - 10. 
#'  
#' It contains the following key elements:
#' 
#' outcomes - a list of all outcome matrices for the Simmodule.
#' 
#' each Simmodule has a \code{simulateRun} method which transforms the simframe. Typically, transformations will 
#' move variables for micro-units in the simframe through multiple iterations (or time steps).  
#' At the end of each iteration, for outcome variables (as defined in the simframe), the current values 
#' for all micro-units are stored in an outcome matrix.
#' An outcome matrix contains the set of values for each micro-unit during each iteration.
#' 
#' At the end of each run a set of run stats is calculated for outcomes. A run stat is essentially a function that takes
#' an outcome matrix and produces an aggregate value for each iteration. 
#' This aggregate value may be a single value (eg: mean), a vector (eg: frequencies, quantiles, summary), 
#' or a matrix (eg: 2 way table). 
#' 
#' Run stats are averaged across multiple runs by collateRunStats to get a final simulation result.
#' 
#' @param name
#'  name of this object
#' 
#' @return 
#'  a list
#' 
#' @export
createSimmodule <- function(name){
    
  return(list(name=name, run_results=list()))		
}

#' Simulate outomes and store them in the outcomes list.
#' 
#' Sub-classes should extend this function.
#' 
#' @param run
#'  numeric on current run number
#' 
#' @param simenv
#'  simulation environment object
#' 
#' @param simulateFun
#'  simulation function input by the user
#' 
#' @return 
#' a matrix contain the outcome 
#' 
#' @export 
simulateRun <- function (run, simenv, simulateFun) {
  
  simulateFun(run, simenv)
}


#' prepend.paths
#'  
#' @export 
prepend.paths <- function(expr, env.base) {
  #expr <- "r1stchildethnLvl3==1 & mhrswrk<21"
  
  catvars <- getOutcomeVars(env.base$simframe, "categorical")
  contvars <- getOutcomeVars(env.base$simframe, "continuous")
  time.variant.vars <- c(catvars, contvars)
  #catvars and contvars are time-variant
  presimvars <- names(env.base$presim.stats)
  
  #presimvars are time-invariant
  
  for (i in 1:length(time.variant.vars)) {
    pos1 <- str_locate(expr, time.variant.vars[i])
    if (sum(is.na(pos1))==0) {
      replace.expr1 <- paste("outcomes$", time.variant.vars[i], sep="")
      expr <- stri_replace_all_fixed(expr, time.variant.vars[i], replace.expr1)	
    }
  }
  for (i in 1:length(presimvars)) {
    #problem if subgroup defintion variable is pregalc as "ga" is in pregalc and gets replaced
    #do ad hoc fix here (but would be better to write a better function)
    #other, non-generic fix is to change the name of ga
    pos2 <- str_locate(expr, presimvars[i])
    if (sum(is.na(pos2))==0) {
      if (presimvars[i]=="ga" &  length(grep("pregalc", expr))>0) {
        #do nothing
      } else {
        replace.expr2 <- paste("simframe$", presimvars[i], sep="")
        ##expr <- str_replace(expr, presimvars[i], replace.expr2)
        expr <- stri_replace_all_fixed(expr, presimvars[i], replace.expr2)
      }
    }
  }
  #browser()
  sg.expr <- paste("sg.var <- ", expr, sep="")
  sg.expr.base <- stri_replace_all_fixed(sg.expr, "outcomes", "base.outcomes.current.run")
  sg.expr.base <- str_replace(sg.expr.base, "sg.var", "sg.var.base")
  sg.expr.base <- stri_replace_all_fixed(sg.expr.base, "simframe", "env.base$simframe")
  result <- list(sg.expr=sg.expr, sg.expr.base=sg.expr.base)
  return(result)
}

#' prepend.paths.for.yr1.CI
#'  
#' @export 
prepend.paths.for.yr1.CI <- function(expr, env.base) {
  #expr <- "r1stchildethnLvl3==1 & mhrswrk<21"
  
  catvars <- getOutcomeVars(env.base$simframe, "categorical")
  contvars <- getOutcomeVars(env.base$simframe, "continuous")
  time.variant.vars <- c(catvars, contvars)
  #catvars and contvars are time-variant
  presimvars <- names(env.base$presim.stats)
  #presimvars are time-invariant
  
  for (i in 1:length(time.variant.vars)) {
    pos1 <- str_locate(expr, time.variant.vars[i])
    if (sum(is.na(pos1))==0) {
      replace.expr1 <- paste("outcomes$", time.variant.vars[i], "[,1]", sep="")
      expr <- stri_replace_all_fixed(expr, time.variant.vars[i], replace.expr1)
    }
  }
  for (i in 1:length(presimvars)) {
    pos2 <- str_locate(expr, presimvars[i])
    if (sum(is.na(pos2))==0) {
      if (presimvars[i]=="ga" &  length(grep("pregalc", expr))>0) {
        #do nothing
      } else {
        replace.expr2 <- paste("simframe$", presimvars[i], sep="")
        ##expr <- str_replace(expr, presimvars[i], replace.expr2)
        expr <- stri_replace_all_fixed(expr, presimvars[i], replace.expr2)
      }
    }
    
  }
  sg.expr <- paste("sg.var <- ", expr, sep="")
  return(sg.expr)
}

#' remove.NA.cols
#'  
#' @export 
remove.NA.cols <- function(collated_results_freqs, num.runs) {
  
  colname <- colnames(collated_results_freqs)
  varname1 <- attr(collated_results_freqs, "meta")["varname"]
  
  #identify rows where there are only NAs (there will be 0s for the
  na.rows.id <- which(collated_results_freqs[, ncol(collated_results_freqs)]==100)
  na.col.id <- which(is.na(colname))
  if (length(na.col.id)==0) {
    na.col.id <- grep("NA", colname)
  }
  
  if (num.runs==1) {
    
    #remove the NA column
    if (length(na.col.id)>0) {
      collated_results_freqs <- as.matrix(collated_results_freqs[,-na.col.id])
      #attach back the column name
      colnames(collated_results_freqs) <- colname[-na.col.id]
      #lost varname meta attribute in the subsetting of the columns - code below returns it
      attr(collated_results_freqs, "meta") <- c(varname1)
    }
    
    #set the 0s in the remaining column (percentages with the characteristic) to be NA
    collated_results_freqs[na.rows.id,] <- NA		
    
  } else if (num.runs>1) {
    
    if (length(na.col.id)>0) {
      #keep only the columns relating to the charactersitic and remove the NA columns
      collated_results_freqs <- as.matrix(collated_results_freqs[,-na.col.id])
      #attach back the column name (though they don't always fall off it see
      colnames(collated_results_freqs) <- colname[-na.col.id]
      #lost varname meta attribute in the subsetting of the columns - code below returns it
      attr(collated_results_freqs, "meta") <- c(varname1)
    }
    #set the 0s in the remaining column (percentages with the characteristic) to be NA
    collated_results_freqs[na.rows.id,] <- NA
    
  }
  return(collated_results_freqs)
}




#' Adjust categorical values to desired proportions in cat.adjustments (if any).
#' 
#' Does not allow subgroup adjustments.
#' 
#' @param x
#'  categorical values to adjust
#' @param varname
#'  varname, used a lookup into cat.adjustments and propensities
#'  
#' @export
#'  
adjustCatVarSimple <- function(x, varname, simenv = simenv, iteration = iteration) {
  cat.adjustments <- simenv$cat.adjustments
  
  if (!varname %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
  
  desiredProps <- cat.adjustments[[varname]][iteration,]
  
  if (any(is.na(desiredProps))) {
    return(x)
  }
  
  cat("Adjusting", varname, ": ", desiredProps, "\n")
  
  modifyProps(x, desiredProps, propensities[[varname]][,,iteration])
}

#' Adjust categorical values to desired proportions in cat.adjustments (if any).
#' 
#' Allows subgroup adjustment if a subgroup expression attribute is attached to the cat.adjustments. 
#' 
#' @param x
#'  categorical values to adjust
#' @param varname
#'  varname, used a lookup into cat.adjustments and propensities
#'  
#' @export
#' 
adjustCatVar <- function(x, varname, propens=NULL, desiredProps=NULL, simenv, iteration) {
  
  cat.adjustments <- simenv$cat.adjustments
  
  varname.no.lvl <- strip_lvl_suffix(varname[1])
  
  if (!varname.no.lvl %in% names(cat.adjustments))
    stop(gettextf("No cat.adjustments for %s", varname))
  
  if (varname %in% c("NPRESCH", "INTERACT", "PUNISH")) {
    iteration <- 1 #just makes it use the 1st (and only) row for cat.adjustments
  }
  
  if (is.null(desiredProps)) {
    
    if(nrow(simenv$cat.adjustments$[[varname.no.lvl]]) < iteration){
      return(x)
    }
    
    #adjustCatVar is being used for scenario testing - get from cat.adjustments
    desiredProps <- cat.adjustments[[varname.no.lvl]][iteration,]
  }
  
  if (any(is.na(desiredProps))) {
    return(x)
  }
  
  #attach logisetexpr attribute to desiredProps
  desiredProps <- structure(desiredProps, varname=varname, logisetexpr=attr(cat.adjustments[[varname.no.lvl]], "logisetexpr"),
                            levels=simenv$dict$codings[[varname]])
  
  logiset <- evaluateLogisetExprAttribute(desiredProps, parent.frame())
  
  valid.subgroup <- check.subgroup.expr(cat.adjustments)
  
  if (valid.subgroup==1) {
    cat("Adjusting", varname.no.lvl, ": ", desiredProps, "\n")
    adj.x.cat <- x
    
    adj.x.cat[!is.na(x)] <- adjust.proportions(x, desiredProps, propens, logiset) 
    
    return(adj.x.cat)
  } else if (valid.subgroup==0) {
    expr <- paste("Scenario adjustments cannot be made for iteration ", iteration, " because the subgroup expression is not defined")
    cat(expr, "\n")
    return(x)
  } else {
    stop("Check valid.subgroup in simulateRun()")
  }
  
}

#' Adjust categorical values to desired proportions in cat.adjustments (if any).
#' 
#' Allows subgroup adjustment if a subgroup expression attribute is attached to the cat.adjustments. 
#' 
#' @param x
#'  categorical values to adjust
#' @param varname
#'  varname, used a lookup into cat.adjustments and propensities
#'  
#' @export
#' 
adjustCatVarCalib <- function(x, varname, propens=NULL, desiredProps=NULL, simenv = simenv, iteration = iteration) {
  
  
  varname.no.lvl <- strip_lvl_suffix(varname[1])
  
  if (varname %in% c("NPRESCH", "INTERACT", "PUNISH")) {
    iteration <- 1 #just makes it use the 1st (and only) row for cat.adjustments
  }
  
  if (any(is.na(desiredProps))) {
    return(x)
  }
  
  desiredProps <- structure(desiredProps, varname=varname, levels=simenv$dict$codings[[varname]])
  
  adj.x.cat <- adjust.proportions(x, desiredProps, propens) 
  return(adj.x.cat)
}


#' Adjust continuous values to desired proportions in cat.adjustments (if any).
#' 
#' Allows subgroup adjustment if a global subgroup expression is set. 
#' 
#' @param x
#' continuous values to adjust
#' @param varname
#'  varname, used a lookup into cat.adjustments and propensities
#'  
#' @export
#'
adjustContVar <- function(x, varname, simenv = simenv, propens=NULL, desiredProps=NULL, iteration) {
  cat.adjustments <- simenv$cat.adjustments
  
  if (!varname %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
  
  if (varname %in% c("INTERACT", "PUNISH", "NPRESCH")) {
    iteration <- 1 #just makes sure the 1st (and only) row from cat.adjustments is used next
  }
  
  if (is.null(desiredProps)) {
    #adjustCatVar is being used for scenario testing - get from cat.adjustments
    desiredProps <- cat.adjustments[[varname]][iteration,]
    
  }
  
  if (any(is.na(desiredProps))) {
    return(x)
  }
  
  #attach logiset attribute to desiredProps
  desiredProps <- structure(desiredProps, varname=varname, logisetexpr=attr(cat.adjustments[[varname]], "logiset"), levels=simenv$dict$codings[[varname]])
  
  logiset <- evaluateLogisetExprAttribute(desiredProps, parent.frame())
  valid.subgroup <- check.subgroup.expr(cat.adjustments)
  
  if (valid.subgroup==1) {
    cat("Adjusting", varname, ": ", desiredProps, "\n")
    
    catToContModels <- attr(cat.adjustments[[varname]], "catToContModel")
    cont.binbreaks <- attr(cat.adjustments[[varname]], "cont.binbreaks")
    
    adj.x.cont <- adjust.proportions(x, desiredProps, propens, logiset, catToContModels, cont.binbreaks, envir=parent.frame())
    return(adj.x.cont)
  } else if (valid.subgroup==0) {
    expr <- paste("Scenario adjustments cannot be made for iteration ", iteration, " because the subgroup expression is not defined")
    cat(expr, "\n")
    return(x)
  } else {
    stop("Check valid.subgroup in simulateRun()")
  }
}



#' Adjust continuous values to desired proportions in cat.adjustments (if any).
#' 
#' Does not allow subgroup adjustment
#' 
#' @param x
#' continuous values to adjust
#' @param varname
#'  varname, used a lookup into cat.adjustments and propensities
#'  
#' @export
#' 
adjustContVarSimple <- function(x, varname, simenv, iteration) {
  cat.adjustments <- simenv$cat.adjustments
  
  if (!varname %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
  
  desiredProps <- cat.adjustments[[varname]][iteration,]
  
  if (any(is.na(desiredProps))) {
    return(x)
  }
  
  cat("Adjusting", varname, ": ", desiredProps, "\n")
  
  catToContModels <- attr(cat.adjustments[[varname]], "catToContModel")
  cont.binbreaks <- attr(cat.adjustments[[varname]], "cont.binbreaks")
  
  adj.x.cont <- modifyPropsContinuous(x, desiredProps, catToContModels, cont.binbreaks, propensities[[varname]][,,iteration])
  
  return(adj.x.cont)
}


#' Adjust continuous values to desired proportions in cat.adjustments (if any).
#' 
#' Does not allow subgroup adjustment
#' 
#' @param x
#' continuous values to adjust
#' @param varname
#'  varname, used a lookup into cat.adjustments and propensities
#'  
#' @export
#' 
adjustContVarCalib <- function(x, varname, propens=NULL, desiredProps=NULL, simenv, iteration) {
  cat.adjustments <- simenv$cat.adjustments
  
  if (varname %in% c("INTERACT", "PUNISH", "NPRESCH")) {
    iteration <- 1 #just makes sure the 1st (and only) row from cat.adjustments is used next
  }
  
  if (any(is.na(desiredProps))) {
    return(x)
  }
  
  desiredProps <- structure(desiredProps, varname=varname, levels=simenv$dict$codings[[varname]])
  
  catToContModels <- attr(cat.adjustments[[varname]], "catToContModel")
  cont.binbreaks <- attr(cat.adjustments[[varname]], "cont.binbreaks")
  
  adj.x.cont <- adjust.proportions(x, desiredProps, propens, logiset=NULL, catToContModels, cont.binbreaks, envir=parent.frame())
  return(adj.x.cont)
}



rowMeans <- 
function (x, na.rm = FALSE, dims = 1L) 
{
	
    x <- as.matrix(x)
    	
	dn <- dim(x)
	
    p <- prod(dn[-(id <- seq_len(dims))])
    dn <- dn[id]
    z <- if (is.complex(x)) 
        .Internal(rowMeans(Re(x), prod(dn), p, na.rm)) + (0+1i) * 
            .Internal(rowMeans(Im(x), prod(dn), p, na.rm))
    else .Internal(rowMeans(x, prod(dn), p, na.rm))
    if (length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[id]
    }
    else names(z) <- dimnames(x)[[1L]]
    z
}
