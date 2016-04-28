

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
#' @param .
#'  Simmodule receiving object.
#' @param name
#'  name of this object
#' 
#' @return 
#'  a list
#' 
#' @export
createSimmodule <- function(name){
    
  return(list(name=name, outcomes=list(),  run_results=list(), run_results_collated=list()))		
}

#' Simulate outomes and store them in the outcomes list.
#' 
#' Sub-classes should extend this function.
#' 
#' @param Simmodule
#'  Simmodule receiving object
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
simulateRun <- function (Simmodule, simenv, simulateFun) {
  
  
  simulateFun(Simmodule, simenv)
}



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

check.subgroup.expr <- function(cat.adjustments, simframe) {
  catadj1 <- cat.adjustments[[1]]
  logisetexpr <- attr(catadj1,"logisetexpr")
  valid.subgroup <- 1
  if (!is.null(logisetexpr)) {
    #evaluate the logiset expression using the simframe prior to the first simulations
    #(e.g. for year 1/basefile data in MELC)
    logiset <- eval(parse(text=logisetexpr), envir=simframe)
    u.logi <- unique(logiset)
    #Fs and NAs due to a combination subgroup such as welfare and alcohol
    if (((length(u.logi)==2) & (NA %in% u.logi)) | (length(table(logiset))==0)) {
      valid.subgroup <- 0							#e.g. due to subgroup alcoholAbuse==1
    }
  }
  return(valid.subgroup)
}



#' Map outcomes to run results for a single run. Run results are typically 
#' descriptive statistics (eg: freqs, means, etc) generated for variables 
#' in outcomes. 
#' 
#' @param moduleName
#'  the list of outcome matrices generated by simulateRun()
#'
#' @param simframe
#'  the simulation environment's simframe. Useful for accessing adjusted variables.
#' 
#' @param outcomes
#'  the list of outcome matrices generated by simulateRun()
#'  
#' @param cat.adjustments
#'  the list of outcome matrices generated by simulateRun()
#'  
#' @param run
#'  the list of outcome matrices generated by simulateRun()
#'   
#' @export
#'   
#' @return 
#'  a list of run results. Each element E1 of run results is a list and is the set of results for a given 
#'  operation. In turn, each element of E1 may be a list, matrix or vector and is the result for
#'  the operation applied to an outcome variable (ie: an element in outcomes). 
map_outcomes_to_run_results <- function(run, moduleName, simframe, outcomes, cat.adjustments) {
  
  cat(gettextf("Generating run results for %s\n", moduleName))
  
  catvars <- getOutcomeVars(simframe, "categorical", moduleName)
  convars <- getOutcomeVars(simframe, "continuous", moduleName)
  convars <- convars[-(which(convars%in%c("mage_years", "no99mage_years", "fage_years")))]
  yrs.vars <- c("mage_years", "no99mage_years", "fage_years")
  all.convars <- c(convars, yrs.vars) #final and intermediate outcomes
  int.convars <- convars[c(1:7, 15)] #intermediate outcomes only 
  
  #browser()
  
  # add additional "all years" row totals to continuous vars
  outcomes_wtotals <- lapply(outcomes[all.convars], function(x) {
    #x <- outcomes[[convars[1]]] 
    structure(cbind(x, "All Years"=rowMeans(x, na.rm=TRUE)), varname=attr(x,"varname"))
  })
  
  #bin/group continuous variables that are displayed as categorical for scenario testing purposes
  #(kids, chres, mhrswrk, fhrswrk, msmoke, fsmoke)	
  binned.list <- binned.list.base <- list()
  for (i in 1:length(int.convars)) {
    tab <- outcomes[c(int.convars[i])]
    binned.tab <- matrix(bin(tab[[1]], binbreaks[[int.convars[i]]]), ncol=NUM_ITERATIONS)
    attr(binned.tab, "meta") <- c(varname=int.convars[[i]])
    binned.list[[i]] <- binned.tab
    names(binned.list)[i] <- int.convars[i]
    
    if (!is.null(attr(cat.adjustments[[1]], "logisetexpr"))) {
      #the env.base$...run1$outcomes object only exits if we are in the scenario environments
      #we only need the base.outcomes.current.run if a subgroup scenario was run 
      #(otherwise, if no subgroup scenario run, they just look at the tables from env.base)
      base.outcomes.expr <- paste("env.base$modules[[1]]$run_results$run", run, "$outcomes", sep="")
      base.outcomes.current.run <- eval(parse(text=base.outcomes.expr))
      
      if (is.null(base.outcomes.current.run)) {
        stop(gettextf("%s does not exist. Cannot create binned.list.base", base.outcomes.expr))
      }
      
      tab.base <- base.outcomes.current.run[c(int.convars[i])]
      binned.tab.base <- matrix(bin(tab.base[[1]], binbreaks[[int.convars[i]]]), ncol=NUM_ITERATIONS)
      attr(binned.tab.base, "meta") <- c(varname=int.convars[[i]])
      binned.list.base[[i]] <- binned.tab.base
      names(binned.list.base)[i] <- int.convars[i]
    }
  }
  
  
  run_results <- list()
  

  rounded.outcomes <- sfLapply(outcomes[convars], round)    
  run_results$confreqs <- lapply(rounded.outcomes, table_mx_cols_MELC, dict=dict)    
  run_results$freqs <- sfLapply(outcomes[c(catvars,"typnode")], table_mx_cols)    
  run_results$freqs_continuousGrouped <- lapply(binned.list, table_mx_cols_MELC, dict=dict)    
  run_results$means <- sfLapply(outcomes_wtotals[convars], mean_mx_cols, na.rm=T)
  
  no99fage_years.mx <- outcomes_wtotals["fage_years"][[1]]
  no99fage_years.mx[no99fage_years.mx==99] <- NA
  
  run_results$summaries <- sfLapply(outcomes_wtotals[convars], summary_mx_cols)    
  run_results$quantiles <- sfLapply(outcomes_wtotals[convars], quantile_mx_cols, new.names=c("Min", "10th", "25th", "50th", "75th","90th","Max"), probs=c(0,.1,.25,.5,.75,.9,1), na.rm = TRUE)
  
  
  run_results$outcomes <- outcomes 
  run_results 
}

#' Collates (ie: reduces) all run results to averaged values and labels collated results.
#'
#' @param Simmodule
#'  receiving Simmodule object. 
#' 
#' @param all_run_results
#'  the list of all run results generated by map_outcomes_to_run_results() 
#' 
#' @param cat.adjustments
#'  the list of outcome matrices generated by simulateRun()
#'  
#' @param simframe
#'  the simulation environment's simframe. Useful for accessing adjusted variables.
#'   
#' @export
#'  
#' @return
#'  any object. Stored in the run_results_collated list.
#' 
collate_all_run_results <- function(Simmodule, all_run_results, cat.adjustments=NULL, simframe) {
  cat(gettextf("Collating all run results for %s\n", Simmodule$name))
  
  outcomes <- Simmodule$outcomes

  all_run_results_zipped <- lzip(all_run_results)
  all_run_results_zipped <- lapply(all_run_results_zipped, lzip)
  
  collated_results <- list()
  
  collated_results$confreqs <- lapply(all_run_results_zipped$confreqs, 
                                      collator_freqs, dict=dict, CI=FALSE) 
  #usually keep as FALSE (though TRUE works)
  collated_results$histogram <- lapply(all_run_results_zipped$confreqs, 
                                       collator_histogram, dict=dict)
  
  
  collated_results$freqs <- lapply(all_run_results_zipped$freqs, 
                                   collator_freqs_remove_zero_cat, dict=dict, CI=TRUE)
  
  collated_results$freqs_continuousGrouped <- 
    lapply(all_run_results_zipped$freqs_continuousGrouped, 
           collator_freqs2, dict=dict, CI=TRUE, cat.adjustments=cat.adjustments)
  
  #CIs are the default for means
  collated_results$means <- lapply(all_run_results_zipped$means, 
                                   collator_means, dict = dict, NA.as.zero=F, CI=TRUE)
  collated_results$means[[15]] <- collated_results$means[[15]][6,]
  collated_results$means[[15]] <- structure(collated_results$means[[15]], meta=c(varname="NPRESCH"))
  collated_results$means[[16]] <- collated_results$means[[16]][6,]
  collated_results$means[[16]] <- structure(collated_results$means[[16]], meta=c(varname="INTERACT"))
  collated_results$means[[17]] <- collated_results$means[[17]][6,]
  collated_results$means[[17]] <- structure(collated_results$means[[17]], meta=c(varname="PUNISH"))
  
  collated_results$summaries <- lapply(all_run_results_zipped$summaries, collator_list_mx)
  collated_results$quantiles <- lapply(all_run_results_zipped$quantiles, 
                                       collator_list_mx, NA.as.zero=FALSE, CI=FALSE)
  collated_results$quantiles[[15]] <- collated_results$quantiles[[15]][6,]
  collated_results$quantiles[[15]] <- structure(collated_results$quantiles[[15]], 
                                                meta=c(varname="NPRESCH"))
  collated_results$quantiles[[16]] <- collated_results$quantiles[[16]][6,]
  collated_results$quantiles[[16]] <- structure(collated_results$quantiles[[16]], 
                                                meta=c(varname="INTERACT"))
  collated_results$quantiles[[17]] <- collated_results$quantiles[[17]][6,]
  collated_results$quantiles[[17]] <- structure(collated_results$quantiles[[17]], meta=c(varname="PUNISH"))
  
  #Add normal theory CIs for year 1
  #browser()
  if (length(all_run_results)>1) {
    collated_results$freqs[1:6] <- lapply(collated_results$freqs[1:6], 
                                          normal.theory.CIs, outcomes=outcomes)
    
    collated_results$freqs_continuousGrouped <- 
      lapply(collated_results$freqs_continuousGrouped, normal.theory.CIs, 
             cat.adjustments=cat.adjustments, simframe=simframe, outcomes=outcomes, 
             grping.logical=FALSE, CI=FALSE) 
    #errors if I change to CI=TRUE
    
    collated_results$means[1:13] <- lapply(collated_results$means[1:13], 
                                           normal.theory.CIs, simframe=simframe, 
                                           outcomes=outcomes, statistic="mean", grping.logical=FALSE)
    
   
  }
  
  #Remove NA columns from conduct, alcohol abuse, and depression
  num.runs <- length(all_run_results)
  
  collated_results$freqs_continuousGrouped <- 
    lapply(collated_results$freqs_continuousGrouped, remove.NA.cols, num.runs=num.runs)
  
  collated_results$freqs <- lapply(collated_results$freqs, remove.NA.cols, num.runs=num.runs)
  
  collated_results
}


#' Map outcomes to run results for a single run. Run results are typically 
#' descriptive statistics (eg: freqs, means, etc) generated for variables 
#' in outcomes. 
#' 
#' @param moduleName
#'  the list of outcome matrices generated by simulateRun()
#'
#' @param simframe
#'  the simulation environment's simframe. Useful for accessing adjusted variables.
#' 
#' @param outcomes
#'  the list of outcome matrices generated by simulateRun()
#'  
#' @param cat.adjustments
#'  the list of outcome matrices generated by simulateRun()
#'  
#' @param run
#'  the list of outcome matrices generated by simulateRun()
#'   
#' @export
#'   
#' @return 
#'  a list of run results. Each element E1 of run results is a list and is the set of results for a given 
#'  operation. In turn, each element of E1 may be a list, matrix or vector and is the result for
#'  the operation applied to an outcome variable (ie: an element in outcomes). 
map_outcomes_to_run_resultsP <- function(run, simframe, outcomes, cat.adjustments) {
  
  dict <- as.list(dict)
  
  outcomes <- outcomes[[run]]
  
  #cat(gettextf("Generating run results for %s\n", .$name))
  
  catvars <- getOutcomeVars(simframe, "categorical", "years1_21")
  convars <- getOutcomeVars(simframe, "continuous", "years1_21")
  convars <- convars[-(which(convars%in%c("mage_years", "no99mage_years", "fage_years")))]
  yrs.vars <- c("mage_years", "no99mage_years", "fage_years")
  all.convars <- c(convars, yrs.vars) #final and intermediate outcomes
  int.convars <- convars[c(1:7, 15)] #intermediate outcomes only 
  
  
  
  # add additional "all years" row totals to continuous vars
  outcomes_wtotals <- lapply(outcomes[all.convars], function(x) {
    #x <- outcomes[[convars[1]]] 
    structure(cbind(x, "All Years"=rowMeans(x, na.rm=TRUE)), varname=attr(x,"varname"))
  })
  
  #bin/group continuous variables that are displayed as categorical for scenario testing purposes
  #(kids, chres, mhrswrk, fhrswrk, msmoke, fsmoke)	
  binned.list <- binned.list.base <- list()
  for (i in 1:length(int.convars)) {
    tab <- outcomes[c(int.convars[i])]
    binned.tab <- matrix(bin(tab[[1]], binbreaks[[int.convars[i]]]), ncol=NUM_ITERATIONS)
    attr(binned.tab, "meta") <- c(varname=int.convars[[i]])
    binned.list[[i]] <- binned.tab
    names(binned.list)[i] <- int.convars[i]
    if (!is.null(attr(cat.adjustments[[1]], "logisetexpr"))) {
      #the env.base$...run1$outcomes object only exits if we are in the scenario environments
      #we only need the base.outcomes.current.run if a subgroup scenario was run 
      #(otherwise, if no subgroup scenario run, they just look at the tables from env.base)
      base.outcomes.expr <- paste("env.base$modules[[1]]$run_results$run", run, "$outcomes", sep="")
      base.outcomes.current.run <- eval(parse(text=base.outcomes.expr))
      
      if (is.null(base.outcomes.current.run)) {
        stop(gettextf("%s does not exist. Cannot create binned.list.base", base.outcomes.expr))
      }
      
      tab.base <- base.outcomes.current.run[c(int.convars[i])]
      binned.tab.base <- matrix(bin(tab.base[[1]], binbreaks[[int.convars[i]]]), ncol=NUM_ITERATIONS)
      attr(binned.tab.base, "meta") <- c(varname=int.convars[[i]])
      binned.list.base[[i]] <- binned.tab.base
      names(binned.list.base)[i] <- int.convars[i]
    }
  }
  
  #user specified subgroup variable
  #prepend "outcomes$", "simframe$", or "env.base$...outcomes$" to variables so they
  #can be found
  #browser()
  if (!is.null(attr(cat.adjustments[[1]], "logisetexpr"))) {
    subgroup.expr <- attr(cat.adjustments[[1]], "logisetexpr")
    prepended.exprs <- prepend.paths(subgroup.expr) 
    sg.expr <- unlist(prepended.exprs["sg.expr"])
    names(sg.expr) <- ""
    sg.expr.base <- unlist(prepended.exprs["sg.expr.base"])
    names(sg.expr.base) <- ""
    eval(parse(text=sg.expr))
    eval(parse(text=sg.expr.base))
  }
  
  run_results <- list()
  

  
  
  rounded.outcomes <- sfLapply(outcomes[convars], round)    	
  
  run_results$confreqs <- sfLapply(rounded.outcomes, table_mx_cols_MELC_list, dict=dict)   
  
  run_results$freqs <- sfLapply(outcomes[c(catvars,"typnode")], table_mx_cols)    
  run_results$freqs_continuousGrouped <- sfLapply(binned.list, table_mx_cols_MELC_list, dict=dict)  
  
  run_results$means <- sfLapply(outcomes_wtotals[convars], mean_mx_cols, na.rm=TRUE)
  
  
  no99fage_years.mx <- outcomes_wtotals["fage_years"][[1]]
  no99fage_years.mx[no99fage_years.mx==99] <- NA
  
  run_results$summaries <- sfLapply(outcomes_wtotals[convars], summary_mx_cols)    
  run_results$quantiles <- sfLapply(outcomes_wtotals[convars], quantile_mx_cols, new.names=c("Min", "10th", "25th", "50th", "75th","90th","Max"), probs=c(0,.1,.25,.5,.75,.9,1), na.rm = TRUE)
  
  
  run_results$outcomes <- outcomes 
  run_results 
}

#' Collates (ie: reduces) all run results to averaged values and labels collated results.
#'
#' @param Simmodule
#'  receiving Simmodule object. 
#' 
#' @param all_run_results
#'  the list of all run results generated by map_outcomes_to_run_results() 
#' 
#' @param cat.adjustments
#'  the list of outcome matrices generated by simulateRun()
#'  
#' @param simframe
#'  the simulation environment's simframe. Useful for accessing adjusted variables.
#'   
#' @export
#'  
#' @return
#'  any object. Stored in the run_results_collated list.
#' 
collate_all_run_resultsP <- function(all_run_results, cat.adjustments=NULL, simframe, outcomes) {
  #cat(gettextf("Collating all run results for %s\n", .$name))
  
  
  #outcomes <- .$outcomes
  
  all_run_results_zipped <- lzip(all_run_results)
  all_run_results_zipped <- lapply(all_run_results_zipped, lzip)
  
  collated_results <- list()
  
  
  
  collated_results$confreqs <- lapply(all_run_results_zipped$confreqs, collator_freqs, dict=dict, CI=FALSE) #usually keep as FALSE (though TRUE works)
  collated_results$histogram <- lapply(all_run_results_zipped$confreqs, collator_histogram, dict=dict)
  
  
  collated_results$freqs <- lapply(all_run_results_zipped$freqs, collator_freqs_remove_zero_cat, dict=dict, CI=TRUE)
  
  collated_results$freqs_continuousGrouped <- lapply(all_run_results_zipped$freqs_continuousGrouped, collator_freqs2, dict=dict, CI=TRUE, cat.adjustments=cat.adjustments)
  
  #CIs are the default for means
  collated_results$means <- lapply(all_run_results_zipped$means, collator_means, dict = dict, NA.as.zero=F, CI=TRUE)
  
  collated_results$summaries <- lapply(all_run_results_zipped$summaries, collator_list_mx)
  collated_results$quantiles <- lapply(all_run_results_zipped$quantiles, collator_list_mx, NA.as.zero=FALSE, CI=FALSE)
  
  #Add normal theory CIs for year 1
  if (length(all_run_results)>1) {
    #browser()
    collated_results$freqs[1:6] <- lapply(collated_results$freqs[1:6], normal.theory.CIs, outcomes=outcomes)
    
    collated_results$freqs_continuousGrouped <- lapply(collated_results$freqs_continuousGrouped, normal.theory.CIs, cat.adjustments=cat.adjustments, simframe=simframe, outcomes=outcomes, grping.logical=FALSE, CI=FALSE) #errors if I change to CI=TRUE
    
    collated_results$means[1:13] <- lapply(collated_results$means[1:13], normal.theory.CIs, simframe=simframe, outcomes=outcomes, statistic="mean", grping.logical=FALSE)
    
    
  }
  
  #Remove NA columns from conduct, alcohol abuse, and depression
  num.runs <- length(all_run_results)
  collated_results$freqs_continuousGrouped <- lapply(collated_results$freqs_continuousGrouped, remove.NA.cols, num.runs=num.runs)
  collated_results$freqs <- lapply(collated_results$freqs, remove.NA.cols, num.runs=num.runs)
  
  collated_results
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
#' @examples
#'  varname <- "z1msmokeLvl1"
#'  varname <- "z1singleLvl1"
#'  adjustCatVar(predSimBinomsSelect(z1single_previousLvl1, models$z1singlePrev0, models$z1singlePrev1), "z1singleLvl1")
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
#' @examples
#'  varname <- "z1msmokeLvl1"
#'  varname <- "z1singleLvl1"
#'  adjustCatVar(predSimBinomsSelect(z1single_previousLvl1, models$z1singlePrev0, models$z1singlePrev1), "z1singleLvl1")
adjustCatVar <- function(x, varname, propens=NULL, desiredProps=NULL, simenv, iteration) {
  
  cat.adjustments <- simenv$cat.adjustments
  
  varname.no.lvl <- strip_lvl_suffix(varname[1])
  
  if (!varname.no.lvl %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
  
  if (varname %in% c("NPRESCH", "INTERACT", "PUNISH")) {
    iteration <- 1 #just makes it use the 1st (and only) row for cat.adjustments
  }
  if (is.null(desiredProps)) {
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
  
  valid.subgroup <- check.subgroup.expr(cat.adjustments, parent.frame())
  
  if (valid.subgroup==1) {
    cat("Adjusting", varname.no.lvl, ": ", desiredProps, "\n")
    
    adj.x.cat <- adjust.proportions(x, desiredProps, propens, logiset) 
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
#' @examples
#'  varname <- "z1msmokeLvl1"
#'  varname <- "z1singleLvl1"
#'  adjustCatVar(predSimBinomsSelect(z1single_previousLvl1, models$z1singlePrev0, models$z1singlePrev1), "z1singleLvl1")
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
#' @examples
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
  valid.subgroup <- check.subgroup.expr(cat.adjustments, parent.frame())
  
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
#' @examples
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
#' @examples
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

#' Calculates a 95% confidence interval using assymptotic normal theory and replaces the first 
#' 	#row of mx with it.
#' 
#' @param mx
#'  A matrix containing collated results with confidence intervals.
#' 
#' @param statistic
#'  if "proportion" (the default), it is assumed that confidence intervals for a proportion are 
#' 	desired otherwise, if "mean" it is assumed that confidence intervals for a mean are desired.
#' 
#' @param outcomes
#' A list of matrices with each matrix containing the outcomes across all iterations (columns
#' 	correspond tpo iterations and rows to individuals).  Must be supplied.
#' 
#' @param simframe
#' the simframe for the current environmenmt.  Used to get the data vector if the variable is 
#' 	not in outcomes.
#' 
#' @param cat.adjustments
#' The cat.adjustments need to be passed to the function if a user specified subgroup has been
#' specified.  The logiset expression from the cat.adjustments will be used to define the 
#' subgroup variable and can also be used to get the binbreaks for continuous variables.
#' 
#' @param grping.logical
#' Takes the value TRUE or FALSE to indicate whether confidence intervals should be done 
#' separately for different subgroups.
#' 
#' @param grpbyName
#' variable name of a grouping variable
#' 
#' @param logiset
#' a vector or matrix containing TRUEs and FALSEs. Confidence intervals 
#' will only be calculated for the TRUE subset of the data
#' 
#' @return
#'  a matrix of the same dimension as mx with the first row replaced with assymnptotic normal 
#' theory 95% confidence intervals. 
#'
#' @export 
normal.theory.CIs <-  function(mx, statistic="proportion", outcomes=NULL, simframe=NULL, 
                               cat.adjustments=NULL, grping.logical=TRUE, grpbyName=NULL, 
                               logiset=NULL, CI=TRUE) {
  if (CI==FALSE) {
    return(mx)
  }
  if (all(is.na(mx[1,]))) {
    return(mx)
  }
  varname <- attr(mx, "meta")["varname"]
  if (is.null(varname)) {
    varname <- attr(mx, "varname")
  }
  
  num.units = nrow(outcomes[[1]])
  
  if (is.null(logiset)) {
    logiset <- rep(TRUE, num.units)
  }
  if (is.null(dim(logiset))) {
    #then logiset is a vector and we convert it to a matrix
    logiset <- matrix(rep(logiset, NUM_ITERATIONS), byrow=F, ncol=NUM_ITERATIONS)
  }
  
  #Define a subgroup variable - either comes from an expression entered in the scenario weightings 
  #screen or is a variable name passed from tableBuilder().  (It will not come from both).
  
  #Create subgroup variable that comes from a user specified expression in the scenario weightings screen: 
  if ((!is.null(attr(cat.adjustments[[1]], "logisetexpr")))&(grping.logical==TRUE)&(sum(logiset[,1])==num.units)) {
    subgroup.expr <- attr(cat.adjustments[[1]], "logisetexpr")								
    prepended.expr <- prepend.paths.for.yr1.CI(subgroup.expr)
    eval(parse(text=prepended.expr))
  } else {
    sg.var <- rep(1, num.units)
  }
  
  #Or the subgroup variable is defined by a variable name passed from tableBuilder().
  if (!is.null(grpbyName)) {
    if (grpbyName=="") {
      grpbyName <- NULL
    }
  }
  if (!is.null(grpbyName)) {
    sg.var <- outcomes[[grpbyName]][,1]
    if (is.null(sg.var)) {
      #subgroup variable is not time-variant and hence is not in outcomes
      sg.var <- simframe[[grpbyName]]
      if (is.null(sg.var)) {
        #variable is a time-invariant Lvl var and is only present in the simframe as three 
        #dichotomous variables with Lvl suffixes
        which.vars <- str_locate_all(names(simframe), grpbyName)
        lvl.vars <- which(lapply(which.vars, length)>0)
        if (grpbyName%in%c("r1stfeduc", "r1stmeduc", "SESBTH")) {
          sg.var <- binary.levels.combine(simframe[[lvl.vars[1]]], simframe[[lvl.vars[2]]], simframe[[lvl.vars[3]]])
        } else if (grpbyName%in%c("r1stchildethn")) {
          sg.var <- binary.levels.combine(simframe[[lvl.vars[1]]], simframe[[lvl.vars[2]]], simframe[[lvl.vars[3]]], simframe[[lvl.vars[4]]])
        } else {
          stop("Check binary levels.combine in normal.theory.CIs")
        }
      }
    }
  }
  #subset the subgroup variable by the logiset
  sg.var <- sg.var[logiset[,1]==TRUE]
  
  if (all(is.na(sg.var))) {
    #cannot do freqs_by_subgroup for this variable, probably because the subgroup variable was not simulated at the current iteration
    return(mx)
  }
  
  #Define the year 1 data vector
  dat <- outcomes[[varname]][,1]
  if (is.null(dat)) {
    #variable is not an outcome (it is time-invariant) so need to use the simframe
    dat <- simframe[[varname]]
    if (is.null(dat)) {
      #variable is a time-invariant "Lvl" variable and is only present in the simframe as three dichotomous variables with Lvl suffixes
      which.vars <- str_locate_all(names(simframe), varname)
      lvl.vars <- which(lapply(which.vars, length)>0)
      if (varname%in%c("r1stfeduc", "r1stmeduc", "SESBTH")) {
        dat <- binary.levels.combine(simframe[[lvl.vars[1]]], simframe[[lvl.vars[2]]], simframe[[lvl.vars[3]]])
      } else if (varname%in%c("r1stchildethn")) {
        dat <- binary.levels.combine(simframe[[lvl.vars[1]]], simframe[[lvl.vars[2]]], simframe[[lvl.vars[3]]], simframe[[lvl.vars[4]]])
      } else {
        stop("Check binary.levels.combine in normal.theory.CIs()")
        #consider making code more generic - rather than having only 2 cases here
      }
    }
  }
  
  if (statistic=="proportion") {
    breaks <- attr(cat.adjustments[[varname]], "cont.binbreaks")
    if (!is.null(breaks)) {
      dat <- bin(dat, breaks)	
    }
    
    #subset the data variable by the logiset
    dat <- dat[logiset[,1]==TRUE]
    
    if (sum(logiset[,1])!=num.units & length(unique(sg.var))==1 & is.null(grpbyName)) {
      #the table being constructed is for a logiset specified by a logisetexpr in tableBuilder
      #there is not other subgrouping - so the table looks non-subgrouped, but it is effectively for a subgroup
      grping.logical <- FALSE
    } 
    
    mx <- normal.theory.CI.proportion(dat, sg.var, mx, grpbyName, cat.adjustments, grping.logical)
  } else if (statistic=="mean") {
    #subset the data variable by the logiset
    dat <- dat[logiset[,1]==TRUE]
    
    mx <- normal.theory.CI.mean(dat, sg.var, mx)
  }
  
  return(mx)
}

#' Called within normal.theory.CIs() to calculates a 95% confidence interval for proportions 
#' using assymptotic normal theory and replaces the first row of mx with it.  Called if 
#' statistic in normal.theory.CIs() is "proportion"
#' 
#' @param dat
#'  The data vector from which to calculate the confidence intervals
#' 
#' @param sg.var
#'  The variable defining the subgroup.  If there is no subgroup then this is a vector of 1s. 
#' 
#' @param mx
#' The matrix of collated results with confidence intervals.  The first row of this matrix will
#' be replaced.
#' 
#' #' @param grpbyName
#' variable name of a grouping variable
#' 
#' #' @param cat.adjustments
#' Used to determine whether "Not in subgroup"/"In subgroup" labels need to be used (used when
#' this function is being called in collate_all_run_results() and tables by subgroup are
#' being collated.
#' 
#' #' @param grping.logical
#' Takes the value TRUE or FALSE to indicate whether confidence intervals should be done 
#' separately for different subgroups.
#' 
#' @return
#'  A matrix of the same dimension as mx with the first row replaced with assymnptotic normal 
#' theory 95% confidence intervals. 
#'
#' @export 
normal.theory.CI.proportion <- function(dat, sg.var, mx, grpbyName, cat.adjustments, grping.logical) {
  n <- apply(table(dat, sg.var), COL, sum) 
  grpsums <- rep(n, each=length(table(dat)))
  p <- table(dat, sg.var)/grpsums
  se <- sqrt(p*(1-p)/grpsums)
  upper <- p + qnorm(.975)*se
  lower <- p - qnorm(.975)*se
  new.CI <- NULL
  for (i in 1: length(se)) {
    new.CI <- c(new.CI, p[i], lower[i], upper[i])
  }
  new.CI[new.CI<0] <- 0
  new.CI[new.CI>1] <- 1
  
  #above CI has CIs for all possible categories (gives 0 (0, 0) if there were no occurences in a 
  #category.  But mx only shows CIs for the categories that were present.  Hence the length of
  #new.CI and mx[1,] may not be the same.  Below code works out which columns of new.CI to use in 
  #replacing mx[1,]
  names.num <- row.names(table(dat, sg.var))
  names.num <- rep(names.num, length(table(sg.var)))
  subgroupnames.num <- colnames(table(dat, sg.var))
  subgroupnames.num <- rep(subgroupnames.num, each=length(table(dat)))
  varname <- attr(mx, "meta")["varname"]
  if (is.null(varname)) {
    varname <- attr(mx, "varname")
  }
  names.words <- cmatch(dict, names.num, varname)
  subgroupnames.words <- cmatch(dict, subgroupnames.num, grpbyName)
  if (sum(subgroupnames.words=="1")==length(subgroupnames.words)) {
    #no subgrouping
    subgroupnames.words <- rep("", length(subgroupnames.words))
  }
  #below if clause means that if a grpbyName has been specified in tableBuilder then the results will be for everyone grouped by the grpbyName variable
  #(that is, not a table by the grpby variable just for those in the user-sepcified subgroup if one has been set)
  #To enable a choice for the user between displaying results for everyone or just for the user-specified subgroup,
  # this function would need to be re-written slightly with another parameter in the tableBuilder function
  # and some code at the beginning of tableBuilder to have NULL logiset or to have the user-specified logiset.
  #There would also likely need to be more changes than that.
  if ((!is.null(attr(cat.adjustments[[1]], "logisetexpr"))) & (grping.logical==TRUE) & (is.null(grpbyName))) {
    subgroupnames.words <- c("Not in subgroup", "In subgroup")
    subgroupnames.words <- rep(subgroupnames.words, each=length(table(dat)))
  }
  names.mean <- names2 <- character(length(names.num))
  present <- rep(NA, length(names.num))
  
  for (i in 1:length(table(dat, sg.var))) {
    if (sum(grepl("%) Mean", colnames(mx)))>=1) {
      if (subgroupnames.words[i]=="") {
        names.mean[i] <- paste(names.words[i], "(%) Mean")
      } else {
        names.mean[i] <- paste(subgroupnames.words[i], names.words[i], "(%) Mean")	
      }
      present[i] <- names.mean[i]%in%colnames(mx)
    } else if (sum(grepl("Mean", colnames(mx)))>=1) {
      if (subgroupnames.words[i]=="") {
        names.mean[i] <- paste(names.words[i], "Mean")
      } else {
        names.mean[i] <- paste(subgroupnames.words[i], names.words[i], "Mean")
      }
      present[i] <- names.mean[i]%in%colnames(mx)
    } else {
      if (subgroupnames.words[i]=="") {
        names2[i] <- names.words[i]
      } else {
        names2[i] <- paste(subgroupnames.words[i], names.words[i])
      }
      present[i] <- names2[i]%in%colnames(mx)
    }
  }
  
  present <- rep(present, each=3)
  if (sum(present)==0) {
    mx[1,] <- rep(0, 3)
  } else {
    mx[1,] <- new.CI[present]*100
  }
  return(mx)
}

#' Called within normal.theory.CIs() to calculates a 95% confidence interval for proportions 
#' using assymptotic normal theory and replaces the first row of mx with it.  Called if 
#' statistic in normal.theory.CIs() is "mean".
#' 
#' @param dat
#'  The data vector from which to calculate the confidence intervals
#' 
#' @param sg.var
#'  The variable defining the subgroup.  If there is no subgroup then this is a vector of 1s. 
#' 
#' @param mx
#' The matrix of collated results with confidence intervals.  The first row of this matrix will
#' be replaced.
#' 
#' @return
#'  A matrix of the same dimension as mx with the first row replaced with assymnptotic normal 
#' theory 95% confidence intervals. 
#'
#' @export 
normal.theory.CI.mean <- function(dat, sg.var, mx) {
  m <- tapply(dat, sg.var, mean)
  n <- table(sg.var)
  sd <- tapply(dat, sg.var, sd)
  se <- sd/sqrt(n)
  upper <- m + qt(.975, n-1)*se
  lower <- m - qt(.975, n-1)*se
  new.CI <- NULL
  for (i in 1:length(se)) {
    new.CI <- c(new.CI, m[i], lower[i], upper[i])
  }
  mx[1,] <- new.CI
  return(mx)
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
