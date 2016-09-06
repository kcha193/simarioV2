


#' Create new simenv object
#' Simenv object - a simulation environment.
#'
#' A simulation environment contains everything required to perform a simulation. Typically 1 Simenv will be created 
#' and used to run a base simulation, and additional Simenvs will be created to test different scenarios.
#'
#' A Simenv consists of a:
#' - a simframe (possibly with adjustments to test a scenario)  
#' - one or more simulation modules (Simmodule). A Simmodule contains outcomes, run stats, and runs.averaged for a simulation
#'   as well as the code to generate them.
#' 
#' Uses the global environment list variable "propensities" when performing categorical adjustment  
#' 
#' This class will be subclassed by specific simulation problems which will provide their own simframe,
#' Simmodules and adjustments.  
#'  
#' @param name
#'  simulation name
#' 
#' @param simframe
#'  simframe
#' 
#' @param dict
#'  a Dictionary object
#' 
#' @param cat.adjustments
#' Categorical variable adjustment matrices.
#' 
#' Each element is an adjustment matrix:
#' 
#'            Non-smoker (%) Smoker (%)
#'     Year 1             NA         NA
#'     Year 2             NA         NA
#' attr(,"varnames")
#' [1] "z1msmokeLvl0" "z1msmokeLvl1"
#' 
#' The values in the first row are used to make adjustments before the simulation begins.
#' Values in subsequent rows can be used during the simulation to set the required proportion
#' during the specified iteration (eg: iteration 2 if a value is specified in Year 2).
#' The variables in the simframe to adjust are specified by the varnames attribute.
#' 
#' @param cont.adjustments
#' A list of time-variant continuous variable adjustment matrices.
#' 
#' Each element is an adjustment matrix with number of rows equal to the number of micro units
#' amd number of columns equal to the number of iterations plus 1 (for the presimulation adjustments).
#' 
#' The user specifies from the user interface desired increments (or decrements) for all micro units
#' in particular categories (e.g. decrease the number of cigarettes smoked per day by 20 for every 
#' child with a mother who smokes 40 or more cigarettes a day), these adjustments are made to the 
#' simulated data from the base simulation and results stored in these matrices.  At each year in the 
#' simulation these cont.adjustment matrices are checked and, if they contain values, they are used
#' instead of the simulated values at that year. 
#' 
#' @param modules
#'  the list of Simmodules for this Simenv
#' 
#' @return 
#'   
#' @export
#'  
createSimenv <- function (name, simframe, dict, modulesName, cat.adjustments=list(), modules=list()) {
  
  
  NUM_ITERATIONS <- NUM_ITERATIONS
  
  cat.adjustments <- 	createEmptyCatAdjustments(simframe, dict, numiterations=NUM_ITERATIONS)
  
  
  modules <- createSimmodule(modulesName)
  
  names(modules[1]) = modulesName
  
  list(name=name,
       num_runs_simulated = 0L,
       simframe=simframe,
       cat.adjustments=cat.adjustments,
       modules=modules,
       dict=dict
  )
}
#' Create empty categorical variable adjustment matrices.
#'   
#' @export
createEmptyCatAdjustments <- function(simframe, dict, numiterations = NUM_ITERATIONS) {
  
  catvars <- getOutcomeVars(simframe, "categorical")
  #catvars <- catvars[!names(catvars) %in% c("typeofchange", "sptype", "typnode")]	#remove
  
  # create per iteration cat adj matrices
  cat.adjustments <- createAdjustmentMatrices(catvars, dict, numiterations)
  #above line creates cat.adjustments for time-variant categorical variables 
  #(those with the Outcome_type specified as categorical in simframedef.csv
  #for MELC these variables are: alcabuse, depression, z1single, z1chpar, welfare.
  #z1accom, z1homeown, z1overcrowd, mumgroup, dadgroup, and z1cond
  
  # create first year only cat adjs
  
  time_invariant_vars <- attr(simframe, "time_invariant_vars")
  
  if(!is.null(time_invariant_vars)){
    for(i in 1:length(time_invariant_vars$Varname)){
      
      
      if(time_invariant_vars[i,2] =="categorical"){
        cat.adjustments$" " <-       
          createAdjustmentMatrix(time_invariant_vars[i,1], dict$codings[[time_invariant_vars[i,1]]], 
                               dict$descriptions[[time_invariant_vars[i,1]]], is_a_level_var=TRUE) 
      } else {
        cat.adjustments$" " <-       
          createAdjustmentMatrix(time_invariant_vars[i,1], binbreaks[[time_invariant_vars[i,1]]][-1], 
                                 dict$descriptions[[time_invariant_vars[i,1]]], is_a_level_var=FALSE,
                                 cont.binbreaks=binbreaks[[time_invariant_vars[i,1]]], 
                                 catToContModels=catToContModels[[time_invariant_vars[i,1]]]) 
      }
      
      names(cat.adjustments)[length(cat.adjustments)] <- time_invariant_vars[i,1]
    }
  }
  
  catcontvar<- names(binbreaks)[!names(binbreaks) %in% time_invariant_vars$Varname]
  
    #create continuous variable cat.adjustments
  for(i in catcontvar){
    cat.adjustments$" " <- 
      createAdjustmentMatrix(i, binbreaks[[i]][-1], numiterations, is_a_level_var=FALSE, 
                             cont.binbreaks=binbreaks[[i]], catToContModels=catToContModels[[i]])
  
    names(cat.adjustments)[length(cat.adjustments)] <- i 
  }
    
 
  cat.adjustments 
}


#' Apply categorical adjustments to simframe.
#' 
#' @param Simenv
#'  simenv receiving object. Simenv$simframe is modified.  
#' @param iteration
#' iteration number - corresponds to a row number in the matrix elements of the cat.adjustments list
#' @param propensities
#' 		named list of propensities for the cat.adjustments
#' @param printAdj
#' 		if TRUE will print new proportions of modified simframe vars
#' @param cat.adjustments
#' 	a list of categorical adjustment matrices whose rows each correpond to desired adjustments for an iteration.
#' 	Each matrix has a 'varname' attribute, indicating which variable in the simenv object is to be adjusted.
#' 	Each matrix may also have a 'logisetexpr' attribute - if so, this is evaluated and becomes a logical vector indicating which observations
#'  of the 'varname' variable to adjust (i.e. the "logisetexpr" attribute gives which subset of the data the row of adjustments are intended for).
#'
#' @return 
#'  NULL. simframe in receiving object is modified directly.
#'   
#' @export
applyAllCatAdjustmentsToSimframe <- function(Simenv, iteration, propensities=NULL, print_adj=TRUE, 
                                             cat.adjustments=Simenv$cat.adjustments) {
  
  contvars <- getOutcomeVars(Simenv$simframe, "continuous")

  temp <- 
  lapply(cat.adjustments, function (catadj) {
    cat_adj_vector <- catadj[iteration, ]	
    varnames <- attr(catadj,"varnames")
    varname <- varnames[length(varnames)]
    varname <- strip_lvl_suffix(varname)
  
    if (varname %in% contvars) {
      cat_adj_vector <- structure(cat_adj_vector, varname=varname, 
                                  logisetexpr=attr(catadj,"logisetexpr"), 
                                  levels=names(binbreaks[[varname]])[-1])
    }else{
      cat_adj_vector <- structure(cat_adj_vector, varname=varname, 
                                  logisetexpr=attr(catadj,"logisetexpr"),
                                  levels=Simenv$dict$codings[[varname]])
    }
    
    if (!any(is.na(cat_adj_vector))) {
      
      catToContModels <- attr(catadj, "catToContModel")
      cont.binbreaks <- attr(catadj, "cont.binbreaks")					
      
      if (is.null(varnames)) {
        stop(gettextf("Missing varnames attribute"))
      }
      
      if (!is.null(catToContModels)) {
         applyContAdjustmentToSimframe(Simenv, varnames, 
                                      iteration, cat_adj_vector, 
                                      catToContModels, cont.binbreaks, propensities)
      } else {
        applyCatAdjustmentToSimframe(Simenv, varnames, cat_adj_vector, iteration, propensities, print_adj)
      }
    }
    
  })
  
  for(i in 1:length(temp)){
    if(is.null(temp[[i]]))  next
    
    Simenv$simframe[,match(names(temp[[i]]), names(Simenv$simframe))] <- temp[[i]]
    
    if(length(names(temp[[i]])) > 1)
      Simenv$simframe[,match(names(temp)[i], names(Simenv$simframe))] <- 
        as.numeric(as.matrix(temp[[i]]) %*% 
                     as.matrix(as.numeric(sapply(strsplit(names(temp[[i]]), "Lvl"), function(x) x[2]))))
  }
  
  return(Simenv)
}




applyAllFixedOutcomesIfSetToSimframe <- function(Simenv) {
  iteration <- 1
  
  lapply(names(Simenv$fixed.outcomes), function(fixedOutcomeName){
    #fixedOutcomeName <- "kids"
    Simenv$simframe[[fixedOutcomeName]] <- 
      selectFixedOutcomeIfSet(Simenv, iteration, Simenv$simframe[[fixedOutcomeName]], fixedOutcomeName)
    
  })
}


#' Apply categorical adjustments to simframe.
#' 
#' @param Simenv
#'  simenv receiving object. Simenv$simframe is modified.  
#' @param varnames
#'  varname(s) of variable(s) to adjust, eg: "catpregsmk2" or c("z1msmokeLvl0","z1msmokeLvl1")
#' @param desired_props
#'  a vector of desired proportions, eg: c(0.1, 0.1, 0.8).
#'  Can have a "logisetexpr" attribute - if so, this is evaulated, and becomes a logical vector indicating which observations of "varname" to adjust.
#' 		(i.e. the "logisetexpr" attribute gives which subset of the data the desired_props are intended for).
#' @param iteration
#'  the current iteration
#' @param propensities
#' 		named list of propensities for the cat.adjustments
#' @param printAdj
#' 		if TRUE will print new proportions of modified simframe vars
#'
#' @return 
#'  NULL. simframe in receiving object is modified directly.
#' 
#' @export
applyCatAdjustmentToSimframe <- function(Simenv, varnames, desired_props, iteration, 
                                         propensities, print_adj = TRUE) {
  
  is_single_variable_to_adjust <- length(varnames) == 1
  
  logiset <- as.logical(evaluateLogisetExprAttribute(desired_props, Simenv$simframe, varnames))
  
  if (is_single_variable_to_adjust) {
    propens <- propensities[[varnames]][,,iteration]
    applyCatAdjustmentToSimframeVarSingle(Simenv, varnames, desired_props, propens, print_adj, 
                                          logiset=logiset)
  } else {
    propens <- propensities[[strip_lvl_suffix(varnames[1])]][,,iteration]
    applyCatAdjustmentToSimframeVarMultipleBinary(Simenv, varnames, desired_props, propens,
                                                  print_adj,logiset=logiset)	
  }
}

#' Adjust the proportions of a single simframe variable.
#' 
#' @param Simenv
#'  simenv receiving object. Simenv$simframe is modified.
#' @param varname
#'  simframe variable to adjust  
#' @param desired_props
#'  a vector of desired proportions, eg: c(0.1, 0.1, 0.8)
#' @param propens
#'  propensities for this variable, if any
#' @param printAdj
#'  if TRUE, display adjusted proportions after adjustment
#' @param logiset
#' 	logical vector indicating which rows to include, or NULL (the default) to include all. 
#' 
#' @return 
#'  NULL. simframe in receiving object is modified directly.
#' 
#' @export
applyCatAdjustmentToSimframeVarSingle <- function(Simenv, varname, desired_props, propens, print_adj = T, logiset=NULL) {
  
  if (print_adj) {
    if(is.null(logiset) || sum(logiset) == 0) {
      cat(varname,"\n")
    } else {
      cat(varname,"- just for the logiset subset: ", "\n")
    }
  }
  
  if (!is.null(logiset) && sum(logiset) > 0) {
    Simenv$simframe[varname]<-modifypropsVarSingle_on_subset(default.vec=Simenv$simframe[varname], 
                                                             desired_props=desired_props, propens=propens,
                                                             logiset=logiset)
  }
  else {
    Simenv$simframe[varname] <- modifyProps(Simenv$simframe[[varname]], desired_props, propens)
  }
  
  if (print_adj) {
    
    if (is.null(logiset) || sum(logiset) == 0) {print(prop.table(table(Simenv$simframe[varname])), digits=3)}
    else {print(prop.table(table(subset(Simenv$simframe[varname],logiset))), digits=3)}
    
  }
  Simenv$simframe[varname]
}


#' Adjust the proportions of a simframe variable that exists in multiple binary level vectors,
#' eg: SESBTHLvl1, SESBTHLvl2, SESBTHLvl3.
#' 
#' @param Simenv
#'  simenv receiving object. Simenv$simframe is modified.  
#' @param binLevelVarnames
#'  vector of binary level varnames, eg: c("z1accomLvl0","z1accomLvl1")
#' @param desiredProps
#'  desired proportions
#' @param propens
#'  propensities, if ANY
#' @param printAdj
#'  if TRUE, display adjusted proportions after adjustment
#' @param logiset
#' 	logical vector indicating which rows to include, or NULL (the default) to include all. 
#'
#' @return 
#'  NULL. simframe in receiving object is modified directly.
#'  
#' @export
applyCatAdjustmentToSimframeVarMultipleBinary <- function (Simenv, binLevelVarnames, desiredProps, 
                                                           propens, printAdj = TRUE, logiset=NULL) {
  
  #NB: simframe may not always contain Lvl0 var. So we construct one if this is 2 level var.
  is2Level <- length(binLevelVarnames) == 2
  varnames <- intersect(binLevelVarnames, names(Simenv$simframe))
  missingLevel <- setdiff(binLevelVarnames, names(Simenv$simframe))
  
  vecs.list <- Simenv$simframe[varnames]
  
  if(is2Level && length(missingLevel)) {
    # add in generated missing level
    vecs.list[missingLevel] <- as.integer(!Simenv$simframe[varnames])  
    
    # order correctly
    vecs.list <- vecs.list[binLevelVarnames] 
  }
  
  if (!is.null(logiset) && sum(logiset) > 0) {
    #subsetting the propensities according to logiset
    propens <- subset(propens, logiset)
    
    #adding a temporary ID variable - a rank column - onto a copy of the simframe portion
    #will enable the subsets to be put back into the same order later
    n <- dim(vecs.list)[1]
    sf <- data.frame(vecs.list,1:n)
    rankcolnum <- ncol(sf) 
    
    
    #subsetting the copy of the simframe according to logiset
    subset_to_change <- subset(sf,logiset)
    
    #keeping those not in the logiset - those that aren't to be passed to modifyprops
    rest_not_to_be_modified <- subset(sf,!logiset)
    
    #modifying the logiset
    subset_to_change_modified <- modifyPropsAsBinLevels(
      as.list(subset_to_change[,-rankcolnum]), 
      desiredProps=desiredProps, 
      propens=propens)
    
    #putting changed set back with those that weren't in the logiset
    new_sf <- rbind(subset_to_change_modified, rest_not_to_be_modified[,-rankcolnum]) 
    
    original.position <- rbind(as.matrix(subset_to_change[,rankcolnum]), as.matrix(rest_not_to_be_modified[,rankcolnum]))
    
    #putting the records back in their orignal order according to the rank column created earlier
    if (length(varnames)==length(binLevelVarnames)) {
      Simenv$simframe[varnames] <- new_sf[order(original.position),]
    } else if ((length(varnames)!=length(binLevelVarnames)) & is2Level) {
      Simenv$simframe[varnames] <- new_sf[order(original.position),2]
    } else {
      stop("add new if clause in applyCatAdjustmentToSimframeVarMultipleBinary()")
    }
  } else {
    #if there is no logiset and the scenario is being applied to everyone
    result <- modifyPropsAsBinLevels(
      vecs.list, 
      desiredProps=desiredProps, 
      propens=propens)
    
      Simenv$simframe[varnames] <- result[varnames] 
  }

  if (printAdj) {
    
    if (is.null(logiset) || sum(logiset) == 0) {
      print(apply(Simenv$simframe[varnames], COL, sum) / apply(Simenv$simframe[varnames], COL, length), digits=3)
      cat("\n")
    } else {
      cat("Just for the logiset subset: ", "\n")
      print(apply(subset(Simenv$simframe[varnames], logiset), COL, sum) / apply(subset(Simenv$simframe[varnames], logiset), COL, length), digits=3)
      cat("\n")
    }
    
    Simenv$simframe[varnames]
  }
  
  
}



#' Apply continuous adjustments to simframe.
#' 
#' @param Simenv
#'  simenv receiving object. Simenv$simframe is modified.  
#' 
#' @param varnames
#'  varname(s) of variable(s) to adjust.
#' 
#' @param iteration'
#'  the current iteration
#' 
#' @param desired_props
#'  a vector of desired proportions, eg: c(0.1, 0.1, 0.8).
#'  Can have a "logisetexpr" attribute - if so, this is evaulated, and becomes a logical vector indicating which observations of "varname" to adjust.
#' 		(i.e. the "logisetexpr" attribute gives which subset of the data the desired_props are intended for).
#' 
#' @param catToContModels
#'  A list of models which will to used to convert the adjusted categorical variable back 
#'  to continuous.
#' 
#' @param cont.binbreaks
#'  Binbreaks for the variable being adjusted if exist.
#' 
#' @param propensities
#' 	named list of propensities for the cat.adjustments
#'
#' @return 
#'  NULL. simframe in receiving object is modified directly.
#' 
#' @export
applyContAdjustmentToSimframe <- function(Simenv, varname, iteration, desiredProps, catToContModels, cont.binbreaks, propensities) {
  propens <- propensities[[varname]][,,iteration]
  logiset <- as.logical(evaluateLogisetExprAttribute(desiredProps, Simenv$simframe))
  cat("Adjusting", varname, ": ", desiredProps, "\n")
  Simenv$simframe[varname] <- adjust.proportions(Simenv$simframe[[varname]], desiredProps, propens, logiset, catToContModels, cont.binbreaks, envir=Simenv$simframe)
  
  
  Simenv$simframe[varname] 
}


#' Check number of valid subgroup while applying cat.adjustment
#' 
#' @param Simenv
#'  simenv receiving object. Simenv$simframe is modified.  
#' 
#' @return 
#'  numeric number. 
#' 
#' @export
check.subgroup.expr <- function(Simenv) {
  
  cat.adjustments <- Simenv$cat.adjustments
  simframe <- Simenv$simframe
  
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

  
#' Perform a simulation of X runs using parallel computing.
#' 
#' NB: if it exists, uses propensities in global environment when doing adjustments for year 1
#'
#'   
#' @param Simenv
#'  Simenv receiving object
#' @param total_runs
#'  total number of runs to simulate
#' @param simulateFun  
#'  a function contains a set of actual simulation to be performed   
#' @param parallel
#'  logical, which allows the user to decide on using parallel computing
#'   
#' @return 
#'  Simenv object with simulated results
#' 
#' @export
simulateSimario <- function(Simenv, total_runs=1, simulateFun, parallel = TRUE) {
  start_time <- proc.time()
  
  cat(gettextf("Simulating %s\n", Simenv$name))
  
  if (!exists("propensities")) propensities <- NULL
  
  valid.subgroup <- check.subgroup.expr(Simenv)
  
  if (valid.subgroup==1) {
    Simenv <- applyAllCatAdjustmentsToSimframe(Simenv, 1, propensities)
    
  } else if (valid.subgroup==0) {
    cat("Pre-simulation scenario adjustments cannot be made because the subgroup expression is not defined \n")
  } else {
    stop("Check creation of valid.subgroup \n")
  }
  
  #at this point after adjusting continuous variables some values may be higher than 
  #the limits set throughout the simulation - can fix here (rather than changing
  #more deep down simario functions)
  if (exists("limits")) {
    for (j in 1:length(limits)) {
      v <- Simenv$simframe[[names(limits)[j]]]
      Simenv$simframe[[names(limits)[j]]][v>limits[[j]]] <- limits[[j]]
    }
  }
 
  Simenv$num_runs_simulated <- total_runs
  
  memSimulateFun <- memoise(simulateFun)
  
  if(parallel){
    
    library(parallel)
    
    cl <- makeCluster(detectCores())
    
    clusterExport(cl, c("binbreaks", "transition_probabilities", "models", 
                        "PropensityModels", "children"))
    
    clusterEvalQ(cl, {library(simarioV2)})
    clusterSetRNGStream(cl, 1)
  
    outcomes <-parLapply(cl, 1:total_runs, simulateRun, simenv=Simenv, simulateFun = memSimulateFun)
    
    stopCluster(cl)
    
  } else {
    outcomes <-lapply(1:total_runs, simulateRun, simenv=Simenv, simulateFun = memSimulateFun)
  }
  
  forget(memSimulateFun)
  
  Simenv$modules$run_results <- outcomes
  
  names(Simenv$modules$run_results) <- paste("run", 1:total_runs, sep="")
  
  end_time <- proc.time()
  
  print(end_time - start_time)
  
  return(Simenv)
}

numberOfUnits <- function(Simenv) {
  dim(Simenv$simframe)[1]
}

