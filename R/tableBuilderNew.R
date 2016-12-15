#' Returns a dataset for use in a table by the Table Builder GUI.
#' 
#' Currently cannot do freqs for final outcomes (can only do freqs for those vars with binbreaks)
#' Also currently can only use a true categorical variable as a groupby variable - this coould be 
#' changed. 
#' 
#' If the user defined a logiset/subgroup expression inthe scenario weightings screen and then
#' goes to tableBuilder() and sets a grpbyName, then the results they get will be on the entire 
#' dataset, not just on their subgroup they defined earlier.  The user can not define a logiset
#' expression in tableBuilder - the logisetexpr parameter is there so it can be used to show the 
#' user, in the scenario weightings screen, the distributions of the variable of interest for 
#' their subgroup only so they can better choose the proportions for their subgroup scenario. 
#'   
#' @param envName 
#'  the environment to use - Base, Scenario etc.
#' 
#' @param statistic
#'  the summary measure to use in producing the dataset - frequencies, means, quantiles
#' 
#' @param variableName
#'  the variable to use in producing the dataset
#'  
#' @param grpbyName
#'  a subgroup by which to examine the variable
#' 
#' @param CI
#'  logical indicating whether 95\% confidence intervals should be generated
#'  
#' @param dict
#'  Dictionary object.
#'  
#' @param logisetexpr
#'  a character expression which defines the logiset variable
#'  
#' @param binbreaks
#'  The binbreaks for the outcome variable. 
#'  
#' @param env.base
#'  Base simulation results
#' 
#' @param basePop
#' logical which to allow the users to used the base population  
#'  
#' @param digits
#' integer indicating the number of decimal places  
#' 
#' @return 
#'  a summary table for the entire or subgroup of the variable of interest.
#'   
#' @export
#' 
#' 
tableBuilderNew <- 
  function (env, statistic = c("frequencies", "means", "quantiles"), variableName, 
            dict = env$dict, grpbyName = "", CI = TRUE, logisetexpr = "", envBase = NULL,
            basePop = FALSE, digits = 1){
    
    library(dplyr)
    library(tidyr)
    
    if(logisetexpr == "")  logisetexpr <- NULL
    
    if(grpbyName == "")  grpbyName <- NULL
    
    statistic <- match.arg(statistic)
    
    nRun <- as.numeric(env$num_runs_simulated)

    if(!is.null(envBase)){
      
      combineSimario <-
        function(base, scenario, index){
          for(i in 1:length(scenario))
            scenario[[i]]<-  c(base[[i]][index], scenario[[i]])
          
          scenario
        }
      
      
      if(basePop){
        env$simframe <- envBase$simframe
        
        env$modules$run_results <- 
          lapply(env$modules$run_results, function(x) x[variableName])
      } 
      
      index <-
        !names(envBase$modules$run_results$run1) %in%
        names(env$modules$run_results$run1)
      
      env$modules$run_results <-
        combineSimario(envBase$modules$run_results,
                       env$modules$run_results, index)
    }
    
    #Time variant variables
    timeVar <- names(env$modules$run_results$run1)
    conVar <- names(binbreaks)
    
    if(variableName %in% timeVar ){
      simulatedDataFull <- 
        sapply(env$modules$run_results, 
               function(x) t(x[[variableName]]))
      
      if(statistic == "frequencies" & variableName %in% conVar)
        simulatedDataFull <- apply(simulatedDataFull,2, function(x) 
          as.numeric(bin(x, binbreaks[[variableName]])))
      
      simulatedData <- 
        tbl_df(data.frame(Year = as.numeric(colnames(env$modules$run_results$run1[[variableName]])), 
                          simulatedDataFull))
    
    }else{
      #Time invariant variables
      simulatedDataFull <- env$simframe[[variableName]]
      
      if(statistic == "frequencies" & variableName %in% conVar)
        simulatedDataFull <- as.numeric(bin(simulatedDataFull, binbreaks[[variableName]]))
      
      simulatedDataFull <- matrix(rep(simulatedDataFull, nRun), ncol =  nRun)
      
      colnames(simulatedDataFull) <- paste("run", 1:env$num_runs_simulated, sep = "")
      
      simulatedData <- 
        tbl_df(data.frame(Year = 1, simulatedDataFull))
    }
    
    simulatedData <- 
      simulatedData %>% gather(Run, Var, -Year) %>% filter(!is.na(Var))
    
    #########################################################################
    #Using logisetexpr and grpbyName
    if(!is.null(logisetexpr) | !is.null(grpbyName)){
      if(!is.null(logisetexpr)){
        grpbyName1 <- trimws(unlist(strsplit(logisetexpr,  "[[:punct:]]+")))
        
        grpbyName1 <- grpbyName1[grpbyName1!=""]
        
        grpbyName1 <- grpbyName1[seq(1,length(grpbyName1), 2)]
      }
      
      groupByDataAll <- NULL
      
      if(is.null(logisetexpr)){
        grpbyNameFull <- grpbyName
        
      } else if(is.null(grpbyName)){
        grpbyNameFull <-  grpbyName1
        
      } else {
        grpbyNameFull <- c(grpbyName, grpbyName1)
      }
      
      grpbyNameFull <- unique(grpbyNameFull)
      
      for(grpby in  grpbyNameFull){
      
        if(grpby %in% names(env$modules$run_results$run1) ){
          
          groupByDataFull <- sapply(env$modules$run_results, function(x) x[[grpby]])
          
          if(grpby %in% conVar)
            groupByDataFull <- apply(groupByDataFull,2, function(x) 
              as.numeric(bin(x, binbreaks[[grpby]])))
          
          groupByData <- 
            tbl_df(data.frame(Year = rep(as.numeric(colnames(env$modules$run_results$run1[[grpby]])), each = 5000), 
                              A0 = 1:5000, groupByDataFull))
          
          groupByData <- 
            groupByData %>% gather(Run, groupByData, -Year, -A0)  %>%   filter(!is.na(groupByData))
          
        } else{
          
          groupByDataFull <- env$simframe[[grpby]]
          
          if(grpby %in% conVar)
            groupByDataFull <- as.numeric(bin(groupByDataFull, 
                                              binbreaks[[grpby]]))

          groupByData <- 
            tbl_df(data.frame(Year = rep(1:21, each = 5000), A0 = 1:5000, 
                              groupByData = groupByDataFull))
        }
        
        names(groupByData)[names(groupByData)=="groupByData"] <- grpby
        
        if(is.null(groupByDataAll))
          groupByDataAll <- groupByData
        else 
          groupByDataAll <- groupByDataAll %>% left_join(groupByData)
      }
      
      groupByData <- groupByDataAll
      
      if(variableName %in% timeVar ){
        simulatedData <- 
          tbl_df(data.frame(Year = as.numeric(colnames(env$modules$run_results$run1[[variableName]])), 
                            A0 = rep(1:5000, each = 
                                       length(as.numeric(colnames(env$modules$run_results$run1[[variableName]])))) ,
                                     simulatedDataFull))
      }else {
        simulatedData <- 
          tbl_df(data.frame(Year = 1, A0 = 1:5000, simulatedDataFull))
      }
      
      simulatedData <- 
        simulatedData %>% gather(Run, Var, -Year, -A0) %>% left_join(groupByData) %>% select(-A0)
      
      
      if(!is.null(logisetexpr)){
        simulatedData <- 
          with(simulatedData, simulatedData[eval(parse(text=logisetexpr)),])
        
        simulatedData <-simulatedData %>% select(-one_of(grpbyName1))
      }
      
      simulatedData <- 
        simulatedData %>% filter(!is.na(Var))
    }

  
    ####################################################################################
    
    if(statistic == "means") {
      
      if(!is.null(grpbyName))  {
        
        names(simulatedData)[names(simulatedData)== grpbyName] <- "groupByData"
        
        result <- 
          simulatedData %>% group_by(Year, groupByData, Run) %>% 
          summarise(Var = mean(Var)) %>% ungroup() %>% 
          group_by(groupByData, Year) %>% 
          summarise(Mean = mean(Var), 
                    Lower = quantile(Var, c(0.025)), 
                    Upper = quantile(Var, c(0.975))) %>% data.frame()
        
        yr <- unique(result$Year[apply(result, 1, 
              function(x) x["Mean"] == x["Lower"] & x["Mean"] == x["Upper"])])
        
        for(i in yr){
          
          simulatedDataSum <- 
            simulatedData %>% group_by(Year, groupByData, Run)  %>%  summarise(Sum = n()) %>% 
            group_by(Year, groupByData) %>% filter(Year ==i) %>% summarise(Sum = unique(Sum))
          
          n <- simulatedDataSum[,"Sum"] %>% unlist()
          
          SD <- simulatedData %>% group_by(Year, groupByData, Run) %>% 
            summarise(SD = sd(Var)) %>% 
            filter(Year ==i) %>%
            group_by(Year, groupByData) %>% 
            summarise(SD = unique(SD))%>% 
            select(SD) 
          
          SD <- SD[,"SD"] %>% unlist()
          
          m <- result %>% filter(Year ==i)  %>% select(Mean) %>% unlist()
          
          result[result$Year == i, c("Mean", "Lower", "Upper")] <- 
            t(sapply(1:length(n), function(x) 
              c(m[x],   m[x] - qt(.975, n[x]-1)*SD[x]/sqrt(n[x]), 
                m[x] + qt(.975, n[x]-1)*SD[x]/sqrt(n[x]))))
        }
        
        result$groupByData <-
          if(grpbyName %in% conVar)  
            names(binbreaks[[grpbyName]])[-1][result$groupByData]
        else 
          names(env$dict$codings[[grpbyName]])[
            match(result$groupByData, env$dict$codings[[grpbyName]])]

        
      } else {
        
        result <- 
          simulatedData %>% group_by(Year, Run) %>% 
          summarise(Var = mean(Var)) %>% ungroup() %>% 
          group_by(Year) %>% 
          summarise(Mean = mean(Var), 
                    Lower = quantile(Var, c(0.025)), 
                    Upper = quantile(Var, c(0.975))) %>% data.frame()
        
        yr <- unique(result$Year[apply(result, 1, 
               function(x) x["Mean"] == x["Lower"] & x["Mean"] == x["Upper"])])
        
        for(i in yr){
          
          simulatedDataSum <- 
            simulatedData %>% filter(Year == i)  %>%   
            group_by(Run) %>% 
            summarise(Sum = n()) %>% 
            summarise(unique(Sum)) 
          
          n <- simulatedDataSum%>% as.numeric()
          
          SD <- simulatedData %>% group_by(Year, Run) %>% 
            summarise(SD = sd(Var)) %>% 
            filter(Year ==i) %>%
            group_by(Year) %>% 
            summarise(Sum = unique(SD)) %>% 
            as.numeric()
          
          m <- result[result$Year == i,] %>% as.numeric()
          
          result[result$Year == i, c("Mean", "Lower", "Upper")] <- 
            c(m[2],  m[2] - qt(.975, n-1)*SD[2]/sqrt(n), 
              m[2] + qt(.975, n-1)*SD[2]/sqrt(n))
        }
      }
      
      result[,c("Mean", "Lower", "Upper")] <-  round(result[,c("Mean", "Lower", "Upper")], digits = digits)
      
    } else if (statistic == "frequencies"){
      
      if(!is.null(grpbyName))  {
        
        names(simulatedData)[names(simulatedData)== grpbyName] <- "groupByData"
        
        simulatedDataSum <- 
          simulatedData %>% group_by(Year, groupByData, Run) %>% 
          summarise(Sum = n()) 
        
        result <- 
          simulatedData %>% group_by(Year, groupByData, Run, Var) %>%  
          summarise(Len = n()) %>% 
          left_join(simulatedDataSum) %>%
          mutate(Prop = Len/Sum) %>%
          select(-Len, -Sum) %>%
          group_by(Var, groupByData, Year) %>% 
          summarise(Mean = mean(Prop), 
                    Lower = quantile(Prop, c(0.025)),
                    Upper = quantile(Prop, c(0.975))) %>% data.frame() 
        
        yr <- unique(result$Year[apply(result, 1, 
              function(x) x["Mean"] == x["Lower"] & x["Mean"] == x["Upper"])])
        
        for(i in yr){
          
          simulatedDataSum <- 
            simulatedData %>% group_by(Year, groupByData, Run)  %>%  
            summarise(Sum = n()) %>% 
            group_by(Year, groupByData) %>% 
            filter(Year ==i) %>% summarise(Sum = unique(Sum))
          
          n <- simulatedDataSum[,"Sum"] %>% unlist()
          
          n <- rep(n, length(unique(result$Var)))
          
          p <- result %>% filter(Year ==i)  %>% select(Mean) %>% unlist()
          
          result[result$Year == i, c("Mean", "Lower", "Upper")] <- 
            t(sapply(1:length(n), 
                     function(x)  c(p[x],  p[x] - qnorm(.975)* sqrt(p[x]*(1-p[x])/n[x]),
                                    p[x] + qnorm(.975)* sqrt(p[x]*(1-p[x])/n[x]))))
        }
        
        result$groupByData <-
         if(grpbyName %in% conVar)  
          names(binbreaks[[grpbyName]])[-1][result$groupByData]
        else 
          names(env$dict$codings[[grpbyName]])[
            match(result$groupByData, env$dict$codings[[grpbyName]])]
        
      } else {
        
        simulatedDataSum <- 
          simulatedData %>% group_by(Year, Run) %>% 
          summarise(Sum = n()) 
            
        result <- 
          simulatedData %>% group_by(Year, Run, Var) %>%  
          summarise(Len = n()) %>% 
          left_join(simulatedDataSum) %>%
          mutate(Prop = Len/Sum) %>%
          select(-Len, -Sum) %>%
          group_by(Var, Year) %>% 
          summarise(Mean = mean(Prop), 
                    Lower = quantile(Prop, c(0.025)),
                    Upper = quantile(Prop, c(0.975))) %>% data.frame() 
        
        yr <- unique(result$Year[apply(result, 1, 
             function(x) x["Mean"] == x["Lower"] & x["Mean"] == x["Upper"])])   
        
        for(i in yr){
          simulatedDataSum <- 
            simulatedData %>% filter(Year == i)  %>%   
            group_by(Run) %>% 
            summarise(Sum = n()) %>% 
            summarise(unique(Sum)) 
          
          n <- simulatedDataSum%>% as.numeric()  
          
          
          result[result$Year == i, c("Mean", "Lower", "Upper")] <- 
            t(sapply(result[result$Year == i, "Mean"], 
                     function(p)  c(p,  p - qnorm(.975)* sqrt(p*(1-p)/n),
                                    p + qnorm(.975)* sqrt(p*(1-p)/n))))
        }
        
      }
      
     
      
      result$Var <-
        if(statistic == "frequencies" & variableName %in% conVar){  
          names(binbreaks[[variableName]])[-1][result$Var]
        }else {
          names(env$dict$codings[[variableName]])[
            match(result$Var,env$dict$codings[[variableName]])]
        }
      # names(result)[names(result)=="Var"] <- variableName
      
      
      result[,c("Mean", "Lower", "Upper")] <-  result[,c("Mean", "Lower", "Upper")]*100
      result[,c("Mean", "Lower", "Upper")] <-  round(result[,c("Mean", "Lower", "Upper")], digits = digits)
      
    } else if (statistic == "quantiles"){
     
      if(!is.null(grpbyName))  {
        
        names(simulatedData)[names(simulatedData)== grpbyName] <- "groupByData"
        
        result <- 
          simulatedData %>% group_by(Year, groupByData, Run) %>% 
          summarise(Min = quantile(Var, 0), 
                    "10th" = quantile(Var, 0.1),
                    "25th" =  quantile(Var, 0.25),
                    "50th" =quantile(Var, 0.5),
                    "75th" =quantile(Var, 0.75),
                    "90th" =quantile(Var, 0.9),
                    Max =quantile(Var, 1))%>% ungroup() %>% 
          group_by(groupByData, Year) %>% 
          summarise_each(funs(mean), -Run) %>% data.frame()
       
        result$groupByData <-
          names(env$dict$codings[[grpbyName]])[
            match( result$groupByData, env$dict$codings[[grpbyName]])]
       
            
      } else {
        
        result <- 
          simulatedData %>% group_by(Year, Run) %>% 
          summarise(Min = quantile(Var, 0), 
                    "10th" = quantile(Var, 0.1),
                    "25th" =  quantile(Var, 0.25),
                    "50th" =quantile(Var, 0.5),
                    "75th" =quantile(Var, 0.75),
                    "90th" =quantile(Var, 0.9),
                    Max =quantile(Var, 1))%>% ungroup() %>% 
          group_by(Year) %>% 
          summarise_each(funs(mean), -Run) %>% data.frame()

      }
      
      index <- c("Min", "X10th","X25th","X50th", "X75th","X90th", "Max")
      
      
      result[,index] <-  round(result[,index], digits = digits)
    }
    

    ageRange <- strsplit(dict$age[variableName], "--")[[1]]
    
    if(!all(is.na(suppressWarnings(as.numeric(ageRange))))){
      ageRange <- as.numeric(ageRange)
    
      if(length(ageRange) == 2)
        result <- result %>% filter(Year >= ageRange[1] & Year <= ageRange[2])
      else 
        result <- result %>% filter(Year == ageRange)
      
    } else {
      result$Year <- ageRange
      result <- result[1,]
    }
    
    
    return(result)
  }       
  


