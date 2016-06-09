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
#'  the summary measure to use in producing the dataset - frequencies, means, quintiles
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
#' @param logisetexpr
#'  a character expression which defines the logiset variable
#' 
#' @return 
#'  a summary table for the entire or subgroup of the variable of interest.
#'   
#' @export
#' 
#' 


tableBuilderNew <- 
  function (env, statistic, variableName, dict = env$dict, grpbyName = "", 
            CI = TRUE, logisetexpr = ""){
    
    if(logisetexpr == "")
      logisetexpr <- NULL
    
    if(grpbyName == "")
      grpbyName <- NULL
    
    
    #Time variant variables
    timeVar <- names(env$modules[[1]]$run_results$run1$outcomes)
    
    if(variableName %in% timeVar ){
      simulatedDataFull <- 
        sapply(env$modules[[1]]$run_results, 
               function(x) t(x$outcomes[[variableName]]))
      
      simulatedData <- 
        tbl_df(data.frame(Year = 1:21, simulatedDataFull))
      
    
    }else {
      simulatedDataFull <- env$simframe[[variableName]]
      
      simulatedDataFull <-matrix(rep(simulatedDataFull, env$num_runs_simulated), 
                                 ncol =  env$num_runs_simulated)
      
      colnames(simulatedDataFull) <- paste("run", 1:env$num_runs_simulated, sep = "")
      
      simulatedData <- 
        tbl_df(data.frame(Year = 1, simulatedDataFull))
      
      
    }
    
    simulatedData <- 
      simulatedData %>% gather(Run, Var, -Year) %>% filter(!is.na(Var))
    
    #########################################################################
    #Using logisetexpr
    
    
    if(!is.null(logisetexpr) | !is.null(grpbyName)){
      
      
      if(!is.null(logisetexpr)){
        grpbyName1 <- unlist(strsplit(logisetexpr, " [[:punct:]]+ "))
        
        grpbyName1 <- grpbyName1[seq(1,length(grpbyName1), 2)]
      }
      
      groupByDataAll <- NULL
      
      if(is.null(logisetexpr)){
        grpbyNameFull <- grpbyName
        
      } else if(!is.null(grpbyName)){
        grpbyNameFull <-  grpbyName1
        
      } else {
        grpbyNameFull <- c(grpbyName, grpbyName1)
      }
      
      for(grpby in  grpbyNameFull){
        if(grpby %in% names(env$modules[[1]]$run_results$run1$outcomes) ){
          
          groupByDataFull <- 
            sapply(env$modules[[1]]$run_results, 
                   function(x) t(x$outcomes[[grpby]]))
          
          groupByData <- 
            tbl_df(data.frame(Year = 1:21,  A0 = 1:5000, groupByDataFull))
          
          groupByData <- 
            groupByData %>% gather(Run, groupByData, -Year, -A0)  %>% 
            filter(!is.na(groupByData))
          
          names(groupByData)[names(groupByData)=="groupByData"] <- grpby
          
          if(is.null(groupByDataAll))
            groupByDataAll <- groupByData
          else 
            groupByDataAll <- groupByDataAll %>% right_join(groupByData)
          
        } else{
          groupByDataFull <- env$simframe[[grpby]]
          
          groupByData <- 
            tbl_df(data.frame(Year = rep(1:21, each = 5000), A0 = rep(1:5000,21), 
                              groupByData = rep(groupByDataFull,21)))
          
          names(groupByData)[names(groupByData)=="groupByData"] <- grpby
          
          if(is.null(groupByDataAll))
            groupByDataAll <- groupByData
          else 
            groupByDataAll <- groupByDataAll %>% left_join(groupByData)
        }
        
      }
      
      groupByData <- groupByDataAll
      
      
      
      if(variableName %in% timeVar ){
        simulatedData <- 
          tbl_df(data.frame(Year = 1:21, A0 = 1:5000,simulatedDataFull))
        
      }else {
        simulatedData <- 
          tbl_df(data.frame(Year = 1, A0 = 1:5000,simulatedDataFull))
      }
      
      simulatedData <- 
        simulatedData %>% gather(Run, Var, -Year, -A0) %>% filter(!is.na(Var))
      
      
      simulatedData <- 
        simulatedData %>% left_join(groupByData) %>% select(-A0)
      
      if(!is.null(logisetexpr)){
        simulatedData <- 
          with(simulatedData, simulatedData[eval(parse(text=logisetexpr)),])
        
        
        simulatedData <-simulatedData %>% select(-one_of(grpbyName1))
      }
      
      simulatedData <- 
        simulatedData %>% filter(!is.na(Var))
    }
    
    if(tolower(statistic)=="means") {
      
      if(!is.null(grpbyName))  {
        
        names(simulatedData)[names(simulatedData)== grpbyName] <- "groupByData"
        
        result <- 
          simulatedData %>% group_by(Year, groupByData, Run) %>% 
          summarise(Var = mean(Var)) %>% ungroup() %>% 
          group_by(groupByData, Year) %>% 
          summarise(Mean = mean(Var), 
                    Lower = quantile(Var, c(0.025)), 
                    Upper = quantile(Var, c(0.975))) %>% data.frame()
        
        if(any(result$Year==1)) {
          
          simulatedDataSum <- 
            simulatedData %>% group_by(Year, groupByData, Run)  %>%  summarise(Sum = n()) %>% 
            group_by(Year, groupByData) %>% filter(Year ==1) %>% summarise(Sum = unique(Sum))
          
          n <- simulatedDataSum[,"Sum"] %>% unlist()
          
          SD <- simulatedData %>% group_by(Year, groupByData, Run) %>% 
            summarise(SD = sd(Var)) %>% 
            filter(Year ==1) %>%
            group_by(Year, groupByData) %>% 
            summarise(SD = unique(SD))%>% 
            select(SD) 
          
          SD <- SD[,"SD"] %>% unlist()
          
          m <- result %>% filter(Year ==1)  %>% select(Mean) %>% unlist()
          
          result[result$Year == 1, c("Mean", "Lower", "Upper")] <- 
            t(sapply(1:length(n), function(x) 
              c(m[x],   m[x] - qt(.975, n[x]-1)*SD[x]/sqrt(n[x]), 
                m[x] + qt(.975, n[x]-1)*SD[x]/sqrt(n[x]))))
        }
        
        result$groupByData <- 
          names(env$dict$codings[[grpbyName]])[
            match( result$groupByData, env$dict$codings[[grpbyName]])]
        
        
        names(result)[names(result)=="groupByData"] <- grpbyName
        
      } else {
        
        result <- 
          simulatedData %>% group_by(Year, Run) %>% 
          summarise(Var = mean(Var)) %>% ungroup() %>% 
          group_by(Year) %>% 
          summarise(Mean = mean(Var), 
                    Lower = quantile(Var, c(0.025)), 
                    Upper = quantile(Var, c(0.975))) %>% data.frame()
        
        if(any(result$Year==1)) {
          
          simulatedDataSum <- 
            simulatedData %>% filter(Year ==1)  %>%   
            group_by(Run) %>% 
            summarise(Sum = n()) %>% 
            summarise(unique(Sum)) 
          
          n <- simulatedDataSum%>% as.numeric()
          
          SD <- simulatedData %>% group_by(Year, Run) %>% 
            summarise(SD = sd(Var)) %>% 
            filter(Year ==1) %>%
            group_by(Year) %>% 
            summarise(Sum = unique(SD)) %>% 
            as.numeric()
          
          m <- result[1,] %>% as.numeric()
          
          result[result$Year == 1, c("Mean", "Lower", "Upper")] <- 
            c(m[2],  m[2] - qt(.975, n-1)*SD[2]/sqrt(n), 
              m[2] + qt(.975, n-1)*SD[2]/sqrt(n))
        }
      }
      
      
      return(result)
      
    } else if (tolower(statistic)=="frequencies"){
      
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
        
        if(any(result$Year==1)) {
          
          simulatedDataSum <- 
            simulatedData %>% group_by(Year, groupByData, Run)  %>%  summarise(Sum = n()) %>% 
            group_by(Year, groupByData) %>% filter(Year ==1) %>% summarise(Sum = unique(Sum))
          
          n <- simulatedDataSum[,"Sum"] %>% unlist()
          
          n <- rep(n, length(unique(result$Var)))
          
          p <- result %>% filter(Year ==1)  %>% select(Mean) %>% unlist()
          
          result[result$Year == 1, c("Mean", "Lower", "Upper")] <- 
            t(sapply(1:length(n), 
                     function(x)  c(p[x],  p[x] - qnorm(.975)* sqrt(p[x]*(1-p[x])/n[x]),
                                    p[x] + qnorm(.975)* sqrt(p[x]*(1-p[x])/n[x]))))
        }
        result$groupByData <- 
          names(env$dict$codings[[grpbyName]])[
            match( result$groupByData, env$dict$codings[[grpbyName]])]
        
        
        names(result)[names(result)=="groupByData"] <- grpbyName
        
        
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
        
        
        if(any(result$Year==1)) {
          simulatedDataSum <- 
            simulatedData %>% filter(Year ==1)  %>%   
            group_by(Run) %>% 
            summarise(Sum = n()) %>% 
            summarise(unique(Sum)) 
          
          n <- simulatedDataSum%>% as.numeric()  
          
          
          result[result$Year == 1, c("Mean", "Lower", "Upper")] <- 
            t(sapply(result[result$Year == 1, "Mean"], 
                     function(p)  c(p,  p - qnorm(.975)* sqrt(p*(1-p)/n),
                                    p + qnorm(.975)* sqrt(p*(1-p)/n))))
        }
        
      }
      
      result$Var <- 
        names(env$dict$codings[[variableName]])[
          match(result$Var,env$dict$codings[[variableName]])]
      
      names(result)[names(result)=="Var"] <- variableName
      
      return(result)
      
    }
  }       
  


