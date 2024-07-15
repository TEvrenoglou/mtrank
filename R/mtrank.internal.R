
## a functions specifying the opposite of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))

## a function that "cleans" long arm level data from:
## for binary data: (i) all-zero event studies and (ii) studies with missing number of event/sample sizes
## for continuous data: studies with missing arm level means, sample size and standard deviation

clean <- function(data,outcome){
  
  if(outcome=="binary"){  
    
    ## Remove NA's    
    data <- data[complete.cases(data$event),]
    
    data <- data[complete.cases(data$n),]
    
    ## Remove 0-0 studies  
    
    events.study <- with(data, tapply(event, studlab, sum))
    
    if (any(events.study == 0)){
      
      zeroevents <- events.study == 0
      keep <- !(zeroevents)
      
      data <- data[data$studlab %in% names(events.study)[keep], ,
                   drop = FALSE]
    }
    
  }
  else if(outcome=="continuous"){
    
    ## Remove NA's    
    
    data <- data[complete.cases(data$mean),]
    
    data <- data[complete.cases(data$sd),]
    
    data <- data[complete.cases(data$n),]
    
  }
  
  ## Remove single arm studies
  
  count <- as.data.frame(table(data$studlab))
  
  E <- which(count$Freq==1)
  
  E1 <- count$Var1[E]
  
  data <- data %>% 
    filter(studlab %!in% E1)
  
  return(data) 
}

## a function transforming wide to long format data

go_long <- function(treat,event,mean,sd,n,studlab,type){
  
  studyid <- unique(studlab)
  
  id <- 1:length(studyid)
  
  studyid_long <- rep(studyid,length(treat))
  
  id_long <- rep(id,length(treat))
  
  treat <- unlist(treat)
  
  if(type=="binary"){
    
    event <- unlist(event)
    
    n <- unlist(n)
    
    data_new <- data.frame("studlab" = studyid_long,
                           "id" = id_long,
                           "treat" = treat,
                           "event" = event,
                           "n" = n
    )
    
  }else if(type=="continuous"){
    
    mean <- unlist(mean)  
    
    sd <- unlist(sd)
    
    n <- unlist(n)
    
    data_new <- data.frame("studlab"=studyid_long,
                           "id" = id_long,
                           "treat" = treat,
                           "mean" = mean,
                           "sd" = sd,
                           "n" = n
    )
    
  }
  
  data_f <- data_new[order(data_new$id),]
  
  data_f$id <- NULL
  
  return(data_f)
}
