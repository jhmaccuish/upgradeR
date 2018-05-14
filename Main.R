###############################################
#       Main R Code for upgrade project
#   Author : Jamie Hentall MacCuish
###############################################
#cat("\014") # this clears the console window (clc in Matlab)
rm(list=ls()) # this clears the data space (clear all in Matlab)

#library(foreign) 
library(np) # load the package "np"
library(plyr)
library(haven)
library(dplyr)
library(sfsmisc)
library(sampleSelection)

numPercentiles <- 4
PercentilePoints <- seq(0,1,length.out= numPercentiles+1)
startAge <- 57
endAge <- 57
q <- array(0,c((endAge-startAge+1),numPercentiles,numPercentiles))
p <- array(NA,c((endAge-startAge+1),8,numPercentiles,numPercentiles))

cleaned_data_waves1_8 <- read_dta("../Stata/data/cleaned_data_waves1-8.dta")
cleaned_data_waves1_8 <- cleaned_data_waves1_8[(cleaned_data_waves1_8$female == 1  ),]

heck <- selection( wpactive  ~ age + over_SPA + marstat,
                   individual_net_emp_inc ~ age +tenure + edu_yr + gor, cleaned_data_waves1_8 )
cleaned_data_waves1_8$y_fitted <- predict( heck )


for(currentAge in 57:57) {

  newTest <- cleaned_data_waves1_8[,c('idauniq','age','bu_non_housing_wlth','wpactive','year_age_SPA','y_fitted')]
  newTest <- newTest[(newTest$idauniq != 150791),]
  newTest <- newTest[((newTest$age==currentAge | newTest$age==(currentAge+1)) &  !is.na(newTest$bu_non_housing_wlth)),]
  newTest <- newTest[order(newTest$idauniq,newTest$age),]
  
  leadData <- newTest %>% group_by(idauniq) %>% mutate( wlth1= lead(bu_non_housing_wlth, n = 1L, default = NA, order_by = age))
  
  countdata <- plyr::count(newTest, vars = c("idauniq"))
  
  deciles1 <- quantile(newTest[(newTest$age==currentAge),]$bu_non_housing_wlth, probs = PercentilePoints,na.rm=TRUE)
  deciles2 <- quantile(newTest[(newTest$age==(currentAge+1)),]$bu_non_housing_wlth, probs = PercentilePoints,na.rm=TRUE)
  
  
  countdata <- countdata[(countdata$freq==2),]
  
  leadData <- leadData[(leadData$idauniq %in% countdata$idauniq),]
  fiter <- function (wealth,age,percentile,selectAge) {
     for (i in 1:length(percentile)){
      if (wealth<percentile[i]){return(i)} 
     }
  }
  
  leadData <- mutate( leadData, d1=fiter(bu_non_housing_wlth,age,deciles1))
  leadData <- mutate( leadData, d12=fiter(wlth1,age,deciles2))
  leadData <- leadData[(leadData$age==currentAge),]
  for (i in 2:(numPercentiles+1)) {
    #print(cat("Current Conditioning Decile", i))
    #print(nrow(leadData[(leadData$d1==i),]))
    hist(leadData[(leadData$d1==i),]$d12, xlim=c(1,numPercentiles+1) ,breaks = seq(1,numPercentiles+1,1), 
         main=paste("Age =",currentAge,"Income decile =", (i-1),"Observations =",nrow(leadData[(leadData$d1==i),])))
    #Sys.sleep(2)
    #kde2d
    den = density(leadData[(leadData$d1==i),]$wlth1)
    #plot(den, main=paste("Age =",currentAge,"Income decile =", (i-1),"Observations =",nrow(leadData[(leadData$d1==i),])))
    #abline(v=deciles2)
    #Sys.sleep(2)
    #readline(prompt="Press [enter] to continue")
    q[currentAge-startAge+1,i-1,1] <- integrate.xy(den$x[(den$x<=deciles2[2])],den$y[(den$x<=deciles2[2])])    
    for (j in 3:(numPercentiles+1)){
      if (length(den$x[(deciles2[j-1]<den$x & den$x<=deciles2[j])])>0){
      q[currentAge-startAge+1,i-1,j-1] <- integrate.xy(den$x[(deciles2[j-1]<den$x & den$x<=deciles2[j])],den$y[(deciles2[j-1]<den$x & den$x<=deciles2[j])])
      }
    }
    if (length(den$x[(deciles2[j]<den$x)])>0){
      q[currentAge-startAge+1,i-1,j-1] <- integrate.xy(den$x[(deciles2[j]<den$x)],den$y[(deciles2[j]<den$x)])
    }    
    #print(cat("Sum CCPs", sum(q[currentAge-startAge+1,i-1,])))
    q[currentAge-startAge+1,i-1,] <- q[currentAge-startAge+1,i-1,]/sum(q[currentAge-startAge+1,i-1,])
    #print(cat("Sum Corrected CCPs", sum(q[currentAge-startAge+1,i-1,])))
    print(q[currentAge-startAge+1,i-1,])
    readline(prompt="Press [enter] to continue")
    for (ageSPA in 60:67) {
      if (nrow(leadData[(leadData$d1==i & leadData$year_age_SPA==ageSPA),])>1) {
        hist(leadData[(leadData$d1==i & leadData$year_age_SPA==ageSPA),]$d12, xlim=c(1,numPercentiles+1) ,breaks = seq(1,numPercentiles+1,1), 
             main=paste("Age =",currentAge,"Income decile =", (i-1),"SPA Age =",ageSPA))
        den = density(leadData[(leadData$d1==i & leadData$year_age_SPA==ageSPA),]$wlth1)
        p[currentAge-startAge+1,ageSPA-59,i-1,1] <- integrate.xy(den$x[(den$x<=deciles2[2])],den$y[(den$x<=deciles2[2])])    
        for (j in 3:(numPercentiles+1)){
          if (length(den$x[(deciles2[j-1]<den$x & den$x<=deciles2[j])])>0){
            p[currentAge-startAge+1,ageSPA-59,i-1,j-1] <- integrate.xy(den$x[(deciles2[j-1]<den$x & den$x<=deciles2[j])],den$y[(deciles2[j-1]<den$x & den$x<=deciles2[j])])
          }
          else {p[currentAge-startAge+1,ageSPA-59,i-1,j-1] = 0}
        }
        if (length(den$x[(deciles2[j]<den$x)])>0){
          p[currentAge-startAge+1,ageSPA-59,i-1,j-1] <- integrate.xy(den$x[(deciles2[j]<den$x)],den$y[(deciles2[j]<den$x)])
        }        
      }
      #readline(prompt="Press [enter] to continue")
    }
  }
}