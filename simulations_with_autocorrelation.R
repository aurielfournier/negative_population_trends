

################
#
# Generate twenty populations
#
################

# mu = mean population
# rho = autocorrelation 
# CV = Coefficient of Variation
# K = 2000

twenty_species <- function(nspp=20, nyears=100, mu=1000,rho=0.5,CV=0.2,plot=F, K=2000) {
  
  ## SD parameter of lognormal distribution with desired variability
  ln_SD <- sqrt(log(CV^2+1))  
  
  ## SD for first value
  ln_SD_e <- sqrt(ln_SD^2/(1-rho^2))  
  
  ## creates empty objects to store results in
  R <- matrix(nrow=nyears, ncol=nspp)
  ## so each row is a year, and each column is a species
  time_series <- matrix(nrow=nyears, ncol=nspp)
  
  ## randomly pulls an R for each species
  for(r in 1:nspp){
    R[,r] <- rnorm(mean=0,sd=1,n=nyears)
  }
  
  
  for(spp in 1:nspp){
    ## Sets the initial population size for each species (column)
    time_series[1,spp] <- ln_SD_e*R[1,spp]
    for (i in 1:(nyears-1)) {
      ## Runs the population model for each species (column)
      time_series[i+1,spp] <- rho*time_series[i,spp]+R[i,spp]*(1-time_series[i,spp]/K)*ln_SD
    }
  }
  
  time_series <- time_series + log(mu) - 0.5*ln_SD_e^2
  time_series <- exp(time_series)
  
  return(time_series)
}


################
#
# Grab the two populations with highest pop at time t=1, see what the trend really is
#
################


realtrend <- function(time_series, nspeciestopick=2){
  
  ## finds the two species with the highest populations in year 1 
  one <- time_series[,which(time_series[1,]==sort(time_series[1,], TRUE)[1])]
  two <- time_series[,which(time_series[1,]==sort(time_series[1,], TRUE)[2])]
  year = 1:nrow(time_series)
  
  ## puts the three vectors into one data frame, ready to be sampled from  
  twospecies <-  data.frame(one, two, year)
  
  ## Creates empty object to put results into
  values <- data.frame(beta=c(NA,NA),pvalue=c(NA,NA), rsquared=c(NA,NA))
  
  ## run two models, one on the first species, one on the second
  speciesmodel1 <- lm(data=twospecies, one ~ year)
  speciesmodel2 <- lm(data=twospecies, two ~ year)
  
  ## extract the beta coefficient, pvalue and rsquared for the first species
  values[1,1] <- speciesmodel1$coefficients[2]
  values[1,2] <- summary(speciesmodel1)$coefficients[,4][2]
  values[1,3] <- summary(speciesmodel1)$r.squared
  
  ## extract the beta coefficient, pvalue and rsquared for the second species
  values[2,1] <- speciesmodel2$coefficients[2]
  values[2,2] <- summary(speciesmodel2)$coefficients[,4][2]
  values[2,3] <- summary(speciesmodel2)$r.squared
  
  ## puts everything in a list to be output
  realstuff <- list()
  realstuff[["values"]] <- values
  realstuff[["twospecies"]] <- twospecies
    
  return(realstuff)
}


##################
#
# sample the real population with some frequency of sampling effort with error
#
##################

samplingeffortwitherror <- function(time_series, freq=NA, sd=0.1){
  
  ## The default is to have freq=NA, which means that the function 
  ## will randomly pull a random set of years to sample on
  ## This checks to see if freq=NA, and if so, does that
  ## If we set freq = to some number it will bipass that and use that instead
  if(is.na(freq)){
      freq <- sort(base::sample(1:100, sample(1:60, 1), replace=FALSE))
  }
  
  ## finds the two species with the highest populations in year 1 
  one <- time_series[,which(time_series[1,]==sort(time_series[1,], TRUE)[1])]
  two <- time_series[,which(time_series[1,]==sort(time_series[1,], TRUE)[2])]
  year = 1:nrow(time_series)

  ## puts the three vectors into one data frame, ready to be sampled from  
  twospecies <-  data.frame(one, two, year)
  
  ## If we have set freq = to some single value, this if statement runs
  if(length(freq)==1){
  sampled_data <- twospecies[seq(from=1, to=nrow(time_series), by=freq),]
  }
  
  ## if freq is a vector of years, than this one runs
  if(length(freq)>1){
  sampled_data <- twospecies[freq,]
  }
  
  ## This takes the years we just sampled, and applies an error term
  ## since its pretty hard to actually estimate the population
  sampled_data[,1:2] <- sampled_data[,1:2] * exp(rnorm(1, mean=0, sd=sd)-0.5*sd^2)
  
  return(sampled_data)
}

##################
#
#  What trend do we see in the sampled data? 
#
##################

sampletrend <- function(sampled_data){
  
  ## Create empty object to store results in
  sampled_values <- data.frame(beta=c(NA,NA),pvalue=c(NA,NA),rsquared=c(NA,NA))
  
  ## run two models, one on the first species, one on the second
  speciesmodel1 <- lm(data=sampled_data, one ~ year)
  speciesmodel2 <- lm(data=sampled_data, two ~ year)
  
  ## extract the beta coefficient, pvalue and rsquared for the first species
  sampled_values[1,1] <- speciesmodel1$coefficients[2]
  sampled_values[1,2] <- summary(speciesmodel1)$coefficients[,4][2]
  sampled_values[1,3] <- summary(speciesmodel1)$r.squared
  
  ## extract the beta coefficient, pvalue and rsquared for the second species  
  sampled_values[2,1] <- speciesmodel2$coefficients[2]
  sampled_values[2,2] <- summary(speciesmodel2)$coefficients[,4][2]
  sampled_values[2,3] <- summary(speciesmodel2)$r.squared
  
  return(sampled_values)
}

##################
#
#  One simulation, all in one simple function
#
##################

onesimulation <- function(freq=NA){
  
  ## Generate the time series for 20 species
  time_series <- twenty_species()
  ## Figure out the 'real trend'
  rdat <- realtrend(time_series)
  ## Sample the real population data, with some error
  sampled_data <- samplingeffortwitherror(time_series, freq=freq, sd=0.1)
  ## figure out what the trend is for the sampled_data
  svalues <- sampletrend(sampled_data)
  ## output everything into one master list
  outputs <- list()
  outputs[["twenty_species"]] <- time_series
  outputs[["values"]] <- rdat$values
  outputs[["twospecies"]] <- rdat$twospecies
  outputs[["sampled_data"]] <- sampled_data
  outputs[["sampled_values"]] <- svalues
  return(outputs)
}

####################
#
#  Run the simulation many times, store everything in a list and spit it out
#
####################

manysimulations <- function(sims=1000, freq=NA){
  
  ## Create empty objects to store results in
  values <- data.frame(beta=rep(NA,sims*2),pvalue=rep(NA,sims*2),rsquared=rep(NA,sims*2))
  svalues <- data.frame(beta=rep(NA,sims*2),pvalue=rep(NA,sims*2),rsquared=rep(NA,sims*2))
  twospecies <- as.data.frame(matrix(ncol=(2*sims), nrow=100))
  sampleddata <- list()
  
  ## Runs through the onesimulation function sims number of times
  ## stores the various results in the different objects
  ## sampledata is a list because the number of rows could vary from sim to sim
  for(i in seq(1,(sims*2),by=2)){
    dd <- onesimulation(freq=freq)
    values[i:(i+1),] <- dd$values
    svalues[i:(i+1),] <- dd$sampled_values
    twospecies[,i:(i+1)] <- dd$twospecies[,1:2]
    sampleddata[[i]] <- dd$sampled_data[,1:2]
  }
  
  ## load everything up into one master list to be outputted
  outputs <- list()
  outputs[["values"]] <- values
  outputs[["svalues"]] <- svalues
  outputs[["twospecies"]] <- twospecies
  outputs[["sampleddata"]] <- sampleddata
  
  return(outputs)
  
}

###########################################################

## Random Sampling (perhaps representative of using citizen science or museum records)

###########################################################

dat <- manysimulations()

library(tidyverse)

values <- dat$values
svalues <- dat$svalues

valuessig <- values %>% filter(pvalue<=0.05) %>% mutate(type="realtrend")
svaluessig <- svalues %>% filter(pvalue<=0.05) %>% mutate(type="surveyeddata")

datdat <- bind_rows(valuessig, svaluessig)

model <- aov(data=datdat, beta ~ type)

summary(model)

ggplot(data=datdat, aes(y=beta, x=type))+ geom_boxplot()


###############

## Regular sampling

###############

dat <- manysimulations(freq=4)

library(tidyverse)

values <- dat$values
svalues <- dat$svalues

valuessig <- values %>% filter(pvalue<=0.05) %>% mutate(type="realtrend")
svaluessig <- svalues %>% filter(pvalue<=0.05) %>% mutate(type="surveyeddata")

datdat <- bind_rows(valuessig, svaluessig)

model <- aov(data=datdat, beta ~ type)

summary(model)

ggplot(data=datdat, aes(y=beta, x=type))+ geom_boxplot()


##################

## Bursts of sampling with bursts of nothing

#################

bursts_of_sampling <- c(1,2,3,4,5,6,15,16,18,19,37,38,39,47,48,56,69,70,71,72,73,74,75,76,77,78,91,92,93,97,99)

dat <- manysimulations(freq=bursts_of_sampling)

library(tidyverse)

values <- dat$values
svalues <- dat$svalues

valuessig <- values %>% filter(pvalue<=0.05) %>% mutate(type="realtrend")
svaluessig <- svalues %>% filter(pvalue<=0.05) %>% mutate(type="surveyeddata")

datdat <- bind_rows(valuessig, svaluessig)

model <- aov(data=datdat, beta ~ type)

summary(model)

ggplot(data=datdat, aes(y=beta, x=type))+ geom_boxplot()


