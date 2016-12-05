

twenty_species <- function(nspp=20, nyears=100, mu=1000,rho=0.5,CV=0.2,plot=F, K=2000) {
  sim.years <- nyears+10
  ln_SD <- sqrt(log(CV^2+1))  #SD parameter of lognormal distribution with desired variability
  ln_SD_e <- sqrt(ln_SD^2/(1-rho^2))  #SD for first value
  
  R <- matrix(nrow=sim.years, ncol=nspp)
  for(r in 1:nspp){
    R[,r] <- rnorm(mean=0,sd=1,n=sim.years)
  }
  
  time_series <- matrix(nrow=sim.years, ncol=nspp)
  for(spp in 1:nspp){
    time_series[1,spp] <- ln_SD_e*R[1,spp]
    for (i in 1:(sim.years-1)) {
      time_series[i+1,spp] <- rho*time_series[i,spp]+R[i,spp]*(1-time_series[i,spp]/K)*ln_SD
    }
  }
  
  time_series <- time_series + log(mu) - 0.5*ln_SD_e^2
  time_series <- exp(time_series)
  return(time_series[-c(1:10),])
}


#################################################################################################

realtrend <- function(tdat, nspeciestopick=2){
  
  one <- tdat[,which(tdat[1,]==sort(tdat[1,], TRUE)[1])]
  two <- tdat[,which(tdat[1,]==sort(tdat[1,], TRUE)[2])]
  year = 1:nrow(tdat)
  
  twospecies <-  data.frame(one, two, year)
  
  values <- data.frame(beta=c(NA,NA),pvalue=c(NA,NA))
  
  speciesmodel1 <- lm(data=twospecies, one ~ year)
  speciesmodel2 <- lm(data=twospecies, two ~ year)
  
  values[1,1] <- speciesmodel1$coefficients[2]
  values[1,2] <- summary(speciesmodel1)$coefficients[,4][2]
  
  values[2,1] <- speciesmodel2$coefficients[2]
  values[2,2] <- summary(speciesmodel2)$coefficients[,4][2]
  
  return(values)
}

##################################################################################################

samplingeffort <- function(tdat, frequency=10){
  
  one <- tdat[,which(tdat[1,]==sort(tdat[1,], TRUE)[1])]
  two <- tdat[,which(tdat[1,]==sort(tdat[1,], TRUE)[2])]
  year = 1:nrow(tdat)
  
  twospecies <-  data.frame(one, two, year)
  
  sampled_data <- twospecies[seq(from=1, to=nrow(tdat), by=frequency),]
  
  return(sampled_data)
}

##################################################################################################

sampletrend <- function(sdat){
  sampled_values <- data.frame(beta=c(NA,NA),pvalue=c(NA,NA))
  
  speciesmodel1 <- lm(data=sdat, one ~ year)
  speciesmodel2 <- lm(data=sdat, two ~ year)
  
  sampled_values[1,1] <- speciesmodel1$coefficients[2]
  sampled_values[1,2] <- summary(speciesmodel1)$coefficients[,4][2]
  
  sampled_values[2,1] <- speciesmodel2$coefficients[2]
  sampled_values[2,2] <- summary(speciesmodel2)$coefficients[,4][2]
  
  return(sampled_values)
}

###################################################################


theproblem <- function(freq=5){
  tdat <- twenty_species()
  values <- realtrend(tdat)
  sdat <- samplingeffort(tdat, frequency=freq)
  svalues <- sampletrend(sdat)
  outputs <- list()
  outputs[["twenty_species"]] <- tdat
  outputs[["values"]] <- values
  outputs[["sampled_data"]] <- sdat
  outputs[["sampled_values"]] <- svalues
  return(outputs)
}

########################################################################

theproblem1000 <- function(sims=1000, freq=5){
  values <- data.frame(beta=rep(NA,sims*2),pvalue=rep(NA,sims*2))
  svalues <- data.frame(beta=rep(NA,sims*2),pvalue=rep(NA,sims*2))
  
  for(i in seq(1,(sims*2),by=2)){
    dd <- theproblem(freq=freq)
    values[i:(i+1),] <- dd$values
    svalues[i:(i+1),] <- dd$sampled_values
  }
  
  outputs <- list()
  outputs[["values"]] <- values
  outputs[["svalues"]] <- svalues
  
  return(outputs)
  
}

###########################################################
###########################################################
###########################################################

dat <- theproblem1000(freq=3)

library(tidyverse)

values <- dat$values
svalues <- dat$svalues

valuessig <- values %>% filter(pvalue<=0.05) %>% mutate(type="realtrend")
svaluessig <- svalues %>% filter(pvalue<=0.05) %>% mutate(type="surveyeddata")

datdat <- bind_rows(valuessig, svaluessig)

model <- aov(data=datdat, beta ~ type)

summary(model)

ggplot(data=datdat, aes(y=beta, x=type))+ geom_boxplot()

