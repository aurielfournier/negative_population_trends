#
# Generate twenty populations
#
################

# mu = mean population
# rho = autocorrelation 
# CV = Coefficient of Variation
library(tidyverse)

############
# spp = number of species


generate_species <- function(nyears=100, mu=1000,rho=0.5,CV.=0.2, spp.=20) {
  sim.years <- nyears+10
  
  ln_SD <- sqrt(log(CV.^2+1))  #SD parameter of lognormal distribution with desired variability
  
  ln_SD_e <- sqrt(ln_SD^2/(1-rho^2))  #SD for first value
  
  time_series_temp <- matrix(rnorm(mean=0,sd=1,n=sim.years*spp.),
                             ncol=spp., nrow=sim.years)
  
  time_series <- matrix(nrow=sim.years,ncol=spp.)
  
  time_series[1,] <- ln_SD_e*time_series_temp[1,]
  
  for (i in 2:(sim.years)) {
    time_series[i,] <- rho*time_series[(i-1),]+time_series_temp[i,]*ln_SD
  }
  
  time_series <- time_series + log(mu) - 0.5*ln_SD_e^2
  
  time_series <- exp(time_series)
  
  return(time_series[-c(1:10),])
}

################
#
# Grab the X populations with highest pop at time t=1, see what the trend really is
#
################


realtrend <- function(time_series, spp.=spp, numyears=100,topnum=2){
  #topnum = how many of the highest species to graph
  
  top_spp_list <- list()
  
  for(i in 1:topnum){
    top_spp_list[[i]] <- time_series[,which(time_series[1,]==sort(time_series[1,], TRUE)[i])]
  }
  
  highestspecies <- do.call(cbind, top_spp_list) %>%
                      as.data.frame() 
  
  loghighestspecies <- log(highestspecies)
  
  colnames(loghighestspecies) <- paste0("logV", 1:topnum)

  highestspecies <- bind_cols(highestspecies, loghighestspecies) %>%
                      mutate(year = 1:nrow(time_series))

  highestspecies <- highestspecies[1:numyears,]
  
  ## Creates empty object to put results into
  values <- data.frame(beta=rep(NA, times=topnum),
                       SE=rep(NA, times=topnum),
                       pvalue=rep(NA, times=topnum), 
                       rsquared=rep(NA, times=topnum))
  
  logvalues <- data.frame(beta=rep(NA, times=topnum),
                         SE=rep(NA, times=topnum),
                         pvalue=rep(NA, times=topnum), 
                         rsquared=rep(NA, times=topnum))
  
  ## run two models, one on the first species, one on the second
  speciesmodellist <- list()
  logspeciesmodellist <- list()
  
  for(i in 1:topnum){
  speciesmodellist[[i]] <- lm(highestspecies[,i] ~ highestspecies$year)
  logspeciesmodellist[[i]] <- lm(highestspecies[,(i+topnum)] ~ highestspecies$year)
  
  values[i,1] <- speciesmodellist[[i]]$coefficients[2]
  values[i,2] <- summary(speciesmodellist[[i]])$coefficients[,2][2]
  values[i,3] <- summary(speciesmodellist[[i]])$coefficients[,4][2]
  values[i,4] <- summary(speciesmodellist[[i]])$r.squared
  
  logvalues[i,1] <- logspeciesmodellist[[i]]$coefficients[2]
  logvalues[i,2] <- summary(logspeciesmodellist[[i]])$coefficients[,2][2]
  logvalues[i,3] <- summary(logspeciesmodellist[[i]])$coefficients[,4][2]
  logvalues[i,4] <- summary(logspeciesmodellist[[i]])$r.squared
  
  }

  ## puts everything in a list to be output
  realstuff <- list()
  realstuff[["real_values_highest_linear"]] <- values
  realstuff[["real_values_highest_log"]] <- logvalues
  realstuff[["real_data_highest"]] <- highestspecies
  
  ###########
  cols <- base::sample(1:ncol(time_series),topnum)

  randomspecies <- time_series[,cols]
  lograndomspecies <- log(randomspecies)
  colnames(lograndomspecies) <- paste0("logV",1:topnum)
  
  randomspecies <- cbind(randomspecies, lograndomspecies) %>%
                    as.data.frame() %>%
                    mutate(year = 1:nrow(time_series))
  
  randomspecies <- randomspecies[1:numyears,]
  
  ## Creates empty object to put results into
  values <- data.frame(beta=rep(NA, times=topnum),
                       SE=rep(NA, times=topnum),
                       pvalue=rep(NA, times=topnum), 
                       rsquared=rep(NA, times=topnum))
  
  logvalues <- data.frame(beta=rep(NA, times=topnum),
                       SE=rep(NA, times=topnum),
                       pvalue=rep(NA, times=topnum), 
                       rsquared=rep(NA, times=topnum))
  
  ## run two models, one on the first species, one on the second
  speciesmodellist <- list()
  logspeciesmodellist <- list()
  
  for(i in 1:topnum){
    speciesmodellist[[i]] <- lm(randomspecies[,i] ~ randomspecies$year)
    logspeciesmodellist[[i]] <- lm(randomspecies[,(i+topnum)] ~ randomspecies$year)
    
    values[i,1] <- speciesmodellist[[i]]$coefficients[2]
    values[i,2] <- summary(speciesmodellist[[i]])$coefficients[,2][2]
    values[i,3] <- summary(speciesmodellist[[i]])$coefficients[,4][2]
    values[i,4] <- summary(speciesmodellist[[i]])$r.squared
    
    logvalues[i,1] <- logspeciesmodellist[[i]]$coefficients[2]
    logvalues[i,2] <- summary(logspeciesmodellist[[i]])$coefficients[,2][2]
    logvalues[i,3] <- summary(logspeciesmodellist[[i]])$coefficients[,4][2]
    logvalues[i,4] <- summary(logspeciesmodellist[[i]])$r.squared
    
  }
  
  ## puts everything in a list to be output
  realstuff[["real_values_random_linear"]] <- values
  realstuff[["real_values_random_log"]] <- logvalues
  realstuff[["real_data_random"]] <- randomspecies
  
  ########
  ########
  ########
  
  ## finds the two species with the highest populations in year 1 
  ## puts the three vectors into one data frame, ready to be sampled from  
  allspecies <-  data.frame(time_series, 
                            year=1:nrow(time_series)) 
  
  logallspecies <- log(allspecies[,1:spp.])
  
  colnames(logallspecies) <- paste0("logX",1:spp.)
  
  allspecies <- bind_cols(allspecies, logallspecies)
  
  ## Creates empty object to put results into
  values <- data.frame(beta=rep(NA,times=spp.),
                       SE=NA,
                       pvalue=NA, 
                       rsquared=NA)
  
  logvalues <- data.frame(beta=rep(NA,times=spp.),
                       SE=NA,
                       pvalue=NA, 
                       rsquared=NA)
  
  for(column in 1:spp.){
    ## run two models, one on the first species, one on the second
    speciesmodel1 <- lm(allspecies[,column] ~ allspecies$year)
    logspeciesmodel1 <- lm(allspecies[,(column+21)] ~ allspecies$year)
    
    ## extract the beta coefficient, pvalue and rsquared for the first species
    values[column,1] <- speciesmodel1$coefficients[2]
    values[column,2] <- summary(speciesmodel1)$coefficients[,2][2]
    values[column,3] <- summary(speciesmodel1)$coefficients[,4][2]
    values[column,4] <- summary(speciesmodel1)$r.squared
    
    logvalues[column,1] <- logspeciesmodel1$coefficients[2]
    logvalues[column,2] <- summary(logspeciesmodel1)$coefficients[,2][2]
    logvalues[column,3] <- summary(logspeciesmodel1)$coefficients[,4][2]
    logvalues[column,4] <- summary(logspeciesmodel1)$r.squared
  }  
  
  ## puts everything in a list to be output
  realstuff[["real_values_all_pops_linear"]] <- values
  realstuff[["real_values_all_pops_log"]] <- logvalues
  return(realstuff)
}


#########################
#
# All Together Now
#
#########################


onesimulation <- function(freq=NA, spp.=20, CV.=0.2, numyears=100, topnum.=2){
  
  ## Generate the time series for 20 species
  time_series <- generate_species(spp.=spp., CV.=CV.)
  
  ## Figure out the 'real trend'
  rdat <- realtrend(time_series, spp.=spp., numyears=numyears, topnum=topnum.)
  
  ## output everything into one master list
  outputs <- list()
  outputs[["twenty_species"]] <- time_series
  outputs[["real_values_highest_linear"]] <- rdat$real_values_highest_linear
  outputs[["real_values_highest_log"]] <- rdat$real_values_highest_log
  outputs[["real_values_random_log"]] <- rdat$real_values_random_log
  outputs[["real_values_random_linear"]] <- rdat$real_values_random_linear
  outputs[["real_data_highest"]] <- rdat$real_data_highest
  outputs[["real_data_random"]] <- rdat$real_data_random
  outputs[["real_values_all_pops_linear"]] <- rdat$real_values_all_pops_linear
  outputs[["real_values_all_pops_log"]] <- rdat$real_values_all_pops_log
  return(outputs)
}




manysimulations <- function(sims=1000, freq=NA, spp=20, CV=0.2, numyears=100, topnum=2){
  
  ## Create empty objects to store results in
  ## sampledata is a list because the number of rows could vary from sim to sim
  real_values_highest_linear <- data.frame(beta=rep(NA,sims*topnum),
                                    SE=rep(NA,sims*topnum),
                                    pvalue=rep(NA,sims*topnum),
                                    rsquared=rep(NA,sims*topnum))
  
  real_values_highest_log <- data.frame(beta=rep(NA,sims*topnum),
                                           SE=rep(NA,sims*topnum),
                                           pvalue=rep(NA,sims*topnum),
                                           rsquared=rep(NA,sims*topnum))
  
  real_values_random_linear <- data.frame(beta=rep(NA,sims*topnum),
                                   SE=rep(NA,sims*topnum),
                                   pvalue=rep(NA,sims*topnum),
                                   rsquared=rep(NA,sims*topnum))
  
  real_values_random_log <- data.frame(beta=rep(NA,sims*topnum),
                                   SE=rep(NA,sims*topnum),
                                   pvalue=rep(NA,sims*topnum),
                                   rsquared=rep(NA,sims*topnum))
  
  
  real_values_all_pops_linear <- list()

  real_values_all_pops_log <- list()
  
  real_data_highest <- as.data.frame(matrix(ncol=(topnum*sims), 
                                            nrow=numyears))
  
  real_data_random <- as.data.frame(matrix(ncol=(topnum*sims), 
                                           nrow=numyears))
  
  twenty_species <- list()
  
  ## Runs through the onesimulation function sims number of times
  ## stores the various results in the different objects
  for(i in seq(1,(sims*topnum),by=2)){
    dd <- onesimulation(freq=freq, spp.=spp, 
                        CV.=CV, numyears=numyears, topnum=topnum)
    
    real_values_highest_log[i:(i+1),] <- dd$real_values_highest_log
    real_values_highest_linear[i:(i+1),] <- dd$real_values_highest_linear
    real_values_random_log[i:(i+1),] <- dd$real_values_random_log
    real_values_random_linear[i:(i+1),] <- dd$real_values_random_linear
    real_values_all_pops_log[[i]] <- dd$real_values_all_pops_log
    real_values_all_pops_linear[[i]] <- dd$real_values_all_pops_linear
    real_data_highest[,i:(i+1)] <- dd$real_data_highest[,1:topnum]
    real_data_random[,i:(i+1)] <- dd$real_data_random[,1:topnum]
    
    colnames(dd$twenty_species) <- paste0("spp",1:spp)
    
    t_species <- data.frame(dd$twenty_species) %>% 
                    mutate(year=1:100) %>% 
                    gather("variable","value", -year) %>% 
                    mutate(sim=i)
    
    twenty_species[[i]] <- t_species
  }
  
  ## load everything up into one master list to be outputted
  outputs <- list()
  outputs[["real_values_highest_log"]] <- real_values_highest_log
  outputs[["real_values_highest_linear"]] <- real_values_highest_linear
  outputs[["real_values_random_log"]] <- real_values_random_log
  outputs[["real_values_random_linear"]] <- real_values_random_linear
  outputs[["real_values_all_pops_log"]] <- do.call(rbind, real_values_all_pops_log)
  outputs[["real_values_all_pops_linear"]] <- do.call(rbind, real_values_all_pops_linear)
  outputs[["real_data_highest"]] <- real_data_highest
  outputs[["real_data_random"]] <- real_data_random
  outputs[["twenty_species"]] <- twenty_species
  
  return(outputs)
  
}

f1s20y2top5 <- manysimulations(freq=1, spp=20, sims=10000, numyears=2, topnum=5)
save(f1s20y2top5, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears2_top5.Rdata")


f1s20y2top5 <- manysimulations(freq=1, spp=20, sims=10000, numyears=2, topnum=10)
save(f1s20y2top10, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears2_top10.Rdata")

# year2 <- manysimulations(freq=2, spp=20, sims=10000, numyears=2)
# save(year2, file="~/../Dropbox/negative_population_trends/10ksims_freq2_spp20_numyears2.Rdata")
# 
# year3 <- manysimulations(freq=5, spp=20, sims=10000, numyears=2)
# save(year3, file="~/../Dropbox/negative_population_trends/10ksims_freq5_spp20_numyears2.Rdata")

#########---------------------

f1s20y5top5 <- manysimulations(freq=1, spp=20, sims=10, numyears=5, topnum=5)
save(f1s20y5top5, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears5_top5.Rdata")

f1s20y5top10 <- manysimulations(freq=1, spp=20, sims=10, numyears=5, topnum=10)
save(f1s20y5top10, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears5_top10.Rdata")
# 
# year2 <- manysimulations(freq=2, spp=20, sims=10000, numyears=5)
# save(year2, file="~/../Dropbox/negative_population_trends/10ksims_freq2_spp20_numyears5.Rdata")
# 
# year3 <- manysimulations(freq=5, spp=20, sims=10000, numyears=5)
# save(year3, file="~/../Dropbox/negative_population_trends/10ksims_freq5_spp20_numyears5.Rdata")

#########---------------------

f1s20y10top5 <- manysimulations(freq=1, spp=20, sims=10000, numyears=10,topnum=5)
save(f1s20y10top5, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears10_top5.Rdata")

f1s20y10top10 <- manysimulations(freq=1, spp=20, sims=10000, numyears=10,topnum=10)
save(f1s20y10top10, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears10_top10.Rdata")

# year2 <- manysimulations(freq=2, spp=20, sims=10000, numyears=10)
# save(year2, file="~/../Dropbox/negative_population_trends/10ksims_freq2_spp20_numyears10.Rdata")
# 
# year3 <- manysimulations(freq=5, spp=20, sims=10000, numyears=10)
# save(year3, file="~/../Dropbox/negative_population_trends/10ksims_freq5_spp20_numyears10.Rdata")

#########---------------------

f1s20y20top5 <- manysimulations(freq=1, spp=20, sims=10000, numyears=20, topnum=5)
save(f1s20y20top5, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears20_top5.Rdata")

f1s20y20top10 <- manysimulations(freq=1, spp=20, sims=10000, numyears=20, topnum=10)
save(f1s20y20top10, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears20_top10.Rdata")

# year2 <- manysimulations(freq=2, spp=20, sims=10000, numyears=20)
# save(year2, file="~/../Dropbox/negative_population_trends/10ksims_freq2_spp20_numyears20.Rdata")
# 
# year3 <- manysimulations(freq=5, spp=20, sims=10000, numyears=20)
# save(year3, file="~/../Dropbox/negative_population_trends/10ksims_freq5_spp20_numyears20.Rdata")

#########---------------------

f1s20y40top5 <- manysimulations(freq=1, spp=20, sims=10000, numyears=40, topnum=5)
save(f1s20y40top5, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears40_top5.Rdata")

f1s20y40top10 <- manysimulations(freq=1, spp=20, sims=10000, numyears=40,topnum=10)
save(f1s20y40top10, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears40_top10.Rdata")



# year2 <- manysimulations(freq=2, spp=20, sims=10000, numyears=40)
# save(year2, file="~/../Dropbox/negative_population_trends/10ksims_freq2_spp20_numyears40.Rdata")
# 
# year3 <- manysimulations(freq=5, spp=20, sims=10000, numyears=40)
# save(year3, file="~/../Dropbox/negative_population_trends/10ksims_freq5_spp20_numyears40.Rdata")

#########---------------------

f1s20y50top5 <- manysimulations(freq=1, spp=20, sims=10000, numyears=50, topnum=5)
save(f1s20y50top5, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears50_top5.Rdata")

f1s20y50top10 <- manysimulations(freq=1, spp=20, sims=10000, numyears=50, topnum=10)
save(f1s20y50top10, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears50_top10.Rdata")

#########---------------------

f1s20y100top5 <- manysimulations(freq=1, spp=20, sims=10000, numyears=100, topnum=5)
save(f1s20y100top5, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears100_top5.Rdata")

f1s20y100top10 <- manysimulations(freq=1, spp=20, sims=10000, numyears=100, topnum=10)
save(f1s20y100top10, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears100_top10.Rdata")

# year2 <- manysimulations(freq=2, spp=20, sims=10000, numyears=100)
# save(year2, file="~/../Dropbox/negative_population_trends/10ksims_freq2_spp20_numyears100.Rdata")
# 
# year3 <- manysimulations(freq=5, spp=20, sims=10000, numyears=100)
# save(year3, file="~/../Dropbox/negative_population_trends/10ksims_freq5_spp20_numyears100.Rdata")


