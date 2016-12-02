

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

realtrend <- function(nspeciestopick=2){
twospecies <- as.data.frame(tdat[,which(tdat[1,]==sort(tdat[1,], TRUE)[1:nspeciestopick])]) %>% 
                  bind_cols(data.frame(year=1:100))

values <- data.frame(beta=c(NA,NA),pvalue=c(NA,NA))

speciesmodel1 <- lm(data=twospecies, V1 ~ year)
speciesmodel2 <- lm(data=twospecies, V2 ~ year)

values[1,1] <- speciesmodel1$coefficients[2]
values[1,2] <- summary(speciesmodel1)$coefficients[,4][2]

values[2,1] <- speciesmodel2$coefficients[2]
values[2,2] <- summary(speciesmodel2)$coefficients[,4][2]

return(values)
}

##################################################################################################

samplingeffort <- function(frequency=5){
  
  twospecies <- as.data.frame(tdat[,which(tdat[1,]==sort(tdat[1,], TRUE)[1:nspeciestopick])]) %>% 
    bind_cols(data.frame(year=1:100))
  
  sampled_data <- twospecies[seq(from=1, to=nyears, by=frequency),]
  
  return(sampled_data)
}

##################################################################################################

sampletrend <- function(){
  sampled_values <- data.frame(beta=c(NA,NA),pvalue=c(NA,NA))
  
  speciesmodel1 <- lm(data=sdat, V1 ~ year)
  speciesmodel2 <- lm(data=sdat, V2 ~ year)
  
  sampled_values[1,1] <- speciesmodel1$coefficients[2]
  sampled_values[1,2] <- summary(speciesmodel1)$coefficients[,4][2]
  
  sampled_values[2,1] <- speciesmodel2$coefficients[2]
  sampled_values[2,2] <- summary(speciesmodel2)$coefficients[,4][2]
  
  return(sampled_values)
}

###################################################################


theproblem <- function(nspp=20, nyears=100, mu=1000,rho=0.5,CV=0.2,plot=F, K=2000, frequency=5,nspeciestopick=2){
  tdat <- twenty_species()
  values <- realtrend()
  sdat <- samplingeffort()
  svalues <- sampletrend()
  outputs <- list()
  outputs[["twenty_species"]] <- tdat
  outputs[["values"]] <- values
  outputs[["sampled_data"]] <- sdat
  outputs[["sampled_values"]] <- svalues
  return(outputs)
}

theproblem1000 <- function(sims=1000){
  values <- data.frame(beta=rep(NA,sims*2),pvalue=rep(NA,sims*2))
  svalues <- data.frame(beta=rep(NA,sims*2),pvalue=rep(NA,sims*2))
  
  for(i in seq(1,sims,by=2)){
    dd <- theproblem()
    values[i:(i+1),] <- dd$values
    svalues[i:(i+1),] <- dd$values
  }
  
  outputs <- list()
  outputs[["values"]] <- values
  outputs[["svalues"]] <- svalues
  
  return(outputs)
  
}