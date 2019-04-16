
# Created by Easton R. White
# Last edited 12-Dec-2017

# Build NULL distribution for expected distribution of slopes for analysis of removing initial years from time series data

source('Empirical_Analyses/scripts/script_remove_initial_years.R')

null_trend = vector('numeric',10000)
for (time_series in 1:length(null_trend)){
  # Run a stochastic simulation of declining population 
  #population = 100 + runif(n = 1,min=-3,max=0)*(1:30) + rnorm(30,mean=0,sd = runif(1,min=0.1,max=3))
  population = 100*exp(runif(n = 1,min=-0.05,max=0)*(1:30)) + rnorm(30,mean=0,sd = runif(1,min=0.1,max=5))
  population=log(population)
  
 population=(population - min(population))/(max(population) - min(population))
  
  estimated_slopes = calculate_slopes_removing_intial_years(population)
  
  if (any(estimated_slopes>0) | calculate_p_value(population)>0.05){
    #trend[j]=NA
    #bias[j]=NA
    null_trend[time_series] = NA
  }else{
    # Calculate the bias from year 10 versys year 1 
    #bias[j]= estimated_slopes[1]/estimated_slopes[11]
    
    # Standardize the values.
    estimated_slopes = (estimated_slopes - min(estimated_slopes))/(max(estimated_slopes) - min(estimated_slopes))
    #plot(estimated_slopes)  
    #plot(outputs,type='l',col=rgb(0.5,0.5,0.5,0.4))
    # a positive value for trend would indicate that slope estimate increases with more time sampled
    # abline(lm(estimated_slopes~c(1:15)),col=rgb(0.5,0.5,0.5,0.5))
    intercept = as.numeric(coef(lm(estimated_slopes~c(1:15)))[1])
    slope = as.numeric(coef(lm(estimated_slopes~c(1:15)))[2])
   
    # a positive value for trend would indicate that slope estimate increases with more time sampled
    null_trend[time_series] = slope
  }
  #print(null_trend[time_series])
}


# Build a plot
null_data=hist(null_trend,breaks=50,freq = T,las=1,cex.lab=1.4,xlab='slope',ylab='frequency',plot = F)
#box()
#points(100*density(null_trend,na.rm = T))

