
# Created by Easton R. White
# Last edited 12-Dec-2017

# Build NULL distribution for expected distribution of slopes for analysis of removing initial years from time series data

null_trend = vector('numeric',50000)
for (time_series in 1:length(null_trend)){
  # Run a stochastic simulation of declining population 
  population = 100 + runif(n = 1,min=-3,max=0)*(1:30) + rnorm(30,mean=0,sd = runif(1,min=0.1,max=3))
  
  outputs = calculate_slopes_removing_intial_years(population)
  
  # Only look at stationary, declining populations
  if (any(outputs>0)){
    null_trend[time_series]=NA
  }else{
    # Starduze the values
    outputs = (outputs - min(outputs))/(max(outputs) - min(outputs))
    
    # a positive value for trend would indicate that slope estimate increases with more time sampled
    null_trend[time_series] = as.numeric(coef(lm(outputs~c(1:15)))[2])
  }
}


# Build a plot
null_data=hist(null_trend,breaks=50,freq = T,las=1,cex.lab=1.4,xlab='slope',ylab='frequency',plot = F)
#box()
#points(100*density(null_trend,na.rm = T))

