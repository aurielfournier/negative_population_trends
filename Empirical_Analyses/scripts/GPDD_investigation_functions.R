

# Scripts from time series paper
calculate_slope= function(x){
  return(as.numeric(coef(lm(c(x)~c(1:length(x))))[2]))
}
calculate_p_value = function(x){
  return(summary(lm(c(x)~c(1:length(x))))$coefficients[2,4])
}




# For "all_time_slope", should I use full time series, start of time series, but of length of high point time seies, or a random time series of the high point time series length
# calculate_slopes_max = function(x){
#   x = x/max(x)
#   high_point = which(x==max(x))
#   
#   if (high_point < (length(x)/2)){
#     high_point_slope = calculate_slope(x[high_point:length(x)])
#     all_time_slope = calculate_slope(x[1:length(high_point:length(x))])
#     return(cbind(all_time_slope,high_point_slope))
#   }else{
#     return(cbind(NA,NA))
#   }
# }


# This alternative function, samples either from the high point forward or on either side of the high point. In both cases, the population is sampled for 15 years to ensure power (White et al. 2017)
calculate_slopes_max = function(x){
  if (length(x)>34){ # Ensure time series is 35+ years
    high_point = which(x==max(x))[1] # Determine high point of time series
    
    # Determine if high point falls within middle of time series. Then make slope calculations for two scenarios: 1) sampling for 15 years starting at the high point, or 2) sampling for 15 total years with the high point in the middle of the time series.
    if (high_point < 28 & high_point>7){
      high_point_slope = calculate_slope(x[high_point:(high_point+14)])
      all_time_slope = calculate_slope(x[(high_point-7):(high_point+7)])
      random_number = sample(1:20,size = 1)
      random_slope = calculate_slope(x[random_number:(random_number+14)])
      return(cbind(all_time_slope,high_point_slope,random_slope))
    }else{
      return(cbind(NA,NA))
    }
  }else{
    return(cbind(NA,NA))
  }
}
