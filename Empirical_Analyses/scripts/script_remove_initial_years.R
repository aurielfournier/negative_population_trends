

require(dplyr)

calculate_slope= function(x){
  return(as.numeric(coef(lm(c(x)~c(1:length(x))))[2]))
}
calculate_p_value = function(x){
  return(summary(lm(c(x)~c(1:length(x))))$coefficients[2,4])
}

calculate_slopes_removing_intial_years = function(population_size){
  #calculate_slope(population_size[1:15])
  slopes=vector('numeric',15)
  for (n in 1:15){
    slopes[n] = calculate_slope(population_size[n:(14+n)])
  }
  return(slopes)
}
