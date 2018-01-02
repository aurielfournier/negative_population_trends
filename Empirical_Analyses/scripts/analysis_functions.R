

# Set of functions to use in Portal analysis, including a plotting function for lm output, extract lm coefficients and p-values


make_lm_plot = function(time,count){
  plot(count~time,cex.axis=1.2,las=1,pch=16,ylab='population size',xlab='time (years)',cex.lab=1.2)
  abline(lm(count~time))
  #output = summary(lm(count~time))
  #print(cbind(output$coefficients[2,1],output$coefficients[2,4]))
}


extract_lm_values = function(time,count){
  model_summary = summary(lm(count~time))
  # Return slope coefficient and p-value of slope
  model_output = as.data.frame(cbind(model_summary$coefficients[2,1],model_summary$coefficients[2,4]))
  colnames(model_output) = c('slope_estimate','slope_p_value')
  return(model_output)
}
