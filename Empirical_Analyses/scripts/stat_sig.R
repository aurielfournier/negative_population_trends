
# Created by Easton R. White
# Last edited 14-Dec-2017


# Build a function that examines statistical significance of each plot

stat_sig <- function(random_slopes,common_slope){
  
  if (common_slope < mean(random_slopes)){
    return(sum(common_slope<random_slopes)/length(random_slopes))
  }else{
    return(sum(common_slope>random_slopes)/length(random_slopes))
  }
  
}
