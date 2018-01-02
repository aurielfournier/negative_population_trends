

# Sample from common plots only

#determine highest abundance plots in first 5 years
common_plots <- single_species %>%
  filter(year<(single_species$year[which(is.na(single_species$count)==F)[1]]+1)) %>%
  group_by(plot) %>%
  summarize(count = sum(count))

ranking_common_plots <- common_plots[order(common_plots$count,decreasing = T),]
#print(ranking_common_plots)



# Need to rank commonness with only the first couple of years of sampling

if (ranking_common_plots$count[2]!=ranking_common_plots$count[3]){
  
  
  single_species_common <- single_species %>% 
    filter(plot %in% ranking_common_plots$plot[1:2]) %>%
    group_by(year) %>%
    summarize(count = sum(count))
  
  if (first5=='no'){
    single_species_common <- single_species_common %>%
      filter(year > (single_species_common$year[which(is.na(single_species$count)==F)[1]] + 5)) 
  }
  
  slope_coefficient = as.numeric(extract_lm_values(single_species_common$year,single_species_common$count)[1])
}else if (ranking_common_plots$count[1]!=ranking_common_plots$count[2]){
  plots_to_average_over = which(ranking_common_plots$count==ranking_common_plots$count[2])
  slope_coefficient = NULL
  for (plot_num in plots_to_average_over){
    single_species_common <- single_species %>% 
      filter(plot %in% ranking_common_plots$plot[c(1,plot_num)]) %>%
      group_by(year) %>%
      summarize(count = sum(count))
    
    if (first5=='no'){
      single_species_common <- single_species_common %>%
        filter(year > (single_species_common$year[which(is.na(single_species$count)==F)[1]] + 5)) 
    }
    
    slope_coefficient = rbind(slope_coefficient, as.numeric(extract_lm_values(single_species_common$year,single_species_common$count)[1]))
    
  }# end of for loop
}else{
  plots_to_average_over = which(ranking_common_plots$count==ranking_common_plots$count[2])
  pairs_of_plots = t(combn(plots_to_average_over, 2))
  slope_coefficient = NULL
  for (plot_num in 1:nrow(pairs_of_plots)){
    single_species_common <- single_species %>% 
      filter(plot %in% ranking_common_plots$plot[pairs_of_plots[plot_num,]]) %>%
      group_by(year) %>%
      summarize(count = sum(count))
    
    #points(single_species_common,type='l',col=plot_num)
    if (first5=='no'){
      single_species_common <- single_species_common %>%
        filter(year > (single_species_common$year[which(is.na(single_species$count)==F)[1]] + 5)) 
    }
    
    slope_coefficient = rbind(slope_coefficient, as.numeric(extract_lm_values(single_species_common$year,single_species_common$count)[1]))
    
  }# end of for loop
}

#print(slope_coefficient)
