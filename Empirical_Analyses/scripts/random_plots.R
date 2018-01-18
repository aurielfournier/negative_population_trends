# Created by Easton R. White
# Last edited 14-Dec-2017

# Function that samples random plots for a single species. The function returns the regression coefficient for a 
          
random_sampling = function(single_species){
  random_plots = base::sample(1:24,2,replace=F) # Subsample plots
  single_species <- single_species %>%
    filter(species == species_name,plot %in% random_plots)

  single_species_by_year <- single_species %>%
    group_by(year) %>%
    summarize(count=sum(count))
  
  return(extract_lm_values(single_species_by_year$year,single_species_by_year$count))
}


