# Biased census sampling code for Portal data

# Created by Easton R White
# Last edited 15-Apr-2019

# This script evaluates each common species in the Portal data and finds the slope of the two most common plots over time with and without the first five years of data.
# This script is used in the "Empirical_Investigation" R markdown file

# Function to set x limits later
if (first5=='yes'){
  RANGES = NULL
  DIFF_COMMON_AND_RANDOM = NULL
  #ranking_common_plots = NULL
}


i=1
for (species_name in common_rodents[1:length(common_rodents)]){

# Subset data for single species and compile data for all plots by year
single_species <- f_rodent %>%
         filter(species == species_name)

single_species_by_year <- single_species %>%
  group_by(year) %>%
  summarize(count=sum(count))


#### 
common_plots <- single_species %>%
  filter(year<(single_species$year[which(is.na(single_species$count)==F)[1]]+1)) %>%
  group_by(plot) %>%
  summarize(count = sum(count))



ranking_common_plots <- common_plots[order(common_plots$count,decreasing = T),]

print(species_name)
print(ranking_common_plots)

ranking_common_plots <- c(ranking_common_plots$plot[1:2])
#print(ranking_common_plots)
#########

#source('Empirical_Analyses/scripts/script_for_common_plots.R')

#### Function for ignoring first five years
if (first5=='no'){
single_species <- single_species %>%
  filter(year>(single_species$year[which(is.na(single_species$count)==F)[1]]+4))


single_species_by_year <- single_species_by_year %>%
  filter(year>(single_species_by_year$year[1]+4))
}

if (first5=='yes'){
single_species <- single_species %>%
  filter(year<as.numeric(names(table(single_species$year))[length(names(table(single_species$year)))])-4)

single_species_by_year <- single_species_by_year %>%
  filter(year<(single_species_by_year$year[length(single_species_by_year$year)]-4))

}

######


# Calculate slope for total population change
trend_for_whole_pop <- coef(lm(single_species_by_year$count ~ single_species_by_year$year))[2]
assign(x = paste(species_name,'_whole_pop_trend','_first5_is_',first5,sep=''),as.numeric(trend_for_whole_pop))


# Sample random slopes
slopes = replicate(50,random_sampling(single_species),simplify = TRUE)
assign(x = paste(species_name,'_slopes','_first5_is_',first5,sep=''),as.numeric(slopes[1,]))




single_species_common <- single_species %>% 
  filter(plot %in% ranking_common_plots) %>%
  group_by(year) %>%
  summarize(count = sum(count))

slope_coefficient = as.numeric(extract_lm_values(single_species_common$year,single_species_common$count)[1])
###### REMOVE CHUNCK$#

assign(x = paste(species_name,'common_plots_slope','_first5_is_',first5,sep=''),mean(slope_coefficient))




# Assign values for the range of the plot
if (first5=='yes'){
  RANGES = rbind(RANGES,range(as.numeric(slopes[1,]))*1.2)
  DIFF_COMMON_AND_RANDOM = rbind(DIFF_COMMON_AND_RANDOM, abs(mean(slope_coefficient) - mean(as.numeric(slopes[1,]))))
}

i = i+1

}






