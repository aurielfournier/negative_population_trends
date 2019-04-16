
# Load data cleaning packages
require(dplyr)
require(tidyr)

rodent <- rodent[!is.na(rodent$species),]

# Select most common species for only control plots (2,4,8,11,12,14,17,22) - See Christensen et al. 2018 paper
f_rodent <- rodent %>%
  select(month,year,plot,species) %>%
  filter(plot %in% c(2,4,8,11,12,14,17,22)) %>% # Look at control plots only
  filter(month %in% 1:12) %>% # Look at all months
  group_by(year,species,plot) %>%
  summarize(count = n())

common_rodents <- f_rodent %>%
  group_by(species) %>%
  #filter(year==1977) %>%
  summarize(total_count = sum(count))

common_rodents_in_1977 <- f_rodent %>%
  group_by(species) %>%
  filter(year==1977) %>%
  summarize(total_count = sum(count))

#common_rodents <- names(sort(table(f_rodent$species),decreasing=T)[sort(table(f_rodent$species)>27,decreasing=T)==T])
common_rodents <- as.character(common_rodents_in_1977$species[which(common_rodents_in_1977$total_count>=5)]) 
                                                                    #& (common_rodents_in_1977$species %in% common_rodents_in_1977$species))])
common_rodents <- common_rodents[common_rodents != ""]



f_rodent <- f_rodent %>%
  filter(species %in% common_rodents)


f_rodent$species = droplevels(f_rodent$species)


# I need to fill in for missing years as well
# Fill in missing plots with 0 counts for each species
#f_rodent <- f_rodent %>% complete(nesting(species,plot=1:24,year=1977:2017),fill = list(count = 0) )
#f_rodent <- f_rodent %>% complete(year = 1977:2017,fill = list(count = 0))

f_rodent <- f_rodent %>%
  group_by(species) %>%
  complete(year=1977:2017,plot=c(2,4,8,11,12,14,17,22),fill=list(count=0))
