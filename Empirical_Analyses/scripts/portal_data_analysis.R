# Biased census sampling code

# Created by Easton R White
# Last edited 17-Oct-2017

# This scripts takes data from the Portal project (link here), cleans it up, and then allows biased sampling of the data. 

# Pull in data
#setwd("~/Desktop/Research/PortalData")
#rodent <- read.csv('Rodents/Portal_rodent.csv',header=T)

# Should you include first 5 years yes (1) or no (0)?

#first5 = 'yes'

# Most common species in the data
common_rodents <- names(sort(table(rodent$species),decreasing=T)[sort(table(rodent$species)>300,decreasing=T)==T])

common_rodents <- common_rodents[common_rodents != ""]
# Load data cleaning packages
require(dplyr)
require(tidyr)

# Select most common species for data only collected in the summer
f_rodent <- rodent %>%
  select(month,year,plot,species) %>%
  filter(species %in% common_rodents,month %in% 5:7) %>%
  group_by(month,year,species,plot) %>%
  summarize(count = n()) 


# Fill in missing plots with 0 counts for each species
f_rodent <- f_rodent %>% complete(plot = 1:24,fill = list(count = 0) )
#f_rodent <- f_rodent %>% complete(year = 1978:2017,fill = list(count = NA))






#par(mfrow=c(9,2),mar=c(1,2.5,1,0),oma=c(4.5,4,1.5,0.5),mgp = c(3, 0.4, 0))
if (first5=='no'){
  RANGES = NULL
  DIFF_COMMON_AND_RANDOM = NULL
}
i=1
for (species_name in common_rodents[1:length(common_rodents)]){

# Subset data for single species and compile data for all plots by year
single_species <- f_rodent %>%
         filter(species == species_name)

single_species_by_year <- single_species %>%
  group_by(year) %>%
  summarize(count=sum(count))



source('scripts/script_for_common_plots.R')

#### Function for ignoring first five years
if (first5=='no'){
single_species <- single_species %>%
  filter(year>(single_species$year[which(is.na(single_species$count)==F)[1]]+5))
}
######


# Sample random slopes
slopes = replicate(300,random_sampling(single_species),simplify = TRUE)
assign(x = paste(species_name,'_slopes','_first5_is_',first5,sep=''),as.numeric(slopes[1,]))

# Plot histogram or density plot
#hist(as.numeric(slopes[1,]),breaks=30,main='',xlab='estimated slope coefficients',
#     xlim=c(-1.5,1.5),las=1,freq = TRUE)

#plot(density(as.numeric(slopes[1,])),main='',
#     xlim=range(as.numeric(slopes[1,]))*1.2,
#     xlab='estimated slope coefficients')

assign(x = paste(species_name,'common_plots_slope','_first5_is_',first5,sep=''),mean(slope_coefficient))
#sum(as.numeric(slopes[1,])>ABcommon_plots_slope_first5_is_no)
#abline(v=ABcommon_plots_slope_first5_is_no,col='red',lwd=3)
#abline(v=mean(as.numeric(slopes[1,])), col='black',lwd=2)

#mtext(paste(species_name,sep=''),3,line = -1.5,adj = 0.05)

if (first5=='no'){
  RANGES = rbind(RANGES,range(as.numeric(slopes[1,]))*1.2)
  DIFF_COMMON_AND_RANDOM = rbind(DIFF_COMMON_AND_RANDOM, abs(mean(slope_coefficient) - mean(as.numeric(slopes[1,]))))
}

i = i+1

}

#axis(1,at = seq(-1.5,1.5,0.25),labels = seq(-1.5,1.5,0.25))
# mtext('frequency',2,line=1,outer=T,adj=0.5,cex=1.2)
# mtext('estimated slope coefficients',1,line=2.5,outer=T,adj=0.52,cex=1.2)
# 
# if (first5=='yes'){
# mtext('with the first 5 years',3,line=0,outer=T,adj=0.5,cex=1.4)
# }else{
# mtext('without the first 5 years',3,line=0,outer=T,adj=0.5,cex=1.4)  
#}




