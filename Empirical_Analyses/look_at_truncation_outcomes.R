




DScommon_plots_slope_first5_is_yes -mean(DS_slopes_first5_is_yes)

diff_bw_common_random = data.frame(common_rodents,first5_yes=rep(0,7),first5_no=rep(0,7))
i=1
for (species_name in common_rodents[1:length(common_rodents)]){
  
  diff_bw_common_random[i,1] = species_name
  first5='yes'
  diff_bw_common_random[i,2] = eval(as.symbol(paste(species_name,'common_plots_slope_first5_is_',first5,sep=''))) - mean(eval(as.symbol(paste(species_name,'_slopes_first5_is_',first5,sep=''))))
  #diff_bw_common_random[i,2] = eval(as.symbol(paste(species_name,'common_plots_slope_first5_is_',first5,sep=''))) 
  
  
  first5='no'
  diff_bw_common_random[i,3] = eval(as.symbol(paste(species_name,'common_plots_slope_first5_is_',first5,sep=''))) - mean(eval(as.symbol(paste(species_name,'_slopes_first5_is_',first5,sep=''))))
  #diff_bw_common_random[i,3] = mean(eval(as.symbol(paste(species_name,'_slopes_first5_is_',first5,sep=''))))
  
  
 i=i+1
}

plot(diff_bw_common_random$first5_yes,diff_bw_common_random$first5_no)
text(diff_bw_common_random$first5_yes,diff_bw_common_random$first5_no,labels = diff_bw_common_random$common_rodents,pos = 3)

segments(-100,-100,100,100)
abline(v=0)
abline(h=0)
t.test(diff_bw_common_random$first5_yes,diff_bw_common_random$first5_no,
       paired = TRUE)
wilcox.test(diff_bw_common_random$first5_yes,diff_bw_common_random$first5_no, paired = TRUE, alternative = "two.sided")


wilcox.test(diff_bw_common_random$first5_yes,paired=F,alternative = "two.sided")

t.test(diff_bw_common_random$first5_yes,diff_bw_common_random$first5_no,
       paired = TRUE)