

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears2.Rdata")


y2bar1 <- year1$real_values_all_pops %>% mutate(bar=1)
y2bar2 <- year1$sampled_values_random %>% mutate(bar=2)
y2bar3 <- year1$sampled_values_highest %>% mutate(bar=3)


y2 <- rbind(y2bar1, y2bar2, y2bar3)

write.csv(y2, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears2.csv", row.names = FALSE)

##

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears5.Rdata")

y5bar1 <- year1$real_values_all_pops %>% mutate(bar=1)
y5bar2 <- year1$sampled_values_random %>% mutate(bar=2)
y5bar3 <- year1$sampled_values_highest %>% mutate(bar=3)

y5 <- rbind(y5bar1, y5bar2, y5bar3)

write.csv(y5, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears5.csv", row.names = FALSE)
##

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears10.Rdata")

y10bar1 <- year1$real_values_all_pops %>% mutate(bar=1)
y10bar2 <- year1$sampled_values_random %>% mutate(bar=2)
y10bar3 <- year1$sampled_values_highest %>% mutate(bar=3)

y10 <- rbind(y10bar1, y10bar2, y10bar3)

write.csv(y10, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears10.csv", row.names = FALSE)

##

##


load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears20.Rdata")

y20bar1 <- year1$real_values_all_pops %>% mutate(bar=1)
y20bar2 <- year1$sampled_values_random %>% mutate(bar=2)
y20bar3 <- year1$sampled_values_highest %>% mutate(bar=3)

y20 <- rbind(y20bar1, y20bar2, y20bar3)

write.csv(y20, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears20.csv", row.names = FALSE)

##

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears50.Rdata")

y50bar1 <- year1$real_values_all_pops %>% mutate(bar=1)
y50bar2 <- year1$sampled_values_random %>% mutate(bar=2)
y50bar3 <- year1$sampled_values_highest %>% mutate(bar=3)

y50 <- rbind(y50bar1, y50bar2, y50bar3)

write.csv(y50, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears50.csv", row.names = FALSE)

##

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears100.Rdata")

y100bar1 <- year1$real_values_all_pops %>% mutate(bar=1)
y100bar2 <- year1$sampled_values_random %>% mutate(bar=2)
y100bar3 <- year1$sampled_values_highest %>% mutate(bar=3)

y100 <- rbind(y100bar1, y100bar2, y100bar3)

write.csv(y100, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears100.csv", row.names = FALSE)