library(tidyverse)

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears2.Rdata")

y2bar1 <- year1$real_values_random %>% 
              mutate(bar="True Population Size, 2 Random Populations")

y2bar2 <- year1$real_values_highest %>% 
              mutate(bar="True Population Size, 2 Highest Populations")

y2bar3 <- year1$sampled_values_random %>% 
              mutate(bar="Sampled Population Size, 2 Random Populations")

y2bar4 <- year1$sampled_values_highest %>% 
              mutate(bar="Sampled Population Size, 2 Highest Populations")


y2 <- rbind(y2bar1, y2bar2, y2bar3, y2bar4)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears2.csv", 
          row.names = FALSE)

##

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears5.Rdata")

y2bar1 <- year1$real_values_random %>% 
            mutate(bar="True Population Size, 2 Random Populations")

y2bar2 <- year1$real_values_highest %>% 
            mutate(bar="True Population Size, 2 Highest Populations")

y2bar3 <- year1$sampled_values_random %>% 
            mutate(bar="Sampled Population Size, 2 Random Populations")

y2bar4 <- year1$sampled_values_highest %>% 
            mutate(bar="Sampled Population Size, 2 Highest Populations")

y5 <- rbind(y2bar1, y2bar2, y2bar3, y2bar4)

write.csv(y5, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears5.csv", 
          row.names = FALSE)
##

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears10.Rdata")

y2bar1 <- year1$real_values_random %>% 
            mutate(bar="True Population Size, 2 Random Populations")

y2bar2 <- year1$real_values_highest %>% 
            mutate(bar="True Population Size, 2 Highest Populations")

y2bar3 <- year1$sampled_values_random %>% 
            mutate(bar="Sampled Population Size, 2 Random Populations")

y2bar4 <- year1$sampled_values_highest %>% 
            mutate(bar="Sampled Population Size, 2 Highest Populations")

y10 <- rbind(y2bar1, y2bar2, y2bar3, y2bar4)

write.csv(y10, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears10.csv", row.names = FALSE)

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears20.Rdata")

y2bar1 <- year1$real_values_random %>% 
            mutate(bar="True Population Size, 2 Random Populations")

y2bar2 <- year1$real_values_highest %>% 
            mutate(bar="True Population Size, 2 Highest Populations")

y2bar3 <- year1$sampled_values_random %>% 
            mutate(bar="Sampled Population Size, 2 Random Populations")

y2bar4 <- year1$sampled_values_highest %>% 
            mutate(bar="Sampled Population Size, 2 Highest Populations")

y20 <- rbind(y2bar1, y2bar2, y2bar3, y2bar4)

write.csv(y20, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears20.csv", row.names = FALSE)

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears50.Rdata")

y2bar1 <- year1$real_values_random %>% 
            mutate(bar="True Population Size, 2 Random Populations")

y2bar2 <- year1$real_values_highest %>% 
            mutate(bar="True Population Size, 2 Highest Populations")

y2bar3 <- year1$sampled_values_random %>% 
            mutate(bar="Sampled Population Size, 2 Random Populations")

y2bar4 <- year1$sampled_values_highest %>% 
            mutate(bar="Sampled Population Size, 2 Highest Populations")

y50 <- rbind(y2bar1, y2bar2, y2bar3, y2bar4)

write.csv(y50, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears50.csv", row.names = FALSE)

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears100.Rdata")

y2bar1 <- year1$real_values_random %>% 
            mutate(bar="True Population Size, 2 Random Populations")

y2bar2 <- year1$real_values_highest %>% 
            mutate(bar="True Population Size, 2 Highest Populations")

y2bar3 <- year1$sampled_values_random %>% 
            mutate(bar="Sampled Population Size, 2 Random Populations")

y2bar4 <- year1$sampled_values_highest %>% 
            mutate(bar="Sampled Population Size, 2 Highest Populations")

y100 <- rbind(y2bar1, y2bar2, y2bar3, y2bar4)

write.csv(y100, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears100.csv", row.names = FALSE)