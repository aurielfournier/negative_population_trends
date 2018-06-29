library(tidyverse)

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears2.Rdata")

y2bar1 <- f1s20y2$real_values_random_linear %>% 
              mutate(bar="True Population Size, 2 Random Populations",
                     model = "linear")

y2bar2 <- f1s20y2$real_values_highest_linear %>% 
              mutate(bar="True Population Size, 2 Highest Populations",
                     model = "linear")

y2bar1log <- f1s20y2$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y2$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears2.csv", 
          row.names = FALSE)

##

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears5.Rdata")

y5bar1 <- f1s20y5$real_values_random_linear %>% 
            mutate(bar="True Population Size, 2 Random Populations",
                   model = "linear")

y5bar2 <- f1s20y5$real_values_highest_linear %>% 
            mutate(bar="True Population Size, 2 Highest Populations",
                   model = "linear")

y5bar1log <- f1s20y5$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y5bar2log <- f1s20y5$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")

y5 <- rbind(y5bar1, y5bar2, y5bar1log, y5bar2log)

write.csv(y5, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears5.csv", 
          row.names = FALSE)
##

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears10.Rdata")

y10bar1 <- f1s20y10$real_values_random_linear %>% 
            mutate(bar="True Population Size, 2 Random Populations",
                   model = "linear")

y10bar2 <- f1s20y10$real_values_highest_linear %>% 
            mutate(bar="True Population Size, 2 Highest Populations",
                   model = "linear")

y10bar1log <- f1s20y10$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y10bar2log <- f1s20y10$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")

y10 <- rbind(y10bar1, y10bar2, y10bar1log, y10bar2log)

write.csv(y10, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears10.csv", row.names = FALSE)

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears20.Rdata")

y20bar1 <- f1s20y20$real_values_random_linear %>% 
            mutate(bar="True Population Size, 2 Random Populations",
                   model = "linear")

y20bar2 <- f1s20y20$real_values_highest_linear %>% 
            mutate(bar="True Population Size, 2 Highest Populations",
                   model = "linear")

y20bar1log <- f1s20y20$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y20bar2log <- f1s20y20$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y20 <- rbind(y20bar1, y20bar2, y20bar1log, y20bar2log)

write.csv(y20, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears20.csv", row.names = FALSE)

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears50.Rdata")

y50bar1 <- f1s20y50$real_values_random_linear %>% 
            mutate(bar="True Population Size, 2 Random Populations",
                   model = "linear")

y50bar2 <- f1s20y50$real_values_highest_linear %>% 
            mutate(bar="True Population Size, 2 Highest Populations",
                   model = "linear")

y50bar1log <- f1s20y50$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y50bar2log <- f1s20y50$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")

y50 <- rbind(y50bar1, y50bar2, y50bar1log, y50bar2log)

write.csv(y50, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears50.csv", row.names = FALSE)

##

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears100.Rdata")

y100bar1 <- f1s20y100$real_values_random_linear %>% 
            mutate(bar="True Population Size, 2 Random Populations",
                   model = "linear")

y100bar2 <- f1s20y100$real_values_highest_linear %>% 
            mutate(bar="True Population Size, 2 Highest Populations",
                   model = "linear")

y100bar1log <- f1s20y100$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y100bar2log <- f1s20y100$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")

y100 <- rbind(y100bar1, y100bar2, y100bar1log, y100bar2log)

write.csv(y100, file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears100.csv", row.names = FALSE)




# Top 5


library(tidyverse)

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears2_top5.Rdata")

y2bar1 <- f1s20y2top5$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y2top5$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y2top5$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y2top5$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears2_top5.csv", 
          row.names = FALSE)


load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears5_top5.Rdata")

y2bar1 <- f1s20y5top5$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y5top5$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y5top5$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y5top5$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears5_top5.csv", 
          row.names = FALSE)

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears10_top5.Rdata")

y2bar1 <- f1s20y10top5$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y10top5$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y10top5$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y10top5$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears10_top5.csv", 
          row.names = FALSE)


load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears20_top5.Rdata")

y2bar1 <- f1s20y20top5$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y20top5$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y20top5$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y20top5$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears20_top5.csv", 
          row.names = FALSE)


load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears50_top5.Rdata")

y2bar1 <- f1s20y50top5$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y50top5$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y50top5$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y50top5$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears50_top5.csv", 
          row.names = FALSE)


load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears100_top5.Rdata")

y2bar1 <- f1s20y100top5$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y100top5$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y100top5$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y100top5$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears100_top5.csv", 
          row.names = FALSE)




# top 10 


library(tidyverse)

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears2_top10.Rdata")

y2bar1 <- f1s20y2top10$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y2top10$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y2top10$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y2top10$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears2_top10.csv", 
          row.names = FALSE)


load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears5_top10.Rdata")

y2bar1 <- f1s20y5top10$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y5top10$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y5top10$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y5top10$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears5_top10.csv", 
          row.names = FALSE)

load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears10_top10.Rdata")

y2bar1 <- f1s20y10top10$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y10top10$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y10top10$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y10top10$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears10_top10.csv", 
          row.names = FALSE)


load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears20_top10.Rdata")

y2bar1 <- f1s20y20top10$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y20top10$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y20top10$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y20top10$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears20_top10.csv", 
          row.names = FALSE)


load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears50_top10.Rdata")

y2bar1 <- f1s20y50top10$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y50top10$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y50top10$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y50top10$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears50_top10.csv", 
          row.names = FALSE)


load("~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_numyears100_top10.Rdata")

y2bar1 <- f1s20y100top10$real_values_random_linear %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "linear")

y2bar2 <- f1s20y100top10$real_values_highest_linear %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "linear")

y2bar1log <- f1s20y100top10$real_values_random_log %>% 
  mutate(bar="True Population Size, 2 Random Populations",
         model = "log")

y2bar2log <- f1s20y100top10$real_values_highest_log %>% 
  mutate(bar="True Population Size, 2 Highest Populations",
         model = "log")


y2 <- rbind(y2bar1, y2bar2, y2bar1log, y2bar2log)

write.csv(y2, 
          file="~/../Dropbox/negative_population_trends/10ksims_freq1_spp20_nyears100_top10.csv", 
          row.names = FALSE)