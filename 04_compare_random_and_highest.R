library(ggplot2)
library(tidyverse)

dat2 <- read.csv("10ksims_freq1_spp20_nyears2.csv") %>%
  mutate(bar = factor(bar),
         years=2)

dat5 <- read.csv("10ksims_freq1_spp20_nyears5.csv") %>%
  mutate(bar = factor(bar),
         years=5)

dat10 <- read.csv("10ksims_freq1_spp20_nyears10.csv") %>%
  mutate(bar = factor(bar),
         years=10)

dat20 <- read.csv("10ksims_freq1_spp20_nyears20.csv") %>%
  mutate(bar = factor(bar),
         years=20)

dat50 <- read.csv("10ksims_freq1_spp20_nyears50.csv") %>%
  mutate(bar = factor(bar),
         years=50)

dat100 <- read.csv("10ksims_freq1_spp20_nyears100.csv") %>%
  mutate(bar = factor(bar),
         years=100)

dat <- rbind(dat2, dat5, dat10, 
             dat20, dat50, dat100) %>%
          mutate(bar = factor(bar))

model <- glm(data=dat, beta ~ years * bar)

summary(model)
