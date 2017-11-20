library(ggplot2)
library(tidyverse)
library(auriel)

br = 3

dat2 <- read.csv("10ksims_freq1_spp20_nyears2.csv") %>%
  filter(bar==br) %>% 
  mutate(bar = factor(bar),
         years=2)

dat5 <- read.csv("10ksims_freq1_spp20_nyears5.csv") %>%
  filter(bar==br) %>% 
  mutate(bar = factor(bar),
         years=5)

dat10 <- read.csv("10ksims_freq1_spp20_nyears10.csv") %>%
  filter(bar==br) %>% 
  mutate(bar = factor(bar),
         years=10)

dat20 <- read.csv("10ksims_freq1_spp20_nyears20.csv") %>%
  filter(bar==br) %>% 
  mutate(bar = factor(bar),
         years=20)

dat50 <- read.csv("10ksims_freq1_spp20_nyears50.csv") %>%
  filter(bar==br) %>% 
  mutate(bar = factor(bar),
         years=50)

dat100 <- read.csv("10ksims_freq1_spp20_nyears100.csv") %>%
  filter(bar==br) %>% 
  mutate(bar = factor(bar),
         years=100)

dat <- rbind(dat2, dat5, dat10, 
             dat20, dat50, dat100) %>%
  mutate(years=factor(years))# %>%
#  filter(beta>=-400&beta<=400)

b <- ggplot(data=dat, aes(x=years, y=beta))+
  geom_boxplot()+
  theme_krementz()+
  geom_hline(aes(yintercept=0))+
  ylab("Regression Slope")+
  xlab("Length of Time \nSeries (years)")


datpercent <- dat %>%
              mutate(negative = ifelse(beta<0,1,0)) %>%
              group_by(years) %>%
              summarize(total = n(),
                        declining = sum(negative),
                        percent = declining/total)

a <- ggplot(data=datpercent, aes(x=years, y=percent))+
      geom_col()+
      ylab("Percent Declines \nDetected")+
      theme_krementz()+
      xlab("Length of Time \nSeries (Years)")+
      ylim(0,1)

ggsave(a, file="~/negative_population_trends/figure1.jpeg", width=10, height=10, units="cm")

ggsave(b, file="~/negative_population_trends/figure2.jpeg", width=10, height=10, units="cm")
