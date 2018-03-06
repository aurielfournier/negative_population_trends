library(ggplot2)
library(tidyverse)
library(auriel)
truebr = 1
br = 3

dat2 <- read.csv("10ksims_freq1_spp20_nyears2.csv") %>%
  #filter(bar==br|bar==truebr) %>% 
  mutate(bar = factor(bar),
         years=2)

dat5 <- read.csv("10ksims_freq1_spp20_nyears5.csv") %>%
  #filter(bar==br|bar==truebr) %>% 
  mutate(bar = factor(bar),
         years=5)

dat10 <- read.csv("10ksims_freq1_spp20_nyears10.csv") %>%
  #filter(bar==br|bar==truebr) %>% 
  mutate(bar = factor(bar),
         years=10)

dat20 <- read.csv("10ksims_freq1_spp20_nyears20.csv") %>%
  #filter(bar==br|bar==truebr) %>% 
  mutate(bar = factor(bar),
         years=20)

dat50 <- read.csv("10ksims_freq1_spp20_nyears50.csv") %>%
  #filter(bar==br|bar==truebr) %>% 
  mutate(bar = factor(bar),
         years=50)

dat100 <- read.csv("10ksims_freq1_spp20_nyears100.csv") %>%
  #filter(bar==br|bar==truebr) %>% 
  mutate(bar = factor(bar),
         years=100)

dat <- rbind(dat2, dat5, dat10, 
             dat20, dat50, dat100) %>%
  mutate(years=factor(years))# %>%
#  filter(beta>=-400&beta<=400)

levelskeep <- levels(dat$bar)[3:4]

b <- dat %>% 
        filter(bar %in% levelskeep) %>% 
  ggplot(aes(x=years, y=beta))+
  geom_boxplot()+
  theme_krementz()+
  geom_hline(aes(yintercept=0))+
  ylab("Regression Slope")+
  xlab("Length of Time \nSeries (years)")+
  facet_wrap(~bar)+
  ylim(-500,500)+
  theme(strip.text = element_blank())


datpercent <- dat %>%
              mutate(negative = ifelse(beta<0,1,0)) %>%
              group_by(years, bar) %>%
              summarize(total = n(),
                        declining = sum(negative),
                        percent = declining/total) 
              

a <- datpercent %>% 
  filter(bar %in% levelskeep) %>% 
  mutate(bar = as.character(bar),
         bar = ifelse(bar=="True Population Size, 2 Highest Populations",
                      "Two Largest Populations, Year 1", bar),
         bar = ifelse(bar=="True Population Size, 2 Random Populations",
                      "Two Random Populations, Year 1", bar)) %>% 
  ggplot(aes(x=years, y=percent))+
      geom_col()+
      ylab("Percent Declines")+
      theme_krementz()+
      xlab("Length of Time \nSeries (Years)")+
      ylim(0,1)+
      facet_wrap(~bar) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())

jpeg(file="~/negative_population_trends/sim_fig_combined.jpeg", width=25, height=15, units="cm", res=300)
cowplot::plot_grid(a,b,nrow=2, align = "h")
dev.off()
