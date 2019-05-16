library(ggplot2)
library(tidyverse)

truebr = 1
br = 3

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
  mutate(years=factor(years)) %>%
  filter(model == "log")


datbar1 <- dat %>% filter(bar=="True Population Size, 2 Highest Populations")

datbar2 <- dat %>% filter(bar=="True Population Size, 2 Random Populations")

b1 <-  ggplot(data=datbar1, aes(x=years, y=beta))+
  geom_boxplot()+
  geom_hline(aes(yintercept=0))+
  ylab("Regression slope")+
  xlab("Length of time series (years)")+
  facet_wrap(~bar)+
  #ylim(-500,500)+
  theme(axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 20), 
        plot.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA), 
        title = element_text(size = 20), 
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_line(colour = "black"), 
        strip.background = element_rect(fill = "white", 
                                       color = "black"), 
        strip.text = element_blank())

b2 <-  ggplot(data=datbar2, aes(x=years, y=beta))+
  geom_boxplot()+
  geom_hline(aes(yintercept=0))+
  ylab("Regression slope")+
  xlab("Length of time series (years)")+
  facet_wrap(~bar)+
  #ylim(-500,500)+
  theme(axis.text.x = element_text(size = 12, color = "black"), 
        #axis.text.y = element_text(size = 12, color = "black"), 
        axis.title.y = element_blank(), 
        axis.text.y=element_blank(),
        plot.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA), 
        title = element_text(size = 20), 
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_line(colour = "black"), 
        strip.background = element_rect(fill = "white", 
                                        color = "black"), 
        strip.text = element_blank())


datpercent <- dat %>%
              mutate(negative = ifelse(beta<0,1,0)) %>%
              group_by(years, bar) %>%
              summarize(total = n(),
                        declining = sum(negative),
                        percent = declining/total) 
              

a <- datpercent %>% 
  mutate(bar = as.character(bar),
         bar = ifelse(bar=="True Population Size, 2 Highest Populations",
                      "Two Largest Populations, Year 1", bar),
         bar = ifelse(bar=="True Population Size, 2 Random Populations",
                      "Two Random Populations, Year 1", bar))

a1dat <- a %>% filter(bar == "Two Largest Populations, Year 1")

a2dat <- a %>% filter(bar =="Two Random Populations, Year 1")



 
a1 <-   ggplot(data=a1dat, aes(x=years, y=percent))+
      geom_col()+
      ylab("Proportion declining")+
      xlab("Length of Time \nSeries (Years)")+
      ylim(0,1)+
      facet_wrap(~bar) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size = 12, color = "black"), 
            axis.title.y = element_text(size = 20), 
            plot.background = element_blank(), 
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = NA), 
            panel.grid.minor = element_line(colour = NA), 
            title = element_text(size = 20), 
            panel.background = element_rect(fill = "white"), 
            axis.line.x = element_line(colour = "black"), 
            axis.line.y = element_line(colour = "black"), 
            strip.background = element_rect(fill="white",color="black"),
            strip.text = element_text(size = 15))



a2 <-   ggplot(data=a2dat, aes(x=years, y=percent))+
  geom_col()+
  ylab("Proportion declining")+
  xlab("Length of Time \nSeries (Years)")+
  ylim(0,1)+
  facet_wrap(~bar) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text.y = element_text(size = 12, color = "black"), 
        #axis.title.y = element_text(size = 20), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        plot.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA), 
        title = element_text(size = 20), 
        panel.background = element_rect(fill = "white"), 
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_line(colour = "black"), 
        strip.background = element_rect(fill="white",color="black"),
        strip.text = element_text(size = 15))


jpeg(file="~/negative_population_trends/sim_fig_combined_top2.jpeg", width=25, height=15, units="cm", res=600)
cowplot::plot_grid(a1,a2,b1,b2,nrow=2, align = "h",
                   labels=c("a","b","c","d"), 
                   label_x=c(0.15, 0.025, 0.15, 0.025),
                   label_y=0.97)
dev.off()

#

# ##### Top 5

#

truebr = 1
br = 3

dat2 <- read.csv("10ksims_freq1_spp20_nyears2_top5.csv") %>%
  mutate(bar = factor(bar),
         years=2)

dat5 <- read.csv("10ksims_freq1_spp20_nyears5_top5.csv") %>%
  mutate(bar = factor(bar),
         years=5)

dat10 <- read.csv("10ksims_freq1_spp20_nyears10_top5.csv") %>%
  mutate(bar = factor(bar),
         years=10)

dat20 <- read.csv("10ksims_freq1_spp20_nyears20_top5.csv") %>%
  mutate(bar = factor(bar),
         years=20)

dat50 <- read.csv("10ksims_freq1_spp20_nyears50_top5.csv") %>%
  mutate(bar = factor(bar),
         years=50)

dat100 <- read.csv("10ksims_freq1_spp20_nyears100_top5.csv") %>%
  mutate(bar = factor(bar),
         years=100)

dat <- rbind(dat2, dat5, dat10, 
             dat20, dat50, dat100) %>%
  mutate(years=factor(years)) %>%
  filter(model == "log")


b <-  ggplot(data=dat, aes(x=years, y=beta))+
  geom_boxplot()+
  geom_hline(aes(yintercept=0))+
  ylab("Regression Slope")+
  xlab("Length of Time \nSeries (years)")+
  facet_wrap(~bar)+
  #ylim(-500,500)+
  theme(axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 20), 
        plot.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA), 
        title = element_text(size = 20), 
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_line(colour = "black"), 
        strip.background = element_rect(fill = "white", 
                                        color = "black"), 
        strip.text = element_blank())


datpercent <- dat %>%
  mutate(negative = ifelse(beta<0,1,0)) %>%
  group_by(years, bar) %>%
  summarize(total = n(),
            declining = sum(negative),
            percent = declining/total) 


a <- datpercent %>% 
  mutate(bar = as.character(bar),
         bar = ifelse(bar=="True Population Size, 2 Highest Populations",
                      "Two Largest Populations, Year 1", bar),
         bar = ifelse(bar=="True Population Size, 2 Random Populations",
                      "Two Random Populations, Year 1", bar)) %>% 
  ggplot(aes(x=years, y=percent))+
  geom_col()+
  ylab("Proportion Declining")+
  xlab("Length of Time \nSeries (Years)")+
  ylim(0,1)+
  facet_wrap(~bar) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 20), 
        plot.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA), 
        title = element_text(size = 20), 
        panel.background = element_rect(fill = "white"), 
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_line(colour = "black"), 
        strip.background = element_rect(fill="white",color="black"),
        strip.text = element_text(size = 15))

jpeg(file="~/negative_population_trends/sim_fig_combined_top5.jpeg", width=25, height=15, units="cm", res=300)
cowplot::plot_grid(a,b,nrow=2, align = "h")
dev.off()


#

###### Top 10 

#



truebr = 1
br = 3

dat2 <- read.csv("10ksims_freq1_spp20_nyears2_top10.csv") %>%
  mutate(bar = factor(bar),
         years=2)

dat5 <- read.csv("10ksims_freq1_spp20_nyears5_top10.csv") %>%
  mutate(bar = factor(bar),
         years=5)

dat10 <- read.csv("10ksims_freq1_spp20_nyears10_top10.csv") %>%
  mutate(bar = factor(bar),
         years=10)

dat20 <- read.csv("10ksims_freq1_spp20_nyears20_top10.csv") %>%
  mutate(bar = factor(bar),
         years=20)

dat50 <- read.csv("10ksims_freq1_spp20_nyears50_top10.csv") %>%
  mutate(bar = factor(bar),
         years=50)

dat100 <- read.csv("10ksims_freq1_spp20_nyears100_top10.csv") %>%
  mutate(bar = factor(bar),
         years=100)

dat <- rbind(dat2, dat5, dat10, 
             dat20, dat50, dat100) %>%
  mutate(years=factor(years)) %>%
  filter(model == "log")


b <-  ggplot(data=dat, aes(x=years, y=beta))+
  geom_boxplot()+
  geom_hline(aes(yintercept=0))+
  ylab("Regression Slope")+
  xlab("Length of Time \nSeries (years)")+
  facet_wrap(~bar)+
  #ylim(-500,500)+
  theme(axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 20), 
        plot.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA), 
        title = element_text(size = 20), 
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_line(colour = "black"), 
        strip.background = element_rect(fill = "white", 
                                        color = "black"), 
        strip.text = element_blank())


datpercent <- dat %>%
  mutate(negative = ifelse(beta<0,1,0)) %>%
  group_by(years, bar) %>%
  summarize(total = n(),
            declining = sum(negative),
            percent = declining/total) 


a <- datpercent %>% 
  mutate(bar = as.character(bar),
         bar = ifelse(bar=="True Population Size, 2 Highest Populations",
                      "Two Largest Populations, Year 1", bar),
         bar = ifelse(bar=="True Population Size, 2 Random Populations",
                      "Two Random Populations, Year 1", bar)) %>% 
  ggplot(aes(x=years, y=percent))+
  geom_col()+
  ylab("Proportion Declining")+
  xlab("Length of Time \nSeries (Years)")+
  ylim(0,1)+
  facet_wrap(~bar) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 20), 
        plot.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA), 
        title = element_text(size = 20), 
        panel.background = element_rect(fill = "white"), 
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_line(colour = "black"), 
        strip.background = element_rect(fill="white",color="black"),
        strip.text = element_text(size = 15))

jpeg(file="~/negative_population_trends/sim_fig_combined_top10.jpeg", width=25, height=15, units="cm", res=300)
cowplot::plot_grid(a,b,nrow=2, align = "h")
dev.off()


#
