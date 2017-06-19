library(cowplot)
library(tidyverse)



theme_krementz <- function()
{
  theme(axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 20), plot.background = element_blank(), 
        panel.border = element_blank(), panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA), title = element_text(size = 20), 
        panel.background = element_rect(fill = "white"), axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_line(colour = "black"), strip.background = element_rect(fill = "white", 
                                                                                      color = "black"), strip.text = element_text(size = 15))
}

load("~/negative_population_trends/panel1.Rdata")
bars1 <- bars %>% 
  separate(bar, into=c("text","bar"), sep=3) %>%
  mutate(populations=20)
load("~/negative_population_trends/panel2.Rdata")
bars2 <- bars %>% 
  separate(bar, into=c("text","bar"), sep=3) %>%
  mutate(populations=5)
load("~/negative_population_trends/panel3.Rdata")
bars3 <- bars %>% 
  separate(bar, into=c("text","bar"), sep=3) %>%
  mutate(populations=50)


barbar <-rbind(bars1, bars2, bars3)#, bars4, bars5)

(panel1 <- ggplot()+
    geom_violin(data=barbar, aes(x=bar, y=beta), draw_quantiles = c(0.25, 0.5, 0.75))+
    theme(axis.title.x=element_blank())+
    facet_wrap(~populations, nrow=1)+
    theme_krementz())

write.csv(barbar, file="~/negative_population_trends/5_20_50_populations.csv", row.names = FALSE)