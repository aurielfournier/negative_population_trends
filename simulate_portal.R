library(tidyverse)


portaltop2 <- manysimulations(freq=1, spp=8, sims=10000, numyears=35, topnum=2)

portaltop3 <- manysimulations(freq=1, spp=8, sims=10000, numyears=35, topnum=2)

portaltop4 <- manysimulations(freq=1, spp=8, sims=10000, numyears=35, topnum=2)


save(portaltop2, file="~/../Dropbox/negative_population_trends/portaltop1.Rdata")


save(portaltop3, file="~/../Dropbox/negative_population_trends/portaltop1.Rdata")


save(portaltop4, file="~/../Dropbox/negative_population_trends/portaltop1.Rdata")
