library(tidyverse)

load("~/negative_population_trends/10ksims_freq1_spp20_numyears100.Rdata")

dim(year1$twenty_species)

dat <- year1$twenty_species

  
############################################################################
## 40 Years
############################################################################

nyears <- 40
fq <- 1

rt_list <- list()
st_list <- list()

for(i in seq(1,length(dat),by=2)){
  if(!is.null(dat[[i]])){
  datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
  rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
  sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
  sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
  sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
  st <- sampletrend(sampled_data = sewe)
  st_list[[i]] <- st
  rt_list[[i]] <- rt
  }
}


rt_df1 <- rt_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")

st_df1 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


fq <- 2

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


st_df2 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


fq <- 5

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


st_df5 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


panel_40_years_df <- bind_rows(rt_df1$real_values_all_pops %>% mutate(box=1), 
st_df1$sampled_trend_random %>% mutate(box=2),
st_df1$sampled_trend_highest %>% mutate(box=3),
st_df2$sampled_trend_highest %>% mutate(box=4),
st_df5$sampled_trend_highest %>% mutate(box=5))

ggplot(data=panel_40_years_df)+
      geom_violin(aes(x=factor(box), y=beta),draw_quantiles = c(0.25, 0.5, 0.75))



############################################################################
## 20 Years
############################################################################

nyears <- 20
fq <- 1

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


rt_df1 <- rt_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")

st_df1 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


fq <- 2

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


st_df2 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


fq <- 5

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


st_df5 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


panel_20_years_df <- bind_rows(rt_df1$real_values_all_pops %>% mutate(box=1), 
                               st_df1$sampled_trend_random %>% mutate(box=2),
                               st_df1$sampled_trend_highest %>% mutate(box=3),
                               st_df2$sampled_trend_highest %>% mutate(box=4),
                               st_df5$sampled_trend_highest %>% mutate(box=5))

ggplot(data=panel_20_years_df)+
  geom_violin(aes(x=factor(box), y=beta),draw_quantiles = c(0.25, 0.5, 0.75))



############################################################################
## 10 Years
############################################################################

nyears <- 10
fq <- 1

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


rt_df1 <- rt_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")

st_df1 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


fq <- 2

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


st_df2 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


fq <- 5

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


st_df5 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


panel_10_years_df <- bind_rows(rt_df1$real_values_all_pops %>% mutate(box=1), 
                               st_df1$sampled_trend_random %>% mutate(box=2),
                               st_df1$sampled_trend_highest %>% mutate(box=3),
                               st_df2$sampled_trend_highest %>% mutate(box=4),
                               st_df5$sampled_trend_highest %>% mutate(box=5))

ggplot(data=panel_10_years_df)+
  geom_violin(aes(x=factor(box), y=beta),draw_quantiles = c(0.25, 0.5, 0.75))



############################################################################
## 5 Years
############################################################################

nyears <- 5
fq <- 1

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


rt_df1 <- rt_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")

st_df1 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


fq <- 2

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


st_df2 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


fq <- 5

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


st_df5 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


panel_5_years_df <- bind_rows(rt_df1$real_values_all_pops %>% mutate(box=1), 
                               st_df1$sampled_trend_random %>% mutate(box=2),
                               st_df1$sampled_trend_highest %>% mutate(box=3),
                               st_df2$sampled_trend_highest %>% mutate(box=4),
                               st_df5$sampled_trend_highest %>% mutate(box=5))

ggplot(data=panel_5_years_df)+
  geom_violin(aes(x=factor(box), y=beta),draw_quantiles = c(0.25, 0.5, 0.75))




############################################################################
## 2 Years
############################################################################

nyears <- 2
fq <- 1

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


rt_df1 <- rt_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")

st_df1 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


fq <- 2

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


st_df2 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


fq <- 5

rt_list <- list()
st_list <- list()

for(i in seq(1,100,by=2)){
  if(!is.null(dat[[i]])){
    datdat <- dat[[i]] %>% select(-sim) %>% spread("variable","value") %>% select(-year) %>% as.matrix()
    rt <- realtrend(time_series = datdat, spp=2, numyears=nyears)
    sewe <- samplingeffortwitherror(time_series = datdat, numyears=nyears, freq=fq)
    sewe$sampled_data_highest <- na.omit(sewe$sampled_data_highest)
    sewe$sampled_data_random  <- na.omit(sewe$sampled_data_random)
    st <- sampletrend(sampled_data = sewe)
    st_list[[i]] <- st
    rt_list[[i]] <- rt
  }
}


st_df5 <- st_list %>%
  # get rid of those NULLs
  discard(is.null) %>% 
  transpose %>% 
  map(bind_rows, .id = "simulation")


panel_2_years_df <- bind_rows(rt_df1$real_values_all_pops %>% mutate(box=1), 
                              st_df1$sampled_trend_random %>% mutate(box=2),
                              st_df1$sampled_trend_highest %>% mutate(box=3),
                              st_df2$sampled_trend_highest %>% mutate(box=4),
                              st_df5$sampled_trend_highest %>% mutate(box=5))

ggplot(data=panel_2_years_df)+
  geom_violin(aes(x=factor(box), y=beta),draw_quantiles = c(0.25, 0.5, 0.75))

