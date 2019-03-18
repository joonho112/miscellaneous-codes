
###'######################################################################
###'
###' EdSurvey Workshop
###'
###' 20190111 JoonHo Lee
###'
###'

###'######################################################################
###'
###' Basic settings
###'
###'

### Start with a clean slate
gc()            # force R to release memory it is no longer using
rm(list=ls())   # delete all the objects in the workspace


### Set working directory
work_dir <- c("~/miscellaneous-codes/NAEP_Workshop")
setwd(work_dir)


### Call packages
library(tidyverse)
library(EdSurvey)



###'######################################################################
###'
###' Reading in Data
###'
###'

sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))



###'######################################################################
###'
###' Generate a Codebook file
###'
###'

codebook <- showCodebook(sdf)


###'######################################################################
###'
###' Get data
###'
###'

gddat <- getData(data = sdf, varnames = c('composite', 'dsex', 'b017451', 'origwt'),
                 addAttributes = TRUE, omittedLevels = FALSE)



###'######################################################################
###'
###' A Simple Analysis of Gender Gap in Math Achievement 
###'
###'

### Extract levels in race variables
df_summary <- summary2(sdf, "sdracem")$summary
race_vec <- as.character(df_summary$sdracem)


### Loop over race levels
meta_list <- list()

for (i in seq_along(race_vec)){
  
  # Gap analysis using EdSurvey::gap()
  gap <- gap(variable = "composite", data = sdf, 
             groupA = (sdracem %in% race_vec[i]) & (dsex %in% "Male"), 
             groupB = (sdracem %in% race_vec[i]) & (dsex %in% "Female"), 
             percentiles = seq(10, 90, by = 10))
  
  # Extract as a dataframe
  df_result <- gap$results %>%
    mutate(race = race_vec[i])

} # condition in groupA, groupB do not take external objects


### (1) White
gap_white <- gap(variable = "composite", data = sdf, 
                 groupA = (sdracem %in% "White") & (dsex %in% "Male"), 
                 groupB = (sdracem %in% "White") & (dsex %in% "Female"), 
                 percentiles = seq(10, 90, by = 10))

df_white <- gap_white$results %>%
  mutate(race = "White")


### (2) Hispanic
gap_hispanic <- gap(variable = "composite", data = sdf, 
                  groupA = (sdracem %in% "Hispanic") & (dsex %in% "Male"), 
                  groupB = (sdracem %in% "Hispanic") & (dsex %in% "Female"), 
                  percentiles = seq(10, 90, by = 10))

df_hispanic <- gap_hispanic$results %>%
  mutate(race = "Hispanic")


### (3) Black
gap_black <- gap(variable = "composite", data = sdf, 
                    groupA = (sdracem %in% "Black") & (dsex %in% "Male"), 
                    groupB = (sdracem %in% "Black") & (dsex %in% "Female"), 
                    percentiles = seq(10, 90, by = 10))

df_black <- gap_black$results %>%
  mutate(race = "Black")


### (4) Asian/Pacific Island
gap_asian <- gap(variable = "composite", data = sdf, 
                 groupA = (sdracem %in% "Asian/Pacific Island") & (dsex %in% "Male"), 
                 groupB = (sdracem %in% "Asian/Pacific Island") & (dsex %in% "Female"), 
                 percentiles = seq(10, 90, by = 10))

df_asian <- gap_asian$results %>%
  mutate(race = "Asian/Pacific Island")


### Combine
meta_list <- list(df_white, df_hispanic, df_black, df_asian)
df_bind <- bind_rows(meta_list)



###'######################################################################
###'
###' Quanilte Effects Plot
###'
###'

p <- ggplot(data = df_bind %>% filter(race != "Asian/Pacific Island"), 
       aes(x = percentiles, 
           y = diffAB, 
           group = race)) +
  
  geom_pointrange(aes(ymin = diffAB - 1.96*diffABse, 
                      ymax = diffAB + 1.96*diffABse, 
                      group = race, 
                      color = race),
                  position = position_jitter(width = 2, 
                                             height = 0)) + 
  geom_hline(yintercept = 0, 
             size = 0.5, 
             col = "black", 
             linetype = "dashed") + 
  
  scale_x_continuous(breaks = seq(from = 0, 
                                  to = 100, 
                                  by = 10)) + 
  facet_wrap(.~race) +
  
  # Themes
  theme_bw( base_family = "serif") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.0)) + 
  
  # Labels
  labs(title = "Gender Gap in Math Achievement by Race/Ethnicity", 
       subtitle = NULL, 
       caption = NULL, 
       y = "Male students' math score - Female students' math score",  
       x = "Percentiles") + 
  
  # Mannual color and shapes
  scale_color_manual(values = c("firebrick1", "dodgerblue1", "forestgreen", "darkorchid1")) 

