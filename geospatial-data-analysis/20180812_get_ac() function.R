
###'######################################################################
###'
###' get_acs() of tidycensus package
###' 
###' 
###' 20180811 JoonHo Lee
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
work_dir <- c("~/data-manipulation-and-visualization")
setwd(work_dir)



###'######################################################################
###'
###' Load the required packages & Set API key
###'
###'

library(tidycensus)
library(tidyverse)
library(viridis)

tidycensus::census_api_key("b6eb1ab42bceaa4ddd3f6d8979b73dab7f95acb5")



###'######################################################################
###'
###' Get ACS data & plot
###' (1) With geometry = TRUE
###'
###'

tarr <- get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)


ggplot(tarr, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")



###'######################################################################
###'
###' Get ACS data & plot
###' (2) Without geometry = TRUE
###'
###'

vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT")

vt %>%
  mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2012-2016 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")
