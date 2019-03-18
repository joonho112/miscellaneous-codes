
###'######################################################################
###'
###' Getting data for every Census tract in the US with purrr and tidycensus
###' 
###' Source: https://walkerke.github.io/2017/05/tidycensus-every-tract/ 
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
###' Get a tibble of total population estimates for all US Census tracts 
###' from the 2011-2015 ACS
###' 
###' 

library(tidycensus)
library(purrr)

# Un-comment below and set your API key
census_api_key("b6eb1ab42bceaa4ddd3f6d8979b73dab7f95acb5")

us <- unique(fips_codes$state)[1:51]

totalpop <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B01003_001", 
          state = x)
})

str(totalpop)




###'######################################################################
###'
###' What if we also want tract geometry for mapping?
###'
###'

library(sf)
options(tigris_use_cache = TRUE)

totalpop_sf <- reduce(
  map(us, function(x) {
    get_acs(geography = "tract", variables = "B01003_001", 
            state = x, geometry = TRUE)
  }), 
  rbind
)

str(totalpop_sf)


