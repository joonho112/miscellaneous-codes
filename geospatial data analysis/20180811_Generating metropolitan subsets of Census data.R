
###'######################################################################
###'
###' Generating metropolitan subsets of Census data with R and tigris
###' 
###' Source: https://walkerke.github.io/2017/05/tigris-metros/
###' 
###' 
###' 20180808 JoonHo Lee
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
###' Load the required packages and 
###' set options to cache data from tigris and load the data as sf objects.
###'
###'

library(tigris)
library(sf)
library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)



###'######################################################################
###'
###' Fetch Census tracts for Oregon and Washington and combine them 
###'
###'

orwa <- rbind_tigris(
  tracts("OR", cb = TRUE), 
  tracts("WA", cb = TRUE)
)

ggplot(orwa) + geom_sf()



###'######################################################################
###'
###' Subset these tracts spatially by locating 
###' the boundary of the Portland metropolitan area 
###'
###'

cb <- core_based_statistical_areas(cb = TRUE)

pdx <- filter(cb, grepl("Portland-Vancouver", NAME))

ggplot(pdx) + geom_sf()



###'######################################################################
###'
###' spatial subsetting in sf 
###' by indexing the data we'd like to subset by the spatial overlay
###' 
###'

p1 <- orwa[pdx,]

ggplot() + 
  geom_sf(data = p1) + 
  geom_sf(data = pdx, fill = NA, color = "red")



###'######################################################################
###'
###' Identify those tracts within the boundary of the metropolitan area
###' 
###' : use purrr to convert w1 to a logical vector and 
###' 
###'   subset our data accordingly
###'
###'

w1 <- st_within(orwa, pdx)

print(length(w1))

print(w1[1:5])

w2 <- map_lgl(w1, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

p2 <- orwa[w2,]

ggplot() + 
  geom_sf(data = p2) + 
  geom_sf(data = pdx, fill = NA, color = "red")



###'######################################################################
###'
###' Define a function that does the following: 
###' 
###' 1. Takes a metropolitan area as input, and 
###'    detects the states in which the metro area is located; 
###'
###' 2. Retrieves tracts for those states; 
###' 
###' 3. Identifies the tracts located within the metro area’s boundary.
###'
###'

metro_tracts <- function(metro_name) {
  
  # First, identify which states intersect the metro area using the
  # `states` function in tigris
  st <- states(cb = TRUE)
  cb <- core_based_statistical_areas(cb = TRUE)
  metro <- filter(cb, grepl(metro_name, NAME))
  
  stcodes <- st[metro,]$STATEFP
  
  # Then, fetch the tracts, using rbind_tigris if there is more
  # than one state
  if (length(stcodes) > 1) {
    tr <- rbind_tigris(
      map(stcodes, function(x) {
        tracts(x, cb = TRUE)
      })
    )
  } else {
    tr <- tracts(x, cb = TRUE)
  }
  
  # Now, find out which tracts are within the metro area
  within <- st_within(tr, metro)
  
  within_lgl <- map_lgl(within, function(x) {
    if (length(x) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Finally, subset and return the output
  output <- tr[within_lgl,]
  
  return(output)
}



###'######################################################################
###'
###' Test the function
###'
###'

chi <- metro_tracts("Chicago")

ggplot(chi) + geom_sf()


bos <- metro_tracts("Boston")

ggplot(bos) + geom_sf()


SF <- metro_tracts("Los Angeles-Long Beach-Anaheim")

ggplot(SF) + geom_sf()

