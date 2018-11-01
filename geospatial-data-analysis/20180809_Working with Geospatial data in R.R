
###'######################################################################
###'
###' Working with Geospatial Data in R
###' 
###' (1) Basic mapping with ggplot and ggmap
###' 
###' DataCamp Exercises
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


### Call libraries
library(tidyverse)
library(ggmap)



###'######################################################################
###'
###' Grabbing a background map
###'
###'

corvallis <- c(lon = -123.2620, lat = 44.5646)


# Get mat at zoom level 5: map_5
map_5 <- get_map(location = corvallis, zoom = 5, scale = 1)


# Plot map at zoom level 5
ggmap(map_5)


# Get map at zoom level 13
corvallis_map <- get_map(location = corvallis, zoom = 13, scale = 1)


# Plot map at zoom level 13
ggmap(corvallis_map)



###'######################################################################
###'
###' Putting it all together
###'
###'

# Look at head() of sales
head(sales)

# Get a map in the background of the plot
ggmap(corvallis_map) + 
  geom_point(aes(lon, lat), data = sales)



###'######################################################################
###'
###' Insight through aesthetics
###'
###'

# Map color to year_built
ggmap(corvallis_map) +
  geom_point(aes(lon, lat, color = year_built), data = sales)

# Map size to bedrooms
ggmap(corvallis_map) +
  geom_point(aes(lon, lat, color = year_built, size = bedrooms), data = sales)

# Map color to price / finished_squarefeet
ggmap(corvallis_map) +
  geom_point(aes(lon, lat, color = price/finished_squarefeet), data = sales)



###'######################################################################
###'
###' Different maps
###' 
###' 

corvallis <- c(lon = -123.2620, lat = 44.5646)

# Add a maptype argument to get a satellite map
corvallis_map_sat <- get_map(corvallis, zoom = 13, maptype = "satellite")

# Edit to display satellite map
ggmap(corvallis_map_sat) +
  geom_point(aes(lon, lat, color = year_built), data = sales)

# Add source and maptype to get toner map from Stamen Maps
corvallis_map_bw <- get_map(corvallis, zoom = 13, maptype = "toner", source = "stamen")

# Edit to display toner map
ggmap(corvallis_map_bw) +
  geom_point(aes(lon, lat, color = year_built), data = sales)










