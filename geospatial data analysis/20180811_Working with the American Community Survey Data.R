
###'######################################################################
###'
###' Working with acs.R
###' 
###' Downloading, managing, analyzing, and presenting data from 
###' the U.S. Census (<https://www.census.gov/data/developers/data-sets.html>), 
###' including
###' 
###'  SF1 (Decennial short-form), 
###'  SF3 (Decennial long-form), and 
###'  the American Community Survey (ACS).
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
###' Load the required packages
###' 
###' 

library(acs)



###'######################################################################
###'
###' Getting and installing a Census API Key
###' 
###' : Visit http://api.census.gov/data/key_signup.html and 
###' fill out the simple form
###'
###'

### Do this once. Never need to do it again
api.key.install(key = "b6eb1ab42bceaa4ddd3f6d8979b73dab7f95acb5")



###'######################################################################
###'
###' Create a geo.set using the geo.make() function
###' 
###' Important: 
###' when creating new geographies, 
###' each set of arguments must match with exactly one known Census geography
###'
###' These new geo.sets are simply placeholders for geographic entities. 
###' They do not actually contain any census data about these places.
###'
###'

washington <- geo.make(state=53)

alabama <- geo.make(state = "Alab")

california <- geo.make(state = "California")

yakima <- geo.make(state = "WA", county = "Yakima")

california

yakima


###' Real geo.sets: complex groups and combinations
###' What if we want to create new complex geographies 
###' made of more than one known census geography?

psrc <- geo.make(state = "WA", county = c(33, 35, 53, 61))

north.mercer.island <- geo.make(state = 53, county = 33, tract = c(24300, 24400))

optional.tract <- geo.make(state = 53, county = 33, tract = 24500)

north.mercer.island.plus <- north.mercer.island + optional.tract  # add in one more tract

str(north.mercer.island.plus)



###'######################################################################
###'
###' Use the acs.lookup() function to explore the variables 
###' we may want to download
###'
###'

geo.lookup(state = "CA")

geo.lookup(state = "CA", county = "Los Angeles")

geo.lookup(state = "CA", county = "Los Angeles", place = "Angeles")


### There is no state with this FIPS code
no.state <- geo.make(state = 3)  

no.state <- geo.make(state = 3, check = T)


### Give it something with a bad county/tract match
shoreline.nw.border <- geo.make(state = 53,
                                county = c(33, 33, 61, 61, 61),
                                tract = c(20100, 20200, 20300, 50600, 50700), 
                                check = T, combine = T,
                                combine.term = "Shoreline NW Tracts")


### Fix the problem and try again
shoreline.nw.border <- geo.make(state = 53, 
                                county = c(33, 33, 33, 61, 61),
                                tract=c(20100, 20200, 20300, 50600, 50700), 
                                check = T, combine = T,
                                combine.term = "Shoreline NW Tracts")

shoreline.nw.border



###'######################################################################
###'
###' Use the acs.fetch() function to download data for our new geography
###'
###'

### table B01003: "Total Population"
acs.fetch(geography = psrc, 
          endyear = 2011, 
          table.number = "B01003")


### table B05001: "Nativity and Citizenship Status in the United States"
acs.fetch(geography = north.mercer.island.plus, 
          endyear = 2011, 
          table.number = "B05001")


###' table B08013: "Aggregate Travel Time To Work (in Minutes) Of Workers By Sex"
###' When combine=T, acs.fetch will aggregate the data
###' (using the sum method for acs-class objects) when it is downloaded.

combine(north.mercer.island.plus) = T

combine.term(north.mercer.island.plus) = "North Mercer Island Tracts"

my.geos <- c(psrc, north.mercer.island.plus, shoreline.nw.border)

acs.fetch(geo = my.geos, 
          table.number = "B08013", 
          endyear = 2011, 
          col.names=c("Total","Male","Female"))


###' Users must specify the latest year for the dataset they are seeking: 
###' for example, endyear = 2011 for the 2007-2011 ACS data 
###' (or, with span = 3, for the 2009-2011 data)
###' 
###' American Community Survey (dataset = "acs")
###' 
###' - 5-Year Data: endyear= 2009 through 2014 
###'   (i.e., six surveys, 2005-2009 through 2010-2014);
###' - 3-Year Data: endyear= 2012, 2013;
###' - 1-Year Data: endyear= 2011, 2012, 2013, 2014)
###' 
###' See http://www.census.gov/data/developers/data-sets.html



###'######################################################################
###'
###' The acs.lookup() function: finding the variables we want
###'
###'

acs.lookup(endyear = 2011, keyword = c("Female", "GED"))
acs.lookup(endyear = 2011, table.number = "B01001", keyword = "Female")

urdu <- acs.lookup(keyword = "Urdu", endyear = 2011)
urdu

age.by.sex <- acs.lookup(table.name = "Age by Sex", endyear = 2011)
age.by.sex

acs.lookup(endyear = 2011, keyword = "Income")


