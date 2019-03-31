
###'######################################################################
###'
###' Category: Import Data
###'           
###' Task    : Import TALIS Data using EdSurvey R Package
###' 
###' Data    : TALIS
###' 
###' Date    : 2019-03-26
###' 
###' Author  : JoonHo Lee (joonho@berkeley.edu)
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
work_dir <- c("~/miscellaneous-codes")
setwd(work_dir)


### Set data containing working directory
data_dir <- c("D:/Data/TALIS2013")


### Call libraries
library(tidyverse)
library(EdSurvey)



###'######################################################################
###'
###' Print out downloading instructions for TALIS 2008 database
###'
###'

downloadTALIS(2008)



###'######################################################################
###' 
###' The following call returns an edsurvey.data.frame to TALIS 2013
###' for US teacher-level data at secondary level
###'

### Set data containing working directory
temp_path <- file.path(data_dir, "SPSS_International")
setwd(temp_path)


usa2013 <- readTALIS(path = temp_path, 
                     isced = "b",
                     dataLevel = "teacher", 
                     countries = "usa")

# Extract a data.frame with a few variables
gg <- getData(usa2013, c("tt2g05b", "tt2g01"))
head(gg)

# Conduct a preliminary analysis on the edsurvey.data.frame
edsurveyTable(tt2g05b ~ tt2g01, data = usa2013)





