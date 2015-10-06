source("./func/osm_helper.R")
require(dplyr)
require(magrittr)

#### 1. obtain monitor coordinates ####
file_dir <- "/Users/Jeremiah/Dropbox/Research/Harvard/1. BostonPol/Analysis/Elem/2/data/"
file_name <- "dStudy.csv"

coord <- 
  read.csv(paste0(file_dir, file_name)) %>% 
  extract(c("SiteID", "Longitude", "Latitude")) %>%
  unique

#### 2. download using osm_get from osm_helper ####
osm_get(coord, radius = 0.5, out_dir = "./Data/", 
        xapi = TRUE, pause = 10)

