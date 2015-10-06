source("./func/osm_helper.R")
require(rgl)
require(dplyr)
require(magrittr)

#### 3. read-in 3d OBJ file and keep only the building objects ####
file_list <- 
  list.files("./Data_3d/") %>% 
  grep("*.obj$", ., value = TRUE) %>%
  gsub("\\.obj$", "", .)

