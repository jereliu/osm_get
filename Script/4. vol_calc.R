source("./func/osm_helper.R")
require(geometry)
require(dplyr)
require(magrittr)

file_dir <- 
  paste0(
    "/Users/Jeremiah/Dropbox/Research/Harvard/1. BostonPol/",
    "Analysis/Elem/2/data/3d_building_osm"
  )

res_dir <- "./output/"
#### 4. read-in 3d OBJ file and keep only the building objects ####
in_dir <- paste0(file_dir, "/Data_3d_building/")

file_list <- 
  list.files(in_dir) %>% 
  grep("*.obj$", ., value = TRUE) %>%
  gsub("\\.obj$", "", .)

# for each file 
#   1. obtain list of vertices for each object
#   2. calculate volumn of each object using convex hull
file_vol <- 
  vector("numeric", length = length(file_list)) %>%
  set_names(file_list)

for (file_name in file_list){
  cat(paste0(file_name, ": creating obj.."))
  obj_list <- read_obj(file_name, in_dir = in_dir)
  cat("calculating volume..")
  vol <- 
    sapply(obj_list, 
           function(obj) 
             tryCatch(convhulln(obj, options = "FA")$vol, 
                      error = function(e) 0
             )
    ) %>% sum
  cat("Done!\n")
  file_vol[file_name] <- vol
}

file_vol %>%
  (function(x) cbind(site = names(x), vol = x)) %>%
  write.csv(file = paste0(res_dir, "site_vol.csv"))

