source("./func/osm_helper.R")
require(rgl)
require(dplyr)
require(magrittr)

file_dir <- 
  "/Users/Jeremiah/Dropbox/Research/Harvard/1. BostonPol/Analysis/Elem/2/data/3d_building_osm"

#### 3. read-in 3d OBJ file and keep only the building objects ####
in_dir <- paste0(file_dir, "/Data_3d/")
out_dir <- paste0(file_dir, "/Data_3d_building/")

file_list <- 
  list.files(in_dir) %>% 
  grep("*.obj$", ., value = TRUE) %>%
  gsub("\\.obj$", "", .)


# run building extractor
outcome_list <-
  vector("list", length(file_list)) %>% 
  set_names(file_list)

fail_file_list <- character(0)
  
for (file_name in file_list) {
  outcome_list[[file_name]] <- 
    tryCatch(
      obj_filter(file_name, key = "^Building$", 
                 verbose = FALSE, 
                 in_dir = in_dir, out_dir = out_dir), 
      error = function(e) e)
  # record fail file name
  if("error" %in% class(outcome_list[[file_name]])){
    cat("FAIL!!!\n")
    fail_file_list <- c(fail_file_list, file_name)
  }
}

# save results 
save(outcome_list, file = "outcome_list.RData")
save(fail_file_list, file = "fail_file_list.RData")
