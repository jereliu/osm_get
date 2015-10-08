source("./func/osm_helper.R")
require(rgl)
require(dplyr)
require(magrittr)

file_dir <- 
  paste0(
    "/Users/Jeremiah/Dropbox/Research/Harvard/1. BostonPol/",
    "Analysis/Elem/2/data/3d_building_osm"
  )

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
  outcome_list[[file_name]] <- res <-
    tryCatch(
      obj_filter(file_name, key = "^Building$", 
                 verbose = FALSE, 
                 in_dir = in_dir, out_dir = out_dir), 
      error = function(e) e)
  # record fail file name
  if("error" %in% class(res)){
    cat("FAIL!!!\n")
    fail_file_list <- c(fail_file_list, file_name)
  }
}

# save/load results 
save(outcome_list, file = "outcome_list.RData")

#### 4. fix stragglers (mess around) ####
debug <- FALSE

if (debug){
  load("outcome_list_1.RData")
  
  # extract files with error and exception
  error_idx <- 
    sapply(outcome_list, 
           function(x) "error" %in% class(x)) 
  error_list <- outcome_list[error_idx]
  
  
  except_list <- 
    sapply(outcome_list[!error_idx], 
           function(x) x != 0) %>%
    (function(idx)  outcome_list[!error_idx][idx]) %>% 
    unlist
  
  # first fix exception
  table(except_list) # all type 1.
}




