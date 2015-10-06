#### 1. For osm_download ####
#### api_func: function to generate api url ====
api_gen <- 
  function(bbox, xapi = xapi){
    # bbox: dataframe with 4 col 1 row, c(left, bottom, right, top)
    if(xapi){
      url <- 
        sprintf(
          "http://overpass.osm.rambler.ru/cgi/xapi_meta?*[bbox=%s,%s,%s,%s]", 
          bbox$left, bbox$bottom, bbox$right, bbox$top)
    } else {
      url <- 
        sprintf("http://api.openstreetmap.org/api/0.6/map?bbox=%s,%s,%s,%s", 
                bbox$left, bbox$bottom, bbox$right, bbox$top)
    }
    return(url)
  }

#### coord_dist: new coord ${dist} km from coord ====
coord_dist <-
  function (lon, lat, bearing, distance) 
  {
    rad <- pi/180
    a1 <- lat * rad
    a2 <- lon * rad
    tc <- bearing * rad
    d <- distance/6378.145
    nlat <- asin(sin(a1) * cos(d) + cos(a1) * sin(d) * cos(tc))
    dlon <- atan2(sin(tc) * sin(d) * cos(a1), cos(d) - sin(a1) * 
                    sin(nlat))
    nlon <- ((a2 + dlon + pi)%%(2 * pi)) - pi
    npts <- cbind(nlon/rad, nlat/rad)
    return(npts)
  }

#### osm_get: main function to obtain bbox-bounded 
####          osm map given coordinate ====
osm_get <- 
  function(coord, radius = 0.5, out_dir = "./Data/", 
           xapi = TRUE, pause = 10)
  {
    # Function to obtain bbox-bounded osm map given coordinate
    # see http://wiki.openstreetmap.org/wiki/Downloading_data
    
    
    # coord:    dataframe, list of coordinates
    #           cols must be c("name", "longitude", "latitude")
    # radius:   numeric, "radius" of bounding box in km
    # out_dir:  string, output directory of files
    
    # xapi:      whether to use extended API,
    #             xAPI provides enhanced search and querying 
    #             capabilities based on main API, see 
    #             <http://wiki.openstreetmap.org/wiki/Xapi>
    
    # pause:    numeric, # of seconds between API request
    
    require(magrittr) # just in case
    
    ## 1. clean up input data
    dat <- coord %>% unique %>% set_names(c("name", "long", "lat")) 
    
    # turn name into character
    dat$name <- as.character(dat$name)
    # check if long & lat both numeric
    if (!any(sapply(dat[, -1], is.numeric)))
      stop("'Longitude' or 'Latitude' not numeric")
    
    # return success msg
    cat("osm_get: data read in with", nrow(dat), "unique locations\n")
    
    ## 2. download file for each site
    cat("osm_get: Starting download...\n")
    cat("         (be warned!!!! This will take at least", 
        round(pause*nrow(dat)/60, 1), "minutes)\n")
    
    cat("==================================================\n")
    for(i in 1:nrow(dat)){
      ## 2.1 prepare bounding box
      name <- dat[i, "name"]
      long <- dat[i, "long"]; lat <- dat[i, "lat"]
      
      bbox <- 
        data.frame(
          top = coord_dist(long, lat, 0, radius)[2],
          right = coord_dist(long, lat, 90, radius)[1],
          bottom = coord_dist(long, lat, 180, radius)[2],
          left = coord_dist(long, lat, 270, radius)[1]
        ) %>% 
        # force to 2 decimal place
        round(2) %>% lapply(as.character)
      
      ## 2.2 download data from API
      url <- api_gen(bbox, xapi = xapi)
      file_name <- sprintf("%s_%skm.osm", name, radius)
      
      cat("osm_get: downloading", file_name, "...")
      download.file(url = url, 
                    destfile = paste0(out_dir, file_name), 
                    quiet = TRUE)
      cat("Done!\n")
      # pause between API requests
      Sys.sleep(pause)
    }
    cat("==================================================\n")
    cat("osm_get: download complete! check\"", out_dir, 
        "\"for output files :)\n")
  }

#### 2. For obj_volcalc ####
list_from_idx <- 
  function(title_idx, text){
  # helper function for obj_filter,
  # group a given character string ('text') into 
  # list by the title index
  title_idx_extend <- 
    c(1, title_idx, length(text) + 1)
  lapply(1:(length(title_idx_extend)-1),
         function(i){
           idx <- 
             title_idx_extend[i]:
             (title_idx_extend[i+1]-1)
           text[idx]
         })
}

vertex_fix <- 
  function(){
    # helper function for obj_filter,
    # re-organize vertex index of a given OBJ file
    
    
  }

obj_filter <- 
  function(file_name, key = "build", 
           verbose = FALSE, 
           in_dir = "./Data_3d/",
           out_dir = "./Data_3d_building/")
  {
    # Function to filter needed component from a OSM2World obj file
    # reference to OBJ format: 
    #       http://www.cs.cmu.edu/~mbz/personal/graphics/obj.html
    
    # > file_name: file_name WITHOUT extension
    # > key: key word to be filtered e.g. 'road', 'building', etc
    
    # 1. read file into string ===
    txt <- readLines(paste0(in_dir, file_name, ".obj"))
    
    ### 2. group txt into list of named objects ===
    # output: a two-tier list. First by group 
    # (e.g. comment, Road, Building etc), 
    # then by object (e.g. Building001)
    
    ## 2.1 create first order list  by object group ---
    # output: group_list
    
    # obtain idx of group title
    group_idx <- grep("^g ", txt) 

    group_list <- 
      # group lines into list by group
      list_from_idx(group_idx, txt) %>% 
      # set title names
      set_names(c("comment", txt[group_idx]))
    
    ## 2.2 create second order list by object ---
    # output: group_obj_list
    group_obj_list <- 
      lapply(group_list, 
             function(group_txt){
               # obtain idx of object title
               title_idx <- grep("^o ", group_txt) 
               title_list <- group_txt[title_idx] %>% gsub("^o ", "", .)
               
               obj_list <- 
                 # for each title, group its lines into one el in list
                 list_from_idx(title_idx, group_txt) %>% 
                 # set title names
                 set_names(c("group", title_list))
             }
      )
    
    ### 3. Filtering using the keyword from 'filter' ===
    # output: target_list
    
    # find available object types, print for user validation
    types <- group_title %>% unique
    types_idx <- 
      grep(key, types, ignore.case = TRUE)
    types_target <- 
      grep(key, types, ignore.case = TRUE, value = TRUE)
    
    if (verbose) {
      cat("Available obj types from '", file_name, "' are:\n ")
      cat(paste(1:length(types), types, "\n"))
      cat("given key is \'", key, "\', below obj types will be returned:\n ")
      cat(paste(types_idx, types_target, "\n"))
    }
    
    # obtain target objects
    target_list <- 
      # first obtain index of target list
      grep(key, names(group_obj_list), ignore.case = TRUE) %>%
      # then retrieve object
      (function(idx) group_obj_list[idx])
    
    ### 4. Write file ===
    ## 4.1 output dir/file configuration ---
    # validate output directory
    if(!dir.exists(out_dir)){
      dir.create(out_dir)
      cat("Output directory", out_dir, "created!\n")
    }
    
    # parse file name then create output obj file 
    out_file_name <- 
      paste0(out_dir, file_name, ".obj")
    file.create(out_file_name, overwrite = TRUE)
    if (verbose) cat("Output file", out_file_name, "created,")
    
    ## 4.2 writing comment and objects in group_obj_list ---
    cat(" writing:\n")
    
    # first write comment (1st element of group_list)
    out_file_con <- file(out_file_name, "w")
    write(group_list[["comment"]], out_file_con)
    cat("comments..")
    
    # now write object by group from group_obj_list
    for (group_idx in 1:length(target_list)){
      group_name <- names(target_list)[[group_idx]]
      cat(group_name, "\n")
      if (verbose) cat("===========================")
      for (obj_idx in 1:length(target_list[[group_idx]])){
        if (verbose) {
          if (obj_idx > 1){ # if obj is not group title
            obj_name <- names(target_list[[group_idx]])[obj_idx]
            cat(obj_name, "..")
          }
        }
        # write lines
        write(target_list[[group_idx]][[obj_idx]], 
              out_file_con)
      }
      if (verbose) cat("===========================\n")
    }
    cat("All done!\n")
    
    # close and cleanup
    close(out_file_con)
    cat("File closed. Exit\n")
  }





