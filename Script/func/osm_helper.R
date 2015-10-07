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
      c(1, title_idx, length(text) + 1) %>% 
      unique %>% sort
    lapply(1:(length(title_idx_extend)-1),
           function(i){
             idx <- 
               title_idx_extend[i]:
               (title_idx_extend[i+1]-1)
             text[idx]
           })
  }

obj_filter <- 
  function(file_name, key = "build", 
           verbose = FALSE, 
           in_dir = "./Data_3d/",
           out_dir = "./Data_3d_building/")
  {
    # Function to 
    # 1. filter needed component from a OSM2World obj file
    # 2. fix vertex index 
    
    # Reference to OBJ format: 
    #       http://www.cs.cmu.edu/~mbz/personal/graphics/obj.html
    
    # > file_name: file_name WITHOUT extension
    # > key: key word to be filtered e.g. 'road', 'building', etc
    
    cat("Processing", file_name, "..")
    # 1. read file into string ===
    txt <- readLines(paste0(in_dir, file_name, ".obj"))
    
    # mark vertex index, "v a b c" => "v22 a b c"
    v_idx <- grep("^v ", txt)
    
    txt[v_idx] <- 
      paste0("v", 1:length(v_idx)) %>% 
      mapply(gsub, "^v", ., txt[v_idx]) %>% 
      as.vector
    
    
    ### 2. group txt into list of named objects ===
    # output: a two-tier list. First by group 
    # (e.g. comment, Road, Building etc), 
    # then by object (e.g. Building001)
    
    ## 2.1 create first order list  by object group ---
    # output: group_list (vertex marked)
    
    # obtain idx of group title
    group_idx <- grep("^g ", txt) 
    group_title <- c("comment", txt[group_idx])
    
    group_list <- 
      # group lines into list by group
      list_from_idx(group_idx, txt) %>% 
      # set title names
      set_names(group_title)
    
    ## 2.2 create second order list by object ---
    # output: group_obj_list (vertex marked)
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
    # output: target_list (vertex marked)
    
    # find available object types, print for user validation
    group_title_exact <- group_title %>% gsub("^g ", "", .)
    
    types <- group_title_exact %>% unique
    types_idx <- grep(key, types)
    types_target <- grep(key, types, value = TRUE)
    
    if (verbose) {
      cat("\n Available obj types from '", file_name, "' are:\n ")
      cat(paste(1:length(types), types, "\n"))
      cat("given key is \'", key, "\', below object types will be returned:\n ")
      cat(paste(types_idx, types_target, "\n"))
    }
    
    # obtain target objects
    target_list_raw <- 
      # first obtain index of target list
      grep(key, group_title_exact) %>%
      # then retrieve object
      (function(idx) group_obj_list[idx])
    
    ### 4. Fix vertex ===
    # input: target_list_raw (vertex marked)
    # output: target_list (vertex index fixed and vertex clean)
    target_txt <- unlist(target_list_raw) %>% as.character
     
    ## 4.1 First check if vertex set includes facet set match
    vertex_set <-
      grep("^v[0-9]* ", target_txt, value = TRUE) %>% 
      # keep only "123" from "v123 a b c"
      gsub("^v| [[:punct:][:digit:][:blank:]]+", "",.) %>%
      as.numeric %>% unique %>% sort
    
    facet_list <- #keep this for 4.2
      grep("^f ", target_txt, value = TRUE) %>% 
      # keep only "a b c" from "f a b c"
      gsub("^f ", "", .) %>% 
      strsplit(" ")  
    
    facet_set <- 
      facet_list %>% unlist %>% 
      as.numeric %>% unique %>% sort
    
    # see if any vextex needed by facet is not in vertex_set
    if (!setequal(facet_set, vertex_set)){
      lost_vert <- setdiff(facet_set, vertex_set)
      extra_vert <- setdiff(vertex_set, vertex_set)
      
      if (length(lost_vert) > 0){
        cat("\n these vertex are needed and can't be found in file: ", 
             paste(lost_vert, collapse = ", "), "\n")
        return(1)
      } else if (length(extra_vert) > 0){
        stop("\n these vertex are not needed by facet in file: ", 
             paste(extra_vert, collapse = ", "), "\n")
        return(2)
      } else {
        stop("\n facet set and vertex set are not equal, weird. \n")
        return(3)
      }
    }
    
    ## 4.2 Now re-index (hash) vertex in facet command 
    # note we assume vertex_set == facet_set
    fct_idx <- grep("^f ", target_txt)
    vtx_idx <- grep("^v", target_txt)
    
    hash_fun <- 
      as.list(1:length(vertex_set)) %>% 
      set_names(as.character(vertex_set))
    
    # break, convert, reconstruct all facet commands
    cat("re-indexing..")
    target_txt[fct_idx] <- 
      sapply(facet_list, 
             function(vert){
               # convert using minusand
               vert_new <- hash_fun[vert] %>% unlist
               # reconstruct
               paste(c("f", vert_new), collapse = " ")
             }
      )
    
    # clean up vertex reference ("v123 a b c" => "v a b c")
    target_txt[vtx_idx] <- 
      target_txt[vtx_idx] %>%
      gsub("^v[0-9]* ", "v ", .)
    
    ## 4.3 recreate group/object set list (target_list)
    grp_idx <- grep("^g ", target_txt)
    obj_idx <- grep("^o ", target_txt)
    
    group_list_clean <- 
      # group lines into list by group
      list_from_idx(grp_idx, target_txt) %>% 
      # set title names
      set_names(c(target_txt[grp_idx]))
    
    target_list <- 
      lapply(group_list_clean, 
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
    
    
    ### 5. Write file ===
    
    ## 5.1 output dir/file configuration ---
    # validate output directory
    if(!dir.exists(out_dir)){
      dir.create(out_dir)
      cat("Output directory", out_dir, "created!\n")
    }
    
    # parse file name then create output obj file 
    out_file_name <- 
      paste0(out_dir, file_name, ".obj")
    file.create(out_file_name, overwrite = TRUE)
    if (verbose) cat("\n Output file", out_file_name, "created,")
    
    ## 5.2 writing comment and objects in group_obj_list ---
    if (verbose) cat(" writing:\n")
    
    # first write comment (1st element of group_list)
    out_file_con <- file(out_file_name, "w")
    write(group_list[["comment"]], out_file_con)
    if (verbose) cat("comments\n")
    
    # now write object by group from group_obj_list
    for (group_idx in 1:length(target_list)){
      group_name <- names(target_list)[[group_idx]]
      
      if (verbose) cat("===========================\n")
      if (verbose) cat(group_name, "\n")
      if (verbose) cat("===========================\n")
      
      for (obj_idx in 1:length(target_list[[group_idx]])){
        if (verbose) {
          if (obj_idx > 1){ # if obj is not group title
            obj_name <- names(target_list[[group_idx]])[obj_idx]
            cat(obj_name, "\n")
          }
        }
        # write lines
        write(target_list[[group_idx]][[obj_idx]], 
              out_file_con)
      }
      if (verbose) cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    }

    # close and cleanup
    close(out_file_con)
    if (verbose) cat("File Connection closed.\n") 
    cat("Success!\n")
    
    return(0)
  }





