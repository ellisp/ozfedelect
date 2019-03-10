library(sf)
library(rmapshaper)


shapefiles <- c(
  "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_ced_2018_aust_shp.zip&1270.0.55.003&Data%20Cubes&BF4D23C712D492CFCA2582F600180556&0&July%202018&28.08.2018&Latest",
  "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_ced_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&447BE1AE2E3E7A3ACA25802C00144C3C&0&July%202016&13.09.2016&Previous",
  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055003_ced_2013_aust_shp.zip&1270.0.55.003&Data%20Cubes&390A0B8BBC1CFC9CCA257BB000117290&0&July%202013&23.07.2013&Latest",
  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055003_ced_2011_aust_shape.zip&1270.0.55.003&Data%20Cubes&AFFAF0F44528F2EFCA2578D40013CA06&0&July%202011&22.07.2011&Previous"
)

sfnames <- paste0("ced_boundaries_", c(2019, 2016, 2013, 2010))

shapes <- list()

# lots of people these days do this sort of thing with purrr but I find it much easier to read as a loop:
for(i in 1:length(sfnames)){
  the_dir <- paste0("raw-data/", sfnames[i])
  the_file <- paste0(the_dir, ".zip")
  download_if_fresh(shapefiles[i], destfile = the_file)
  unzip(the_file, exdir = the_dir)
  
  
  tmp <- st_read(the_dir,
                         layer = gsub("\\.shp$", "", list.files(the_dir, pattern = ".shp$")),
                         stringsAsFactors = FALSE) %>%
    mutate(election_year = str_extract(sfnames[i], "[0-9]+")) %>%
    rename_all(tolower) %>%
    rename_all(function(x){gsub("[0-9]+", "", x)})
  
  # crop the map so all of them have the same bounding box
  tmp <- st_crop(tmp, 
                 xmin = 96.816941,
                 ymin = -43.740510,
                 xmax = 159.109219,
                 ymax = -9.142176)

  names(tmp) <- gsub("areasqkm", "area_sqkm", names(tmp))
  names(tmp) <- gsub("^sqkm$", "area_sqkm", names(tmp))
  
  # simplify to 10% of the size
  shapes[[i]] <- ms_simplify(tmp, keep = 0.1)
  
}

ced_boundaries <- do.call("rbind", shapes)

save(ced_boundaries, file = "pkg/data/ced_boundaries.rda")


