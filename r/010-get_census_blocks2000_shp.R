## r/010-get_census_blocks2000_shp.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

suppressPackageStartupMessages({
  library(CLmisc); library(sf); library(tidycensus); library(tigris);
  library(geoarrow); library(arrow); 
})

##Set wd using the here package
setwd(here::here("r/"))

options(tigris_use_cache = TRUE)

census_api_key(keyring::key_get("census_api_key"))

states <- c(state.abb, "DC") %>% sort

output_dir <- here::here("census-block-shp/2000/")

f_dwnld_shp <- function(st) {

  tmp <- tigris::blocks(state = st, year = 2000)

  return(NULL)
}

lapply(states, f_dwnld_shp)



f_census_dwnld <- function(st) {
  
  save_file_loc <- paste0(output_dir, paste0(st, ".rds"))
  
  if (file.exists(save_file_loc)) return()
  
  dt <- get_decennial(geography = "block", variables = c("H001001", "P001001"),
                      year = 2000, state = st) %>%
    as.data.table()
  
  dt_cleaned <- dt %>% 
    dcast(GEOID ~ variable, value.var = "value") %>%
    setnames("H001001", "hh2000") %>%
    setnames("P001001", "pop2000")

  dt_sf <- tigris::blocks(state = st, year = 2000) %>%
    as.data.table() %>%
    setnames("BLKIDFP00", "GEOID") %>%
    select_by_ref(c("GEOID", "geometry"))

  dt_out <- dt_cleaned[dt_sf, on = "GEOID"]
  
  saveRDS(dt_out, save_file_loc)

  
  return(NULL)
  
}

## lapply(states, f_dwnld)

future::plan(future::multisession(workers = future::availableCores()))
future.apply::future_lapply(states, f_dwnld)

