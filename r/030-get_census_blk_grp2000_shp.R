## r/030-get_census_blk_grp2000_shp.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("r/"))

suppressPackageStartupMessages({
  library(CLmisc); library(sf); library(tidycensus); library(tigris);
  library(geoarrow); library(arrow); 
})

options(tigris_use_cache = TRUE)

census_api_key(keyring::key_get("census_api_key"))

states <- c(state.abb, "DC") %>% sort

output_dir <- here::here("census-blk-grp-shp/2000/")
mkdir_p(output_dir)

f_dwnld_shp <- function(st) {

  tmp <- tigris::block_groups(state = st, year = 2000)

  return(NULL)
}

lapply(states, f_dwnld_shp)

f_census_dwnld <- function(st) {
  
  save_file_loc <- paste0(output_dir, paste0(st, ".rds"))
  
  if (file.exists(save_file_loc)) return()
  
  dt <- get_decennial(geography = "block group", variables = c("H001001", "P001001"),
                      year = 2000, state = st) %>%
    as.data.table()
  
  dt_cleaned <- dt %>% 
    dcast(GEOID ~ variable, value.var = "value") %>%
    setnames("H001001", "hh2000") %>%
    setnames("P001001", "pop2000")

  dt_sf <- tigris::block_groups(state = st, year = 2000) %>%
    st_cast(to = "MULTIPOLYGON") %>%
    as.data.table() %>%
    setnames("BKGPIDFP00", "GEOID") %>%
    select_by_ref(c("GEOID", "geometry"))

  dt_out <- dt_cleaned[dt_sf, on = "GEOID"]
  
  saveRDS(dt_out, save_file_loc)

  
  return(NULL)
  
}

## f_census_dwnld("NH")

future::plan(future::multisession(workers = future::availableCores()))
future.apply::future_lapply(states, f_census_dwnld)

dt_all <- lapply(states, function(st) {
  readRDS(paste0(output_dir, paste0(st, ".rds")))
}) %>%
  rbindlist() %>%
  .[order(GEOID)]

saveRDS(dt_all, here::here("census-blk-grp-shp/census_blk_grp2000_shp.rds"))
