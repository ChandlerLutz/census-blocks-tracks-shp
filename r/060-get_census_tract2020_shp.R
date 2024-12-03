## r/060-get_census_tract2020_shp.R

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

output_dir <- here::here("census-tract-shp/2020/")
mkdir_p(output_dir)

f_dwnld_shp <- function(st) {

  tmp <- tigris::tracts(state = st, year = 2020)

  return(NULL)
}

lapply(states, f_dwnld_shp)

f_census_dwnld <- function(st) {
  
  save_file_loc <- paste0(output_dir, paste0(st, ".rds"))
  
  if (file.exists(save_file_loc)) return()
  
  dt <- get_decennial(geography = "tract", variables = c("H1_001N", "P1_001N"),
                      year = 2020, state = st) %>%
    as.data.table()
  
  dt_cleaned <- dt %>% 
    dcast(GEOID ~ variable, value.var = "value") %>%
    setnames("H1_001N", "hh2020") %>%
    setnames("P1_001N", "pop2020")

  dt_sf <- tigris::tracts(state = st, year = 2020) %>%
    st_cast(to = "MULTIPOLYGON") %>%
    as.data.table() %>%
    setnames("GEOID", "GEOID") %>%
    select_by_ref(c("GEOID", "geometry"))

  dt_out <- dt_cleaned[dt_sf, on = "GEOID"]
  
  saveRDS(dt_out, save_file_loc)
  
  return(NULL)
  
}

f_census_dwnld("NH")

future::plan(future::multisession(workers = future::availableCores()))
future.apply::future_lapply(states, f_census_dwnld)

dt_all <- lapply(states, function(st) {
  
  save_file_loc <- paste0(output_dir, paste0(st, ".rds"))
  
  return(readRDS(save_file_loc))
  
}) %>% rbindlist()

saveRDS(dt_all, here::here("census-tract-shp/census_tract2020_shp.rds"))
