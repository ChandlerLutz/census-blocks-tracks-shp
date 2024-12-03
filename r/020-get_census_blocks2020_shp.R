## r/020-get_census_blocks2020_shp.R

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

output_dir <- here::here("census-block-shp/2020/")
mkdir_p(output_dir)

f_dwnld_shp <- function(st) {

  tmp <- tigris::blocks(state = st, year = 2020)

  return(NULL)
}

lapply(states, f_dwnld_shp)

cnty2020_dt <- tigris::counties(year = 2020) %>%
  as.data.table() %>%
  setnames("COUNTYFP", "cntyfp20") %>%
  setnames("STATEFP", "stfp") %>%
  select_by_ref(c("cntyfp20", "stfp"))

dt_st <- tigris::states(year = 2020) %>%
  as.data.table() %>%
  setnames("STATEFP", "stfp") %>%
  setnames("STUSPS", "state") %>%
  select_by_ref(c("state", "stfp")) %>%
  .[order(stfp)]


f_census_dwnld <- function(st) {

  ## Note: 2020 census will only allow downloads of blocks by county
  
  save_file_loc <- paste0(output_dir, paste0(st, ".rds"))
  
  if (file.exists(save_file_loc)) return()

  st_fips <- dt_st[state == st, stfp]
  cnty_fips <- cnty2020_dt[stfp == c(st_fips), cntyfp20]

  dt <- lapply(cnty_fips, function(cntyfp) {
    get_decennial(geography = "block", variables = c("H1_001N", "P1_001N"), 
                  year = 2020, state = st_fips, county = cntyfp) %>%
      as.data.table() %>%
      dcast(GEOID ~ variable, value.var = "value") %>%
      setnames("H1_001N", "hh2020") %>%
      setnames("P1_001N", "pop2020")
  }) %>%
    rbindlist() %>%
    setkey("GEOID")
  
  dt_sf <- tigris::blocks(state = st, year = 2020) %>%
    as.data.table() %>%
    setnames("GEOID20", "GEOID") %>%
    select_by_ref(c("GEOID", "geometry"))

  if (nrow(dt) < nrow(dt_sf))
    stop(paste0("Error: For state ", st, " there are ", nrow(dt_sf) - nrow(dt), " blocks missing from the census data"))

  dt_out <- dt[dt_sf, on = "GEOID"]
  
  saveRDS(dt_out, save_file_loc)
  
  return(NULL)
  
}

## f_census_dwnld("NH")

future::plan(future::multisession(workers = future::availableCores()))
future.apply::future_lapply(states, f_census_dwnld)
