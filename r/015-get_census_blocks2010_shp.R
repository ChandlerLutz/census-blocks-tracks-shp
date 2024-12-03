## r/015-get_census_blocks2010_shp.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())



suppressPackageStartupMessages({
  library(CLmisc); library(sf); library(tidycensus); library(tigris);
})

##Set wd using the here package
setwd(here::here("r/"))

options(tigris_use_cache = TRUE)

census_api_key(keyring::key_get("census_api_key"))

states <- c(state.abb, "DC") %>% sort

output_dir <- here::here("census-block-shp/2010/")
mkdir_p(output_dir)

f_dwnld_shp <- function(st) {

  tmp <- tigris::blocks(state = st, year = 2010)

  return(NULL)
}

lapply(states, f_dwnld_shp)

cnty2010_dt <- tigris::counties(year = 2010) %>%
  as.data.table() %>%
  setnames("COUNTYFP", "cntyfp10") %>%
  setnames("STATEFP", "stfp") %>%
  select_by_ref(c("cntyfp10", "stfp"))

dt_st <- tigris::states(year = 2010) %>%
  as.data.table() %>%
  setnames("STATEFP10", "stfp") %>%
  setnames("STUSPS10", "state") %>%
  select_by_ref(c("state", "stfp")) %>%
  .[order(stfp)]
  
  

f_census_dwnld <- function(st) {

  ## Note: 2010 census will only allow downloads of blocks by county
  
  save_file_loc <- paste0(output_dir, paste0(st, ".rds"))
  
  if (file.exists(save_file_loc)) return()

  st_fips <- dt_st[state == st, stfp]
  cnty_fips <- cnty2010_dt[stfp == c(st_fips), cntyfp10]

  dt <- lapply(cnty_fips, function(cntyfp) {
    get_decennial(geography = "block", variables = c("H001001", "P001001"), 
                  year = 2010, state = st_fips, county = cntyfp) %>%
      as.data.table() %>%
      dcast(GEOID ~ variable, value.var = "value") %>%
      setnames("H001001", "hh2010") %>%
      setnames("P001001", "pop2010")
  }) %>%
    rbindlist() %>%
    setkey("GEOID")
  
  dt_sf <- tigris::blocks(state = st, year = 2010) %>%
    as.data.table() %>%
    setnames("GEOID10", "GEOID") %>%
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
