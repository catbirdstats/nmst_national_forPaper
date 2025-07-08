# latest run 7/8/2025, using updated explanatory matrix.
# copied over reserve data files; deleted intermediate and compiled files.
# downloaded explanatory matrix as excel file to use a permanent version.

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(tibble)
library(vegan)
# library(googlesheets4)
# library(googledrive)


path <- here::here("data", "reserve_level")
outpath <- here::here("data", "intermediate")
reserves <- stringr::str_sub(dir(path, pattern = "_veg.xlsx$"), end = -10)

# read in functions
source(here::here("R", "sourced", "functions_natl.R"))

# get set up for PI-vs-OC processing paths
source(here::here("R", "sourced", "001b_setup-for-PI-or-OC.R"))



strt<-Sys.time()

# process all stations
for(i in seq_along(reserves)) {
    res <- reserves[i]

    # path setup
    file_dat <- here::here(path, paste0(res, "_veg.xlsx"))
    file_specs <- here::here(path, paste0(res, "_veg-specs.xlsx"))
    file_out_grouped <- here::here(outpath, paste0(res, "_veg-grouped.RData"))

    
    print(res)
    
    # process the data
    source(here::here("R", "sourced", "001_pre-processing.R"))
    
    
    assign(res, dat_grouped)
    save(list = res, file = file_out_grouped)
    
    # remove all the data frames and stuff
    rm(list = res,
       dat, dat_grouped, dat_long, eis, species_info, stn_tbl)
    
}

Sys.time() - strt
beepr::beep(8)