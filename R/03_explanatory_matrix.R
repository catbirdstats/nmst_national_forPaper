# re-run 7/8/25 due to updated explanatory matrix; changed EIR to EMI

# re-run 11/6/24 due to updated Distances in NIW file

# re-run 9/27 to remove tidal fresh sites (also reran 02_combining)

# re-run 4/17 after removing original ACE files (ACE has split into 2 files)

# updated 4/16/24 to get updates and try to address joining issues
# updated 2/20/24 (general updates)

# read in explanatory matrix from the google drive
# then connect with the combined veg data file
# and write out both separate and combined data frames

library(tidyverse)
library(readxl)

expl_sheet <- here::here("latest-explanatory-matrices", "Explanatory Matrix.xlsx")

# just one row per reserve/site combination - will be repeated for every plot and date
time_component_no <- read_xlsx(expl_sheet, 
                      sheet = "Time removed",
                      skip = 5) %>% # unless someone adds another row
    filter(`Reserve File` != "EXAMPLE")  %>% 
    janitor::remove_empty("cols") %>% 
    select(Reserve = `Reserve File`,
           SiteID = `Site ID (reserve files)`,
           everything()) %>% 
    rename(`SET change` = `SET change (mm/yr)`) 

# a row for every year at each reserve - will be repeated for every site/transect/plot within a year
# but be different in different years
time_component_yes <- read_xlsx(expl_sheet, 
                             sheet = "Time added",
                             skip = 6,
                             na = c("", "NA"))  %>% 
    janitor::remove_empty("cols") %>% 
    rename(Reserve = `Reserve File`)

# make sure ACE shows up as both ACE-EIN and ACE-EIS
# that should be the only one with problems
ace2 <- time_component_yes %>% 
    filter(Reserve == "ACE") %>% 
    mutate(Reserve = "ACE-EIN")
time_component_yes <- time_component_yes %>% 
    mutate(Reserve = case_when(Reserve == "ACE" ~ "ACE-EIS",
                               .default = Reserve)) %>% 
    bind_rows(ace2)
rm(ace2)



# average time-based components to reserve level  
time_based_avgs <- time_component_yes %>% 
    summarize(.by = Reserve,
              across(c(`Local tidal range`,
                `Climate - temp avg`,
                `Climate - precip sum (avg)`),
              function(x) mean(x, na.rm = TRUE)))

# read in veg file  
veg <- read.csv(here::here("data", "compiled",
                           "national_plot-level.csv"))

# reserve name matching ----
# veg/slopes_by_site don't have the suffixes
# explanatory variables do have suffixes in reserve name
resStMatching <- time_component_no %>% 
    select(File = Reserve, 
           SiteID) %>% 
    separate(File, into = c("Res", "St"),
             remove = FALSE,
             fill = "right") %>% 
    select(-St)

veg <- veg %>% 
    left_join(resStMatching, by = c("Reserve" = "Res",
                                    "SiteID")) %>% 
    relocate(File) %>% 
    mutate(Reserve2 = case_when(is.na(File) ~ Reserve,
                                File != Reserve ~ File,
                                .default = Reserve),
           Vegetation_Zone = case_match(Vegetation_Zone,
                                    "M-Mudflat" ~ "Low",
                                    "S-Seaward Edge" ~ "Low",
                                    "L-Low Marsh" ~ "Low",
                                    "P-Pools/Pannes" ~ "Low",
                                    "T-Transition" ~ "Mid",
                                    "H-High Marsh" ~ "Mid",
                                    "UE-Upland Edge" ~ "Up",
                                    "FT-Freshwater Tidal" ~ "Up",
                                    "U-Upland" ~ "Up",
                                    .default = "Other")) %>% 
    relocate(Reserve2) %>% 
    select(-File, -Reserve) %>% 
    rename(Reserve = Reserve2)

# join into one data frame 
veg_and_expl <- left_join(veg, time_component_no, by = c("Reserve", "SiteID")) %>% 
    left_join(time_component_yes, by = c("Reserve", "Year"))

veg_and_expl$...7 <- NULL

# write out RData file with the data frames separate (but joinable)  
save(veg, time_component_no, time_component_yes, time_based_avgs,
     file = here::here("data", "compiled", "veg_and_expl_dfs.RData"))

# write out joined csv  
write.csv(veg_and_expl,
          file = here::here("data", "compiled", "veg_and_expl.csv"),
          row.names = FALSE,
          na = "")

# AFTER THIS POINT, TRANSITION TO NATIONAL SLOPE DF CONSTRUCTION.R


beepr::beep(8)
