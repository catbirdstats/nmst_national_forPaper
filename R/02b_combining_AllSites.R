# last update/run 
# 7/8/25 - using updated explanatory matrix; EIR changed to EMI
# 1/27/25 - remove "excluded" sites so all sites are present,
# per Chris P. email request
# 11/6/24 - updated Distances in NIW file
# 9/27/24 - had to modify APA-LSM in expl matrix and remove FW sites

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(purrr)

path <- here::here("data", "intermediate")
outpath <- here::here("data", "compiled")
file_out_grouped <- here::here(outpath, "national_plot-level_AllSites.csv")

reserves <- stringr::str_sub(dir(path, pattern = "_veg-grouped.RData"), end = -19)

# read in functions
source(here::here("R", "sourced", "functions_natl.R"))

# explanatory matrix for screening out sites - commented out; shouldn't need
# expl_sheet <- here::here("data", "Explanatory Matrix.xlsx")

# just one row per reserve/site combination - will be repeated for every plot and date
# expl_dat <- read_xlsx(expl_sheet, 
#                       sheet = "Time removed",
#                       skip = 5) %>% # unless someone adds another row
#     filter(`Reserve File` != "EXAMPLE")  %>% 
#     janitor::remove_empty("cols") %>% 
#     select(Reserve = `Reserve File`,
#            SiteID = `Site ID (reserve files)`,
#            everything()) %>% 
#     janitor::clean_names()
# sites_to_remove <- expl_dat[which(expl_dat$remove_site_from_national_analysis == "Y"), 1:2]
# sites_to_remove <- sites_to_remove %>% 
#     mutate(reserve = case_when(str_starts(reserve, "CBM") ~ "CBM",
#                                str_starts(reserve, "NOC") ~ "NOC",
#                                str_starts(reserve, "APA") ~ "APA",
#                                .default = reserve),
#            ResSt = paste(reserve, site_id))

strt<-Sys.time()

# read all stations in to lists and then bind into data frames with purrr

groupedSpecies <- map(reserves, 
                      ~get(load(here::here(path, paste0(., "_veg-grouped.RData"))))) %>% 
    list_rbind()


# REMOVE PLOTS BASED ON SCREENING CRITERIA (from Explanatory Matrix)
groupedSpecies <- groupedSpecies %>% 
    mutate(ResSite = paste(Reserve, SiteID)) 
    # filter(!(ResSite %in% sites_to_remove$ResSt))


# put columns in a nicer order?
groupedSpecies <- groupedSpecies %>% 
    select(# identifying cols
        Reserve:Years_sinceStart,
        Latitude, Longitude, Orthometric_Height,
        Distance_to_Water,
        # abiotic catebories   
        `Total unvegetated`, Bare, Dead, Rock, Wood,
        Wrack, Water, `Other Unvegetated`,   # Overstory, 
        # live plant categories   
        `Total live veg`, `A-Algae`, `B-Brackish`,
        `F-Freshwater`, `H-Halophyte`, `U-Upland`,
        `Other live vegetation`,
        # metrics and ratios
        EMI, Richness, SWdiv, 
        Invasive_Cover, Unveg_to_veg,
        Salt_to_Total,
        # specific species
        any_of(c("Spartina alterniflora", "Spartina patens",
        "Juncus roemerianus", "Salicornia pacifica")),
        # catchall
        everything()
    )


# write out the data frames  
write.csv(groupedSpecies, file = file_out_grouped, row.names = FALSE, na = "")

Sys.time() - strt
beepr::beep(8)
