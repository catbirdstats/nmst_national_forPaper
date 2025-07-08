# most of this is copied from reserve level 01_Veg_analyses.Rmd

# check and see if reserve needs to go down PI pathway or not
pi_reserve <- res %in% pi_conv$reserve

# and send it down that pathway.... or not
if(pi_reserve){
    source(here::here("R", "sourced", "001c_pre-processing_PItoOC.R"))
    dat <- dat_out %>% 
        select(-uniqueID)
    # species_info sheet is read in already from 001c
} else {
    dat <- get_data(file_dat, cover_only = TRUE) %>%  # gets rid of density and height columns; keeps F_ columns
        select(Reserve, SiteID, TransectID, PlotID, Year, Month, Day,
               Total:ncol(.)) 
    species_info <- get_species_info(file_dat)
    otherLayers <- unlist(species_info[species_info$Cover_Categories == "Other layer", "Species"])
    dat <- dat %>% 
        select(-any_of(otherLayers))
    rm(otherLayers)
    
    # force to 100
    dat_toForce <- dat %>% 
        select(any_of(species_info$Species))
    dat_toForce$rowTotal <- rowSums(dat_toForce, na.rm = TRUE)
    to_process <- names(dat_toForce)[-which(names(dat_toForce) == "rowTotal")]
    dat_forcedTo100 <- dat_toForce %>% 
        mutate(across(all_of(to_process),
                      .fns = ~round(./rowTotal * 100, 2))) %>% 
        select(-rowTotal)
    dat_forcedTo100$Total <- rowSums(dat_forcedTo100, na.rm = TRUE)
    
    # verify
    hist(dat_forcedTo100$Total, breaks = 20)
    
    # replace columns in dat with the normalized ones
    common_columns <- intersect(names(dat), names(dat_forcedTo100))
    dat[common_columns] <- dat_forcedTo100[common_columns]
    
    # clean up
    rm(dat_toForce, dat_forcedTo100, to_process, common_columns)
}

# now do the rest of the pre-processing

# if there is an F_Record column, get rid of it
if("F_Record" %in% names(dat)) dat$F_Record <- NULL

stn_tbl <- get_stn_table(file_dat)
stn_tbl <- stn_tbl %>%
    mutate(PlotID_full = paste(SiteID, TransectID, PlotID, sep = "-")) %>% 
    select(-Type) %>%    # at least one file has multiple rows per plot based on different types
    distinct()
eis <- get_ecotone_invaders(file = file_specs)

# remove vegetation zones from eis data frame that don't appear in the station table
eis <- eis[eis$Vegetation_Zone %in% unique(stn_tbl$Vegetation_Zone), ]

# add some columns to dat
# and get rid of troublesome columns
dat <- dat %>% 
    select(-any_of(starts_with("Distance")), 
           -any_of(c("Vegetation_Zone", 
                   "Orthometric_Height"))) %>% 
    dplyr::mutate(Date = lubridate::decimal_date(lubridate::ymd(paste(Year, Month, Day, sep = "-"))),
                  Years_sinceStart = round(Date - min(Date), 4),
                  StTrns = paste(SiteID, TransectID, sep = "-"),
                  StTrnsPlt = paste(SiteID, TransectID, PlotID, sep = "-"),
                  Unique_ID = paste0(Reserve, SiteID, TransectID, PlotID,
                                     Year, Month, Day)) %>% 
    dplyr::relocate(c(Date, Years_sinceStart), .before = Year) %>% 
    dplyr::relocate(c(StTrns, StTrnsPlt, Unique_ID), .after = PlotID)


dat <- remove_suspect_values(dat, flags = c("-3"))  # remove suspect values. also removes F_columns.
dat <- remove_unsampleds(dat)  # should get rid of any dates where a plot was not sampled or was entirely rejected. 
dat <- na_to_0(dat)  # turn remaining NAs into 0s - this first makes sure all species columns are numeric
dat <- join_zones(dat, stn_tbl)  # add vegetation zones to each row
dat_long <- dat %>%
    pivot_longer(-(Reserve:Total),
                 names_to = "Species",
                 values_to = "Cover") %>% 
    left_join(species_info, by = "Species") %>% 
    mutate(Invasive = case_when(Native_Classification %in% c("Native invasive", "Non-native invasive") ~ "Invasive",
                                .default = "not invasive"),
           Salt_or_Fresh = case_when(Plant_Categories %in% c("B-Brackish", "H-Halophyte", "A-Algae") ~ "Salt",
                                     Plant_Categories %in% c("F-Freshwater", "U-Upland") ~ "Fresh",
                                     .default = "Neither"),
           Salt_or_Fresh = factor(Salt_or_Fresh, levels = c("Salt", "Fresh", "Neither")),
           grouping_category = case_when(!is.na(NMST_Groupings) ~ NMST_Groupings,
                                         !is.na(Plant_Categories) ~ Plant_Categories,
                                         .default = Cover_Categories))


# make the grouped data frame
dat_grouped <- dat_long %>% 
    summarize(.by = c(Reserve, SiteID, TransectID, PlotID, Year, Month, Day, grouping_category),
              Cover = sum(Cover, na.rm = TRUE)) %>% 
    mutate(grouping_category = case_when(grouping_category == "Live vegetation" ~ "Other live vegetation",
                                         .default = grouping_category)) %>% 
    pivot_wider(names_from = grouping_category,
                values_from = Cover)


# calculate metrics and add to both data frames
spps_live <- species_info$Species[species_info$Cover_Categories == "Live vegetation"]
spps_live <- spps_live[which(spps_live %in% names(dat))]
dat_live <- dat %>% 
    select(all_of(spps_live))
live_totals <- rowSums(dat_live, na.rm = TRUE) # for later joining to wide dfs
swdiv <- diversity(dat_live, index = "shannon")
rich <- specnumber(dat_live)
dat_div <- dat %>% 
    select(Reserve, SiteID, TransectID, PlotID, Year, Month, Day) %>% 
    mutate(SWdiv = swdiv,
           Richness = rich)
dat <- left_join(dat, dat_div)
dat_grouped <- left_join(dat_grouped, dat_div)
rm(dat_live, swdiv, rich, dat_div)

# attach EMI to data frame
# changed from EIR to EMI, 7/8/25
EMI <- dat_long %>% 
    left_join(eis) %>% 
    mutate(Invader = case_when(is.na(Invader) ~ 0,
                               .default = 1),
           EI_Cover = Cover * Invader) %>% 
    group_by(Year, Month, Day, Reserve, SiteID, TransectID, PlotID) %>% 
    summarize(EMI = round(sum(EI_Cover) / sum(Cover), 5))
dat <- left_join(dat, EMI)
dat_grouped <- left_join(dat_grouped, EMI)
rm(EMI)

Invasives <- dat_long %>% 
    summarize(.by = c(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Invasive),
              Invasive_Cover = sum(Cover, na.rm = TRUE)) %>% 
    filter(Invasive == "Invasive") %>% 
    select(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Invasive_Cover)
dat <- left_join(dat, Invasives)
dat_grouped <- left_join(dat_grouped, Invasives)
rm(Invasives)


# modify this to be Salt/Total, per email chain from 1/8/2024
# Re: Nat'l data frame - Re: Namaste TWG follow up
# by "Total", we want total live vegetation

Salt_to_Total <- dat_long %>%
    summarize(.by = c(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Salt_or_Fresh),
              Cover = sum(Cover, na.rm = TRUE)) %>%
    pivot_wider(names_from = Salt_or_Fresh,
                names_expand = TRUE,
                values_from = Cover) %>%
    mutate(Salt = case_when(is.na(Salt) ~ 0,
                            .default = Salt),
           Total_Live = live_totals,
           Salt_to_Total = case_when(Total_Live == 0 ~ NA_real_,
                                     .default = Salt / Total_Live)) %>%
    select(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Salt_to_Total)
dat <- left_join(dat, Salt_to_Total)
dat_grouped <- left_join(dat_grouped, Salt_to_Total)
rm(Salt_to_Total)

Unveg_to_veg <- dat_long %>% 
    summarize(.by = c(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Cover_Categories),
              Cover = sum(Cover, na.rm = TRUE)) %>% 
    pivot_wider(names_from = Cover_Categories,
                values_from = Cover) %>% 
    mutate(Unveg_to_veg = `Unvegetated category` / `Live vegetation`) %>% 
    select(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Unveg_to_veg)
dat <- left_join(dat, Unveg_to_veg)
dat_grouped <- left_join(dat_grouped, Unveg_to_veg)
rm(Unveg_to_veg)


total_live_abiotic <- dat_long %>% 
    summarize(.by = c(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Cover_Categories),
              Cover = sum(Cover, na.rm = TRUE)) %>% 
    mutate(Cover_Categories = case_when(Cover_Categories == "Unvegetated category" ~ "Total unvegetated",
                                        Cover_Categories == "Live vegetation" ~ "Total live veg",
                                        .default = Cover_Categories)) %>% 
    pivot_wider(names_from = Cover_Categories,
                values_from = Cover) %>% 
    select(Year, Month, Day, Reserve, SiteID, TransectID, PlotID,
           `Total unvegetated`,
           `Total live veg`)
dat <- left_join(dat, total_live_abiotic)
dat_grouped <- left_join(dat_grouped, total_live_abiotic)
rm(total_live_abiotic)

# add all relevant columns from dat to dat_grouped
# including specified species
cols_from_dat <- c("Reserve", "SiteID", "TransectID", "PlotID",
                     "Vegetation_Zone", "Year", "Month", "Day",
                     "Date", "Unique_ID", "Years_sinceStart",
                     "Spartina alterniflora", "Juncus roemerianus",
                     "Spartina patens", "Salicornia pacifica")
dat_grouped <- dat %>% 
    select(any_of(cols_from_dat)) %>% 
    full_join(dat_grouped)


# add relevant columns from station table to both data frames
cols_from_stntbl <- c("Reserve", "SiteID", "TransectID", "PlotID",
                      "Latitude", "Longitude",
                      "Orthometric_Height", 
                      "Distance_to_Water")
tmp <- stn_tbl %>% 
    select(any_of(cols_from_stntbl)) %>% 
    distinct()
dat <- left_join(dat, tmp)
dat_grouped <- left_join(dat_grouped, tmp)
rm(cols_from_dat, cols_from_stntbl, tmp)

