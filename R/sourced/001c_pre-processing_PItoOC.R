reserve <- res

filename <- paste0(reserve, "_veg.xlsx")
file_in <- here::here("data", "reserve_level", filename)

mult_fct <- pi_conv$pi_mult_factor[which(pi_conv$reserve == reserve)]

# read data
dat_full <- get_data(file_in, cover_only = TRUE) %>%  # gets rid of density and height columns; keeps F_ columns
    mutate(uniqueID = paste0(SiteID, TransectID, PlotID, Year, Month, Day)) %>% 
    relocate(uniqueID)

dat_to_convert <- dat_full %>% 
    select(Total:ncol(.),
           -any_of(starts_with("F_")),
           -Total)    

# remove 'Other layer' species
species_info <- get_species_info(file_dat)
otherLayers <- unlist(species_info[species_info$Cover_Categories == "Other layer", "Species"])
dat_to_convert <- dat_to_convert %>% 
    select(-any_of(otherLayers))
rm(otherLayers)

# convert so it's like there were 100 points
dat_to100pts <- dat_to_convert %>% 
    mutate(across(everything(), function(x) x*mult_fct))

# apply regression/morphological correction factor
# need to connect species to archetypes first
morph_toJoin <- morph_dat %>% 
    select(species = "Species.name",
           Morphology = "Morphological Type") %>% 
    left_join(morph_conv) %>% 
    distinct()

dat_toMorph <- dat_to100pts %>% 
    mutate(uniqueID = dat_full$uniqueID) %>% 
    pivot_longer(-uniqueID,
                names_to = "species",
                values_to = "cover") %>% 
    left_join(morph_toJoin) %>% 
    mutate(Correction_Factor = case_when(is.na(Correction_Factor) ~ 1,
                                         .default = Correction_Factor),
           corrected_cover = cover * Correction_Factor)

dat_morphed <- dat_toMorph %>% 
    select(uniqueID, species, corrected_cover) %>% 
    pivot_wider(names_from = species,
                values_from = corrected_cover) %>% 
    column_to_rownames("uniqueID")


# normalize to 100
dat_morphed$rowTotal <- rowSums(dat_morphed, na.rm = TRUE)
to_process <- names(dat_morphed)[-which(names(dat_morphed) == "rowTotal")]
dat_forcedTo100 <- dat_morphed %>% 
    mutate(across(all_of(to_process),
                  .fns = ~round(./rowTotal * 100, 2))) %>% 
    select(-rowTotal) %>% 
    rownames_to_column("uniqueID")

rowTotals <- rowSums(dat_forcedTo100[2:ncol(dat_forcedTo100)], na.rm = TRUE)

# verify
hist(rowTotals, breaks = 20)

dat_forcedTo100$Total <- rowTotals


# reconnect it to the full data
dat_meta <- dat_full %>% 
    select(uniqueID:Total,
           -Unique_ID,
           -Total)
dat_out <- left_join(dat_meta, dat_forcedTo100) %>% 
    relocate(Total, .after = Notes)
# and F_ columns
f_cols <- paste0("F_", names(dat_forcedTo100))
dat_fs <- dat_full %>% 
    select(uniqueID,
           any_of(f_cols))
dat_out <- left_join(dat_out, dat_fs)


# write that out for later qa/qc checking
fileout <- paste0(reserve, "_OCfromPI.csv")
file_out <- here::here("data", "PItoOC", fileout)
write.csv(dat_out, file_out, 
          row.names = FALSE,
          na = "")

# cleanup
rm(reserve, filename, file_in, mult_fct,
   dat_full, dat_to_convert, dat_to100pts,
   morph_toJoin, dat_toMorph, dat_morphed,
   to_process, dat_forcedTo100,
   f_cols, dat_fs,
   dat_meta, fileout, file_out)

