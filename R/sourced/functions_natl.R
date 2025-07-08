# mostly a subset of functions used at reserve level

# Importing ----

get_data <- function(file,
                     cover_only = FALSE){
    
    if(cover_only == FALSE){
        readxl::read_xlsx(file,
                          sheet = "Cover",
                          guess_max = 5000) %>% 
            mutate(across(c(Reserve, SiteID, TransectID, PlotID), as.character)) %>% 
            arrange(Year, Month, Day, SiteID, TransectID, PlotID)
    } else {
        readxl::read_xlsx(file,
                          sheet = "Cover",
                          guess_max = 5000) %>% 
            dplyr::select(-starts_with("Average Canopy Height"),
                          -starts_with("Maximum Canopy Height"),
                          -starts_with("Density"),
                          -starts_with("Height"),
                          -starts_with("Diameter")) %>% 
            mutate(across(c(Reserve, SiteID, TransectID, PlotID), as.character)) %>% 
            arrange(Year, Month, Day, SiteID, TransectID, PlotID)
    }
}

get_stn_table <- function(file){
    readxl::read_xlsx(file,
                      sheet = "Station_Table") %>% 
        mutate(across(c(Reserve, SiteID, TransectID, PlotID), as.character))
}

get_species_info <- function(file){
    tmp <- readxl::read_xlsx(file,
                             sheet = "Species_Names")
    # find the first column that starts with "Species"
    # rename it to only be "Species"
    spcol <- min(which(str_starts(names(tmp), "Species")))
    names(tmp)[spcol] <- "Species"
    
    tmp %>% 
        select(Species, Plant_Categories, Native_Classification,
               Cover_Categories, NMST_Groupings)
}

get_zone_ordering <- function(file){
    tmp <- read_xlsx(file,
                     sheet = "Ecotone_Invaders")
    # only keep zones that are in the station table
    tmp <- tmp[names(tmp) %in% unique(stn_tbl$Vegetation_Zone)]
    # put them in order
    zones <- data.frame(order = 1:ncol(tmp),
                        zone = names(tmp))
    forcats::fct_reorder(zones$zone, zones$order)
}

get_eis <- function(file){
    read_xlsx(file,
              sheet = "Ecotone_Invaders") %>% 
        mutate(rownum = row_number()) %>% 
        pivot_longer(-rownum,
                     names_to = "Vegetation_Zone",
                     values_to = "Species") %>% 
        filter(!is.na(Species)) %>% 
        mutate(Invader = 1) %>% 
        select(-rownum) %>% 
        arrange(Vegetation_Zone, Species)
}

get_anaSpecs <- function(file){
    read_xlsx(file,
              sheet = "Analysis_Specs") %>% 
        select(R_anaName, R_varName, Choice) 
}


find_unsampleds <- function(data){
    to_find_empties <- data %>% 
        dplyr::select(-(Reserve:Total),
                      -starts_with("F"))
    # from code behind janitor::remove_empty; line 12
    empty_rows <- rowSums(is.na(to_find_empties)) == ncol(to_find_empties)
    data[empty_rows, ] %>% 
        dplyr::select(SiteID, TransectID, PlotID, Year, Month, Day)
}

# Modifying ----

remove_unsampleds <- function(data){
    to_find_empties <- data %>% 
        dplyr::select(-(Reserve:Total),
                      -starts_with("F"))
    # from code behind janitor::remove_empty; line 12
    empty_rows <- rowSums(is.na(to_find_empties)) == ncol(to_find_empties)
    data[!empty_rows, , drop = FALSE]
}


na_to_0 <- function(data){
    dat_tmp <- data
    start <- which(names(dat_tmp) == "Total") + 1  # first species
    end <- ncol(dat_tmp)  # last species (F_ columns should already have been removed)
    dat_tmp[start:end] <- apply(dat_tmp[start:end], MARGIN = 2, as.numeric) # make sure everything is numeric
    dat_tmp[start:end][is.na(dat_tmp[start:end])] <- 0
    dat_tmp
}


find_suspect_values <- function(data, flags){
    
    # THIS IS IMPERFECT -
    # if "1" is given as a flag, -1 will also be found
    # negative flags work fine (e.g. specifying -3 will not remove 3)
    
    # split data frame into data and qaqc columns
    
    # there may or may not be an F_Record column somewhere before 'Total'
    # so pick the first F_ column after the 'Total' column
    # what are all the ones that start with F_
    qaqc_cols <- which(str_starts(names(data), "F_"))
    # where is the "Total" column
    total_col <- which(names(data) == "Total")
    # what's the value of the first qaqc_col after Total
    qaqc_cols_start <- qaqc_cols[min(which(qaqc_cols > total_col))]
    
    # now get the data; should end right before the first qaqc col
    data_alone <- data[, 1:(qaqc_cols_start-1)]
    qaqc_cols <- data[, qaqc_cols_start:ncol(data)]
    sampling_info <- data[, c("SiteID", "TransectID", "PlotID", 
                              "Year", "Month", "Day")]
    
    # make sure qaqc columns are character format
    qaqc_cols <- qaqc_cols %>% 
        mutate(across(everything(), as.character))
    
    # loop through qaqc columns and find any values containing the flags to be removed
    outs <- list()
    for(i in seq_along(qaqc_cols)){
        non_na <- which(!is.na(qaqc_cols[, i]))
        vals <- qaqc_cols[non_na, i]
        tmp <- data.frame(Species = rep(names(qaqc_cols)[i], nrow(vals)),
                          Rows = non_na,
                          Codes = vals)
        names(tmp)[3] <- "Codes"
        
        collapsed <- paste(flags, collapse = "|")
        tmp$kick_out <- grepl(collapsed, tmp$Codes)
        tmp <- tmp[tmp$kick_out == TRUE, ]
        
        outs[[i]] <- tmp
    }
    
    outs2 <- dplyr::bind_rows(outs) %>% 
        mutate(Species = str_remove(Species, "F_"))
    
    meta_flags <- sampling_info %>% 
        mutate(Rows = row_number()) %>% 
        filter(Rows %in% outs2$Rows)
    
    final_flags <- left_join(outs2, meta_flags) %>% 
        select(SiteID, TransectID, PlotID,
               Year, Month, Day,
               Species, "Flag" = Codes) %>% 
        arrange(Year, Month, Day, SiteID, TransectID, PlotID, Species)
    return(final_flags)
}


remove_suspect_values <- function(data,
                                  flags){
    
    # THIS IS IMPERFECT -
    # if "1" is given as a flag, -1 will also be removed
    # negative flags work fine (e.g. specifying -3 will not remove 3)
    
    # split data frame into data and qaqc columns
    
    # there may or may not be an F_Record column somewhere before 'Total'
    # so pick the first F_ column after the 'Total' column
    # what are all the ones that start with F_
    qaqc_cols <- which(str_starts(names(data), "F_"))
    # where is the "Total" column
    total_col <- which(names(data) == "Total")
    # what's the value of the first qaqc_col after Total
    qaqc_cols_start <- qaqc_cols[min(which(qaqc_cols > total_col))]  
    
    # now get the data; should end right before the first qaqc col
    data_alone <- data[, 1:(qaqc_cols_start-1)]
    qaqc_cols <- data[, qaqc_cols_start:ncol(data)]
    
    # make sure qaqc columns are character format
    qaqc_cols <- qaqc_cols %>% 
        mutate(across(everything(), as.character))
    
    # loop through qaqc columns and find any values containing the flags to be removed
    outs <- list()
    for(i in seq_along(qaqc_cols)){
        non_na <- which(!is.na(qaqc_cols[, i]))
        vals <- qaqc_cols[non_na, i]
        tmp <- data.frame(Species = rep(names(qaqc_cols)[i], nrow(vals)),
                          Rows = non_na,
                          Codes = vals)
        names(tmp)[3] <- "Codes"
        
        collapsed <- paste(flags, collapse = "|")
        tmp$kick_out <- grepl(collapsed, tmp$Codes)
        tmp <- tmp[tmp$kick_out == TRUE, ]
        
        outs[[i]] <- tmp
    }
    
    outs2 <- dplyr::bind_rows(outs) %>% 
        mutate(Species = str_remove(Species, "F_"))
    
    # now in the data-only frame, replace with NAs
    # loop through each species in output, ID rows, and replace
    for(i in seq_along(unique(outs2$Species))){
        nm <- unique(outs2$Species)[i]
        rows <- outs2 %>% 
            filter(Species == nm) %>% 
            select(Rows) %>% 
            unlist()
        data_alone[rows, nm] <- NA
    }
    
    return(data_alone)
}


lump_species <- function(data, summ_fun, n){
    # 'data' input is a long data frame, 'dat_long'
    summ <- data %>% 
        group_by(Species) %>% 
        summarize(y_mean = mean(Cover, na.rm = TRUE),
                  y_quantile = quantile(Cover, probs = 0.95, na.rm = TRUE),
                  y_nonzero = sum(Cover != 0, na.rm = TRUE)) %>% 
        ungroup()
    
    if(summ_fun == "mean"){
        tops <- summ %>% 
            slice_max(order_by = y_mean, n = n) %>% 
            select(Species) %>% 
            unlist()
    }
    
    if(summ_fun == "quantile"){
        tops <- summ %>% 
            slice_max(order_by = y_quantile, n = n) %>% 
            select(Species) %>% 
            unlist()
    }
    
    if(summ_fun == "nonzero"){
        tops <- summ %>% 
            slice_max(order_by = y_nonzero, n = n) %>% 
            select(Species) %>% 
            unlist()
    }
    
    out <- data %>% 
        mutate(Species = case_when(!(Species %in% tops) ~ "Other",
                                   .default = Species)) %>% 
        group_by(Reserve, SiteID, TransectID, PlotID,
                 Vegetation_Zone,
                 Year, Month, Day,
                 Species) %>% 
        summarize(Cover = sum(Cover, na.rm = TRUE)) %>% 
        ungroup()
    
    return(out)
    
}


# Ecotone Invader species - get list by plant category  
get_ei_spps_from_groups <- function(data){
    # input is previously created 'eis' data frame
    ei_groups <- data %>% filter(Group == TRUE)
    
    to_add <- list()
    
    for(i in seq_along(unique(ei_groups$eiID))){
        ei <- unique(ei_groups$eiID)[i]
        
        # find which vegetation zones this group occurs in
        zones <- ei_groups %>% 
            filter(eiID == ei) %>% 
            select(Vegetation_Zone) %>% 
            unlist()
        
        # pull out list of all species in this group
        spps <- species_info %>% 
            filter(Plant_Categories == ei) %>% 
            select(Species) %>% 
            unlist()
        
        # make a data frame of all vegetation zones crossed with all these species; Invader = 1, Species = TRUE, Group = FALSE
        addls <- expand.grid(zones, spps, stringsAsFactors = FALSE)
        addls$Invader <- 1
        addls$Species <- TRUE
        addls$Group <- FALSE
        names(addls)[1:2] <- c("Vegetation_Zone", "eiID")
        
        to_add[[i]] <- addls
    }
    
    bind_rows(to_add)
}

# Ecotone Invaders - do the whole thing
get_ecotone_invaders <- function(file){
    eis <- get_eis(file)  # ecotone invaders
    names(eis)[2] <- "eiID"
    
    # figure out whether what's given is a species or a group
    eis$Species <- eis$eiID %in% species_info$Species
    eis$Group <- eis$eiID %in% species_info$Plant_Categories
    
    # make a data frame including named species as well as all species within named groups
    ei_spps <- get_ei_spps_from_groups(data = eis)
    eis <- bind_rows(eis, ei_spps) %>% 
        filter(Species == TRUE) %>% 
        select(-Species, -Group) %>% 
        rename(Species = eiID) %>% 
        distinct() %>% 
        arrange(Vegetation_Zone, Species)
    
    return(eis)
}

relevel_spps <- function(data){
    # data is a vector of species
    if("Unvegetated" %in% data){
        data <- forcats::fct_relevel(data, "Unvegetated", after = Inf)
    } else if ("unvegetated" %in% data){
        data <- forcats::fct_relevel(data, "unvegetated", after = Inf)
    }
    
    out <- forcats::fct_relevel(data, "Other", after = Inf)
    
    return(out)
}


# Joins ----  

join_zones <- function(data = dat,
                       station_info = stn_tbl){
    zones_to_join <- dplyr::distinct(station_info[, c("Reserve", "SiteID", "TransectID", "PlotID", "Vegetation_Zone")])
    # used 'distinct' above because at least one file has multiple rows per plot
    # one for type 'E', one for type 'S'
    
    dplyr::left_join(data, zones_to_join) %>% 
        relocate(Vegetation_Zone, .before = SiteID)
}


# Summaries ----

sum_category <- function(plant_cat,
                         data = dat, 
                         spp_data = species_info){
    
    cat_names <- spp_data$Species[which(spp_data$Plant_Categories == plant_cat)]
    cat_cols <- which(names(data) %in% cat_names)
    apply(data[,c(cat_cols)], 1, function(x) sum(x, na.rm = TRUE))
}


sum_spp_categories <- function(data = dat, 
                               spp_data = species_info){
    # uses sum_category() from above
    cats_to_summ <- unique(spp_data$Plant_Categories)
    as.data.frame(sapply(cats_to_summ, sum_category, USE.NAMES = TRUE))
}

make_spec_df <- function(data, specs){
    # input 'data' is long data frame
    # specs is a vector of choices, from the user input file
    # output is a long data frame
    
    # if a species was named as a choice, but also appears in a group
    # that was named, it will be presented only as the species
    # and the 'group' total will not include it
    df <- data
    tmp <- specs
    df %>% 
        left_join(species_info, by = "Species") %>% 
        mutate(Species_or_Group = case_when(Species %in% tmp ~ Species,
                                            Plant_Categories %in% tmp ~ Plant_Categories,
                                            .default = "Other")) %>% 
        group_by(Reserve, SiteID, TransectID, PlotID,
                 StTrns, StTrnsPlt,
                 Vegetation_Zone,
                 Year, Month, Day, Years_sinceStart,
                 Species_or_Group) %>% 
        summarize(Cover = sum(Cover, na.rm = TRUE)) %>% 
        ungroup() 
}
# would like to figure out how to look at the provided Plant_Categories
# and see if any species were removed because they were named separately
# and if so, rename it to "Other [category]"


# Tables ----
kbl_nmst <- function(data, ...) {
    kable(data, 
          format.args = list(scientific = FALSE),
          ...) %>% 
        kable_styling("striped",
                      full_width = FALSE,
                      position = "left")
}


# Plots ----

plot_through_time <- function(data,
                              group,
                              panels){
    ggplot(data, aes(x = Year, y = Cover, 
                     group = {{group}}, color = {{group}}, fill = {{group}})) +
        geom_point() +
        geom_smooth(method = "loess", se = FALSE) +
        facet_wrap(enquo(panels))
}


# the next two functions are so geom_label_repel in 2 layers will recognize each other
# https://github.com/slowkow/ggrepel/issues/153#issuecomment-866866201
# many thanks to Atusy in Posit forums for this code
geom_label_repel2 <- function(...) {
    layer <- ggrepel::geom_label_repel(...)
    layer$ggrepel <- TRUE
    class(layer) <- c("ggrepel", class(layer))
    return(layer)
}

ggplot_add.ggrepel <- function(object, plot, object_name) {
    if (any(do.call(c, lapply(plot$layer, function(x) x$ggrepel)))) {
        warning(
            "There is more than one ggrepel layers. ",
            "This may cause overlap of labels"
        )
    }
    # Optionally, one may modify `object` here.
    NextMethod("ggplot_add")
}


plot_nmds <- function(scores = data.scores,
                      species = species.scores,
                      env.vars = en_coord_cat,
                      axes = c(1, 2)){
    xax <- paste0("NMDS", axes[1])
    yax <- paste0("NMDS", axes[2])
    
    pals_zone_abbrev <- palcols_zones
    names(pals_zone_abbrev) <- NULL
    
    ggplot() +
        geom_point(data = scores,
                   aes(x = !!ensym(xax), y = !!ensym(yax),
                       col = Zone_abbrev),
                   size = 1, alpha = 0.5) +
        geom_segment_interactive(data = species, 
                                 aes(x = 0, y = 0,
                                     xend = !!ensym(xax), yend = !!ensym(yax),
                                     tooltip = species),
                                 col = "black",
                                 arrow = arrow(length = unit(0.15, "inches")),
                                 linewidth = 0.4)  +
        geom_point_interactive(data = env.vars, 
                               aes(x = !!ensym(xax), y = !!ensym(yax),
                                   tooltip = Zone_Years, data_id = Zone_Timegroup), 
                               shape = "diamond", 
                               size = 4, alpha = 0.8, colour = "navy")  +
        geom_label_repel2(data = species,  # species labels
                          aes(x = !!ensym(xax), y = !!ensym(yax)),
                          label = species$species,
                          col = "black",
                          size = 2.5,
                          force_pull = 0.2,
                          force = 2,
                          direction = "both",
                          label.size = NA,
                          label.padding = 0.1,
                          fill = alpha(c("white"),0.5)) + 
        geom_label_repel2(data = env.vars,  # zone/time labels
                          aes(x = !!ensym(xax), y = !!ensym(yax),
                              col = Zone_abbrev
                          ),
                          label = env.vars$Zone_Timegroup,
                          size = 3,
                          fontface = "bold",
                          max.overlaps = 20,
                          min.segment.length = 0.1,
                          force = 5,
                          force_pull = 0.2,
                          direction = "both",
                          point.padding = 0.5,
                          label.size = NA,
                          label.padding = 0.2,
                          fill = alpha(c("white"),0.9)) +
        # theme_bw() +
        scale_color_manual(values = pals_zone_abbrev) +
        scale_fill_manual(values = pals_zone_abbrev) +
        labs(title = paste("Ordination results, axes", axes[1], "and", axes[2]),
             subtitle = "Diamonds: Zone/Time centroids. Arrows: Species.") +
        theme(legend.position = "none")
}




# Models ----

# homemade function to get marginal trends out of models  
get_trends <- function(model){
    nm <- enexpr(model)
    tmp <- data.frame(emtrends(model, 
                               pairwise ~ Vegetation_Zone, 
                               var = "Years_sinceStart")$emtrends)
    # tmp$mod <- enexpr(model)
    # tmp$mod <- as.character(nm)
    return(tmp)
}

check_singularity <- function(model){
    # model needs to be from lme4
    nm <- enexpr(model)
    mod <- model
    cat(paste0(nm, ": \n"))
    if(isSingular(mod)){
        cat("is singular. VarCorr follows. \n \n")
        print(summary(mod)$varcor)
        cat("\n \n")
    } else {
        cat("fit is okay (not singular) \n \n")
    }
}