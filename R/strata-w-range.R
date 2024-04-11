###
###
###
###


strata_w_range <- function(species = NULL,
                           source = "eBird",
                           seasonal_range = "breeding",
                           range_map = NULL,
                           crs_equal_area_custom = NULL,
                           coverage_strata_custom = NULL,
                           resolution = "27km"){


if(is.null(species)){
  stop("species argument is NULL, please select a species")
}
  # test if custom coverage strata
  if(is.null(coverage_strata_custom)){
    # if no coverage strata provided, the load the defaul 120km NA-wide grid
    f <- system.file("maps", package = "SurveyCoverage") %>%
      list.files(pattern = "hexagonal_grid_west_hemisphere_120km", full.names = TRUE)

    cov_grid <- readRDS(f)
  }else{ #if user supplied custom coverage strata (custom grid) then check valid
    cov_grid <- coverage_strata_custom

    if(any(!sf::st_is_valid(cov_grid))){
      cov_grid <- cov_grid %>%
        sf::st_make_valid()
    }
  }
  if(is.null(crs_equal_area_custom)){
    crs_equal_area_custom <- sf::st_crs(cov_grid)
  }

 if(!is.null(range_map)){
   # if a range map is provided, then skip the eBird download


  }else{
    #if range map not provided, then download the seasonal eBird range map

    species_ebird <- ebirdst::get_species(species)
    message(paste("Downloading ebirdst range data for",species,species_ebird))
    down <- try(ebirdst::ebirdst_download_status(species_ebird,
                                        download_ranges = TRUE,
                                        download_abundance = FALSE),
                silent = TRUE)

    if(class(down) == "try-error"){
      stop(paste("eBirdst range data do not exist for",species,species_ebird))
    }

    abd_seasonal_range <- ebirdst::load_ranges(species = species, #metric = "mean",
                                      resolution = resolution)  #27km low resolution



    if(seasonal_range %in% abd_seasonal_range$season){
      abd_range <- abd_seasonal_range %>%
        dplyr::filter(season == seasonal_range)
    }else{
      warning(paste("user selected season",seasonal_range,"does not exist in the
             eBirdst database. Trying eBirdst data for resident - year round"))
      seasonal_range <- "resident"
      abd_range <- abd_seasonal_range %>%
        dplyr::filter(season == seasonal_range)
    }


    west_crop <- cov_grid %>%
      sf::st_buffer(.,10000) %>%
      sf::st_transform(.,crs = sf::st_crs(abd_range)) %>%
      sf::st_make_valid()

    abd_range <- abd_range %>%
      sf::st_make_valid() %>%
      sf::st_crop(.,west_crop) %>%
      sf::st_make_valid() %>%
      sf::st_transform(.,crs = sf::st_crs(crs_equal_area_custom))

    message("intersecting range map and coverage grid")
    hex_range <- sf::st_intersection(abd_range,cov_grid) %>%
      sf::st_make_valid() %>%
      sf::st_union(.,by_feature = TRUE)


    message("recalculating area of grid cells inside range")
    hex_range <- hex_range %>%
      dplyr::mutate(area_km2 = as.numeric(sf::st_area(hex_range)/1e6))

    strata_w_range <- hex_range %>%
      dplyr::select(strata_name) %>%
      dplyr::mutate(ebird = TRUE)


    }

ret_list <- list(strata_w_range = strata_w_eBird_range,
                 coverage_grid = cov_grid)

return(ret_list)

}





