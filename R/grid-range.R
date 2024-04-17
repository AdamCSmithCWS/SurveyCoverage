###
###
###
###


#' Title
#'
#' @param species character English common name of bird species to
#' support range data download using eBirdst package
#' @param seasonal_range character default = breeding, season of
#' monitoring program data and if possible range map
#' @param range_map optional sf package simple features polygon layer defining
#' the species' seasonal range
#' @param crs_equal_area_custom optional crs numerical identifier of equal area
#' coordinate system
#' @param coverage_grid_custom optional sf package simple features polygon
#' layer, equal area grid within which to calculate coverage
#' @param resolution character default = 27km (low resolution), argument passed
#' to `ebirdst::load_ranges()` function
#'
#' @return A list
#' @export
#'
#' @examples
#' # requires an ebirdst access key
#' # see ?ebirdst::set_ebirdst_access_key
#'
#' ex_range <- grid_range(species = "Baird's Sparrow")
#'
grid_range <- function(species = NULL,
                           seasonal_range = "breeding",
                           range_map = NULL,
                           crs_equal_area_custom = NULL,
                           coverage_grid_custom = NULL,
                           resolution = "27km"){


if(is.null(species)){
  stop("species argument is NULL, please select a species")
}
  # test if custom coverage grid
  if(is.null(coverage_grid_custom)){
    # if no coverage grid provided, the load the defaul 120km NA-wide grid
    f <- system.file("maps", package = "SurveyCoverage") %>%
      list.files(pattern = "hexagonal_grid_west_hemisphere_120km", full.names = TRUE)

    cov_grid <- readRDS(f)
  }else{ #if user supplied custom coverage grid (custom grid) then check valid
    cov_grid <- coverage_grid_custom

    if(any(!sf::st_is_valid(cov_grid))){
      cov_grid <- cov_grid %>%
        sf::st_make_valid()
    }
  }
  if(is.null(crs_equal_area_custom)){
    crs_equal_area_custom <- sf::st_crs(cov_grid)
  }else{
    crs_equal_area_custom <- sf::st_crs(crs_equal_area_custom)
  }

 if(!is.null(range_map)){
   # if a range map is provided, then skip the eBird download
   stop("SurveyCoverage package does not yet handle custom range maps.")

  }else{
    #if range map not provided, then download the seasonal eBird range map

    species_ebird <- ebirdst::get_species(species)
    message(paste("Downloading ebirdst range data for",species,species_ebird))
    down <- try(ebirdst::ebirdst_download_status(species_ebird,
                                        download_ranges = TRUE,
                                        download_abundance = FALSE),
                silent = TRUE)

    if(methods::is(down,"try-error")){
      stop(paste("eBirdst range data do not exist for",species,species_ebird))
    }

    abd_seasonal_range <- ebirdst::load_ranges(species = species, #metric = "mean",
                                      resolution = resolution)  #27km low resolution



    if(seasonal_range %in% abd_seasonal_range$season){
      abd_range_original <- abd_seasonal_range %>%
        dplyr::filter(season == seasonal_range)%>%
        sf::st_make_valid()
    }else{
      warning(paste("user selected season",seasonal_range,"does not exist in the
             eBirdst database. Trying eBirdst data for resident - year round"))
      seasonal_range <- "resident"
      abd_range_original <- abd_seasonal_range %>%
        dplyr::filter(season == seasonal_range) %>%
        sf::st_make_valid()
    }


    message("cropping range map to the Western Hemisphere")
    #generate a simplified cropping polygon to exclude range outside
    #of the coverage grid (supplied or default)
    west_crop <- cov_grid %>%
      sf::st_union() %>%
      sf::st_buffer(.,100000) %>%
      sf::st_transform(.,crs = sf::st_crs(abd_range_original)) %>%
      sf::st_make_valid()

    #intersection of cropping polygon and species' range
    abd_range <- suppressWarnings(sf::st_intersection(abd_range_original,west_crop)) %>%
      sf::st_make_valid()
    #transform to an equal area projection
    abd_range <- suppressWarnings(
      sf::st_transform(abd_range,crs = crs_equal_area_custom)
    )
    abd_range <- sf::st_make_valid(abd_range)

    message("intersecting range map and coverage grid")
    hex_range <- suppressWarnings(sf::st_intersection(abd_range,cov_grid)) %>%
      sf::st_make_valid() %>%
      sf::st_union(.,by_feature = TRUE)


    message("recalculating land area of grid cells inside range")
    hex_range <- hex_range %>%
      dplyr::mutate(area_km2_inrange = as.numeric(sf::st_area(hex_range)/1e6))

      #generate map of the grid with proportion of gridcell area inside range
    grid_w_range <- hex_range %>%
      dplyr::select(grid_cell_name,
                    area_km2_inrange)
      #join proportion estimates to the coverage grid map
    grid_w_range_join <- grid_w_range %>%
      sf::st_drop_geometry()


    cov_grid <- cov_grid %>%
      dplyr::left_join(.,grid_w_range_join,
                by = "grid_cell_name") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(area_km2_inrange = ifelse(is.na(area_km2_inrange),
                                       0,area_km2_inrange),
             proportion_in_range = area_km2_inrange/area_km2)


    }

  full_r <- sum(sf::st_area(abd_range)) %>%
    units::set_units(.,"km^2")
  full_r_grid <- sum(sf::st_area(grid_w_range))%>%
    units::set_units(.,"km^2")


ret_list <- list(coverage_grid = cov_grid,
                 range_map = abd_range,
                 rangemap_area = full_r,
                 range_area_gridded = full_r_grid)

return(ret_list)

}





