###
###
###
###


#' Title
#'
#' @param species character English common name of bird species to
#' support range data download using eBirdst package
#' @param seasonal_range character, ignored if range_map is supplied. default = breeding, season of range map to
#' download. Acceptable values depend on the species' range estimates available
#' from ebirdst, including: "breeding", "nonbreeding", "postbreeding_migration",
#' and "prebreeding_migration", for migratory species and "resident" for
#' non-migratory species.
#' @param range_map Optional user-loaded range map, required to be
#' e.g., sf package polygon layer defining the species' seasonal range
#' @param crs_equal_area_custom optional crs numerical identifier of equal area
#' coordinate system
#' @param coverage_grid_custom optional sf package simple features polygon
#' layer, equal area grid within which to calculate coverage
#' @param resolution character default = "27km" (low resolution), argument passed
#' to `ebirdst::load_ranges()` function. Alternate high resolution = "9km".
#' @param quiet logical FALSE by default, set to TRUE to suppress warning and
#' progress messages
#'
#' @return A list with four components:
#'
#' 1.  coverage_grid - a map representing the species' eBird range map intersected with a regular grid. A simple feature `sf` polygon object of the regular grid applied to the range map.  If the defaults were used in the `grid_range()` function, then this will be a regular hexagonal grid that covers the Western Hemisphere with individual hexagons measuring approximately 120 km between parallel sides. If a custom regular grid was supplied to the function, e.g., `grid_range(..., coverage_grid_custom = my_regular_grid)`, then this will reflect the resolution and extent of that custom grid. The attributes will include at least these four columns: a unique name for each grid cell (grid_cell_name), the land area of the grid cell in squared kilometers (area_km2), the land area of the grid cell that is covered by the species' range (area_km2_inrange), the proportion of the grid cell's land area covered by the species' range (proportion_in_range).
#'
#' 2.  range_map - a map. A simple feature sf polygon object that represents the downloaded eBird range map.
#'
#' 3.  range_area - numeric. The total land area within the species' eBird range map.
#'
#' 4.  range_area_gridded - numeric. The total land area of grid cells that intersect with the species' eBird range map.
#'
#' @export
#'
#' @examples
#' # requires an ebirdst access key
#' # see ?ebirdst::set_ebirdst_access_key
#'
#' ex_range <- grid_range(species = "Baird's Sparrow")
#'
#' ex_non_breeding_range <- grid_range(species = "American Robin",
#' "nonbreeding")
#'
grid_range <- function(species = NULL,
                           seasonal_range = "breeding",
                           range_map = NULL,
                           crs_equal_area_custom = NULL,
                           coverage_grid_custom = NULL,
                           resolution = "27km",
                       quiet = FALSE){


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

   abd_range_original <- range_map %>%
     sf::st_make_valid()

  }else{
    #if range map not provided, then download the seasonal eBird range map

    species_ebird <- ebirdst::get_species(species)
    if(!quiet){
      message(paste("Downloading ebirdst range data for",species,species_ebird))
    }
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
      stop(paste("user selected season",seasonal_range,"does not exist in the
             eBirdst database. For",species,"try one of the following[",paste(
               unique(abd_seasonal_range$season), collapse = ", "),"]"))
    }



  }#end of eBird range map download

  if(!quiet){
    message("cropping range map to the Western Hemisphere")
  }

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

    if(!quiet){
      message("intersecting range map and coverage grid")
    }
    hex_range <- suppressWarnings(sf::st_intersection(abd_range,cov_grid)) %>%
      sf::st_make_valid() %>%
      sf::st_union(.,by_feature = TRUE)


    if(!quiet){
      message("recalculating land area of grid cells inside range")
    }
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




  just_r <- cov_grid %>%
    dplyr::filter(proportion_in_range > 0) %>%
    sf::st_drop_geometry()

  full_r <- sum(just_r$area_km2_inrange)

  full_r_grid <- sum(just_r$area_km2)


ret_list <- list(coverage_grid = cov_grid,
                 range_map = abd_range,
                 range_area = full_r,
                 range_area_gridded = full_r_grid)

return(ret_list)

}





