#' Identify grid with monitoring data
#'
#' @param range list - output object from `grid_range()` function.
#' @param survey_sites required - two options: 1 - dataframe or 2 - map
#' Dataframe input option requires a dataframe with at least three columns
#' representing the site-names, x-coordinate, and y-coordinates or monitoring
#' sites. Dataframe may also include column to indicate the year of a survey
#' event, if the goal is to estimate annual survey coverage. Dataframe option
#' is only suitable if survey site locations can be treated as points (e.g.,
#' survey sites are small areas or linear features in relation to the gridcells
#' used to calculate coverage - 120km across in the default grid)
#' Map option requires a LINE or POLYGON object simple features object from
#' package `sf` that represents the survey units as areas or transects. Object
#' must have an identified coordinate reference system (crs), but it can be
#' either a geographic or projected crs because the function will transform it
#' to match the crs of the maps in argument `range`
#' @param sites character column name in `survey_sites` that identifies the
#' unique site names
#' @param years optional character column name in `survey_sites` that identifies
#' years each site was surveyed
#' @param proportion_area_included optional numeric [0-1] minimum proportion of
#' a grid cell that overlaps a species range for the cell to be considered part
#' of the species' range (for example, if set to 0.25, then only grid cells
#' with at least 25 percent of their area inside the species' range are included in the
#' calculations of coverage and range size. Default = 0.0. May be relevant for
#' very patchy species' ranges, or at range boundaries.
#' @param minimum_n_years  optional integer, minimum number of years with surveys
#' to consider a grid-cell to be covered in the calculation of overall coverage.
#' Default is 1, which means even a single year with data results in coverage.
#' @param x_coord character - name of column that contains decimal degrees
#' format x-coordinate if `survey_sites` is a dataframe.
#' @param y_coord character - name of column that contains decimal degrees
#' format y-coordinate if `survey_sites` is a dataframe.
#' @param crs_site_coordinates one of (i) character: a string accepted by GDAL,
#' (ii) integer, a valid EPSG value (numeric), or (iii) an object of class crs.
#' Ignore if `survey_sites` is not a dataframe. Default = 4326 (WGS 84). Accepts
#' any valid input type allowed by `sf::st_crs`.
#' @param add_survey_sites_to_range logical - whether grid cells with surveys
#' but outside of the species' range should be included as part of the species
#' range. Default = TRUE. Assumes that `survey_sites` represents locations where
#' the species has been observed during monitoring, and not just locations of
#' multi-species surveys. Setting to FALSE may generate non-finite estimates of
#' coverage if positive values of area covered within regions with no area in
#' species' range
#' @param largest logical passed to `sf::st_join` function. Default is FALSE
#' @param quiet logical FALSE by default, set to TRUE to suppress warning and
#' progress messages
#'
#' @return A list with the following five objects
#'
#' 1.    coverage_map - map of the cells in the species' range that intersect the survey data in each year. If a year column was included with the input data, then this mapped information is repeated for each year of the data. If no year information was included, then this object will be identical to the cumulative_coverage_map
#'
#' 2.    cumulative_coverage_map - map of cells in the species' range that intersect survey data in at least 1 year (if default value of minimum_n_years was used).
#'
#' 3.    total_gridded_range_area - land area of gridded range.
#'
#' 4.    annual_coverage_estimate - dataframe of area and proportion of range covered in each year
#'
#' 5.    cumulative_coverage_estimate = dataframe (1-row) of area and proportion of range covered.
#'
#' @export
#'
#' @examples
#' # requires an ebirdst access key
#' # see ?ebirdst::set_ebirdst_access_key
#'
#' ex_range <- grid_range(species = "Baird's Sparrow")
#' #ex_coverage <- overlay_range_data(ex_range,
#'  #                                   survey_sites = example_basp_bbs_data)
#'

overlay_range_data <- function(range, #output list from grid_w_range()
                          survey_sites, # dataframe of points and years or sf spatial object
                          sites = "site",
                          years = "year",
                          proportion_area_included = 0.0,
                          minimum_n_years = 1,
                          x_coord = "longitude",
                          y_coord = "latitude",
                          crs_site_coordinates = 4326,
                          add_survey_sites_to_range = TRUE,
                          largest = FALSE,
                          quiet = FALSE) {

  # test survey_sites structure
  if(!sites %in% names(survey_sites)){
    stop(paste("sites column",sites,"is missing from survey_sites object
               check that argument sites is the column name (character) that
               identifies the unique site-identifier"))
  }

  if(!years %in% names(survey_sites)){
    if(!quiet){
    warning(paste("since column",years,"is missing from survey_sites object
               continuing with coverage assuming it is constant through time.
               If annual survey information is available and desired, check
               that argument years identifies the column name (character) that
               contains the unique year-identifier"))
    }
    survey_sites <- survey_sites %>%
      dplyr::mutate(year = 1)

  }

  #rename survey_sites columns for tidy select
  names(survey_sites)[which(names(survey_sites) == sites)] <- "site"
  names(survey_sites)[which(names(survey_sites) == years)] <- "year"




    if(class(survey_sites)[1] == "sf"){
      if(!quiet){
        message("input survey_sites is a simple features spatial object")
      }
    }else{
      if(!quiet){

      message(paste("input survey_sites is data frame. Creating spatial POINTS
                    using coordinates provided in arguments x_coord and y_coord.
                    Please ensure that the coordinates are latitude and longitude
                    decimal degrees with coordinate reference system WGS 84,
                    or that the correct coordinate reference system was provided
                    to the argument crs_site_coordinates"))
}

      survey_sites <- survey_sites %>%
        sf::st_as_sf(.,
                     coords = c(x_coord,y_coord),
                     crs = sf::st_crs(crs_site_coordinates))
    }



  if(any(!sf::st_is_valid(survey_sites))){
    survey_sites <- survey_sites %>%
      sf::st_make_valid()
  }


cov_grid <- range[["coverage_grid"]] %>%
  dplyr::mutate(area_km2_inrange = ifelse(proportion_in_range >= proportion_area_included,
                                          area_km2_inrange,
                                          0),
                proportion_in_range = ifelse(proportion_in_range >= proportion_area_included,
                                             proportion_in_range,
                                             0))

grid_w_range <- cov_grid %>%
  dplyr::filter(.,area_km2_inrange > 0,
                proportion_in_range >= proportion_area_included)

crs_equal_area_custom <- sf::st_crs(cov_grid)

  # transform site map to the same projected crs as the coverage grid

  survey_sites <- sf::st_transform(survey_sites,
                                   crs_equal_area_custom)


  #perform spatial join
  if(!quiet){

  message("Performing spatial join with range, this may take 5-10 minutes...")
  }
 suppressWarnings(
   map_coverage <- cov_grid %>%
    sf::st_join(.,
                survey_sites,
                join = sf::st_intersects,
                largest = largest) %>%
    dplyr::mutate(coverage = ifelse(is.na(site),
                             FALSE,
                             TRUE)) %>% #ID grid with data
    dplyr::group_by(.,grid_cell_name,year,coverage) %>%
    dplyr::summarise(.,
              n_sites = dplyr::n(), #number of monitoring sites within each grid cell
              do_union = TRUE,
              .groups = "keep") %>%
    dplyr::mutate(n_sites = ifelse(coverage,
                                   n_sites,
                                   n_sites-1))
  )

  map_coverage$area_km2 <- as.numeric(sf::st_area(map_coverage)/1e6)

  if(add_survey_sites_to_range){
    if(!quiet){

    message("Reconciling range area to include grid cells with
          data outside of species' seasonal range")
    }
    #ID grid cells with some data only
    w_some_data <- map_coverage %>%
      dplyr::filter(coverage) %>%
      dplyr::select(grid_cell_name)

    w_some_data <- unique(w_some_data$grid_cell_name)

  }else{
    #ID grid cells with data and that have some range according to range map
    w_some_data <- map_coverage %>%
      dplyr::filter(coverage,
             grid_cell_name %in% unique(grid_w_range$grid_cell_name)) %>%
      dplyr::select(grid_cell_name)

    w_some_data <- unique(w_some_data$grid_cell_name)

  }

    # map_coverage retains only grid cells with range
    #
  map_coverage <- map_coverage %>%
    dplyr::mutate(w_mapped_range = ifelse(grid_cell_name %in% unique(grid_w_range$grid_cell_name),
                                   TRUE,FALSE),
           w_data = ifelse(grid_cell_name %in% w_some_data,
                           TRUE,
                           FALSE),
           w_range = ifelse(w_mapped_range | w_data,
                            TRUE,
                            FALSE)) %>%
    dplyr::filter(w_range) # drop cells outside of range

  #map of cells considered inside range
  total_grid_range <- cov_grid %>%
    dplyr::filter(grid_cell_name %in% map_coverage$grid_cell_name) %>%
    dplyr::distinct() #
  # estiamte the total area of range based on grid cells included
  total_grid_range_area <- sum(sf::st_area(total_grid_range))%>%
    units::set_units(.,"km^2")

  #expand to complete grid-cell by year dataset
  years_all <- sort(unique(map_coverage$year))
  total_grid_range_yearly <- NULL
  for(y in years_all){ # yes, it seems silly to have to loop this
    # but I don't know of an expand.grid style function that works
    # with sf spatial objects
  tmp <- total_grid_range %>%
    dplyr::mutate(year = y)
  total_grid_range_yearly <- dplyr::bind_rows(total_grid_range_yearly,
                                       tmp)
  }

  map_coverage_join <- map_coverage %>%
    dplyr::filter(!is.na(year)) %>%
    sf::st_drop_geometry()

  cells_w_mapped_range <- map_coverage %>%
    dplyr::filter(w_mapped_range)
  cells_w_mapped_range <- unique(cells_w_mapped_range$grid_cell_name)
  #combine the coverage map with the total_grid_range_yearly to
  #create a complete map of annual coverage
  map_coverage_full <- total_grid_range_yearly %>%
    dplyr::left_join(.,map_coverage_join,
                     by = c("year",
                            "grid_cell_name",
                            "area_km2")) %>%
    dplyr::mutate(coverage = ifelse(is.na(coverage),FALSE,coverage),
           n_sites = ifelse(is.na(n_sites),0,n_sites),
           w_data = ifelse(is.na(w_data),FALSE,w_data),
           w_mapped_range = ifelse(grid_cell_name %in% cells_w_mapped_range,
                                   TRUE,FALSE),
           w_range = TRUE)

# calculate the annual coverage rate
  annual_coverage <- map_coverage %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(year,coverage) %>%
    dplyr::summarise(coverage_area = sum(area_km2),
              coverage_proportion = as.numeric(coverage_area/total_grid_range_area))

  # idenfity grid-cells with sufficient annual coverage
  nyears_covered <- map_coverage %>%
    sf::st_drop_geometry() %>%
    dplyr::ungroup() %>%
    dplyr::filter(coverage) %>%
    dplyr::group_by(grid_cell_name) %>%
    dplyr::summarise(n_years = dplyr::n()) %>%
    dplyr::filter(n_years >= minimum_n_years)

  # calculate the cumulative coverage of grid-cells with at least
  # minimum_n_years of coverage to consider a grid-cell covered
  total_coverage <- map_coverage %>%
    sf::st_drop_geometry() %>%
    dplyr::ungroup() %>%
    dplyr::filter(coverage,
                  grid_cell_name %in% nyears_covered$grid_cell_name) %>%
    dplyr::select(-c(year,n_sites)) %>%
    dplyr::distinct() %>%
    dplyr::summarise(coverage_area = sum(area_km2),
              coverage_proportion = as.numeric(coverage_area/total_grid_range_area))

  cumulative_coverage_map <- total_grid_range %>%
    dplyr::mutate(coverage = ifelse(grid_cell_name %in% nyears_covered$grid_cell_name,
                                    TRUE,FALSE))


  ret_list <- list(coverage_map = map_coverage_full,
                   cumulative_coverage_map = cumulative_coverage_map,
                   total_gridded_range_area = total_grid_range_area,
                   annual_coverage_estimate = annual_coverage,
                   cumulative_coverage_estimate = total_coverage)


  return(ret_list)
}

