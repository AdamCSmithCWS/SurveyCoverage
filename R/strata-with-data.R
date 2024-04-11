#' Identify strata with monitoring data


strata_w_data <- function(range_data, #output list from strata_w_range()
                          survey_sites,
                          sites = "site",
                          years = "year",
                          sites_as_points = FALSE,
                          crs_site_coordinates = 4326,
                          x_coord = "longitude",
                          y_coord = "latitude",
                          largest = FALSE) {

  # test survey_sites structure
  if(!sites %in% names(survey_sites)){
    stop(paste("sites column",sites,"is missing from survey_sites object
               check that argument sites is the column name (character) that
               identifies the unique site-identifier"))
  }

  #rename survey_sites columns for tidy select
  survey_sites <- survey_sites %>%
    dplyr::rename_with(., ~stringr::str_replace(.,
                                                pattern = sites,
                                                replacement = "site"),
                       .cols = tidyselect::matches(sites)) %>%
    dplyr::rename_with(., ~stringr::str_replace(.,
                                                pattern = years,
                                                replacement = "year"),
                       .cols = tidyselect::matches(years))

  if(sites_as_points){
  survey_sites <- survey_sites %>%
    sf::st_as_sf(.,
                 coords = c(x_coord,y_coord),
                 crs = sf::st_crs(crs_site_coordinates))
  }

  if(any(!sf::st_is_valid(survey_sites))){
    survey_sites <- survey_sites %>%
      sf::st_make_valid()
  }

cov_grid <- range_data[["coverage_grid"]]
grid_w_range <- range_data[["strata_w_range"]]

crs_equal_area_custom <- sf::st_crs(cov_grid)

  # transform both maps to the same projected crs

  survey_sites <- sf::st_transform(survey_sites,
                                   crs_equal_area_custom)

cov_grid <- cov_grid %>%
  mutate(w_mapped_range = ifelse(strata_name %in% unique(grid_w_range$strata_name),
         TRUE,FALSE))

  #perform spatial join
  message("Performing spatial join with range, this may take a few minutes...")
  map_coverage <- cov_grid %>%
    sf::st_join(.,
                survey_sites,
                join = st_intersects,
                largest = largest) %>%
    dplyr::mutate(coverage = ifelse(is.na(site),
                             FALSE,
                             TRUE)) %>% #ID strata with data
    dplyr::group_by(.,strata_name,year,coverage) %>%
    dplyr::summarise(.,
              n_sites = n(),
              do_union = TRUE,
              .groups = "keep") %>%
    dplyr::mutate(n_sites = ifelse(coverage,
                                   n_sites,
                                   n_sites-1))

  map_coverage$area_km2 <- as.numeric(sf::st_area(map_coverage)/1e6)





  return(map_coverage)
}

