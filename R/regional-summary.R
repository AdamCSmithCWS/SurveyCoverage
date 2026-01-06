## summary assessments
##
##

#' Title
#'
#' @param coverage list output of `overlay_range_data`
#' @param regions Map - a POLYGON simple features object from
#' package `sf` that represents the spatial regions within which
#' to summarise the coverage. Object must have an identified coordinate
#' reference system (crs), but it can be any type because the function
#' will transform it to match the crs of the maps in argument `coverage`
#' @param region_name character column in `regions` object that contains
#' the unique region name
#' @param quiet logical FALSE by default, set to TRUE to suppress warning and
#' progress messages
#' @param use_intersection logical TRUE by default. uses an intersection of the
#' coverage grid with the summary regions, so that regions with some small
#' portion of a coerage grid cell are included in the calculations. If set to
#' FALSE, faster summaries but uses a spatial join that allocates partial coverage
#' grid cells to the region with greatest overlap.
#'
#'
#' @return A list with the following five objects
#'
#' 1.   regional_coverage_map - map of the annual coverage grid that includes an indicator of which region the grid cell is allocated to (based on spatial join).
#'
#' 2.    regional_cumulative_coverage_map - map of the cumulative coverage grid that includes an indicator column for region.
#'
#' 3.    regional_map - a map of the polygons used to define the regions.
#'
#' 4.    regional_annual_coverage_estimate - dataframe of area and proportion of range covered in each year plus an additional column indicating which region each grid cell belongs to.
#'
#' 5.    regional_cumulative_coverage_estimate - dataframe of area and proportion of range covered by region.
#'
#' @export
#'
#' @examples
#' #not run
#'
#'
regional_summary <- function(coverage = NULL,
                              regions = NULL,
                              region_name = "country",
                             quiet = FALSE,
                             use_intersection = TRUE){


  if(is.null(regions) | class(regions)[[1]] != "sf"){
    stop(paste("regions object must be a simple features object
               representing a polygon map of the desired regions
               within which to summarise coverage"))
  }


  cumulative_coverage <- coverage[["cumulative_coverage_map"]]
  annual_coverage <- coverage[["coverage_map"]]

  regions_t <- regions %>%
    sf::st_transform(.,
                     crs = sf::st_crs(cumulative_coverage))

  names(regions_t)[which(names(regions_t) == region_name)] <- "summary_region"

  regions_t <- regions_t %>%
    dplyr::select(summary_region) %>%
    dplyr::group_by(summary_region) %>%
    dplyr::summarise()

  if(use_intersection){
    regional_cumulative_coverage <- cumulative_coverage %>%
      sf::st_intersection(.,regions_t,
                          snap = s2_snap_precision(3))
  }else{
suppressWarnings(
  regional_cumulative_coverage <- cumulative_coverage %>%
      sf::st_join(.,regions_t,
                largest = TRUE)
      )
}

  regional_summary <- regional_cumulative_coverage %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(summary_region) %>%
    dplyr::summarise(.,
                     area_region = sum(area_km2))

  regional_cumulative_coverage_summary <- regional_cumulative_coverage %>%
    dplyr::inner_join(.,regional_summary,
               by = "summary_region") %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(p_region = area_km2/area_region) %>%
    dplyr::group_by(summary_region,coverage) %>%
    dplyr::summarise(.,
                     proportion_of_region = sum(p_region),
                     area_km2 = sum(area_km2),
                     .groups = "drop") %>%
    dplyr::arrange(-coverage)


  # tst <- ggplot()+
#   geom_sf(data = regions_t,
#           aes(colour = summary_region))+
#   geom_sf(data = regional_cumulative_coverage,
#           aes(fill = summary_region))+
#   facet_wrap(vars(coverage))
# tst

  if(!quiet){
message("joining annual coverage map with regions")
}
suppressWarnings(
  regional_annual_coverage <- annual_coverage %>%
sf::st_join(.,regions_t,
              largest = TRUE)
)


regional_annual_coverage_summary <- regional_annual_coverage %>%
  dplyr::inner_join(.,regional_summary,
                    by = "summary_region") %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(p_region = area_km2/area_region) %>%
  dplyr::group_by(summary_region,year,coverage) %>%
  dplyr::summarise(.,
                   proportion_of_region = sum(p_region),
                   area_km2 = sum(area_km2),
                   .groups = "drop") %>%
  dplyr::arrange(-coverage,year)


# %>%
#   dplyr::rename_with(.,
#                      ~ gsub("summary_region",
#                             replacement = region_name,
#                             x = .x,fixed = TRUE),
#                      .cols = dplyr::matches("summary_region",
#                                             ignore.case = FALSE))


# regional_annual_coverage_t <- regional_annual_coverage %>%
#   filter(year %in% c(1970,2000,2022))

# tst <- ggplot()+
#   geom_sf(data = regions_t)+
#   geom_sf(data = regional_annual_coverage_t,
#           aes(fill = coverage))+
#   facet_grid(rows = vars(summary_region),
#              cols = vars(year))
# tst

ret_list <- list(regional_coverage_map = regional_annual_coverage,
                 regional_cumulative_coverage_map = regional_cumulative_coverage,
                 regional_map = regions_t,
                 regional_annual_coverage_estimate = regional_annual_coverage_summary,
                 regional_cumulative_coverage_estimate = regional_cumulative_coverage_summary)


return(ret_list)

}

