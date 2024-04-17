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
#'
#' @return A list
#' @export
#'
#' @examples
#' #not run
#'
#'
regional_summary <- function(coverage = NULL,
                              regions = NULL,
                              region_name = "country"){


  if(is.null(regions) | class(regions)[[1]] != "sf"){
    stop(paste("regions object must be a simple features object
               representing a polygon map of the desired regions
               within which to summarise coverage"))
  }


  overall_coverage <- coverage[["cumulative_coverage_map"]]
  annual_coverage <- coverage[["coverage_map"]]

  regions_t <- regions %>%
    sf::st_transform(.,
                     crs = sf::st_crs(overall_coverage))

  names(regions_t)[which(names(regions_t) == region_name)] <- "summary_region"

  regions_t <- regions_t %>%
    dplyr::select(summary_region) %>%
    dplyr::group_by(summary_region) %>%
    dplyr::summarise()

suppressWarnings(
  regional_overall_coverage <- overall_coverage %>%
      sf::st_join(.,regions_t,
                largest = TRUE)
      )


  regional_summary <- regional_overall_coverage %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(summary_region) %>%
    dplyr::summarise(.,
                     area_region = sum(area_km2))

  regional_overall_coverage_summary <- regional_overall_coverage %>%
    dplyr::inner_join(.,regional_summary,
               by = "summary_region") %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(p_region = area_km2/area_region) %>%
    dplyr::group_by(summary_region,coverage) %>%
    dplyr::summarise(.,
                     proportion_of_region = sum(p_region),
                     area_km2 = sum(area_km2)) %>%
    dplyr::arrange(-coverage)


  # tst <- ggplot()+
#   geom_sf(data = regions_t,
#           aes(colour = summary_region))+
#   geom_sf(data = regional_overall_coverage,
#           aes(fill = summary_region))+
#   facet_wrap(vars(coverage))
# tst

message("joining annual coverage map with regions")

suppressWarnings(
  regional_annual_coverage <- regional_annual_coverage %>%
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
                   area_km2 = sum(area_km2)) %>%
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

ret_list <- list(regional_map = regions_t,
                 regional_overall_coverage_map = regional_overall_coverage,
                 regional_annual_coverage_map = regional_annual_coverage,
                 regional_overall_coverage_summary = regional_overall_coverage_summary,
                 regional_annual_coverage_summary = regional_annual_coverage_summary)


return(ret_list)

}

