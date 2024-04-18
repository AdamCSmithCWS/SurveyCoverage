
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SurveyCoverage

<!-- badges: start -->
<!-- badges: end -->

The goal of SurveyCoverage is to provide some common tools to assess the
spatial coverage of long-term land-bird monitoring programs. Here the
“coverage” of a monitoring program is defined as an approximate
proportion of the species’ range that is within the region sampled by a
given monitoring program. The metrics here are approximations and
provide only one example of a potential approach.

Measuring the proportion of a species’ range that is covered by
monitoring helps to understand the potential for bias in estimates of
that species’ population status and trend. A species’ status can vary
across its range due to variation in factors that affect its status,
such as human activity, landcover-change, interactions with other
species, and climate. If our monitoring data are derived exclusively
from a portion of that range, estimates may not reflect the species’
trends and status in the un-monitored portion.

Our overall goal is to provide simple tools that can be applied across
many monitoring programs to support a coarse metric of reliability and
potential bias in the trend estimates.

## Installation

You can install SurveyCoverage from [GitHub](https://github.com/) with:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github(AdamCSmithCWS/SurveyCoverage")
```

### Required additional package

SurveyCoverage currently requires the package
[ebirdst](https://github.com/ebird/ebirdst)

``` r
remotes::install_github("ebird/ebirdst")
```

#### ebirdst Access Request

Data access to download ebirdst is granted through an Access Request
Form at: <https://ebird.org/st/request>. Access with this form generates
a key to be used with this R package and is provided immediately (as
long as commercial use is not requested).

After completing the Access Request Form, you will be provided a Status
and Trends Data Products access key, which you will need when
downloading data. To store the key so the package can access it when
downloading data, use the function `set_ebirdst_access_key("XXXXX")`,
where `"XXXXX"` is the access key provided to you.

## Example

This is a basic example which shows you how to assess the coverage of
the North American Breeding Bird Survey (BBS) of Baird’s Sparrow. The
BBS survey data for Baird’s Sparrow are supplied with the package. These
example data are the survey event information for all surveys on BBS
routes where the species has been observed at least once since 1966 -
essentially, the survey events that would be included in a standard
trend analysis of the BBS for this particular species. These survey data
will be used in the second step of the example.

### Download and apply a regular grid to the species’ ebird range map

The package allows the user to download a range map using `ebirdst`,
then to stratify that range map based on an equal area, regular grid.

``` r
library(SurveyCoverage)

example_species <- "Baird's Sparrow"

range_info <- grid_range(example_species)
#> Downloading ebirdst range data for Baird's Sparrow baispa
#> Downloading Status Data Products for baispa
#> Data already exists, use force = TRUE to re-download.
#> cropping range map to the Western Hemisphere
#> intersecting range map and coverage grid
#> recalculating land area of grid cells inside range
# will fail if ebirdst access key was not successfully set-up.
```

The output of `grid_range()` is a large list that includes the following
objects:

1.  coverage_grid - a map representing the species’ eBird range map
    intersected with a regular grid. A simple feature `sf` polygon
    object of the regular grid applied to the range map. If the defaults
    were used in the `grid_range()` function, then this will be a
    regular hexagonal grid that covers the Western Hemisphere with
    individual hexagons measuring approximately 120 km between parallel
    sides. If a custom regular grid was supplied to the function, e.g.,
    `grid_range(..., coverage_grid_custom = my_regular_grid)`, then this
    will reflect the resolution and extent of that custom grid. The
    attributes will include at least these four columns: a unique name
    for each grid cell (grid_cell_name), the land area of the grid cell
    in squared kilometers (area_km2), the land area of the grid cell
    that is covered by the species’ range (area_km2_inrange), the
    proportion of the grid cell’s land area covered by the species’
    range.

2.  range_map - a map. A simple feature `sf` polygon object that
    represents the downloaded eBird range map.

3.  range_area - numeric. The total land area within the species’ eBird
    range map.

4.  range_area_gridded - numeric. The total land area of grid cells that
    intersect with the species’ eBird range map.

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
