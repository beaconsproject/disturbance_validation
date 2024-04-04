# Validation of linear and areal disturbance attributes
# PV 2024-03-11

library(sf)
library(dplyr)

test_types <- function(data_pkg, lyr_line, lyr_poly) {

#data_pkg='../beacons_data/geopackage_creator/fda10ab_disturb.gpkg'
#lyr_line='linear_disturbance'
#lyr_poly='areal_disturbance'

  valid_types <- 'input/yg_industry_disturbance_types.csv'
  #valid_types <- readr::read_csv('https://raw.githubusercontent.com/beaconsproject/beacons_tools/main/validation/yg_industry_disturbance_types.csv')
  valid_lines <- readr::read_csv(valid_types) |>
    filter(TYPE_FEATURE=='Linear') |>
    select(TYPE_INDUSTRY, TYPE_DISTURBANCE) |>
    mutate(TYPE_COMBINED=paste0(TYPE_INDUSTRY,"***",TYPE_DISTURBANCE)) |>
    unique() |>
    pull()

  test_lines <- st_read(data_pkg, lyr_line, quiet=TRUE) |>
    st_drop_geometry() |>
    mutate(TYPE_COMBINED=paste0(TYPE_INDUSTRY,"***",TYPE_DISTURBANCE)) |>
    select(TYPE_COMBINED) |>
    unique() |>
    pull()
  
  cat('\n\nTHESE LINEAR TYPES ARE NOT VALID:\n')
  for (i in test_lines) {
    if (!i %in% valid_lines) {
      cat('  ',i, '\n')
    }
  }

  valid_polys <- readr::read_csv(valid_types) |>
    filter(TYPE_FEATURE=='Areal') |>
    select(TYPE_INDUSTRY, TYPE_DISTURBANCE) |>
    mutate(TYPE_COMBINED=paste0(TYPE_INDUSTRY,"***",TYPE_DISTURBANCE)) |>
    unique() |>
    pull()

  test_polys <- st_read(data_pkg, lyr_poly, quiet=TRUE) |>
    st_drop_geometry() |>
    mutate(TYPE_COMBINED=paste0(TYPE_INDUSTRY,"***",TYPE_DISTURBANCE)) |>
    select(TYPE_COMBINED) |>
    unique() |>
    pull()

  cat('\n\nTHESE AREAL TYPES ARE NOT VALID:\n')
  for (i in test_polys) {
    if (!i %in% valid_polys) {
      cat('  ',i, '\n')
    }
  }
}
