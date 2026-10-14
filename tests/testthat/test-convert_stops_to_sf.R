data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------

test_that("raises a deprecation warning and returns get_stops_geometry()", {
  expect_warning(
    stops_sf <- convert_stops_to_sf(gtfs, stop_id = "18848", crs = 4674),
    class = "deprecated_convert_stops_to_sf"
  )
  expect_identical(
    stops_sf,
    get_stops_geometry(gtfs, stop_id = "18848", crs = 4674)
  )
})
