data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------

test_that("raises a deprecation warning and returns get_shapes_geometry()", {
  expect_warning(
    shapes_sf <- convert_shapes_to_sf(gtfs, shape_id = "17846", crs = 4674),
    class = "deprecated_convert_shapes_to_sf"
  )
  expect_identical(
    shapes_sf,
    get_shapes_geometry(gtfs, shape_id = "17846", crs = 4674)
  )
})

test_that("sort_sequence is deprecated", {
  suppressWarnings(
    expect_warning(
      convert_shapes_to_sf(gtfs, "17846", sort_sequence = TRUE),
      class = "deprecated_sort_sequence"
    ),
    classes = "deprecated_convert_shapes_to_sf"
  )
  suppressWarnings(
    expect_error(
      convert_shapes_to_sf(gtfs, "17846", sort_sequence = FALSE),
      class = "gtfstools_defunct_sort_sequence_error"
    ),
    classes = "deprecated_convert_shapes_to_sf"
  )
  suppressWarnings(
    expect_error(
      convert_shapes_to_sf(gtfs, sort_sequence = NA),
      regexp = "sort_sequence"
    ),
    classes = "deprecated_convert_shapes_to_sf"
  )
})
