spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)
spo_shape <- "68962"
bbox <- sf::st_bbox(get_shapes_geometry(spo_gtfs, spo_shape))
polygon <- sf::st_as_sf(sf::st_buffer(sf::st_as_sfc(bbox), 0))

tester <- function(gtfs = spo_gtfs,
                   geom = bbox,
                   spatial_operation = sf::st_intersects,
                   keep = TRUE) {
  filter_by_spatial_extent(gtfs, geom, spatial_operation, keep)
}

# tests -------------------------------------------------------------------

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(spo_gtfs)))

  expect_error(tester(geom = unclass(bbox)))
  expect_error(tester(geom = sf::st_transform(polygon, 4674)))

  expect_error(tester(spatial_operation = "sf::st_intersect"))
  expect_error(tester(spatial_operation = sf::st_crop))

  expect_error(tester(keep = "TRUE"))
  expect_error(tester(keep = c(TRUE, TRUE)))
  expect_error(tester(keep = NA))
})

test_that("results in a dt_gtfs object", {
  # a dt_gtfs object is a list with "dt_gtfs" and "gtfs" classes
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")
  smaller_gtfs <- tester(spo_gtfs, bbox)
  expect_s3_class(smaller_gtfs, dt_gtfs_class)
  expect_type(smaller_gtfs, "list")

  # all objects inside a dt_gtfs are data.tables
  invisible(lapply(smaller_gtfs, expect_s3_class, "data.table"))
})

test_that("doesn't change given gtfs", {
  # (except for some tables' indices)

  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  smaller_gtfs <- tester(gtfs, bbox)
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
})

test_that("supports sf, sfc and bbox objects", {
  # identical only because no geometry lies near this bbox's edges
  result1 <- tester(spo_gtfs, polygon)
  result2 <- tester(spo_gtfs, sf::st_geometry(polygon))
  result3 <- tester(spo_gtfs, bbox)

  expect_identical(result1, result2)
  expect_identical(result1, result3)
})

test_that("'keep' and 'spatial_operation' arguments work correctly", {
  # st_intersects

  shapes <- get_shapes_geometry(spo_gtfs)
  shapes_intersected <- sf::st_intersects(polygon, shapes, sparse = FALSE)
  shapes_intersected <- shapes[shapes_intersected, ]$shape_id

  trips <- get_trip_geometry(spo_gtfs, method = "euclidean")
  trips_intersected <- sf::st_intersects(polygon, trips, sparse = FALSE)
  trips_intersected <- trips[trips_intersected, ]$trip_id

  smaller_keeping <- tester(spo_gtfs, bbox)
  expect_true(all(smaller_keeping$trips$trip_id %chin% trips_intersected))
  expect_true(all(smaller_keeping$shapes$shape_id %chin% shapes_intersected))

  smaller_not_keeping <- tester(spo_gtfs, bbox, keep = FALSE)
  expect_true(!any(smaller_not_keeping$trips$trip_id %chin% trips_intersected))
  expect_true(
    !any(smaller_not_keeping$shapes$shape_id %chin% shapes_intersected)
  )

  # st_contains

  shapes_contained <- sf::st_contains(polygon, shapes, sparse = FALSE)
  shapes_contained <- shapes[shapes_contained, ]$shape_id

  trips_contained <- sf::st_contains(polygon, trips, sparse = FALSE)
  trips_contained <- trips[trips_contained, ]$trip_id

  smaller_keeping <- tester(
    spo_gtfs,
    bbox,
    spatial_operation = sf::st_contains
  )
  expect_true(all(smaller_keeping$trips$trip_id %chin% trips_contained))
  expect_true(all(smaller_keeping$shapes$shape_id %chin% shapes_contained))

  smaller_not_keeping <- tester(
    spo_gtfs,
    bbox,
    spatial_operation = sf::st_contains,
    keep = FALSE
  )
  expect_true(!any(smaller_not_keeping$trips$trip_id %chin% trips_contained))
  expect_true(
    !any(smaller_not_keeping$shapes$shape_id %chin% shapes_contained)
  )
})

test_that("selects the union of trips selected by shapes and by stop_times", {
  # a trip is selected if its shape or the path through its stops intersects
  # with the polygon. 'keep = FALSE' must drop all of these trips. the western
  # half of the feed's extent selects some trips only through their stops

  shapes <- get_shapes_geometry(spo_gtfs)
  half_bbox <- sf::st_bbox(shapes)
  half_bbox["xmax"] <- (half_bbox["xmin"] + half_bbox["xmax"]) / 2
  half_polygon <- sf::st_as_sfc(half_bbox)

  shapes_hit <- sf::st_intersects(half_polygon, shapes, sparse = FALSE)
  shapes_hit <- shapes[shapes_hit, ]$shape_id
  trips_hit_by_shape <- spo_gtfs$trips[shape_id %chin% shapes_hit]$trip_id

  trips <- get_trip_geometry(spo_gtfs, method = "euclidean")
  trips_hit_by_stops <- sf::st_intersects(half_polygon, trips, sparse = FALSE)
  trips_hit_by_stops <- trips[trips_hit_by_stops, ]$trip_id

  # make sure the test is not vacuous: some trips are selected by only one of
  # the criteria

  expect_false(setequal(trips_hit_by_shape, trips_hit_by_stops))

  selected_trips <- union(trips_hit_by_shape, trips_hit_by_stops)
  not_selected_trips <- setdiff(spo_gtfs$trips$trip_id, selected_trips)

  smaller_keeping <- tester(spo_gtfs, half_polygon)
  expect_setequal(smaller_keeping$trips$trip_id, selected_trips)

  smaller_not_keeping <- tester(spo_gtfs, half_polygon, keep = FALSE)
  expect_setequal(smaller_not_keeping$trips$trip_id, not_selected_trips)
})

test_that("doesn't add attributes to the given stop_times table", {
  gtfs <- read_gtfs(spo_path)
  original_attributes <- attributes(gtfs$stop_times)

  smaller_gtfs <- tester(gtfs, bbox)
  expect_identical(attributes(gtfs$stop_times), original_attributes)
})

test_that("works with sf describing two features", {
  another_shape <- "17846"
  another_bbox <- sf::st_bbox(get_shapes_geometry(spo_gtfs, another_shape))
  another_polygon <- sf::st_as_sf(sf::st_as_sfc(another_bbox))
  bigger_polygon <- rbind(polygon, another_polygon)

  smaller_gtfs <- tester(
    spo_gtfs,
    bigger_polygon,
    spatial_operation = sf::st_contains
  )

  # shape 17847 is also contained inside 17846's bbox
  expect_true(
    all(smaller_gtfs$shapes$shape_id %chin% c("68962", "17846", "17847"))
  )
})

test_that("error if gtfs doesn't contain neither shapes nor stop_times table", {
  spo_gtfs$shapes <- NULL
  spo_gtfs$stop_times <- NULL
  expect_error(tester(spo_gtfs, bbox))
})

test_that("ignores stops with missing coordinates", {
  na_gtfs <- read_gtfs(spo_path)
  na_gtfs$shapes <- NULL
  na_stops <- na_gtfs$stop_times[trip_id == "CPTM L07-0"]$stop_id[1:3]
  na_gtfs$stops[stop_id %chin% na_stops, stop_lat := NA]

  expect_s3_class(tester(na_gtfs), "dt_gtfs")

  # the trip is still selected by its stops with coordinates

  feed_bbox <- sf::st_bbox(get_stops_geometry(spo_gtfs))
  result <- tester(na_gtfs, geom = feed_bbox)
  expect_true("CPTM L07-0" %chin% result$trips$trip_id)
})

test_that("a bbox is treated as a rectangle in longitude and latitude", {
  # each box is 0.0001 degree (about 11 m) from the geometry tested. a polygon
  # with geodesic edges or enlarged by a zero buffer gets these cases wrong.
  # shapes are tested without stop_times, so that trips are not selected by
  # their stops

  box <- function(xmin, ymin, xmax, ymax) {
    sf::st_bbox(
      c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
      crs = 4326
    )
  }
  shape_bbox <- function(id) sf::st_bbox(get_shapes_geometry(spo_gtfs, id))
  kept_shapes <- function(geom, op = sf::st_intersects) {
    no_stop_times_gtfs <- spo_gtfs
    no_stop_times_gtfs$stop_times <- NULL
    tester(no_stop_times_gtfs, geom, op)$shapes$shape_id
  }

  # narrow box just west of the shape

  sb <- shape_bbox("17844")
  west <- box(
    sb[["xmin"]] - 0.01,
    sb[["ymin"]],
    sb[["xmin"]] - 0.0001,
    sb[["ymax"]]
  )
  expect_false("17844" %chin% kept_shapes(west))

  # wide box just north of the shape

  sb <- shape_bbox("68962")
  cx <- (sb[["xmin"]] + sb[["xmax"]]) / 2
  north <- box(cx - 0.5, sb[["ymax"]] + 0.0001, cx + 0.5, sb[["ymax"]] + 0.01)
  expect_false("68962" %chin% kept_shapes(north))

  # a shape touching the box's edges is covered, but not properly contained

  expect_true("68962" %chin% kept_shapes(sb, sf::st_covers))
  expect_false("68962" %chin% kept_shapes(sb, sf::st_contains_properly))

  # wide box containing the shape

  sb <- shape_bbox("17856")
  cx <- (sb[["xmin"]] + sb[["xmax"]]) / 2
  around <- box(
    cx - 0.5,
    sb[["ymin"]] - 0.0001,
    cx + 0.5,
    sb[["ymax"]] + 0.0001
  )
  expect_true("17856" %chin% kept_shapes(around, sf::st_contains))

  # trips are tested through their stops when the feed has no shapes

  no_shapes_gtfs <- spo_gtfs
  no_shapes_gtfs$shapes <- NULL
  trip_stops <- spo_gtfs$stop_times[trip_id == "6450-51-0"]$stop_id
  sb <- sf::st_bbox(get_stops_geometry(spo_gtfs, trip_stops))
  west <- box(
    sb[["xmin"]] - 0.01,
    sb[["ymin"]],
    sb[["xmin"]] - 0.0001,
    sb[["ymax"]]
  )
  result <- tester(no_shapes_gtfs, west)
  expect_false("6450-51-0" %chin% result$trips$trip_id)
})

test_that("predicates that require additional arguments are not accepted", {
  expect_error(
    tester(spatial_operation = sf::st_equals_exact),
    regexp = "geometric binary predicate"
  )
  expect_error(
    tester(spatial_operation = sf::st_is_within_distance),
    regexp = "geometric binary predicate"
  )
  expect_s3_class(tester(spatial_operation = sf::st_within), "dt_gtfs")
})
