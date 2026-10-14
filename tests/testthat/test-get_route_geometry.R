data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)
ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
ggl_gtfs <- read_gtfs(ggl_path)
ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
ber_gtfs <- read_gtfs(ber_path)
route_id <- "CPTM L07"

tester <- function(gtfs = get("gtfs", envir = parent.frame()),
                   route_id = NULL,
                   method = "shapes",
                   crs = 4326) {
  get_route_geometry(gtfs, route_id, method, crs)
}

# the variants of each route, computed from the geometries of all trips listed
# in 'trips' (only their first entry), with the first trip_id (in C-locale
# order, as data.table sorts) and the count of trips of each variant

oracle <- function(gtfs, method = "shapes") {
  trip_geom <- suppressWarnings(get_trip_geometry(gtfs, method = method))
  trips <- gtfs$trips[!duplicated(gtfs$trips$trip_id)]
  trip_geom <- trip_geom[trip_geom$trip_id %chin% trips$trip_id, ]

  variants <- data.table::data.table(
    route_id = trips$route_id[match(trip_geom$trip_id, trips$trip_id)],
    wkb = sf::st_as_binary(sf::st_geometry(trip_geom), hex = TRUE),
    trip_id = trip_geom$trip_id
  )
  data.table::setorderv(variants, "trip_id")
  variants <- variants[
    ,
    .(trip_pattern = trip_id[1], n_trips = .N),
    by = .(route_id, wkb)
  ]
  data.table::setorderv(variants, c("route_id", "wkb"))

  return(variants[])
}

# the output of get_route_geometry() in the same format as oracle()

as_variants <- function(route_geom) {
  variants <- data.table::data.table(
    route_id = route_geom$route_id,
    wkb = sf::st_as_binary(sf::st_geometry(route_geom), hex = TRUE),
    trip_pattern = route_geom$trip_pattern,
    n_trips = route_geom$n_trips
  )
  data.table::setorderv(variants, c("route_id", "wkb"))

  return(variants[])
}

# a feed with several edge cases: a duplicated trip_id in 'trips' (whose
# second entry is ignored), trips without shape_id, a trip without route_id, a
# stop without coordinates, two stop_ids with the same coordinates, a trip
# listed in 'stop_times' but not in 'trips' and unordered 'stop_times'

edge_feed <- function() {
  feed <- read_gtfs(data_path)
  trips <- feed$trips$trip_id

  duplicated_trip <- feed$trips[1]
  duplicated_trip[, `:=`(route_id = "other_route", shape_id = "17846")]
  feed$trips <- rbind(feed$trips, duplicated_trip)
  feed$trips[trip_id %chin% trips[2:3], shape_id := NA]
  feed$trips[trip_id == trips[4], route_id := NA]

  stop_ids <- feed$stop_times[trip_id == trips[5]]$stop_id
  feed$stops[stop_id == stop_ids[2], `:=`(stop_lat = NA, stop_lon = NA)]
  same_coords_stop <- feed$stops[stop_id == stop_ids[3]]
  same_coords_stop[, stop_id := "same_coords"]
  feed$stops <- rbind(feed$stops, same_coords_stop)

  copy_trip <- feed$stop_times[trip_id == trips[6]]
  copy_trip[, trip_id := "copy_trip"]
  copy_trip[stop_id == stop_ids[3], stop_id := "same_coords"]
  ghost_trip <- feed$stop_times[trip_id == trips[6]]
  ghost_trip[, trip_id := "ghost_trip"]
  feed$stop_times <- rbind(feed$stop_times, copy_trip, ghost_trip)
  feed$trips <- rbind(
    feed$trips,
    feed$trips[trip_id == trips[6]][, trip_id := "copy_trip"]
  )

  feed$stop_times <- feed$stop_times[rev(seq_len(nrow(feed$stop_times)))]

  return(feed)
}

# tests -------------------------------------------------------------------

test_that("raises errors due to incorrect input types/value", {
  expect_error(tester(unclass(gtfs)))
  expect_error(tester(route_id = as.factor(route_id)))
  expect_error(tester(route_id = NA))
  expect_error(tester(method = "stop_times"))
  expect_error(tester(method = c("shapes", "euclidean")))
  expect_error(tester(crs = "4674"))
})

test_that("raises errors if gtfs doesn't have required files/fields", {
  expect_error(tester(copy_gtfs_without_file(gtfs, "trips")))
  expect_error(tester(copy_gtfs_without_file(gtfs, "stop_times")))
  expect_error(tester(copy_gtfs_without_file(gtfs, "stops")))
  expect_error(tester(copy_gtfs_without_field(gtfs, "trips", "route_id")))
  expect_error(tester(copy_gtfs_without_field(gtfs, "stop_times", "stop_id")))
  expect_error(tester(copy_gtfs_without_field(gtfs, "stops", "stop_lat")))
  expect_error(
    tester(copy_gtfs_without_field(gtfs, "stop_times", "stop_sequence"))
  )
  expect_error(
    tester(copy_gtfs_diff_field_class(gtfs, "trips", "shape_id", "factor"))
  )
  expect_error(
    tester(copy_gtfs_without_field(gtfs, "shapes", "shape_pt_lat"))
  )
})

test_that("results in a LINESTRING sf with the right columns", {
  result <- tester()
  expect_s3_class(result, "sf")
  expect_s3_class(sf::st_geometry(result), "sfc_LINESTRING")
  expect_identical(
    names(result),
    c("route_id", "trip_pattern", "n_trips", "geometry")
  )
  expect_type(result$route_id, "character")
  expect_type(result$trip_pattern, "character")
  expect_type(result$n_trips, "integer")
  expect_identical(sf::st_crs(result), sf::st_crs(4326))
  expect_false(anyDuplicated(as_variants(result)[, .(route_id, wkb)]) > 0)

  # rows are ordered by route_id and decreasing n_trips

  row_order <- data.table::data.table(
    route_id = result$route_id,
    n_trips = result$n_trips
  )
  expected_order <- data.table::setorderv(
    data.table::copy(row_order),
    c("route_id", "n_trips"),
    order = c(1L, -1L)
  )
  expect_identical(row_order, expected_order)
})

test_that("variants and their numbers of trips are correct", {
  for (method in c("shapes", "euclidean")) {
    expect_identical(as_variants(tester(method = method)), oracle(gtfs, method))
  }

  # ggl doesn't have shapes, ber does

  expect_identical(
    as_variants(tester(ggl_gtfs, method = "euclidean")),
    oracle(ggl_gtfs, "euclidean")
  )
  expect_identical(as_variants(tester(ber_gtfs)), oracle(ber_gtfs))
})

test_that("handles edge cases correctly", {
  feed <- edge_feed()

  for (method in c("shapes", "euclidean")) {
    result <- suppressWarnings(tester(feed, method = method))
    expect_identical(as_variants(result), oracle(feed, method))

    # the ghost trip and the second entry of the duplicated trip are ignored,
    # and the trip without route_id gets a NA route_id

    expect_false("other_route" %chin% result$route_id)
    expect_true(NA_character_ %chin% result$route_id)
    expect_identical(
      sum(result$n_trips),
      sum(unique(feed$stop_times$trip_id) %chin% feed$trips$trip_id)
    )
  }
})

test_that("returns the geometries of the given routes", {
  result <- tester(route_id = route_id)
  expect_true(all(result$route_id == route_id))
  expected <- oracle(gtfs)
  is_route <- expected$route_id == route_id
  expect_identical(as_variants(result), expected[is_route])

  route_ids <- c(route_id, "2002-10")
  result <- tester(route_id = route_ids)
  expect_setequal(result$route_id, route_ids)
})

test_that("raises warnings and returns empty sf with non-existent route_ids", {
  expect_warning(
    result <- tester(route_id = c(route_id, "nonexistent")),
    class = "gtfstools_invalid_route_id"
  )
  expect_identical(result, tester(route_id = route_id))

  expect_warning(
    empty <- tester(route_id = "nonexistent", crs = 4674),
    class = "gtfstools_invalid_route_id"
  )
  expect_s3_class(empty, "sf")
  expect_identical(nrow(empty), 0L)
  expect_identical(
    names(empty),
    c("route_id", "trip_pattern", "n_trips", "geometry")
  )
  expect_type(empty$route_id, "character")
  expect_type(empty$trip_pattern, "character")
  expect_type(empty$n_trips, "integer")
  expect_s3_class(sf::st_geometry(empty), "sfc_LINESTRING")
  expect_identical(sf::st_crs(empty), sf::st_crs(4674))
})

test_that("crs is honoured", {
  expected <- sf::st_transform(tester(route_id = route_id), 4674)
  expect_equal(tester(route_id = route_id, crs = 4674), expected)
  expect_equal(
    tester(route_id = route_id, crs = sf::st_crs(4674)),
    expected
  )
})

test_that("falls back to straight lines when shapes are unavailable", {
  no_shapes_gtfs <- copy_gtfs_without_file(gtfs, "shapes")
  no_shape_id_gtfs <- copy_gtfs_without_field(gtfs, "trips", "shape_id")
  expected <- tester(method = "euclidean")

  for (feed in list(no_shapes_gtfs, no_shape_id_gtfs)) {
    n_warnings <- 0
    result <- withCallingHandlers(
      tester(feed),
      gtfstools_shapes_unavailable = function(cnd) {
        n_warnings <<- n_warnings + 1
        invokeRestart("muffleWarning")
      }
    )
    expect_identical(n_warnings, 1)
    expect_identical(result, expected)
  }
})

test_that("warns about every trip not linked to a usable shape", {
  feed <- read_gtfs(data_path)
  trip <- feed$trips$trip_id[1]

  # 'copy_trip' has the same route, shape and stops as 'trip', so only the
  # geometry of 'trip' is generated, but the warning must list both trips

  feed$trips[trip_id == trip, shape_id := "nonexistent"]
  copy_trip <- feed$stop_times[trip_id == trip][, trip_id := "copy_trip"]
  feed$stop_times <- rbind(feed$stop_times, copy_trip)
  feed$trips <- rbind(
    feed$trips,
    feed$trips[trip_id == trip][, trip_id := "copy_trip"]
  )

  trip_warning <- tryCatch(
    get_trip_geometry(feed),
    gtfstools_trips_without_shape = function(cnd) cnd
  )
  expect_warning(
    route_warning <- tryCatch(
      tester(feed),
      gtfstools_trips_without_shape = function(cnd) cnd
    ),
    NA
  )
  expect_identical(route_warning$trip_id, c(trip, "copy_trip"))
  expect_identical(
    conditionMessage(route_warning),
    conditionMessage(trip_warning)
  )
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  expect_identical(original_gtfs, gtfs)

  for (method in c("shapes", "euclidean")) {
    result <- tester(method = method)
    result <- tester(route_id = route_id, method = method)
    expect_identical(original_gtfs, gtfs)
  }
})
