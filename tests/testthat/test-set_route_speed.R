data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)

route_trips <- function(gtfs, route) gtfs$trips[route_id %chin% route]$trip_id

test_that("raises errors due to incorrect input types/value", {
  no_class_gtfs <- gtfs
  attr(no_class_gtfs, "class") <- NULL
  expect_error(set_route_speed(no_class_gtfs, "CPTM L07", 50))
  expect_error(set_route_speed(gtfs, as.factor("CPTM L07"), 50))
  expect_error(set_route_speed(gtfs, NA_character_, 50))
  expect_error(set_route_speed(gtfs, "CPTM L07", "50"))
  expect_error(
    set_route_speed(gtfs, c("CPTM L07", "2105-10", "6450-51"), c(50, 60))
  )
  expect_error(set_route_speed(gtfs, c("CPTM L07", "2105-10"), c(50, NA)))
  expect_error(
    set_route_speed(gtfs, "CPTM L07", 0),
    class = "gtfstools_invalid_speed"
  )
  expect_error(set_route_speed(gtfs, "CPTM L07", 50, unit = "kms/h"))
  expect_error(set_route_speed(gtfs, "CPTM L07", 50, by_reference = NA))
})

test_that("raises errors if gtfs doesn't have required files/fields", {
  no_routeid_gtfs <- copy_gtfs_without_field(gtfs, "trips", "route_id")
  no_stop_times_gtfs <- copy_gtfs_without_file(gtfs, "stop_times")

  expect_error(set_route_speed(no_routeid_gtfs, "CPTM L07", 50))
  expect_error(set_route_speed(no_stop_times_gtfs, "CPTM L07", 50))
})

test_that("validates the other args even if routes have no trips", {
  expect_error(set_route_speed(gtfs, "ola", 50, unit = "kmh"))
  expect_error(set_route_speed(gtfs, "ola", 50, from = "9am"))
})

test_that("gives the same result as set_trip_speed() with the route's trips", {
  trips <- route_trips(gtfs, "CPTM L07")

  by_route <- set_route_speed(gtfs, "CPTM L07", 50)
  by_trip <- set_trip_speed(gtfs, trips, 50)
  expect_s3_class(by_route, "dt_gtfs")
  expect_equal(by_route$stop_times, by_trip$stop_times, ignore_attr = TRUE)

  by_route <- suppressWarnings(
    set_route_speed(
      gtfs,
      "CPTM L07",
      15,
      unit = "m/s",
      first_stop = "18917",
      from = "04:00:00"
    )
  )
  by_trip <- suppressWarnings(
    set_trip_speed(
      gtfs,
      trips,
      15,
      unit = "m/s",
      first_stop = "18917",
      from = "04:00:00"
    )
  )
  expect_s3_class(by_route, "dt_gtfs")
  expect_equal(by_route$stop_times, by_trip$stop_times, ignore_attr = TRUE)
})

test_that("only changes the trips that depart within from and to", {
  poa_path <- system.file("extdata/poa_gtfs.zip", package = "gtfstools")
  poa_gtfs <- read_gtfs(poa_path)

  by_route <- set_route_speed(
    poa_gtfs,
    "T2",
    25,
    from = "07:00:00",
    to = "09:00:00"
  )
  by_trip <- set_trip_speed(
    poa_gtfs,
    route_trips(poa_gtfs, "T2"),
    25,
    from = "07:00:00",
    to = "09:00:00"
  )
  expect_equal(by_route$stop_times, by_trip$stop_times, ignore_attr = TRUE)
  expect_false(isTRUE(all.equal(by_route$stop_times, poa_gtfs$stop_times)))
})

test_that("sets the same speed for every route", {
  routes <- c("CPTM L07", "2105-10")
  new_gtfs <- set_route_speed(gtfs, routes, 50)

  speeds <- get_trip_speed(new_gtfs, route_trips(gtfs, routes))

  expect_true(all(abs(speeds$speed - 50) < 0.5))

  # one speed per route is not accepted

  expect_error(set_route_speed(gtfs, routes, c(50, 20)))
})

test_that("warns about invalid route_ids and changes the valid ones", {
  expect_warning(
    new_gtfs <- set_route_speed(gtfs, c("CPTM L07", "ola"), 50),
    class = "gtfstools_invalid_route_id"
  )
  expect_equal(
    new_gtfs$stop_times,
    set_route_speed(gtfs, "CPTM L07", 50)$stop_times,
    ignore_attr = TRUE
  )

  expect_warning(
    new_gtfs <- set_route_speed(gtfs, "ola", 50),
    class = "gtfstools_invalid_route_id"
  )
  expect_equal(new_gtfs$stop_times, gtfs$stop_times, ignore_attr = TRUE)
})

test_that("ignores trips without stop times, without a warning", {
  trips <- route_trips(gtfs, "CPTM L07")
  partial_gtfs <- gtfs
  partial_gtfs$stop_times <- data.table::copy(gtfs$stop_times)[
    trip_id != trips[1]
  ]

  expect_no_warning(new_gtfs <- set_route_speed(partial_gtfs, "CPTM L07", 50))
  expect_equal(
    new_gtfs$stop_times,
    set_trip_speed(partial_gtfs, trips[-1], 50)$stop_times,
    ignore_attr = TRUE
  )
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  expect_identical(original_gtfs, gtfs)

  set_route_speed(gtfs, "CPTM L07", 50)
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
})

test_that("'by_reference' parameter works adequately", {
  gtfs <- read_gtfs(data_path)
  expected <- set_route_speed(gtfs, "CPTM L07", 50)

  result <- withVisible(
    set_route_speed(gtfs, "CPTM L07", 50, by_reference = TRUE)
  )
  expect_false(result$visible)
  expect_equal(gtfs$stop_times, expected$stop_times, ignore_attr = TRUE)
})
