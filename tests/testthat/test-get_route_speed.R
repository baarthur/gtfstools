data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)
route_id <- c("CPTM L07", "2002-10")

stat_cols <- c(
  "min_speed",
  "mean_speed",
  "median_speed",
  "max_speed",
  "sd_speed"
)

tester <- function(gtfs = get("gtfs", envir = parent.frame()),
                   route_id = NULL,
                   method = "shapes",
                   unit = "km/h") {
  get_route_speed(gtfs, route_id, method, unit)
}

# aggregates the output of get_trip_speed() independently, to compare with
# get_route_speed()

manual_route_speed <- function(trip_speeds) {
  trip_speeds <- trip_speeds[!is.na(route_id)]
  summarise <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(as.list(rep(NA_real_, 5)))
    list(min(x), mean(x), stats::median(x), max(x), stats::sd(x))
  }
  result <- trip_speeds[, summarise(speed), by = route_id]
  data.table::setnames(result, c("route_id", stat_cols))
  data.table::setorderv(result, "route_id")
  result[]
}

# tests -------------------------------------------------------------------

test_that("raises errors due to incorrect input types/value", {
  expect_error(tester(unclass(gtfs)))
  expect_error(tester(route_id = NA))
  expect_error(tester(route_id = c("CPTM L07", NA_character_)))
  expect_error(tester(route_id = factor(route_id)))
  expect_error(tester(method = "straight"))
  expect_error(tester(route_id = route_id, method = "straight"))
  expect_error(tester(unit = "km"))

  no_route_gtfs <- copy_gtfs_without_field(gtfs, "trips", "route_id")
  expect_error(tester(no_route_gtfs), class = "missing_required_field")

  factor_route_gtfs <- copy_gtfs_diff_field_class(
    gtfs,
    "trips",
    "route_id",
    "factor"
  )
  expect_error(tester(factor_route_gtfs), class = "wrong_class_field")
})

test_that("falls back to euclidean distances when shapes is missing", {
  no_shapes_gtfs <- copy_gtfs_without_file(gtfs, "shapes")
  expect_warning(
    route_speeds <- tester(no_shapes_gtfs, route_id = route_id),
    class = "gtfstools_shapes_unavailable"
  )
  expect_identical(
    route_speeds,
    tester(route_id = route_id, method = "euclidean")
  )
})

test_that("works with other feeds", {
  ggl_gtfs <- read_gtfs(
    system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
  )
  route_speeds <- suppressWarnings(tester(ggl_gtfs))
  expect_identical(names(route_speeds), c("route_id", stat_cols))
  expect_equal(
    route_speeds,
    manual_route_speed(suppressWarnings(get_trip_speed(ggl_gtfs)))
  )
})

test_that("outputs a data.table with the right columns and types", {
  route_speeds <- tester()
  expect_s3_class(route_speeds, "data.table")
  expect_identical(names(route_speeds), c("route_id", stat_cols))
  expect_type(route_speeds$route_id, "character")
  for (col in stat_cols) expect_type(route_speeds[[col]], "double")
  expect_false(is.unsorted(route_speeds$route_id))
  expect_false(anyDuplicated(route_speeds$route_id) > 0)
})

test_that("summarises the speeds calculated by get_trip_speed()", {
  expect_equal(tester(), manual_route_speed(get_trip_speed(gtfs)))

  route_speeds <- tester(route_id = route_id)
  expect_identical(route_speeds$route_id, sort(route_id))

  trip_ids <- gtfs$trips[route_id %chin% c("CPTM L07", "2002-10")]$trip_id
  expect_equal(
    route_speeds,
    manual_route_speed(get_trip_speed(gtfs, trip_ids))
  )

  # method and unit are passed to get_trip_speed()

  expect_equal(
    tester(route_id = route_id, method = "euclidean", unit = "mph"),
    manual_route_speed(
      get_trip_speed(gtfs, trip_ids, method = "euclidean", unit = "mph")
    )
  )
})

test_that("handles invalid and empty route_ids", {
  expect_warning(
    route_speeds <- tester(route_id = c(route_id, "nonexistent")),
    class = "gtfstools_invalid_route_id"
  )
  expect_identical(route_speeds, tester(route_id = route_id))

  expect_warning(
    empty_speeds <- tester(route_id = "nonexistent"),
    class = "gtfstools_invalid_route_id"
  )
  expect_identical(nrow(empty_speeds), 0L)
  expect_identical(names(empty_speeds), c("route_id", stat_cols))
  expect_type(empty_speeds$route_id, "character")
  for (col in stat_cols) expect_type(empty_speeds[[col]], "double")

  expect_silent(no_route_speeds <- tester(route_id = character(0)))
  expect_identical(no_route_speeds, empty_speeds)
})

test_that("doesn't return routes that were not requested", {
  # a trip listed under two routes in 'trips'

  dup_gtfs <- read_gtfs(data_path)
  route_trip <- dup_gtfs$trips[route_id == "CPTM L07"][1]
  dup_trip <- data.table::copy(route_trip)
  dup_trip[, route_id := "2002-10"]
  dup_gtfs$trips <- rbind(dup_gtfs$trips, dup_trip)

  route_speeds <- tester(dup_gtfs, route_id = "CPTM L07")
  expect_identical(route_speeds$route_id, "CPTM L07")
})

test_that("routes whose trips all have NA speeds get NA statistics", {
  # trips whose first and last times are the same have no positive duration

  na_gtfs <- read_gtfs(data_path)
  na_gtfs$stop_times <- data.table::copy(na_gtfs$stop_times)
  route_trips <- na_gtfs$trips[route_id == "CPTM L07"]$trip_id
  na_gtfs$stop_times[
    trip_id %chin% route_trips,
    `:=`(arrival_time = "08:00:00", departure_time = "08:00:00")
  ]

  route_speeds <- tester(na_gtfs, route_id = route_id)
  expect_identical(route_speeds$route_id, sort(route_id))

  na_route <- route_speeds[route_id == "CPTM L07"]
  for (col in stat_cols) {
    expect_type(na_route[[col]], "double")
    expect_true(is.na(na_route[[col]]))
  }

  # 2002-10 has a single trip, so only its sd_speed is NA

  expect_false(anyNA(route_speeds[route_id == "2002-10", !"sd_speed"]))

  # also when no route has a known speed

  only_na_speeds <- tester(na_gtfs, route_id = "CPTM L07")
  expect_identical(only_na_speeds, na_route)
})

test_that("sd_speed is NA for routes with a single trip", {
  single_gtfs <- read_gtfs(data_path)
  single_trip <- single_gtfs$trips[route_id == "CPTM L07"]$trip_id[1]
  single_gtfs$trips <- single_gtfs$trips[
    route_id != "CPTM L07" | trip_id == single_trip
  ]

  route_speeds <- tester(single_gtfs, route_id = "CPTM L07")
  expect_true(is.na(route_speeds$sd_speed))
  expect_identical(route_speeds$min_speed, route_speeds$max_speed)
  expect_identical(
    route_speeds$mean_speed,
    get_trip_speed(single_gtfs, single_trip)$speed
  )
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  expect_identical(original_gtfs, gtfs)

  result <- tester(gtfs)
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)

  result <- tester(gtfs, route_id = route_id)
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
})
