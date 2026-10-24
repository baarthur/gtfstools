# creates a feed with a trip that serves 6 stops on the equator, along a
# straight shape. only the first and last stops have times, and the trip is
# faster than 30 km/h. the feed also includes a duplicated stop time, and a
# route and a stop that no trip uses

hand_feed <- function() {
  feed <- list(
    routes = data.table::data.table(route_id = c("r1", "r_unused")),
    trips = data.table::data.table(
      trip_id = "t1",
      route_id = "r1",
      shape_id = "sh1"
    ),
    shapes = data.table::data.table(
      shape_id = "sh1",
      shape_pt_lat = c(0, 0),
      shape_pt_lon = c(0, 0.05),
      shape_pt_sequence = 1:2
    ),
    stops = data.table::data.table(
      stop_id = c(paste0("s", 1:6), "s_unused"),
      stop_lat = 0,
      stop_lon = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 1)
    ),
    stop_times = data.table::data.table(
      trip_id = "t1",
      arrival_time = c("08:00:00", "08:00:00", "", "", "", "", "08:01:00"),
      departure_time = c("08:00:00", "08:00:00", "", "", "", "", "08:01:00"),
      stop_id = c("s1", "s1", paste0("s", 2:6)),
      stop_sequence = c(1L, 1L, 2:6)
    )
  )

  return(gtfsio::new_gtfs(feed, "dt_gtfs"))
}

hand_gtfs <- hand_feed()

# creates a feed with three trips whose intermediate stops have dwell times
# below and above 30 seconds. trip A has a stop whose departure is written as
# "00:01:00", after midnight, and trip C visits stop Y twice

dwell_feed <- function() {
  feed <- list(
    routes = data.table::data.table(route_id = "r1"),
    trips = data.table::data.table(
      trip_id = c("A", "B", "C"),
      route_id = "r1"
    ),
    stops = data.table::data.table(
      stop_id = c("s0", "X", "Y", "Z", "s9"),
      stop_lat = 0,
      stop_lon = c(0, 0.01, 0.02, 0.03, 0.04)
    ),
    stop_times = data.table::data.table(
      trip_id = rep(c("A", "B", "C"), c(5, 3, 4)),
      arrival_time = c(
        "08:00:00", "08:05:00", "08:10:00", "23:59:00", "24:10:00",
        "09:00:00", "09:05:00", "09:10:00",
        "10:00:00", "10:05:00", "10:10:00", "10:20:00"
      ),
      departure_time = c(
        "08:00:00", "08:05:00", "08:12:00", "00:01:00", "24:10:00",
        "09:00:00", "09:05:00", "09:10:00",
        "10:00:00", "10:05:00", "10:12:00", "10:20:00"
      ),
      stop_id = c(
        "s0", "X", "Y", "Z", "s9",
        "s0", "Y", "s9",
        "s0", "Y", "Y", "s9"
      ),
      stop_sequence = c(1:5, 1:3, 1:4)
    )
  )

  return(gtfsio::new_gtfs(feed, "dt_gtfs"))
}

dwell_gtfs <- dwell_feed()

spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")

tester <- function(gtfs = hand_gtfs,
                   max_speed = 30,
                   speed_unit = "km/h",
                   ...) {
  fix_gtfs(gtfs, max_speed, speed_unit, ...)
}

secs <- function(x) string_to_seconds(x)

# tests ---

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(hand_gtfs)))
  expect_error(tester(max_speed = -1))
  expect_error(tester(max_speed = NA_real_))
  expect_error(tester(speed_unit = "kms/h"), regexp = "speed_unit")
  expect_error(tester(min_dwell_time = -1), regexp = "min_dwell_time")
  expect_error(
    tester(min_dwell_time = 30, dwell_time_unit = "sec"),
    regexp = "dwell_time_unit"
  )
})

test_that("raises errors if gtfs doesn't have required tables", {
  expect_error(
    tester(copy_gtfs_without_file(hand_gtfs, "trips")),
    regexp = "trips"
  )
  expect_error(
    tester(copy_gtfs_without_file(hand_gtfs, "stop_times")),
    regexp = "stop_times"
  )
  expect_error(
    tester(copy_gtfs_without_file(hand_gtfs, "stops")),
    regexp = "stops"
  )
})

test_that("results in a dt_gtfs object and doesn't change given gtfs", {
  original_gtfs <- hand_feed()
  gtfs <- hand_feed()
  expect_identical(original_gtfs, gtfs)

  result <- tester(gtfs)
  expect_s3_class(result, "dt_gtfs")
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)

  # spo has neither duplicated stop times nor blank times, so its dwell times
  # are set in the table returned by remove_duplicates(), not in a copy made
  # by a later step

  original_spo <- read_gtfs(spo_path)
  spo <- read_gtfs(spo_path)
  result <- tester(spo, max_speed = NULL, min_dwell_time = 30)
  expect_false(
    identical(result$stop_times$departure_time, spo$stop_times$departure_time)
  )
  expect_equal(original_spo, spo, ignore_attr = TRUE)
})

test_that("removes duplicated entries and unused ids", {
  result <- tester()

  expect_identical(nrow(result$stop_times), 6L)
  expect_identical(result$routes$route_id, "r1")
  expect_identical(result$stops$stop_id, paste0("s", 1:6))
})

test_that("caps the speed before interpolating stop times", {
  result <- tester()
  times <- result$stop_times

  # capping the whole span between the two stops with times delays the last
  # stop by ceiling() of the span's minimum duration. capping each segment
  # after interpolating would round up each segment instead

  unique_gtfs <- remove_duplicates(hand_gtfs)
  segment_secs <- get_trip_length(unique_gtfs, by = "segment")$length /
    30 * 3600
  span_secs <- as.integer(ceiling(sum(segment_secs)))
  expect_false(span_secs == sum(ceiling(segment_secs)))

  start <- secs("08:00:00")
  expect_identical(secs(times$arrival_time[6]), start + span_secs)
  expect_identical(secs(times$departure_time[6]), start + span_secs)

  # blank stops are filled in proportion to the distance along the capped span

  expected_second <- as.integer(
    start + segment_secs[1] / sum(segment_secs) * span_secs + 0.5
  )
  expect_identical(secs(times$arrival_time[2]), expected_second)

  filled <- secs(times$arrival_time)
  expect_false(anyNA(filled))
  expect_true(all(diff(filled) > 0))
})

test_that("doesn't cap speeds if max_speed is NULL", {
  result <- tester(max_speed = NULL)
  times <- result$stop_times

  expect_identical(times$arrival_time[c(1, 6)], c("08:00:00", "08:01:00"))
  expect_false(anyNA(secs(times$arrival_time)))
})

test_that("sets a minimum dwell time at intermediate stops", {
  result <- tester(dwell_gtfs, max_speed = NULL, min_dwell_time = 30)
  times <- result$stop_times

  # dwell times below 30 seconds are raised and the later times shifted.
  # longer and negative dwell times are kept, and so are the first and last
  # stops of each trip

  expect_identical(
    times$arrival_time,
    c(
      "08:00:00", "08:05:00", "08:10:30", "23:59:30", "24:10:30",
      "09:00:00", "09:05:00", "09:10:30",
      "10:00:00", "10:05:00", "10:10:30", "10:20:30"
    )
  )
  expect_identical(
    times$departure_time,
    c(
      "08:00:00", "08:05:30", "08:12:30", "00:01:30", "24:10:30",
      "09:00:00", "09:05:30", "09:10:30",
      "10:00:00", "10:05:30", "10:12:30", "10:20:30"
    )
  )
  expect_identical(times$stop_id, dwell_gtfs$stop_times$stop_id)

  # nothing is below the minimum anymore, so fixing the result again changes
  # nothing

  expect_no_warning(
    fixed_again <- tester(result, max_speed = NULL, min_dwell_time = 30)
  )
  expect_identical(fixed_again, result)
})

test_that("keeps the first stop with times when earlier stops stay blank", {
  # the trip's first and last stops have no times to be interpolated from, so
  # they stay blank. the first and last stops with times (X and Z) keep their
  # dwell times

  feed <- dwell_feed()
  feed$stop_times <- data.table::data.table(
    trip_id = "A",
    arrival_time = c("", "08:00:00", "08:05:00", "08:10:00", ""),
    departure_time = c("", "08:00:00", "08:05:00", "08:10:00", ""),
    stop_id = c("s0", "X", "Y", "Z", "s9"),
    stop_sequence = 1:5
  )
  result <- suppressWarnings(
    tester(feed, max_speed = NULL, min_dwell_time = 30)
  )

  expect_identical(
    result$stop_times$arrival_time,
    c("", "08:00:00", "08:05:00", "08:10:30", "")
  )
  expect_identical(
    result$stop_times$departure_time,
    c("", "08:00:00", "08:05:30", "08:10:30", "")
  )
})

test_that("sets the minimum dwell time at stops with interpolated times", {
  without_min <- tester()
  result <- tester(min_dwell_time = 10)

  expect_identical(nrow(result$stop_times), 6L)
  expect_identical(result$stop_times$stop_id, paste0("s", 1:6))
  expect_identical(
    get_dwell_time(result)$dwell_time,
    c(0, 10, 10, 10, 10, 0)
  )
  expect_identical(result$stop_times$departure_time[1], "08:00:00")
  expect_identical(
    secs(result$stop_times$arrival_time[6]),
    secs(without_min$stop_times$arrival_time[6]) + 40L
  )
})

test_that("works without shapes, with a warning", {
  no_shapes <- copy_gtfs_without_file(hand_gtfs, "shapes")
  warnings <- capture_warnings(result <- tester(no_shapes))
  expect_true(any(grepl("shapes", warnings)))
  expect_s3_class(result, "dt_gtfs")
  expect_false(anyNA(secs(result$stop_times$arrival_time)))
})

test_that("fills shape_dist_traveled, only in feeds with shapes", {
  result <- tester()

  # stops on the equator, 0.01 degrees apart, on the sphere used by {s2}

  expect_equal(
    result$stop_times$shape_dist_traveled,
    (0:5) * 0.01 * 6371010 * pi / 180
  )
  expect_equal(
    result$shapes$shape_dist_traveled,
    c(0, 0.05) * 6371010 * pi / 180
  )

  no_shapes <- copy_gtfs_without_file(hand_gtfs, "shapes")
  result <- suppressWarnings(tester(no_shapes))
  expect_false("shape_dist_traveled" %chin% names(result$stop_times))

  no_shape_id <- copy_gtfs_without_field(hand_gtfs, "trips", "shape_id")
  result <- suppressWarnings(tester(no_shape_id))
  expect_false("shape_dist_traveled" %chin% names(result$stop_times))
})

test_that("doesn't make up times for trips that end before they start", {
  # some trips of this feed have times after midnight written as "00:xx:xx",
  # so their times are not interpolated nor capped. only their first and last
  # stops have times, so no minimum dwell time is set either

  poa_gtfs <- read_gtfs(
    system.file("extdata/poa_gtfs.zip", package = "gtfstools")
  )
  trip <- "T2-1@1#2310"

  warning_classes <- character(0)
  result <- withCallingHandlers(
    tester(poa_gtfs, max_speed = 30, min_dwell_time = 30),
    warning = function(w) {
      warning_classes <<- c(warning_classes, class(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true("gtfstools_backwards_times" %in% warning_classes)

  # shape_dist_traveled is added to every trip, so only the original columns
  # are compared

  original_cols <- names(poa_gtfs$stop_times)
  expect_identical(
    result$stop_times[trip_id == trip, .SD, .SDcols = original_cols],
    poa_gtfs$stop_times[trip_id == trip]
  )
  expect_identical(
    setdiff(names(result$stop_times), original_cols),
    "shape_dist_traveled"
  )
})

test_that("unit arguments work correctly", {
  expect_identical(
    tester(max_speed = 36),
    tester(max_speed = 10, speed_unit = "m/s")
  )
  expect_identical(
    tester(min_dwell_time = 1, dwell_time_unit = "min"),
    tester(min_dwell_time = 60)
  )
  expect_identical(tester(min_dwell_time = 0), tester())
})
