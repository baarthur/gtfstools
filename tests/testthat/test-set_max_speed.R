spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)

tester <- function(gtfs = hand_gtfs,
                   max_speed = 30,
                   route_id = NULL,
                   trip_id = NULL,
                   unit = "km/h") {
  set_max_speed(gtfs, max_speed, route_id, trip_id, unit)
}

# creates a feed whose trips serve the same stops on the equator, along a
# straight shape. 'times' is a list with the arrival and departure times of
# each trip

synthetic_feed <- function(times, route_id, stop_id = paste0("s", 1:5)) {
  trip_ids <- paste0("t", seq_along(times))

  feed <- list(
    trips = data.table::data.table(
      trip_id = trip_ids,
      route_id = route_id,
      shape_id = "sh1"
    ),
    shapes = data.table::data.table(
      shape_id = "sh1",
      shape_pt_lat = c(0, 0),
      shape_pt_lon = c(0, 0.04),
      shape_pt_sequence = 1:2
    ),
    stops = data.table::data.table(
      stop_id = paste0("s", 1:5),
      stop_lat = 0,
      stop_lon = c(0, 0.01, 0.02, 0.03, 0.04)
    ),
    stop_times = data.table::rbindlist(
      lapply(
        seq_along(times),
        function(i) {
          data.table::data.table(
            trip_id = trip_ids[i],
            arrival_time = times[[i]]$arr,
            departure_time = times[[i]]$dep,
            stop_id = stop_id,
            stop_sequence = seq_along(stop_id)
          )
        }
      )
    )
  )

  return(gtfsio::new_gtfs(feed, "dt_gtfs"))
}

hand_gtfs <- synthetic_feed(
  list(
    # fast first and last spans, and a blank stop inside a slow span
    list(
      arr = c("08:00:00", "08:01:00", "", "08:10:00", "08:11:00"),
      dep = c("08:00:00", "08:02:00", "", "08:10:30", "08:11:00")
    ),
    # blank first stop, and stops with only one of the times
    list(
      arr = c("", "08:00:00", "", "08:05:00", ""),
      dep = c("", "", "08:03:00", "", "08:05:30")
    ),
    # zero-duration first span
    list(
      arr = c("08:00:00", "08:00:00", "08:10:00", "08:20:00", "08:30:00"),
      dep = c("08:00:00", "08:00:00", "08:10:00", "08:20:00", "08:30:00")
    ),
    # slow trip
    list(
      arr = c("08:00:00", "08:10:00", "08:20:00", "08:30:00", "08:40:00"),
      dep = c("08:00:00", "08:10:00", "08:20:00", "08:30:00", "08:40:00")
    ),
    # trip without times
    list(arr = rep("", 5), dep = rep("", 5))
  ),
  route_id = c("r1", "r1", "r2", "r2", "r2")
)

# minimum duration, in seconds, of spans with one and two segments at 30 km/h

segment_km <- get_trip_length(hand_gtfs, "t1", by = "segment")$length[1]
min_one <- as.integer(ceiling(segment_km / 30 * 3600))
min_two <- as.integer(ceiling(2 * segment_km / 30 * 3600))

secs <- function(x) string_to_seconds(x)
times_of <- function(gtfs, trip) {
  gtfs$stop_times[trip_id == trip, .(arrival_time, departure_time)]
}

# tests ---

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(hand_gtfs)))
  expect_error(tester(max_speed = "30"))
  expect_error(tester(max_speed = 0))
  expect_error(tester(max_speed = -1))
  expect_error(tester(max_speed = NA_real_))
  expect_error(tester(max_speed = c(30, 40)))
  expect_error(tester(unit = "kms/h"))
  expect_error(tester(unit = c("km/h", "m/s")))
  expect_error(tester(trip_id = NA_character_))
  expect_error(tester(trip_id = 1))
  expect_error(tester(route_id = NA_character_))
  expect_error(tester(route_id = 1))
})

test_that("raises errors if gtfs doesn't have required files/fields", {
  no_sequence <- copy_gtfs_without_field(
    hand_gtfs,
    "stop_times",
    "stop_sequence"
  )
  expect_error(tester(no_sequence))
  expect_error(tester(copy_gtfs_without_file(hand_gtfs, "stops")))

  # without shapes, lengths are calculated along straight lines

  expect_warning(tester(copy_gtfs_without_file(hand_gtfs, "shapes")))

  # 'trips' is only required to select trips by route

  no_trips <- copy_gtfs_without_file(hand_gtfs, "trips")
  expect_error(tester(no_trips, route_id = "r1"))
  expect_no_error(suppressWarnings(tester(no_trips, trip_id = "t1")))
})

test_that("raises warnings if non-existent ids are given", {
  expect_warning(tester(trip_id = c("t1", "ola")))
  expect_warning(
    tester(route_id = c("r1", "ola")),
    class = "gtfstools_invalid_route_id"
  )

  # an unknown trip_id doesn't result in all trips being changed

  expect_warning(result <- tester(trip_id = "ola"))
  expect_identical(result, hand_gtfs)
})

test_that("raises an error if times would be later than 9999:59:59", {
  expect_error(
    tester(max_speed = 1e-6),
    class = "gtfstools_time_out_of_range"
  )
})

test_that("results in a dt_gtfs object", {
  result <- tester()
  expect_s3_class(result, "dt_gtfs")
  expect_s3_class(result, "gtfs")
  expect_type(result, "list")
  expect_s3_class(result$stop_times, "data.table")
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  result <- tester(gtfs, max_speed = 20)
  expect_false(identical(result$stop_times, gtfs$stop_times))
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)

  original_hand <- data.table::copy(hand_gtfs$stop_times)
  result <- tester()
  expect_identical(hand_gtfs$stop_times, original_hand)
})

test_that("caps the speed of every trip by default", {
  result <- tester(spo_gtfs, max_speed = 40)

  speeds <- get_trip_speed(result, by = "segment")
  expect_true(all(speeds$speed <= 40 + 1e-8, na.rm = TRUE))

  original_speeds <- get_trip_speed(spo_gtfs, by = "segment")
  expect_true(any(original_speeds$speed > 40, na.rm = TRUE))

  # trips that were not faster than the maximum speed don't change

  fast_trips <- unique(original_speeds[speed > 40]$trip_id)
  expect_identical(
    result$stop_times[!trip_id %chin% fast_trips],
    spo_gtfs$stop_times[!trip_id %chin% fast_trips]
  )
})

test_that("keeps dwell times and blank times in feeds with blank times", {
  poa_gtfs <- read_gtfs(
    system.file("extdata/poa_gtfs.zip", package = "gtfstools")
  )
  expect_warning(
    result <- tester(poa_gtfs, max_speed = 20),
    class = "gtfstools_backwards_times"
  )

  original_arr <- secs(poa_gtfs$stop_times$arrival_time)
  original_dep <- secs(poa_gtfs$stop_times$departure_time)
  new_arr <- secs(result$stop_times$arrival_time)
  new_dep <- secs(result$stop_times$departure_time)

  expect_false(identical(new_arr, original_arr))
  expect_identical(is.na(new_arr), is.na(original_arr))
  expect_identical(is.na(new_dep), is.na(original_dep))
  expect_identical(new_dep - new_arr, original_dep - original_arr)
  expect_true(all(new_arr >= original_arr, na.rm = TRUE))
})

test_that("slows down fast spans and shifts the later times", {
  result <- tester(trip_id = "t1")
  times <- times_of(result, "t1")

  # the extra times of the first and last spans accumulate. the dwell times
  # are kept and the blank stop inside the slow span stays blank

  extra_first <- min_one - 60L
  extra_last <- min_one - 30L

  expect_identical(
    secs(times$arrival_time),
    c(
      secs("08:00:00"),
      secs("08:01:00") + extra_first,
      NA,
      secs("08:10:00") + extra_first,
      secs("08:11:00") + extra_first + extra_last
    )
  )
  expect_identical(
    secs(times$departure_time),
    c(
      secs("08:00:00"),
      secs("08:02:00") + extra_first,
      NA,
      secs("08:10:30") + extra_first,
      secs("08:11:00") + extra_first + extra_last
    )
  )
  expect_identical(times$arrival_time[3], "")
  expect_identical(times$departure_time[3], "")

  # the span with the blank stop has two segments and wasn't fast

  expect_true(min_two < secs("08:10:00") - secs("08:02:00"))
})

test_that("handles blank times at the first stop and at one of the times", {
  result <- tester(trip_id = "t2")
  times <- times_of(result, "t2")

  extra_third <- min_one - 120L
  extra_last <- min_one - 30L

  expect_identical(
    times$arrival_time,
    c(
      "",
      "08:00:00",
      "",
      seconds_to_string(secs("08:05:00") + extra_third),
      ""
    )
  )
  expect_identical(
    times$departure_time,
    c(
      "",
      "",
      "08:03:00",
      "",
      seconds_to_string(secs("08:05:30") + extra_third + extra_last)
    )
  )
})

test_that("caps spans with zero duration", {
  result <- tester(trip_id = "t3")
  times <- times_of(result, "t3")
  original <- times_of(hand_gtfs, "t3")

  expect_identical(times[1], original[1])
  expect_identical(
    secs(times$arrival_time[-1]),
    secs(original$arrival_time[-1]) + min_one
  )
})

test_that("leaves spans that end before they start unchanged, with a warning", {
  # times after midnight written as "00:xx:xx". the first span is fast, the
  # second ends before it starts and the last one is fast

  feed <- synthetic_feed(
    list(
      list(
        arr = c("23:58:00", "23:59:00", "00:01:00", "00:30:00", "00:31:00"),
        dep = c("23:58:00", "23:59:00", "00:01:00", "00:30:00", "00:31:00")
      )
    ),
    route_id = "r1"
  )

  expect_warning(
    result <- tester(feed),
    class = "gtfstools_backwards_times"
  )

  extra_first <- min_one - 60L
  extra_last <- min_one - 60L

  expect_identical(
    secs(result$stop_times$arrival_time),
    secs(feed$stop_times$arrival_time) +
      c(0L, extra_first, extra_first, extra_first, extra_first + extra_last)
  )
})

test_that("warns only about the selected trips with backwards spans", {
  backwards <- list(
    arr = c("23:58:00", "23:59:00", "00:01:00", "00:30:00", "00:31:00"),
    dep = c("23:58:00", "23:59:00", "00:01:00", "00:30:00", "00:31:00")
  )
  forwards <- list(
    arr = c("08:00:00", "08:10:00", "08:20:00", "08:30:00", "08:40:00"),
    dep = c("08:00:00", "08:10:00", "08:20:00", "08:30:00", "08:40:00")
  )
  feed <- synthetic_feed(
    list(backwards, backwards, forwards),
    route_id = "r1"
  )

  warning_message <- NULL
  withCallingHandlers(
    tester(feed, trip_id = c("t1", "t3")),
    gtfstools_backwards_times = function(w) {
      warning_message <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_match(warning_message, "1 trip has")
  expect_match(warning_message, "t1")
  expect_no_match(warning_message, "t2")

  expect_no_warning(tester(feed, trip_id = "t3"))
})

test_that("doesn't warn about backwards spans whose length is unknown", {
  # the third stop is not in 'stops', so the backwards span that ends at it
  # has an unknown length

  feed <- synthetic_feed(
    list(
      list(
        arr = c("23:58:00", "23:59:00", "00:01:00", "00:30:00", "00:31:00"),
        dep = c("23:58:00", "23:59:00", "00:01:00", "00:30:00", "00:31:00")
      )
    ),
    route_id = "r1",
    stop_id = c("s1", "s2", "sX", "s4", "s5")
  )

  expect_no_warning(result <- tester(feed))
})

test_that("leaves slow trips and trips without times unchanged", {
  result <- tester(trip_id = c("t4", "t5"))
  expect_identical(result, hand_gtfs)
})

test_that("skips only the spans whose length is unknown", {
  # the third stop is not in 'stops', so the lengths of the segments from and
  # to it are NA

  feed <- synthetic_feed(
    list(
      list(
        arr = c("08:00:00", "08:01:00", "08:01:10", "08:01:20", "08:02:20"),
        dep = c("08:00:00", "08:01:00", "08:01:10", "08:01:20", "08:02:20")
      )
    ),
    route_id = "r1",
    stop_id = c("s1", "s2", "sX", "s4", "s5")
  )

  result <- tester(feed)
  extra_first <- min_one - 60L
  extra_last <- min_one - 60L

  expect_identical(
    secs(result$stop_times$arrival_time),
    secs(c("08:00:00", "08:01:00", "08:01:10", "08:01:20", "08:02:20")) +
      c(0L, rep(extra_first, 3), extra_first + extra_last)
  )
})

test_that("works when stop_times is not ordered by stop_sequence", {
  unordered <- hand_gtfs
  unordered$stop_times <- hand_gtfs$stop_times[
    rev(seq_len(nrow(hand_gtfs$stop_times)))
  ]

  result <- tester(unordered)
  expected <- tester()

  data.table::setorderv(result$stop_times, c("trip_id", "stop_sequence"))
  expect_identical(result$stop_times, expected$stop_times)
})

test_that("selects the trips of route_id and trip_id", {
  changed_trips <- function(result) {
    changed <- result$stop_times$arrival_time !=
      hand_gtfs$stop_times$arrival_time
    return(unique(result$stop_times$trip_id[changed]))
  }

  expect_identical(changed_trips(tester()), c("t1", "t2", "t3"))
  expect_identical(changed_trips(tester(trip_id = "t1")), "t1")
  expect_identical(changed_trips(tester(route_id = "r2")), "t3")
  expect_identical(
    changed_trips(tester(route_id = "r2", trip_id = "t1")),
    c("t1", "t3")
  )

  expect_identical(tester(trip_id = character(0)), hand_gtfs)
})

test_that("unit argument works correctly", {
  expect_identical(
    tester(max_speed = 36),
    tester(max_speed = 10, unit = "m/s")
  )

  result <- tester(spo_gtfs, max_speed = 20, unit = "mph")
  speeds <- get_trip_speed(result, by = "segment", unit = "mph")
  expect_true(all(speeds$speed <= 20 + 1e-8, na.rm = TRUE))
})

test_that("updates pre-existing *_secs columns", {
  gtfs <- convert_time_to_seconds(hand_gtfs)
  result <- tester(gtfs)

  expect_identical(
    result$stop_times$arrival_time_secs,
    secs(result$stop_times$arrival_time)
  )
  expect_identical(
    result$stop_times$departure_time_secs,
    secs(result$stop_times$departure_time)
  )
})
