spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
poa_path <- system.file("extdata/poa_gtfs.zip", package = "gtfstools")
ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(spo_path)
poa_gtfs <- read_gtfs(poa_path)

tester <- function(gtfs = get("gtfs", envir = parent.frame()),
                   route_id = NULL,
                   from = NULL,
                   to = NULL,
                   date = NULL) {
  get_vkt(gtfs, route_id, from, to, date)
}

# expected vkt of spo on a single date, independent of the calendar and
# departure helpers used by get_vkt(): every spo trip is listed in
# 'frequencies', whose entries are valid, unique and don't overlap, so the
# departures of an entry are every headway_secs from start_time until (but not
# including) end_time

expected_spo_vkt <- function(date) {
  day_gtfs <- filter_by_date(gtfs, date)

  entries <- day_gtfs$frequencies[
    ,
    .(
      trip_id,
      start_secs = string_to_seconds(start_time),
      end_secs = string_to_seconds(end_time),
      headway_secs
    )
  ]
  departures <- entries[
    ,
    .(n = sum((end_secs - start_secs - 1L) %/% headway_secs + 1L)),
    by = trip_id
  ]
  lengths <- get_trip_length(day_gtfs, unit = "km")

  trips <- day_gtfs$trips[, .(trip_id, route_id)]
  trips[, n := departures$n[match(trip_id, departures$trip_id)]]
  trips[, length := lengths$length[match(trip_id, lengths$trip_id)]]

  return(trips[, .(vkt = sum(length * n)), keyby = route_id])
}

# expected vkt of poa (whose trips are all scheduled) on the given dates and
# time of day, from the first departure of each trip in 'stop_times'

expected_poa_vkt <- function(dates, from, to) {
  from_secs <- string_to_seconds(from)
  to_secs <- string_to_seconds(to)

  vkt_by_day <- lapply(
    dates,
    function(date) {
      day_gtfs <- filter_by_date(poa_gtfs, date)
      departures <- day_gtfs$stop_times[
        ,
        .(
          departure_secs = min(
            string_to_seconds(departure_time),
            na.rm = TRUE
          )
        ),
        by = trip_id
      ]
      departures <- departures[
        departure_secs >= from_secs & departure_secs < to_secs
      ]
      lengths <- get_trip_length(
        day_gtfs,
        trip_id = departures$trip_id,
        unit = "km"
      )

      trips <- day_gtfs$trips[trip_id %chin% departures$trip_id]
      trips <- trips[, .(trip_id, route_id)]
      trips[, length := lengths$length[match(trip_id, lengths$trip_id)]]
      return(trips[, .(vkt = sum(length)), by = route_id])
    }
  )

  expected <- data.table::rbindlist(vkt_by_day)
  return(expected[, .(n_days = .N, vkt = sum(vkt)), keyby = route_id])
}

# tests -------------------------------------------------------------------

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(gtfs)))
  expect_error(tester(route_id = 1))
  expect_error(tester(route_id = NA_character_))
  expect_error(tester(from = "07:00", to = "09:00:00"))
  expect_error(tester(from = 7, to = "09:00:00"))
  expect_error(tester(from = "07:00:00", to = c("08:00:00", "09:00:00")))
  expect_error(tester(date = "2019-13-01"), class = "gtfstools_bad_date_error")
  expect_error(tester(date = 20190301))

  expect_error(
    tester(from = "07:00:00"),
    class = "gtfstools_invalid_time_of_day"
  )
  expect_error(
    tester(to = "09:00:00"),
    class = "gtfstools_invalid_time_of_day"
  )
  expect_error(
    tester(from = "09:00:00", to = "07:00:00"),
    class = "gtfstools_invalid_time_of_day"
  )
})

test_that("returns a data.table with one row per route", {
  result <- tester()

  expect_s3_class(result, "data.table")
  expect_named(result, c("route_id", "n_days", "vkt", "vkt_daily_mean"))
  expect_type(result$route_id, "character")
  expect_type(result$n_days, "integer")
  expect_type(result$vkt, "double")
  expect_type(result$vkt_daily_mean, "double")

  expect_identical(result$route_id, sort(unique(gtfs$trips$route_id)))
  expect_false(is.unsorted(result$route_id))
  expect_equal(result$vkt_daily_mean, result$vkt / result$n_days)

  route_result <- tester(route_id = c("CPTM L07", "2002-10"))
  expect_identical(route_result$route_id, c("2002-10", "CPTM L07"))
  expect_identical(
    route_result,
    result[route_id %chin% c("CPTM L07", "2002-10")]
  )
})

test_that("weights trip lengths by departures and service days", {
  # a weekday and a saturday, on which spo's routes run different services

  weekday <- "2019-03-04"
  saturday <- "2019-03-09"
  expected_weekday <- expected_spo_vkt(weekday)
  expected_saturday <- expected_spo_vkt(saturday)

  weekday_result <- tester(date = weekday)
  expect_identical(weekday_result$route_id, expected_weekday$route_id)
  expect_equal(weekday_result$vkt, expected_weekday$vkt)
  expect_identical(unique(weekday_result$n_days), 1L)
  expect_identical(weekday_result$vkt_daily_mean, weekday_result$vkt)

  # with two dates, the vkt of each route is the sum of its daily vkts

  both_days <- data.table::rbindlist(list(expected_weekday, expected_saturday))
  expected <- both_days[, .(n_days = .N, vkt = sum(vkt)), keyby = route_id]

  result <- tester(date = c(weekday, saturday))
  expect_identical(result$route_id, expected$route_id)
  expect_identical(result$n_days, expected$n_days)
  expect_equal(result$vkt, expected$vkt)
  expect_equal(result$vkt_daily_mean, expected$vkt / expected$n_days)
})

test_that("counts departures within the time of day", {
  # poa's trips are scheduled. on saturdays, a trip of route 176 departs at
  # 07:00:00, and on mondays one departs at 09:00:00

  dates <- c("2019-01-19", "2019-01-21")
  result <- tester(
    poa_gtfs,
    from = "07:00:00",
    to = "09:00:00",
    date = dates
  )
  expected <- expected_poa_vkt(dates, "07:00:00", "09:00:00")
  expect_identical(result$route_id, expected$route_id)
  expect_identical(result$n_days, expected$n_days)
  expect_equal(result$vkt, expected$vkt)

  # a departure at 'from' is counted, and one at 'to' isn't. they are the only
  # trips of poa that depart at 07:00:00 or 09:00:00

  route_vkt <- function(from, to, date) {
    result <- tester(poa_gtfs, "176", from = from, to = to, date = date)
    return(sum(result$vkt))
  }

  at_from <- get_trip_length(poa_gtfs, "176-1@2#700", unit = "km")$length
  expect_gt(at_from, 0)
  expect_equal(
    route_vkt("07:00:00", "08:00:00", "2019-01-19") -
      route_vkt("07:00:01", "08:00:00", "2019-01-19"),
    at_from
  )

  at_to <- get_trip_length(poa_gtfs, "176-1@1#900", unit = "km")$length
  expect_gt(at_to, 0)
  expect_equal(
    route_vkt("08:00:00", "09:00:01", "2019-01-21") -
      route_vkt("08:00:00", "09:00:00", "2019-01-21"),
    at_to
  )
})

test_that("counts the days on which each route runs", {
  # ber has many 'calendar_dates' exceptions. the days of each route are
  # compared to those found by the running sum of get_service_period()

  ber_gtfs <- read_gtfs(ber_path)
  result <- tester(ber_gtfs)

  expected_days <- vapply(
    result$route_id,
    function(route) {
      route_gtfs <- filter_by_route_id(ber_gtfs, route)
      length(get_service_period(route_gtfs))
    },
    integer(1),
    USE.NAMES = FALSE
  )
  expect_identical(result$n_days, expected_days)
  expect_identical(result$route_id, sort(unique(ber_gtfs$trips$route_id)))
})

test_that("warns about unknown route_ids and drops routes without service", {
  expect_warning(
    result <- tester(route_id = c("CPTM L07", "not_a_route")),
    class = "gtfstools_invalid_route_id"
  )
  expect_identical(result$route_id, "CPTM L07")

  expect_warning(
    result <- tester(route_id = "not_a_route"),
    class = "gtfstools_invalid_route_id"
  )
  expect_identical(nrow(result), 0L)

  # a date without service and a time of day without departures leave no
  # route

  no_service <- tester(date = "2030-01-01")
  expect_identical(nrow(no_service), 0L)
  expect_named(no_service, c("route_id", "n_days", "vkt", "vkt_daily_mean"))

  no_departures <- tester(poa_gtfs, from = "02:00:00", to = "03:00:00")
  expect_identical(nrow(no_departures), 0L)
})

test_that("works with missing optional files", {
  # without shapes, lengths are measured between stops

  no_shapes <- copy_gtfs_without_file(gtfs, "shapes")
  expect_warning(
    result <- tester(no_shapes),
    class = "gtfstools_shapes_unavailable"
  )
  expect_false(anyNA(result$vkt))
  expect_gt(min(result$vkt), 0)

  no_calendar <- copy_gtfs_without_file(gtfs, "calendar")
  expect_identical(nrow(tester(no_calendar)), 0L)

  # ggl's trips have no usable stops to measure their lengths, so its route
  # has an NA vkt, but still counts the days on which it runs

  ggl_gtfs <- read_gtfs(
    system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
  )
  expect_warning(
    result <- tester(ggl_gtfs),
    class = "gtfstools_shapes_unavailable"
  )
  expect_identical(result$n_days, length(get_dates(ggl_gtfs)))
  expect_identical(result$vkt, NA_real_)
  expect_identical(result$vkt_daily_mean, NA_real_)
})

test_that("only raises frequencies errors for trips that run", {
  # 'frequencies' entries with a zero headway are invalid. those of a trip
  # whose service never runs are not used

  invalid_gtfs <- read_gtfs(spo_path)
  invalid_gtfs$frequencies[trip_id == "6450-51-0", headway_secs := 0L]
  expect_error(
    tester(invalid_gtfs),
    class = "gtfstools_invalid_frequencies"
  )

  invalid_gtfs$trips[trip_id == "6450-51-0", service_id := "never"]
  expect_no_error(result <- tester(invalid_gtfs))
  expect_identical(
    result[route_id != "6450-51"],
    tester()[route_id != "6450-51"]
  )
})

test_that("calculate_vkt() returns a row for groups that don't run", {
  # poa's service T2@5 never runs, and T2@1 runs on the 65 weekdays of its 13
  # weeks

  trips <- rbind(
    poa_gtfs$trips[service_id == "T2@5", .(trip_id, service_id, group = "a")],
    poa_gtfs$trips[service_id == "T2@1", .(trip_id, service_id, group = "b")]
  )
  result <- calculate_vkt(poa_gtfs, trips, date = NULL, window = NULL)

  expect_identical(result$group, c("a", "b"))
  expect_identical(result$n_days, c(0L, 65L))
  expect_identical(result$vkt[1], NA_real_)
  expect_identical(result$vkt_daily_mean[1], NA_real_)
  expect_gt(result$vkt[2], 0)
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  calls <- list(
    list(),
    list(route_id = "CPTM L07"),
    list(from = "07:00:00", to = "09:00:00"),
    list(date = "2019-03-04")
  )
  for (call_args in calls) {
    result <- do.call(tester, c(list(gtfs), call_args))
    expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
  }
})
