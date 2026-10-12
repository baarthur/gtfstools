skip_if_not_installed("ggplot2")
skip_if_not_installed("knitr")
skip_if_not_installed("rmarkdown")
skip_if_not(rmarkdown::pandoc_available(), "Pandoc is not available")

spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
poa_path <- system.file("extdata/poa_gtfs.zip", package = "gtfstools")
ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(spo_path)

tester <- function(gtfs = get("gtfs", envir = parent.frame()),
                   date = NULL,
                   route_id = NULL,
                   mode = NULL,
                   agency = NULL,
                   from = NULL,
                   to = NULL,
                   output_dir = tempfile("get_summary"),
                   open_browser = FALSE) {
  get_summary(
    gtfs,
    date = date,
    route_id = route_id,
    mode = mode,
    agency = agency,
    from = from,
    to = to,
    output_dir = output_dir,
    open_browser = open_browser
  )
}

totals_value <- function(result, indicator) {
  result$totals$value[result$totals$indicator == indicator]
}

# tests -------------------------------------------------------------------

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(gtfs)))
  expect_error(tester(tempfile(fileext = ".zip")))
  expect_error(tester(date = "2019-13-01"), class = "gtfstools_bad_date_error")
  expect_error(tester(route_id = 1))
  expect_error(tester(mode = "spaceship"))
  expect_error(tester(mode = NA))
  expect_error(tester(agency = 1))
  expect_error(tester(from = "07:00"))
  expect_error(tester(output_dir = 1))
  expect_error(tester(open_browser = NA))

  expect_error(
    tester(from = "07:00:00"),
    class = "gtfstools_invalid_time_of_day"
  )
  expect_error(
    tester(from = "09:00:00", to = "07:00:00"),
    class = "gtfstools_invalid_time_of_day"
  )
  expect_error(
    tester(agency = "not_an_agency"),
    class = "gtfstools_invalid_agency_id_error"
  )
})

test_that("raises error if the report dependencies are missing", {
  testthat::local_mocked_bindings(report_deps_available = function() FALSE)
  expect_error(tester(), class = "gtfstools_report_dependencies_error")
})

test_that("raises error if no trips are selected", {
  expect_error(
    tester(mode = "funicular"),
    class = "gtfstools_empty_selection_error"
  )
})

test_that("returns a list of data.tables and writes the report", {
  output_dir <- tempfile("get_summary")
  result <- tester(output_dir = output_dir)

  expect_type(result, "list")
  expect_named(
    result,
    c(
      "files", "agency", "services", "totals", "speed", "frequency",
      "distance"
    )
  )
  for (element in result) expect_s3_class(element, "data.table")

  stats_cols <- c("n", "min", "q25", "median", "mean", "q75", "max", "unit")
  expect_named(result$files, c("file", "n_rows"))
  expect_named(result$totals, c("indicator", "value", "unit"))
  expect_named(
    result$services,
    c(
      "start_date", "end_date", "n_days", "n_service_days", "n_services",
      "mean_trips_per_day", "busiest_date", "max_trips_per_day",
      "feed_start_date", "feed_end_date"
    )
  )
  expect_identical(nrow(result$services), 1L)
  expect_identical(
    result$totals$indicator,
    c(
      "n_agencies", "n_routes", "n_trips", "n_stops", "total_shape_length",
      "mean_shape_length", "total_trip_length", "mean_trip_length",
      "total_trip_duration", "mean_trip_duration", "mean_trip_speed",
      "mean_route_headway"
    )
  )
  expect_named(result$speed, stats_cols)
  expect_named(result$distance, stats_cols)
  expect_named(result$frequency, c(stats_cols, "date", "from", "to"))
  expect_true("agency_timezone" %in% names(result$agency))
  expect_s3_class(result$frequency$date, "Date")

  expect_identical(nrow(result$files), length(gtfs))
  expect_identical(
    totals_value(result, "n_trips"),
    as.numeric(nrow(gtfs$trips))
  )

  expect_true(file.exists(file.path(output_dir, "gtfs_summary.html")))
  expect_true(file.exists(file.path(output_dir, "gtfs_summary.md")))
  expect_true(
    file.exists(file.path(output_dir, "gtfs_summary_calendar.png"))
  )

  # a path and the feed read from it give the same result

  expect_identical(tester(spo_path), result)
})

test_that("filters the feed", {
  full <- tester()

  route_result <- tester(route_id = "CPTM L07")
  expect_identical(totals_value(route_result, "n_routes"), 1)
  expect_lt(
    totals_value(route_result, "n_trips"),
    totals_value(full, "n_trips")
  )

  poa_full <- tester(poa_path)
  poa_dates <- tester(poa_path, date = "2019-01-19")
  expect_lt(
    totals_value(poa_dates, "n_trips"),
    totals_value(poa_full, "n_trips")
  )

  agency_result <- tester(agency = "1")
  expect_identical(unique(agency_result$agency$agency_id), "1")

  window_result <- tester(from = "07:00:00", to = "09:00:00")
  expect_identical(window_result$frequency$from, "07:00:00")
  expect_identical(window_result$frequency$to, "09:00:00")

  # every spo trip is frequency-based, so its trips are all kept by the time
  # of day filter. poa's scheduled trips are filtered

  poa_window <- tester(poa_path, from = "07:00:00", to = "09:00:00")
  expect_lt(
    totals_value(poa_window, "n_trips"),
    totals_value(poa_full, "n_trips")
  )

  expect_identical(tester(mode = "bus"), tester(mode = 3))
  expect_lt(
    totals_value(tester(mode = "bus"), "n_routes"),
    totals_value(full, "n_routes")
  )
})

test_that("totals and services match the feed", {
  result <- tester()

  expect_identical(
    totals_value(result, "n_stops"),
    as.numeric(data.table::uniqueN(gtfs$stop_times$stop_id))
  )
  expect_equal(
    totals_value(result, "total_trip_duration"),
    sum(get_trip_duration(gtfs, unit = "h")$duration)
  )
  expect_equal(
    totals_value(result, "mean_trip_duration"),
    mean(get_trip_duration(gtfs, unit = "min")$duration)
  )
  expect_equal(
    totals_value(result, "mean_trip_length"),
    mean(get_trip_length(gtfs, unit = "km")$length)
  )

  used_shapes <- unique(gtfs$trips$shape_id)
  expect_equal(
    totals_value(result, "mean_shape_length"),
    mean(get_shape_length(gtfs, shape_id = used_shapes)$length)
  )

  service_days <- get_service_days(gtfs)
  services <- result$services
  expect_identical(services$start_date, min(service_days$date))
  expect_identical(services$end_date, max(service_days$date))
  expect_identical(services$n_service_days, nrow(service_days))
  expect_identical(
    services$n_days,
    as.integer(services$end_date - services$start_date) + 1L
  )
  expect_identical(
    services$n_services,
    data.table::uniqueN(gtfs$trips$service_id)
  )
  expect_identical(services$busiest_date, result$frequency$date)
  expect_identical(
    services$max_trips_per_day,
    as.numeric(max(service_days$n_trips))
  )
  expect_true(is.na(services$feed_start_date))
})

test_that("measures frequency on the busiest of the given days", {
  poa_gtfs <- read_gtfs(poa_path)
  service_days <- get_service_days(poa_gtfs)
  busiest_day <- service_days$date[which.max(service_days$n_trips)]

  result <- tester(poa_gtfs)
  expect_identical(result$frequency$date, busiest_day)

  # with given dates, the day is one of them, even if the feed's busiest day
  # is another one

  given_dates <- as.Date(c("2019-01-19", "2019-01-20"))
  expect_false(busiest_day %in% given_dates)
  result <- tester(poa_gtfs, date = given_dates)
  given_days <- service_days[service_days$date %in% given_dates]
  expect_identical(
    result$frequency$date,
    given_days$date[which.max(given_days$n_trips)]
  )
})

test_that("the default time of day spans the day's departures", {
  result <- tester()

  day_gtfs <- filter_by_date(gtfs, result$frequency$date)
  departures <- get_trip_departures(day_gtfs, day_gtfs$trips$trip_id)
  first_departure <- min(departures$departure_secs)
  last_departure <- max(departures$departure_secs)

  expect_identical(
    string_to_seconds(result$frequency$from),
    as.integer(first_departure %/% 3600 * 3600)
  )
  expect_identical(
    string_to_seconds(result$frequency$to),
    as.integer((last_departure %/% 3600 + 1) * 3600)
  )
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  # filter_by_time_of_day() adds columns by reference, so the time-of-day
  # filter is checked along with the other filters and the default call

  calls <- list(
    list(),
    list(mode = "bus"),
    list(date = "2008-01-01"),
    list(agency = "1"),
    list(route_id = "CPTM L07"),
    list(from = "07:00:00", to = "09:00:00")
  )
  for (call_args in calls) {
    result <- do.call(tester, c(list(gtfs), call_args))
    expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
    expect_identical(names(original_gtfs$stop_times), names(gtfs$stop_times))
    expect_identical(
      names(original_gtfs$frequencies),
      names(gtfs$frequencies)
    )
  }
})

test_that("works with missing optional files", {
  no_shapes <- copy_gtfs_without_file(gtfs, "shapes")
  expect_warning(
    result <- tester(no_shapes),
    class = "gtfstools_shapes_unavailable"
  )
  expect_identical(totals_value(result, "total_shape_length"), NA_real_)
  expect_identical(totals_value(result, "mean_shape_length"), NA_real_)

  no_frequencies <- copy_gtfs_without_file(gtfs, "frequencies")
  result <- tester(no_frequencies)
  expect_gt(result$frequency$n, 0)

  no_calendar <- copy_gtfs_without_file(gtfs, "calendar")
  expect_warning(
    result <- tester(no_calendar),
    class = "gtfstools_no_service_days"
  )
  expect_identical(result$frequency$n, 0L)
  expect_identical(result$services$n_service_days, 0L)
  expect_true(is.na(result$services$start_date))
  expect_identical(result$frequency$mean, NA_real_)
  expect_true(is.na(result$frequency$date))
})

test_that("handles blank agency_ids, unused shapes and missing times", {
  poa_gtfs <- read_gtfs(poa_path)

  # single-agency feeds may leave routes' agency_id blank

  blank_agency <- poa_gtfs
  blank_agency$routes <- data.table::copy(blank_agency$routes)
  blank_agency$routes[, agency_id := ""]
  result <- tester(blank_agency)
  expect_identical(nrow(result$agency), 1L)
  expect_identical(totals_value(result, "n_agencies"), 1)

  # and the feed's single agency can still be selected

  agency_result <- tester(blank_agency, agency = blank_agency$agency$agency_id)
  expect_identical(
    totals_value(agency_result, "n_trips"),
    totals_value(result, "n_trips")
  )

  # shapes not used by the selected trips are not summed

  route_result <- tester(poa_gtfs, route_id = poa_gtfs$routes$route_id[1])
  used_shapes <- unique(
    poa_gtfs$trips[route_id == poa_gtfs$routes$route_id[1]]$shape_id
  )
  expect_equal(
    totals_value(route_result, "total_shape_length"),
    sum(get_shape_length(poa_gtfs, shape_id = used_shapes)$length)
  )

  # trips without times don't make the total duration infinite

  no_times <- poa_gtfs
  no_times$stop_times <- data.table::copy(no_times$stop_times)
  no_times$stop_times[, `:=`(arrival_time = "", departure_time = "")]
  result <- suppressWarnings(tester(no_times))
  expect_identical(totals_value(result, "total_trip_duration"), NA_real_)
  expect_identical(totals_value(result, "mean_trip_duration"), NA_real_)

  # trips without times are left out of the mean duration

  one_blank <- poa_gtfs
  one_blank$stop_times <- data.table::copy(one_blank$stop_times)
  blank_trip <- one_blank$stop_times$trip_id[1]
  one_blank$stop_times[
    trip_id == blank_trip,
    `:=`(arrival_time = "", departure_time = "")
  ]
  result <- suppressWarnings(tester(one_blank))
  durations <- get_trip_duration(poa_gtfs, unit = "min")
  expect_equal(
    totals_value(result, "mean_trip_duration"),
    mean(durations[trip_id != blank_trip]$duration, na.rm = TRUE)
  )
})

test_that("uses the default output directory", {
  result <- get_summary(gtfs, open_browser = FALSE)
  expect_true(
    file.exists(file.path(tempdir(), "gtfstools", "gtfs_summary.html"))
  )
})

test_that("escapes markdown in the report", {
  odd_gtfs <- read_gtfs(spo_path)
  odd_gtfs$agency <- data.table::copy(odd_gtfs$agency)
  odd_gtfs$agency[, agency_name := "A $x$ *b* [c]"]

  output_dir <- tempfile("get_summary")
  result <- tester(odd_gtfs, output_dir = output_dir)
  md <- readLines(file.path(output_dir, "gtfs_summary.md"), encoding = "UTF-8")
  expect_true(
    any(grepl("A \\$x\\$ \\*b\\* \\[c\\]", md, fixed = TRUE))
  )

  # escaped brackets must not become display math in the HTML

  html <- readLines(
    file.path(output_dir, "gtfs_summary.html"),
    encoding = "UTF-8"
  )
  expect_false(any(grepl("math display", html, fixed = TRUE)))
})

test_that("works with other feeds", {
  # ggl's 'routes' has no agency_id field, and its trips have no usable stops
  # to measure their lengths, so their total length is NA instead of 0

  expect_warning(
    result <- tester(ggl_path),
    class = "gtfstools_shapes_unavailable"
  )
  expect_identical(nrow(result$agency), 1L)
  expect_identical(totals_value(result, "total_trip_length"), NA_real_)
  expect_identical(totals_value(result, "mean_trip_length"), NA_real_)

  # ggl's feed_info has empty validity dates, so they are NA. given dates are
  # reported as they are

  expect_true(is.na(result$services$feed_start_date))

  ggl_gtfs <- read_gtfs(ggl_path)
  ggl_gtfs$feed_info <- data.table::copy(ggl_gtfs$feed_info)
  ggl_gtfs$feed_info[, feed_start_date := as.Date("2006-01-01")]
  ggl_gtfs$feed_info[, feed_end_date := "20061231"]
  result <- suppressWarnings(tester(ggl_gtfs))
  expect_identical(result$services$feed_start_date, as.Date("2006-01-01"))
  expect_identical(result$services$feed_end_date, as.Date("2006-12-31"))
})
