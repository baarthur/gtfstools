spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)

test_that("raises errors due to incorrect input types/value", {
  expect_error(convert_seconds_to_time(-1))
  expect_error(convert_seconds_to_time(1.5))
  expect_error(convert_seconds_to_time(Inf))
  expect_error(convert_seconds_to_time(max_time_secs + 1))
  expect_error(convert_seconds_to_time("3600"))
  expect_error(convert_seconds_to_time(list(3600)))
})

test_that("converts difftime objects to seconds", {
  expect_identical(
    convert_seconds_to_time(as.difftime(5, units = "mins")),
    "00:05:00"
  )
})

test_that("converts seconds to time strings", {
  expect_identical(
    convert_seconds_to_time(c(0, 59, 3600, 86400 + 1800)),
    c("00:00:00", "00:00:59", "01:00:00", "24:30:00")
  )
  expect_identical(
    convert_seconds_to_time(c(0L, max_time_secs)),
    c("00:00:00", "9999:59:59")
  )
})

test_that("converts NAs to blank strings", {
  expect_identical(convert_seconds_to_time(c(60, NA)), c("00:01:00", ""))
  expect_identical(convert_seconds_to_time(NA), "")
})

test_that("rounds values within tolerance of a whole number", {
  expect_identical(convert_seconds_to_time(3 - 1e-10), "00:00:03")
})

test_that("handles empty input", {
  expect_identical(convert_seconds_to_time(integer(0)), character(0))
  expect_identical(convert_seconds_to_time(numeric(0)), character(0))
})

test_that("is the counterpart of convert_time_to_seconds()", {
  converted <- convert_time_to_seconds(spo_gtfs, file = "stop_times")

  expect_identical(
    convert_seconds_to_time(converted$stop_times$departure_time_secs),
    spo_gtfs$stop_times$departure_time
  )
})
