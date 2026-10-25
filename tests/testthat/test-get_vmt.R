spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
poa_path <- system.file("extdata/poa_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(spo_path)

tester <- function(gtfs = get("gtfs", envir = parent.frame()),
                   route_id = NULL,
                   from = NULL,
                   to = NULL,
                   date = NULL) {
  get_vmt(gtfs, route_id, from, to, date)
}

# tests -------------------------------------------------------------------

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(gtfs)))
  expect_error(tester(route_id = 1))
  expect_error(tester(from = "07:00", to = "09:00:00"))
  expect_error(tester(date = "2019-13-01"), class = "gtfstools_bad_date_error")
  expect_error(
    tester(from = "07:00:00"),
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
  expect_named(result, c("route_id", "n_days", "vmt", "vmt_daily_mean"))
  expect_type(result$route_id, "character")
  expect_type(result$n_days, "integer")
  expect_type(result$vmt, "double")
  expect_type(result$vmt_daily_mean, "double")

  # an empty result keeps the columns

  no_service <- tester(date = "2030-01-01")
  expect_identical(nrow(no_service), 0L)
  expect_named(no_service, c("route_id", "n_days", "vmt", "vmt_daily_mean"))
})

test_that("converts the vkt to international miles", {
  poa_gtfs <- read_gtfs(poa_path)
  calls <- list(
    list(gtfs),
    list(gtfs, route_id = c("CPTM L07", "2002-10"), date = "2019-03-04"),
    list(poa_gtfs, from = "07:00:00", to = "09:00:00")
  )

  for (call_args in calls) {
    vkt <- do.call(get_vkt, call_args)
    vmt <- do.call(get_vmt, call_args)

    expect_identical(vmt$route_id, vkt$route_id)
    expect_identical(vmt$n_days, vkt$n_days)
    expect_equal(vmt$vmt, vkt$vkt / 1.609344)
    expect_equal(vmt$vmt_daily_mean, vkt$vkt_daily_mean / 1.609344)
  }

  # one mile is 1.609344 km, so a route's vmt is smaller than its vkt

  vkt <- get_vkt(gtfs, route_id = "CPTM L07", date = "2019-03-04")
  vmt <- tester(route_id = "CPTM L07", date = "2019-03-04")
  expect_lt(vmt$vmt, vkt$vkt)
})

test_that("warns about unknown route_ids", {
  expect_warning(
    result <- tester(route_id = c("CPTM L07", "not_a_route")),
    class = "gtfstools_invalid_route_id"
  )
  expect_identical(result$route_id, "CPTM L07")
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  result <- tester(gtfs, from = "07:00:00", to = "09:00:00")
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
})
