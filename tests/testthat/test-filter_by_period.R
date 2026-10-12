ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
ggl_gtfs <- read_gtfs(ggl_path)
spo_gtfs <- read_gtfs(spo_path)
ber_gtfs <- read_gtfs(
  system.file("extdata/ber_gtfs.zip", package = "gtfstools")
)

# filter_by_date() applied to each day of the period, used as the reference

by_days <- function(gtfs, start_date, end_date, keep = TRUE) {
  days <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  filter_by_date(gtfs, days, keep = keep)
}

# services are read from calendar because the trips of ggl only use WE

kept_services <- function(gtfs) sort(unique(gtfs$calendar$service_id))

test_that("raises errors due to incorrect input types/value", {
  no_class_gtfs <- ggl_gtfs
  attr(no_class_gtfs, "class") <- NULL
  expect_error(filter_by_period(no_class_gtfs, "2006-07-01", "2006-07-07"))
  expect_error(filter_by_period(ggl_gtfs, 20060701, "2006-07-07"))
  expect_error(filter_by_period(ggl_gtfs, "2006-07-01", NA))
  expect_error(
    filter_by_period(ggl_gtfs, c("2006-07-01", "2006-07-02"), "2006-07-07")
  )
  expect_error(
    filter_by_period(ggl_gtfs, "2006-07-01", "2006-07-07", keep = "TRUE")
  )
  expect_error(
    filter_by_period(ggl_gtfs, "2006-07-01", "2006-07-07", keep = NA)
  )
  expect_error(
    filter_by_period(ggl_gtfs, "2006-07-01", "2006-07-07", trim = "TRUE")
  )
  expect_error(
    filter_by_period(ggl_gtfs, "2006-07-01", "2006-07-07", trim = NA)
  )
  expect_error(
    filter_by_period(
      ggl_gtfs,
      "2006-07-01",
      "2006-07-07",
      trim = c(TRUE, TRUE)
    )
  )
})

test_that("errors name the date argument that is malformed", {
  expect_error(
    filter_by_period(ggl_gtfs, "2006-13-01", "2006-07-07"),
    regexp = "start_date",
    class = "gtfstools_bad_date_error"
  )
  expect_error(
    filter_by_period(ggl_gtfs, "2006-07-01", "07/07/2006"),
    regexp = "end_date",
    class = "gtfstools_bad_date_error"
  )
})

test_that("raises an error if end_date is earlier than start_date", {
  expect_error(
    filter_by_period(ggl_gtfs, "2006-07-07", "2006-07-01"),
    class = "gtfstools_invalid_period"
  )
})

test_that("results in a dt_gtfs object", {
  result <- filter_by_period(ggl_gtfs, "2006-07-01", "2006-07-07")
  expect_s3_class(result, "dt_gtfs")
})

test_that("without trim, gives the same result as filter_by_date()", {
  periods <- list(
    list(ggl_gtfs, "2006-07-01", "2006-07-07"),
    list(ggl_gtfs, "2006-07-03", "2006-07-04"),
    list(spo_gtfs, "2010-03-01", "2010-03-31"),
    list(ber_gtfs, "2021-01-04", "2021-01-10")
  )

  for (period in periods) {
    for (keep in c(TRUE, FALSE)) {
      expect_identical(
        filter_by_period(
          period[[1]],
          period[[2]],
          period[[3]],
          keep = keep,
          trim = FALSE
        ),
        by_days(period[[1]], period[[2]], period[[3]], keep = keep)
      )
    }
  }
})

test_that("accepts Dates, IDates and mixed formats", {
  expected <- filter_by_period(ggl_gtfs, "2006-07-01", "2006-07-07")

  expect_identical(
    filter_by_period(ggl_gtfs, "20060701", "2006-07-07"),
    expected
  )
  expect_identical(
    filter_by_period(ggl_gtfs, as.Date("2006-07-01"), as.Date("2006-07-07")),
    expected
  )
  expect_identical(
    filter_by_period(
      ggl_gtfs,
      data.table::as.IDate("2006-07-01"),
      data.table::as.IDate("2006-07-07")
    ),
    expected
  )
})

test_that("a single-day period is the same as filter_by_date()", {
  expect_identical(
    filter_by_period(ggl_gtfs, "2006-07-03", "2006-07-03", trim = FALSE),
    filter_by_date(ggl_gtfs, "2006-07-03")
  )
})

test_that("includes both ends of the period", {

  # WE runs on 2006-07-01 (a saturday), the end of the period, and WD on
  # 2006-07-31 (a monday), its start. the other days are outside the calendar

  result <- filter_by_period(ggl_gtfs, "2006-06-30", "2006-07-01")
  expect_identical(kept_services(result), "WE")

  result <- filter_by_period(ggl_gtfs, "2006-07-31", "2006-08-05")
  expect_identical(kept_services(result), "WD")

  # WE runs on the day after the end of the period, which is not included

  result <- filter_by_period(ggl_gtfs, "2006-06-29", "2006-06-30")
  expect_identical(kept_services(result), character(0))
})

test_that("handles periods outside or beyond the feed's dates", {

  # a period outside the feed's dates is the same as a day without service

  for (keep in c(TRUE, FALSE)) {
    expect_identical(
      filter_by_period(ggl_gtfs, "2010-01-01", "2010-12-31", keep = keep),
      filter_by_date(ggl_gtfs, "2010-01-01", keep = keep)
    )
  }

  # a period of decades is clipped to the feed's own dates

  expect_identical(
    filter_by_period(spo_gtfs, "1900-01-01", "2099-12-31"),
    filter_by_period(spo_gtfs, "2008-01-01", "2020-05-01")
  )
})

test_that("works with only one of the calendar tables, or empty ones", {
  no_calendar_gtfs <- copy_gtfs_without_file(ggl_gtfs, "calendar")
  expect_identical(
    filter_by_period(
      no_calendar_gtfs,
      "2006-07-01",
      "2006-07-07",
      trim = FALSE
    ),
    by_days(no_calendar_gtfs, "2006-07-01", "2006-07-07")
  )

  empty_calendar_gtfs <- copy_gtfs_without_file(spo_gtfs, "calendar")
  empty_calendar_gtfs$calendar <- spo_gtfs$calendar[0]
  expect_no_warning(
    result <- filter_by_period(empty_calendar_gtfs, "2010-01-01", "2010-01-31")
  )
  expect_identical(
    result,
    filter_by_date(empty_calendar_gtfs, "2010-01-01")
  )

  # without both tables, the gtfs is returned unchanged with a warning

  no_tables_gtfs <- copy_gtfs_without_file(no_calendar_gtfs, "calendar_dates")
  expect_warning(
    result <- filter_by_period(no_tables_gtfs, "2006-07-01", "2006-07-07"),
    class = "gtfstools_no_calendar_warning"
  )
  expect_identical(result, no_tables_gtfs)
})

test_that("trims the calendars to exactly the service dates in the period", {
  poa_gtfs <- read_gtfs(
    system.file("extdata/poa_gtfs.zip", package = "gtfstools")
  )
  periods <- list(
    list(ggl_gtfs, "2006-07-01", "2006-07-07"),
    list(ggl_gtfs, "2006-07-03", "2006-07-04"),
    list(spo_gtfs, "2010-03-01", "2010-03-31"),
    list(poa_gtfs, "2019-03-04", "2019-03-10")
  )

  for (period in periods) {
    gtfs <- period[[1]]
    period_days <- seq(as.Date(period[[2]]), as.Date(period[[3]]), "day")
    result <- filter_by_period(gtfs, period[[2]], period[[3]])

    expect_identical(
      get_dates(result),
      get_dates(gtfs)[get_dates(gtfs) %in% period_days]
    )
  }
})

test_that("trims feeds with only one of the calendar tables", {
  period_days <- seq(as.Date("2006-07-01"), as.Date("2006-07-07"), "day")

  for (file in c("calendar", "calendar_dates")) {
    gtfs <- copy_gtfs_without_file(ggl_gtfs, file)
    result <- filter_by_period(gtfs, "2006-07-01", "2006-07-07")
    expect_identical(
      get_dates(result),
      get_dates(gtfs)[get_dates(gtfs) %in% period_days]
    )
  }
})

test_that("keeps services that run in the period only through additions", {

  # on 2006-07-03 and 2006-07-04, WE runs only through additions in
  # calendar_dates, and WD is removed

  result <- filter_by_period(ggl_gtfs, "2006-07-03", "2006-07-04")

  expect_identical(result$calendar$service_id, "WE")
  expect_identical(result$calendar$start_date, as.Date("2006-07-03"))
  expect_identical(result$calendar$end_date, as.Date("2006-07-04"))
  expect_true(
    all(result$calendar_dates$date %in% as.Date(c("2006-07-03", "2006-07-04")))
  )
  expect_identical(
    get_dates(result),
    as.Date(c("2006-07-03", "2006-07-04"))
  )

  # with a calendar entry that doesn't overlap the period, only the addition
  # is kept

  gtfs <- ggl_gtfs
  gtfs$calendar <- data.table::copy(ggl_gtfs$calendar)
  gtfs$calendar[service_id == "WE", start_date := as.Date("2006-07-10")]
  result <- filter_by_period(gtfs, "2006-07-03", "2006-07-04")

  expect_identical(nrow(result$calendar), 0L)
  expect_identical(get_dates(result), as.Date(c("2006-07-03", "2006-07-04")))
})

test_that("drops calendar entries with missing dates", {
  gtfs <- ggl_gtfs
  gtfs$calendar <- data.table::copy(ggl_gtfs$calendar)
  gtfs$calendar[service_id == "WD", end_date := as.Date(NA)]

  result <- filter_by_period(gtfs, "2006-07-01", "2006-07-31")
  expect_false("WD" %in% result$calendar$service_id)
  expect_identical(
    get_dates(result),
    get_dates(filter_by_period(gtfs, "2006-07-01", "2006-07-31", trim = FALSE))
  )
})

test_that("keeps the class of IDate calendar columns", {
  gtfs <- ggl_gtfs
  gtfs$calendar <- data.table::copy(ggl_gtfs$calendar)
  gtfs$calendar[, start_date := data.table::as.IDate(start_date)]
  gtfs$calendar[, end_date := data.table::as.IDate(end_date)]

  result <- filter_by_period(gtfs, "2006-07-05", "2006-07-12")
  expect_s3_class(result$calendar$start_date, "IDate")

  # also when a single calendar entry is left

  single_row <- filter_by_period(gtfs, "2006-07-03", "2006-07-04")
  expect_identical(nrow(single_row$calendar), 1L)
  expect_s3_class(single_row$calendar$start_date, "IDate")
  expect_s3_class(single_row$calendar$end_date, "IDate")
  expect_identical(
    get_dates(result),
    get_dates(filter_by_period(ggl_gtfs, "2006-07-05", "2006-07-12"))
  )
})

test_that("ignores trim when keep = FALSE", {
  expect_identical(
    filter_by_period(ggl_gtfs, "2006-07-03", "2006-07-04", keep = FALSE),
    filter_by_period(
      ggl_gtfs,
      "2006-07-03",
      "2006-07-04",
      keep = FALSE,
      trim = FALSE
    )
  )
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(ggl_path)
  gtfs <- read_gtfs(ggl_path)
  expect_identical(original_gtfs, gtfs)

  filter_by_period(gtfs, "2006-07-01", "2006-07-07")
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
})
