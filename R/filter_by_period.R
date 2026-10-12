#' Filter GTFS object by period
#'
#' Filters a GTFS object by the services that run on at least one day of a
#' period, keeping (or dropping) the relevant entries in each file.
#'
#' @template gtfs
#' @param start_date,end_date A `Date` (including `IDate`) or a string in the
#'   `"YYYYMMDD"` or `"YYYY-MM-DD"` formats. The first and last days of the
#'   period, both included. `end_date` must not be earlier than `start_date`.
#' @param keep A logical. Whether the entries related to the services that run
#'   in the specified period should be kept or dropped (defaults to `TRUE`,
#'   which keeps the entries).
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the filtering
#'   process.
#'
#' @section Details:
#' Filtering by a period is the same as filtering by each of its days with
#' [filter_by_date()], whose details also apply here: the services that run
#' on at least one day of the period are kept (or dropped) as a whole, so they
#' may still run on days outside it.
#'
#' The period is first clipped to the dates covered by the `calendar` and
#' `calendar_dates` tables, on which the services can run, so the days of the
#' period outside them add no work. The memory used still grows with the
#' number of days the period shares with each service's calendar.
#'
#' @family filtering functions
#' @seealso [filter_by_date()], [get_active_services()], [get_dates()]
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' data_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' gtfs$calendar
#' gtfs$calendar_dates
#'
#' # keeps the services that run in the first week of july 2006
#' smaller_gtfs <- filter_by_period(gtfs, "2006-07-01", "2006-07-07")
#' smaller_gtfs$calendar
#'
#' # dates may also be given as Date objects. drops the services that run in
#' # the period
#' smaller_gtfs <- filter_by_period(
#'   gtfs,
#'   as.Date("2006-07-03"),
#'   as.Date("2006-07-04"),
#'   keep = FALSE
#' )
#' smaller_gtfs$calendar
#' @export
filter_by_period <- function(gtfs, start_date, end_date, keep = TRUE) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)

  # lengths are checked before formats, so that two dates raise a length
  # error

  checkmate::assert(
    checkmate::check_date(start_date, len = 1, any.missing = FALSE),
    checkmate::check_string(start_date),
    .var.name = "start_date"
  )
  checkmate::assert(
    checkmate::check_date(end_date, len = 1, any.missing = FALSE),
    checkmate::check_string(end_date),
    .var.name = "end_date"
  )
  checkmate::assert_logical(keep, len = 1, any.missing = FALSE)

  start_date <- parse_dates(start_date, arg = "start_date")
  end_date <- parse_dates(end_date, arg = "end_date")

  if (end_date < start_date) {
    cli::cli_abort(
      paste0(
        "{.arg end_date} ({.val {format(end_date)}}) must not be earlier ",
        "than {.arg start_date} ({.val {format(start_date)}})."
      ),
      class = "gtfstools_invalid_period"
    )
  }

  # without calendar tables, filter_by_date() returns the gtfs unchanged, with
  # a warning

  has_calendar <- gtfsio::check_file_exists(gtfs, "calendar")
  has_calendar_dates <- gtfsio::check_file_exists(gtfs, "calendar_dates")

  if (!has_calendar && !has_calendar_dates) {
    return(filter_by_date(gtfs, start_date, keep = keep))
  }

  # services only run on days covered by the calendar tables, so the period is
  # clipped to them before being expanded into days. dates are compared as
  # integers (days since 1970-01-01)

  feed_days <- integer(0)
  if (has_calendar) {
    gtfsio::assert_field_class(
      gtfs,
      "calendar",
      c("start_date", "end_date"),
      c("Date", "Date")
    )
    feed_days <- c(
      as.integer(gtfs$calendar$start_date),
      as.integer(gtfs$calendar$end_date)
    )
  }
  if (has_calendar_dates) {
    gtfsio::assert_field_class(gtfs, "calendar_dates", "date", "Date")
    feed_days <- c(feed_days, as.integer(gtfs$calendar_dates$date))
  }
  feed_days <- feed_days[!is.na(feed_days)]

  first_day <- as.integer(start_date)
  last_day <- as.integer(end_date)
  if (length(feed_days) > 0) {
    first_day <- max(first_day, min(feed_days))
    last_day <- min(last_day, max(feed_days))
  }

  # without any day in common, no service runs in the period, as when
  # filter_by_date() finds no service on the given dates

  if (length(feed_days) == 0 || first_day > last_day) {
    gtfs <- filter_by_service_id(gtfs, character(0), keep)
    return(gtfs)
  }

  days <- as.Date(seq(first_day, last_day), origin = "1970-01-01")
  gtfs <- filter_by_date(gtfs, days, keep = keep)

  return(gtfs)
}
