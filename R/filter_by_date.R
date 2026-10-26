#' Filter GTFS object by date
#'
#' Filters a GTFS object by the services that run on the given dates, keeping
#' (or dropping) the relevant entries in each file.
#'
#' @template gtfs
#' @param date A `Date` vector (including `IDate`) or a character vector of
#'   dates in the `"YYYYMMDD"` or `"YYYY-MM-DD"` formats. The dates used to
#'   filter the data.
#' @param keep A logical. Whether the entries related to the services that run
#'   on the specified dates should be kept or dropped (defaults to `TRUE`,
#'   which keeps the entries).
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the filtering
#'   process.
#'
#' @section Details:
#' The services that run on at least one of the given dates are kept (or
#' dropped) as a whole: their `calendar` and `calendar_dates` entries are not
#' trimmed to the given dates, so they may still run on other dates.
#'
#' A service runs on a date if its `calendar` entry spans the date and flags
#' its weekday, unless the date is removed from it in `calendar_dates`
#' (`exception_type` 2), or if the date is added to it in `calendar_dates`
#' (`exception_type` 1). An addition wins over a removal on the same date.
#' Either table may be missing. If both are missing, the GTFS object is
#' returned unchanged, with a warning.
#'
#' @family filtering functions
#' @seealso [filter_by_weekday()], [get_calendar_overlap()]
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' data_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' # on 2006-07-03 (a monday) the weekday service "WD" is replaced by the
#' # weekend service "WE", according to the calendar_dates table
#' gtfs$calendar_dates
#'
#' smaller_gtfs <- filter_by_date(gtfs, "2006-07-03")
#' smaller_gtfs$calendar
#'
#' # dates may also be given as Date objects. drops the services that run on
#' # 2006-07-05 (a wednesday)
#' smaller_gtfs <- filter_by_date(gtfs, as.Date("2006-07-05"), keep = FALSE)
#' smaller_gtfs$calendar
#'
#' # a vector of dates keeps the services that run on at least one of them
#' week <- seq(as.Date("2006-07-01"), as.Date("2006-07-07"), by = "day")
#' smaller_gtfs <- filter_by_date(gtfs, week)
#' smaller_gtfs$calendar
#' @export
filter_by_date <- function(gtfs, date, keep = TRUE) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  date <- parse_dates(date)
  checkmate::assert_logical(keep, len = 1, any.missing = FALSE)

  if (
    !gtfsio::check_file_exists(gtfs, "calendar") &&
      !gtfsio::check_file_exists(gtfs, "calendar_dates")
  ) {
    cli::cli_warn(
      c(
        "The GTFS object has neither a {.file calendar} nor a
         {.file calendar_dates} table.",
        "i" = "It was returned unchanged."
      ),
      class = "gtfstools_no_calendar_warning"
    )
    return(gtfs)
  }

  services_on_days <- get_services_on_days(gtfs, unique(as.integer(date)))
  relevant_services <- unique(services_on_days$service_id)
  gtfs <- filter_by_service_id(gtfs, relevant_services, keep)

  return(gtfs)
}
