#' Convert seconds after midnight to time strings
#'
#' Converts times in seconds after midnight to strings in the "HH:MM:SS"
#' format, as used in `stop_times` and `frequencies`. It is the counterpart of
#' [convert_time_to_seconds()], useful to write back times calculated from the
#' `_secs` columns.
#'
#' @param seconds A numeric vector with whole numbers of seconds after
#'   midnight, between 0 and 35999999 (`"9999:59:59"`). Values within
#'   floating-point tolerance of a whole number are rounded. `difftime`
#'   objects are converted to seconds first.
#'
#' @return A character vector with the same length as `seconds`. `NA`s are
#'   converted to `""`, as blank times in GTFS. Times past midnight keep
#'   counting the hours (e.g. `"25:30:00"`). Names are dropped.
#'
#' @seealso [convert_time_to_seconds()]
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' convert_seconds_to_time(c(0, 3600, 91800, NA))
#'
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#' gtfs <- convert_time_to_seconds(gtfs, file = "stop_times")
#'
#' # delay every departure by 5 minutes
#' delayed_departure <- convert_seconds_to_time(
#'   gtfs$stop_times$departure_time_secs + 300
#' )
#' head(delayed_departure)
#'
#' @export
convert_seconds_to_time <- function(seconds) {
  # difftime objects would otherwise be read in their own units (e.g. minutes)

  if (inherits(seconds, "difftime")) {
    seconds <- as.numeric(seconds, units = "secs")
  }

  checkmate::assert_integerish(seconds, lower = 0, upper = max_time_secs)

  # rounded, because whole numbers are checked within a tolerance: without
  # rounding, 2.9999999999 would be truncated to 2

  seconds <- as.integer(round(seconds))

  return(seconds_to_string(seconds))
}
