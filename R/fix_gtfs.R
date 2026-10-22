#' Fix common problems in GTFS feeds
#'
#' Cleans a GTFS feed in one call, by removing duplicated entries and unused
#' ids, optionally capping the speed of trips, filling blank stop times and
#' filling the distances travelled along shapes.
#'
#' @template gtfs
#' @param max_speed A number. The maximum speed of trips, in the unit given by
#'   `unit`. If `NULL` (the default), speeds are not capped.
#' @param unit A string representing the unit in which `max_speed` is given.
#'   One of `"km/h"` (the default), `"m/s"` or `"mph"` (miles per hour).
#'
#' @return The fixed GTFS object.
#'
#' @section Details:
#' The feed goes through the following steps, in this order:
#'
#' 1. [remove_duplicates()] removes duplicated entries from every table;
#' 2. [remove_unused_ids()] removes the trips without `stop_times`, and the
#'    entries of every table that no remaining trip uses (see its
#'    documentation for the exceptions). Note that this removes data from the
#'    feed;
#' 3. [set_max_speed()], only if `max_speed` is given, slows down the parts of
#'    trips that are faster than `max_speed`. Only spans between stops with
#'    known times are capped, and spans that end before they start (e.g.
#'    times after midnight written as `"00:30:00"`) are left unchanged, with
#'    a warning;
#' 4. [interpolate_stop_times()] fills blank stop times from the times of the
#'    stops around them. Speeds are capped before interpolating, so the filled
#'    times follow the capped times. Interpolated times are rounded to the
#'    second. Stops whose times can't be interpolated are left blank, with a
#'    warning;
#' 5. [interpolate_shape_dist_traveled()], only if the feed has a `shapes`
#'    table and `trips` has a `shape_id` column, replaces the
#'    `shape_dist_traveled` columns of `stop_times` and `shapes` with the
#'    distances, in meters, from the start of each shape. Existing values,
#'    whatever their unit, are replaced, and stops that can't be located along
#'    their trip's shape get `NA`.
#'
#' The `trips` and `stop_times` tables are always required, and `stops` is
#' required to cap speeds, to fill blank times and to fill the distances
#' travelled along shapes. The warnings raised are
#' those of the functions above, so some warnings (e.g. about a missing
#' `shapes` table) may be raised twice.
#'
#' @seealso [remove_duplicates()], [remove_unused_ids()], [set_max_speed()],
#'   [interpolate_stop_times()], [interpolate_shape_dist_traveled()]
#'
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#' }
#' library(dplyr)
#'
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' fixed_gtfs <- fix_gtfs(gtfs, max_speed = 40)
#'
#' # duplicated agencies were removed, and the trip is now slower
#' gtfs$agency
#' fixed_gtfs$agency
#'
#' gtfs$stop_times |>
#'   filter(trip_id == "CPTM L07-0") |>
#'   slice_tail(n = 1)
#'
#' fixed_gtfs$stop_times |>
#'   filter(trip_id == "CPTM L07-0") |>
#'   slice_tail(n = 1)
#' \dontshow{data.table::setDTthreads(old_dt_threads)}
#'
#' @export
fix_gtfs <- function(gtfs, max_speed = NULL, unit = "km/h") {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_number(
    max_speed,
    lower = .Machine$double.eps,
    finite = TRUE,
    null.ok = TRUE
  )
  checkmate::assert(
    checkmate::check_string(unit),
    checkmate::check_names(unit, subset.of = c("km/h", "m/s", "mph")),
    combine = "and"
  )

  # remove duplicates first, so the other steps don't see repeated rows

  gtfs_fixed <- remove_duplicates(gtfs)

  # remove unused ids

  gtfs_fixed <- remove_unused_ids(gtfs_fixed)

  # cap max speed. this is done before interpolating, so the interpolated
  # times follow the capped times

  if (!is.null(max_speed)) {
    gtfs_fixed <- set_max_speed(gtfs_fixed, max_speed = max_speed, unit = unit)
  }

  # interpolate stop_times

  gtfs_fixed <- interpolate_stop_times(gtfs_fixed)

  # interpolate shape_dist_traveled. feeds without shapes have nothing to be
  # measured along, so the step is skipped

  if (has_shapes(gtfs_fixed)) {
    gtfs_fixed <- interpolate_shape_dist_traveled(gtfs_fixed)
  }

  return(gtfs_fixed)
}
