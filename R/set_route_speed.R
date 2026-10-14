#' Set route average speed
#'
#' Sets the average speed of the trips of each specified `route_id`, or of a
#' segment of them, by changing the `arrival_time` and `departure_time` columns
#' in `stop_times`.
#'
#' @template gtfs
#' @param route_id A character vector including the `route_id`s to have the
#'   average speed of their trips set.
#' @param speed A numeric representing the speed to be set. Its length must
#'   either equal 1, in which case the value is recycled for all `route_id`s,
#'   or equal `route_id`'s length. Must be positive and finite.
#' @inheritParams set_trip_speed
#'
#' @return If `by_reference` is set to `FALSE`, returns a GTFS object with the
#'   time columns of its `stop_times` adjusted. Else, returns a GTFS object
#'   invisibly (note that in this case the original GTFS object is altered).
#'
#' @section Details:
#' Every trip of each route, as listed in `trips`, gets the route's speed, as
#' in [set_trip_speed()], whose details also apply here: the duration of each
#' trip's segment (by default, the whole trip) is set to its length divided by
#' `speed`, the times at stops inside the segment are set to `""`, and `from`
#' and `to` restrict the changes to the trips that depart from the segment's
#' first stop within the time of day. Warnings about trips that can't be
#' changed refer to their `trip_id`s. Trips without `stop_times` entries are
#' ignored.
#'
#' @seealso [set_trip_speed()], [get_trip_speed()], [get_route_speed()]
#'
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#' }
#' library(dplyr)
#'
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' route_trips <- gtfs$trips |>
#'   filter(route_id == "CPTM L07") |>
#'   pull(trip_id)
#' get_trip_speed(gtfs, route_trips)
#'
#' new_speed_gtfs <- set_route_speed(gtfs, route_id = "CPTM L07", speed = 50)
#' get_trip_speed(new_speed_gtfs, route_trips)
#'
#' # set the speed only of the trips that depart within a time of day
#' poa_path <- system.file("extdata/poa_gtfs.zip", package = "gtfstools")
#' poa_gtfs <- read_gtfs(poa_path)
#'
#' poa_new_speed <- set_route_speed(
#'   poa_gtfs,
#'   route_id = "T2",
#'   speed = 25,
#'   from = "07:00:00",
#'   to = "09:00:00"
#' )
#'
#' # the arrival at the last stop of a trip that departs at 07:02 changes
#' poa_gtfs$stop_times |>
#'   filter(trip_id == "T2-1@1#702") |>
#'   slice_tail(n = 1)
#'
#' poa_new_speed$stop_times |>
#'   filter(trip_id == "T2-1@1#702") |>
#'   slice_tail(n = 1)
#' \dontshow{data.table::setDTthreads(old_dt_threads)}
#'
#' @export
set_route_speed <- function(
  gtfs,
  route_id,
  speed,
  unit = "km/h",
  first_stop = NULL,
  last_stop = NULL,
  from = NULL,
  to = NULL,
  by_reference = FALSE
) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(route_id, any.missing = FALSE)
  gtfsio::assert_field_class(
    gtfs,
    "trips",
    c("trip_id", "route_id"),
    c("character", "character")
  )

  # select the trips of the given routes that have stop times, so that
  # set_trip_speed() doesn't warn about trips without them

  ids <- route_id
  trips <- unique(gtfs$trips[route_id %chin% ids, .(trip_id, route_id)])
  if (is.character(gtfs$stop_times$trip_id)) {
    trips <- trips[trip_id %chin% gtfs$stop_times$trip_id]
  }

  # one speed per route becomes one speed per trip. speed and the other
  # arguments are validated by set_trip_speed()

  trip_speed <- speed
  if (length(speed) > 1 && length(speed) == length(route_id)) {
    trip_speed <- speed[match(trips$route_id, route_id)]
  }

  result <- set_trip_speed(
    gtfs,
    trip_id = trips$trip_id,
    speed = trip_speed,
    unit = unit,
    first_stop = first_stop,
    last_stop = last_stop,
    from = from,
    to = to,
    by_reference = by_reference
  )

  # warn about invalid routes after set_trip_speed() has validated the other
  # arguments, so that errors come first

  invalid_route_id <- unique(route_id[! route_id %chin% gtfs$trips$route_id])

  if (!identical(invalid_route_id, character(0))) {
    cli::cli_warn(
      paste0(
        "{.file trips} doesn't contain the following route_id{?s}: ",
        "{.val {invalid_route_id}}."
      ),
      class = "gtfstools_invalid_route_id"
    )
  }

  if (by_reference) {
    return(invisible(result))
  }

  return(result)
}
