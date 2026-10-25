#' Filter GTFS object by `stop_id`
#'
#' Filters a GTFS object by `stop_id`s, keeping (or dropping) relevant entries
#' in each file.
#'
#' @template gtfs
#' @param stop_id A character vector. The `stop_id`s used to filter the data.
#' @param keep A logical. Whether the `stop_times` entries at the specified
#'   `stop_id`s (and the entries related to them in other tables) should be
#'   kept or dropped (defaults to `TRUE`, which keeps the entries). Trips that
#'   still have entries in `stop_times` are kept. To keep or drop entire trips,
#'   pass the `trip_id`s that serve these stops to [filter_by_trip_id()].
#' @param include_children A logical. Whether the filtered output should
#'   keep/drop children stops of those specified in `stop_id`. Defaults to
#'   `TRUE` - i.e. by default children stops are kept if their parents are kept
#'   and dropped if their parents are dropped.
#' @param include_parents A logical. Whether the filtered output should
#'   keep/drop parent stations of those specified in `stop_id`. Defaults to
#'   the same value of `keep` - i.e. by default parent stations are kept both
#'   when their children are kept and dropped, because they can be parents of
#'   multiple stops that are not necessarily dropped, even if their sibling are.
#' @param full_trips Defunct. Deprecated in version 1.3.0 and removed in the
#'   following major version: using it, with any value, raises an error. The
#'   function now always filters by the specified stops. To keep all stops of
#'   the trips that pass through them, subset `stop_times` by `stop_id` and
#'   pass the resulting `trip_id`s to [filter_by_trip_id()].
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the filtering
#'   process.
#'
#' @family filtering functions
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#' stop_ids <- c("18848", "940004157")
#'
#' object.size(gtfs)
#'
#' # keeps the stop_times entries at the specified stop_ids
#' smaller_gtfs <- filter_by_stop_id(gtfs, stop_ids)
#' object.size(smaller_gtfs)
#'
#' # drops the stop_times entries at the specified stop_ids
#' smaller_gtfs <- filter_by_stop_id(gtfs, stop_ids, keep = FALSE)
#' object.size(smaller_gtfs)
#' @export
filter_by_stop_id <- function(gtfs,
                              stop_id,
                              keep = TRUE,
                              include_children = TRUE,
                              include_parents = keep,
                              full_trips) {
  if (!missing(full_trips)) full_trips_defunct_error()

  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(stop_id, any.missing = FALSE)
  checkmate::assert_logical(keep, len = 1, any.missing = FALSE)
  checkmate::assert_logical(include_children, len = 1, any.missing = FALSE)
  checkmate::assert_logical(include_parents, len = 1, any.missing = FALSE)

  # the feed may contain some stop_ids listed in stop_times that are not listed
  # in stops, in which case get_children_stops() and get_parent_station() will
  # throw warnings. we suppress these warnings, as they might feel a bit "out of
  # place" in this function.
  # (although invalid, this may happen when the production of a feed is
  # still on going, for example)

  if (gtfsio::check_field_exists(gtfs, "stops", "parent_station")) {
    if (include_children) {
      suppressWarnings(children <- get_children_stops(gtfs, stop_id))
      stop_id <- unique(c(stop_id, children$stop_id))
    }

    if (include_parents) {
      suppressWarnings(parents <- get_parent_station(gtfs, stop_id))
      stop_id <- unique(c(stop_id, parents$stop_id))
    }
  }

  `%ffilter%` <- `%chin%`
  if (!keep) `%ffilter%` <- Negate(`%chin%`)

  # 'stops', 'stop_times', 'transfers' and 'pathways' can be filtered using
  # 'stop_id' itself, so `%ffilter%` is used. the other files depend on
  # relational associations with 'stop_id' that come from these 3 tables, so we
  # use %chin%

  # 'transfers', 'pathways' and 'stops' (stop_id)

  gtfs <- filter_transfers_from_stop_id(gtfs, stop_id, `%ffilter%`)
  gtfs <- filter_pathways_from_stop_id(gtfs, stop_id, `%ffilter%`)

  # 'stops' (stop_id)

  gtfs <- filter_stops_from_stop_id(gtfs, stop_id, `%ffilter%`)

  # 'fare_rules' (zone_id) and 'levels' (level_id)

  gtfs <- filter_zones_levels_from_stops(gtfs)

  # 'stop_times' (stop_id)

  gtfs <- filter_stop_times_from_stop_id(gtfs, stop_id, `%ffilter%`)

  # 'stop_times' allows us to filter by 'trip_id'

  relevant_trips <- unique(gtfs$stop_times$trip_id)

  # 'trips', 'frequencies' and 'transfers' (trip_id)

  gtfs <- filter_trips_from_trip_id(gtfs, relevant_trips, `%chin%`)
  gtfs <- filter_frequencies_from_trip_id(gtfs, relevant_trips, `%chin%`)
  gtfs <- filter_transfers_from_trip_id(gtfs, relevant_trips, `%chin%`)

  # 'trips' allows us to filter by 'route_id', 'service_id' and 'shape_id'

  relevant_routes <- unique(gtfs$trips$route_id)
  relevant_services <- unique(gtfs$trips$service_id)
  relevant_shapes <- unique(gtfs$trips$shape_id)

  # 'shapes' (shape_id)

  gtfs <- filter_shapes_from_shape_id(gtfs, relevant_shapes, `%chin%`)

  # 'calendar' and 'calendar_dates' (service_id)

  gtfs <- filter_calendar_from_service_id(gtfs, relevant_services, `%chin%`)
  gtfs <- filter_calend_dates_from_service_id(gtfs, relevant_services, `%chin%`)

  # 'routes', 'fare_rules' and 'transfers' (route_id)

  gtfs <- filter_transfers_from_route_id(gtfs, relevant_routes, `%chin%`)
  gtfs <- filter_routes_from_route_id(gtfs, relevant_routes, `%chin%`)
  gtfs <- filter_fare_rules_from_route_id(gtfs, relevant_routes, `%chin%`)

  # 'fare_attributes' (fare_id) and 'agency' (agency_id, that comes both from
  # routes and fare_attributes)

  gtfs <- filter_fares_and_agency(gtfs)

  return(gtfs)
}

full_trips_defunct_error <- function() {
  cli::cli_abort(
    class = "gtfstools_defunct_full_trips_error",
    call = parent.frame(),
    message = c(
      paste0(
        "The {.arg full_trips} argument of {.fn filter_by_stop_id} was ",
        "deprecated in gtfstools 1.3.0 and is now defunct."
      ),
      "i" = paste0(
        "{.fn filter_by_stop_id} now always filters by the specified stops, ",
        "as {.code full_trips = FALSE} did. Please remove {.arg full_trips} ",
        "from your call."
      ),
      "i" = paste0(
        "To keep all stops of the trips that pass through the specified ",
        "stops (the old {.code full_trips = TRUE} behavior), subset the ",
        "{.code stop_times} table by {.code stop_id} and pass the resulting ",
        "{.code trip_id}s to {.fn filter_by_trip_id}."
      )
    )
  )
}
