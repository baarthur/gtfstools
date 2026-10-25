#' Filter GTFS object by `route_id`
#'
#' Filters a GTFS object by `route_id`s, keeping (or dropping) the relevant
#' entries in each file.
#'
#' @template gtfs
#' @param route_id A character vector. The `route_id`s used to filter the data.
#' @param keep A logical. Whether the entries related to the specified
#'   `route_id`s should be kept or dropped (defaults to `TRUE`, which keeps the
#'   entries).
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the filtering
#' process.
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
#' route_ids <- c("6450-51", "CPTM L11")
#'
#' object.size(gtfs)
#'
#' # keeps entries related to passed route_ids
#' smaller_gtfs <- filter_by_route_id(gtfs, route_ids)
#' object.size(smaller_gtfs)
#'
#' # drops entries related to passed route_ids
#' smaller_gtfs <- filter_by_route_id(gtfs, route_ids, keep = FALSE)
#' object.size(smaller_gtfs)
#'
#' @export
filter_by_route_id <- function(gtfs, route_id, keep = TRUE) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(route_id, any.missing = FALSE)
  checkmate::assert_logical(keep, len = 1, any.missing = FALSE)

  `%ffilter%` <- `%chin%`
  if (!keep) `%ffilter%` <- Negate(`%chin%`)

  # 'routes', 'trips', 'fare_rules' and 'transfers' can be filtered using
  # 'route_id' itself, so `%ffilter%` is used. the other files depend on
  # relational associations with 'route_id' that come from these 4 tables, so we
  # use %chin%

  # 'routes' (route_id)

  gtfs <- filter_routes_from_route_id(gtfs, route_id, `%ffilter%`)

  # 'transfers' and 'trips' (route_id)

  gtfs <- filter_transfers_from_route_id(gtfs, route_id, `%ffilter%`)
  gtfs <- filter_trips_from_route_id(gtfs, route_id, `%ffilter%`)

  # 'trips' allows us to filter by 'trip_id', 'service_id' and 'shape_id'

  relevant_trips <- unique(gtfs$trips$trip_id)
  relevant_services <- unique(gtfs$trips$service_id)
  relevant_shapes <- unique(gtfs$trips$shape_id)

  # 'shapes' (shape_id)

  gtfs <- filter_shapes_from_shape_id(gtfs, relevant_shapes, `%chin%`)

  # 'calendar' and 'calendar_dates' (service_id)

  gtfs <- filter_calendar_from_service_id(gtfs, relevant_services, `%chin%`)
  gtfs <- filter_calend_dates_from_service_id(gtfs, relevant_services, `%chin%`)

  # 'stop_times', 'frequencies' and 'transfers' (trip_id)

  gtfs <- filter_transfers_from_trip_id(gtfs, relevant_trips, `%chin%`)
  gtfs <- filter_frequencies_from_trip_id(gtfs, relevant_trips, `%chin%`)
  gtfs <- filter_stop_times_from_trip_id(gtfs, relevant_trips, `%chin%`)

  # 'stops', 'transfers' and 'pathways' (the stops in 'stop_times' and their
  # parents)

  gtfs <- filter_stops_from_stop_times(gtfs)

  # 'fare_rules' (zone_id) and 'levels' (level_id)

  gtfs <- filter_zones_levels_from_stops(gtfs)

  # fare_rules (route_id)

  gtfs <- filter_fare_rules_from_route_id(gtfs, route_id, `%ffilter%`)

  # 'fare_attributes' (fare_id) and 'agency' (agency_id, that comes both from
  # routes and fare_attributes)

  gtfs <- filter_fares_and_agency(gtfs)

  return(gtfs)
}
