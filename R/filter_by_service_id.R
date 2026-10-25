#' Filter GTFS object by `service_id`
#'
#' Filters a GTFS object by `service_id`s, keeping (or dropping) the relevant
#' entries in each file.
#'
#' @template gtfs
#' @param service_id A character vector. The `service_id`s used to filter the
#' data.
#' @param keep A logical. Whether the entries related to the specified
#' `service_id`s should be kept or dropped (defaults to `TRUE`, which keeps the
#' entries).
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
#' service_ids <- c("USD", "U__")
#'
#' object.size(gtfs)
#'
#' # keeps entries related to the specified service_ids
#' smaller_gtfs <- filter_by_service_id(gtfs, service_ids)
#' object.size(smaller_gtfs)
#'
#' # drops entries related to the specified service_ids
#' smaller_gtfs <- filter_by_service_id(gtfs, service_ids, keep = FALSE)
#' object.size(smaller_gtfs)
#'
#' @export
filter_by_service_id <- function(gtfs, service_id, keep = TRUE) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(service_id, any.missing = FALSE)
  checkmate::assert_logical(keep, len = 1, any.missing = FALSE)

  `%ffilter%` <- `%chin%`
  if (!keep) `%ffilter%` <- Negate(`%chin%`)

  # 'calendar', 'calendar_dates' and 'trips' can be filtered using 'service_id'
  # itself, so `%ffilter%` is used. the other files depend on relational
  # associations with 'service_id' that come from these 3 files, so we use
  # %chin%

  # 'calendar', 'calendar_dates' and 'trips' (service_id)

  gtfs <- filter_calendar_from_service_id(gtfs, service_id, `%ffilter%`)
  gtfs <- filter_calend_dates_from_service_id(gtfs, service_id, `%ffilter%`)
  gtfs <- filter_trips_from_service_id(gtfs, service_id, `%ffilter%`)

  # 'trips' allows us to filter by 'route_id', 'shape_id' and 'trip_id'

  relevant_routes <- unique(gtfs$trips$route_id)
  relevant_shapes <- unique(gtfs$trips$shape_id)
  relevant_trips <- unique(gtfs$trips$trip_id)

  # 'shapes' (shape_id)

  gtfs <- filter_shapes_from_shape_id(gtfs, relevant_shapes, `%chin%`)

  # 'routes', 'fare_rules' and 'transfers' (route_id)

  gtfs <- filter_routes_from_route_id(gtfs, relevant_routes, `%chin%`)
  gtfs <- filter_fare_rules_from_route_id(gtfs, relevant_routes, `%chin%`)
  gtfs <- filter_transfers_from_route_id(gtfs, relevant_routes, `%chin%`)

  # 'frequencies', 'stop_times' and 'transfers' (trip_id)

  gtfs <- filter_transfers_from_trip_id(gtfs, relevant_trips, `%chin%`)
  gtfs <- filter_frequencies_from_trip_id(gtfs, relevant_trips, `%chin%`)
  gtfs <- filter_stop_times_from_trip_id(gtfs, relevant_trips, `%chin%`)

  # 'stops', 'transfers' and 'pathways' (the stops in 'stop_times' and their
  # parents)

  gtfs <- filter_stops_from_stop_times(gtfs)

  # 'fare_rules' (zone_id) and 'levels' (level_id)

  gtfs <- filter_zones_levels_from_stops(gtfs)

  # 'fare_attributes' (fare_id) and 'agency' (agency_id, that comes both from
  # routes and fare_attributes)

  gtfs <- filter_fares_and_agency(gtfs)

  return(gtfs)
}
