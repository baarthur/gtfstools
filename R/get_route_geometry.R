#' Get route geometry
#'
#' Returns the distinct geometries of the trips of each specified `route_id`
#' (its "variants", e.g. each direction and each short-turn service), with the
#' number of trips that follow each of them.
#'
#' @template gtfs
#' @param route_id A character vector including the `route_id`s to have their
#'   geometries generated. If `NULL` (the default), the function generates
#'   geometries for every `route_id` in the GTFS. `route_id`s not listed in
#'   `trips` are ignored, with a warning.
#' @param method A string, either `"shapes"` (the default) or `"euclidean"`,
#'   specifying how the geometry of each trip is generated. Please see
#'   [get_trip_geometry()] for details. If the GTFS object doesn't have a
#'   `shapes` table, or if its `trips` table doesn't have a `shape_id` column,
#'   `"euclidean"` is used instead, with a warning.
#' @param crs The CRS of the resulting object, either as an EPSG code or as an
#'   `crs` object. Defaults to 4326 (WGS 84).
#'
#' @return A `LINESTRING sf` with one row per route and distinct trip
#'   geometry, with the columns `route_id`, `n_trips` (the number of trips of
#'   the route with that geometry) and `geometry`. Rows are ordered by
#'   `route_id` (with `NA` last) and, within each route, by decreasing
#'   `n_trips`.
#'
#' @section Details:
#' The geometry of each trip is generated as in [get_trip_geometry()], so it
#' goes from the trip's first to its last stop, either along its shape or as
#' straight lines between its stops, and the same limitations and warnings
#' apply. The trips of a route whose geometries are identical are then merged
#' into a single row. Trips that visit stops at different points of the same
#' shape thus result in different rows, even if their geometries overlap.
#'
#' Only trips listed both in `trips` and in `stop_times` are considered, so
#' routes without any trip listed in `stop_times` are not included in the
#' output. When a `trip_id` is listed more than once in `trips`, only its first
#' entry is considered. With `route_id = NULL`, trips with a missing
#' `route_id` are grouped together, under an `NA` `route_id`. Trips without
#' any stop with coordinates (and, with `method = "shapes"`, trips that can't
#' be located along a shape) get an empty geometry, as in
#' [get_trip_geometry()], and these trips are counted in an empty-geometry row
#' of their route.
#'
#' @seealso [get_trip_geometry()], [get_shapes_geometry()]
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' route_geometry <- get_route_geometry(gtfs)
#' head(route_geometry)
#'
#' route_geometry <- get_route_geometry(gtfs, route_id = "CPTM L07")
#' route_geometry
#' plot(route_geometry["n_trips"])
#'
#' @export
get_route_geometry <- function(gtfs,
                               route_id = NULL,
                               method = "shapes",
                               crs = 4326) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(route_id, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert(
    checkmate::check_string(method),
    checkmate::check_names(method, subset.of = c("shapes", "euclidean")),
    combine = "and"
  )
  checkmate::assert(
    checkmate::check_number(crs),
    checkmate::check_class(crs, "crs"),
    combine = "or"
  )

  gtfsio::assert_field_class(
    gtfs,
    "trips",
    c("trip_id", "route_id"),
    c("character", "character")
  )

  gtfsio::assert_field_class(
    gtfs,
    "stop_times",
    c("trip_id", "stop_id", "stop_sequence"),
    c("character", "character", "integer")
  )
  gtfsio::assert_field_class(
    gtfs,
    "stops",
    c("stop_id", "stop_lat", "stop_lon"),
    c("character", "numeric", "numeric")
  )

  # shapes are only used if they are available. otherwise get_trip_geometry()
  # warns and falls back to straight lines, which don't depend on the shapes

  uses_shapes <- method == "shapes" && has_shapes(gtfs)
  if (uses_shapes) {
    gtfsio::assert_field_class(gtfs, "trips", "shape_id", "character")
  }

  # only trips listed both in 'stop_times' and in 'trips' are used. each trip
  # is linked to its route (and shape) by its first entry in 'trips', as in
  # get_trip_geometry()

  trip_ids <- unique(gtfs$stop_times$trip_id)
  trip_row <- data.table::chmatch(trip_ids, gtfs$trips$trip_id)
  is_selected <- !is.na(trip_ids) & !is.na(trip_row)

  if (!is.null(route_id)) {
    invalid_route_id <- unique(
      route_id[! route_id %chin% gtfs$trips$route_id]
    )

    if (length(invalid_route_id) > 0) {
      cli::cli_warn(
        paste0(
          "{.file trips} doesn't contain the following route_id{?s}: ",
          "{.val {invalid_route_id}}."
        ),
        class = "gtfstools_invalid_route_id"
      )
    }

    trip_route <- gtfs$trips$route_id[trip_row]
    is_selected <- is_selected & trip_route %chin% route_id
  }

  # trips of the same route that visit the same sequence of stops (along the
  # same shape, if shapes are used) have identical geometries, so only the
  # geometry of the first trip of each group is generated. routes, shapes and
  # stops are described by integer codes, compared by cpp_sequence_pattern_id(),
  # whose ids are numbered in order of first appearance

  # when every trip is selected, its stop_times rows are selected without
  # matching them against the selected trips, which is faster

  st <- prepare_trip_stops(
    gtfs,
    if (all(is_selected)) NULL else trip_ids[is_selected]
  )

  trip_start <- which(!duplicated(st$trip_id))
  n_stops <- diff(c(trip_start, nrow(st) + 1L))
  st_trip_ids <- st$trip_id[trip_start]
  st_trip_row <- data.table::chmatch(st_trip_ids, gtfs$trips$trip_id)
  st_route_id <- gtfs$trips$route_id[st_trip_row]

  route_code <- data.table::chmatch(st_route_id, unique(st_route_id))
  stop_code <- data.table::chmatch(st$stop_id, unique(st$stop_id))
  columns <- list(rep.int(route_code, n_stops), stop_code)

  if (uses_shapes) {
    st_shape_id <- gtfs$trips$shape_id[st_trip_row]
    shape_code <- data.table::chmatch(st_shape_id, unique(st_shape_id))
    columns <- c(columns, list(rep.int(shape_code, n_stops)))
  }

  group <- cpp_sequence_pattern_id(n_stops, columns)
  group_trip <- which(!duplicated(group))
  group_trip_ids <- st_trip_ids[group_trip]

  # the warning about trips not linked to a usable shape would only list the
  # first trip of each group. it's raised again with every trip of the warned
  # groups, as if the geometries of all trips had been generated

  trip_geometry <- withCallingHandlers(
    get_trip_geometry(gtfs, group_trip_ids, method, crs),
    gtfstools_trips_without_shape = function(cnd) {
      warned_group <- data.table::chmatch(cnd$trip_id, group_trip_ids)
      warn_trips_without_shape(st_trip_ids[group %in% warned_group])
      invokeRestart("muffleWarning")
    }
  )

  geometry_row <- data.table::chmatch(group_trip_ids, trip_geometry$trip_id)
  geometry <- sf::st_geometry(trip_geometry)[geometry_row]

  # different groups of the same route may still have identical geometries
  # (e.g. trips with the same first and last stops along the same shape), so
  # they are merged into a single variant. geometries are compared by their
  # exact binary representation

  groups <- data.table::data.table(
    route_code = route_code[group_trip],
    geometry_key = sf::st_as_binary(geometry, hex = TRUE)
  )
  is_variant_first <- !duplicated(groups)
  variants <- groups[is_variant_first]
  group_variant <- variants[groups, on = names(variants), which = TRUE]

  n_trips <- tabulate(group_variant[group], nbins = nrow(variants))

  # variants are ordered by route_id and decreasing number of trips. the
  # ordering is stable, so ties keep the order of first appearance

  variant_order <- data.table::data.table(
    route_id = st_route_id[group_trip][is_variant_first],
    n_trips = n_trips,
    variant = seq_len(nrow(variants))
  )
  data.table::setorderv(
    variant_order,
    c("route_id", "n_trips"),
    order = c(1L, -1L),
    na.last = TRUE
  )

  routes_sf <- data.table::data.table(
    route_id = variant_order$route_id,
    n_trips = variant_order$n_trips,
    geometry = geometry[is_variant_first][variant_order$variant]
  )
  routes_sf <- sf::st_as_sf(routes_sf)

  return(routes_sf)
}
