#' Get trip geometry
#'
#' Returns the geometry of each specified `trip_id`, from its first to its last
#' stop, either along the trip's shape or as straight lines between stops.
#'
#' @template gtfs
#' @param trip_id A character vector including the `trip_id`s to have their
#'   geometries generated. If `NULL` (the default), the function generates
#'   geometries for every `trip_id` in the GTFS.
#' @param method A string, either `"shapes"` (the default) or `"euclidean"`.
#'   `"shapes"` returns the part of the trip's shape, described in the `shapes`
#'   table, between its first and last stops, while `"euclidean"` links the
#'   consecutive stops of the trip along straight lines. If the GTFS object
#'   doesn't have a `shapes` table, or if its `trips` table doesn't have a
#'   `shape_id` column, `"euclidean"` is used instead, with a warning.
#' @param crs The CRS of the resulting object, either as an EPSG code or as an
#'   `crs` object. Defaults to 4326 (WGS 84).
#' @param sort_sequence Deprecated; do not supply. Timetables and shapes are
#'   now always sorted by `stop_sequence` and `shape_pt_sequence`. Passing
#'   `TRUE` raises a warning, and passing `FALSE` raises an error.
#' @param file Deprecated. Use `method` instead (`file = "stop_times"`
#'   corresponds to `method = "euclidean"`).
#'
#' @return A `LINESTRING sf` with the `trip_id` and the `geometry` of each trip
#'   listed in `stop_times`.
#'
#' @section Details:
#' With `method = "shapes"`, the first and last stops of each trip are
#' projected onto its shape, as in [get_trip_length()], and the geometry is the
#' part of the shape between them. Thus, its length matches the length
#' returned by [get_trip_length()] (when this length is not `NA`, see below),
#' and the limitations described there apply. Trips whose stops are all
#' located at the same point of the shape get a zero-length geometry.
#' Use [get_shapes_geometry()] for the geometry of entire shapes.
#'
#' With `method = "euclidean"`, the geometry links the consecutive stops of
#' each trip along straight lines (stops' coordinates are retrieved from the
#' `stops` table), so its resolution tends to be much lower than that of the
#' geometry generated from the shapes. Trips with a single stop get a
#' single-point geometry.
#'
#' Both methods ignore stops with missing coordinates, or not listed in
#' `stops` (for which [get_trip_length()] returns `NA` lengths). Trips
#' without any stop with coordinates get an empty geometry, as do, with
#' `method = "shapes"`, trips with fewer than two stops with coordinates or
#' not linked to a shape with at least two distinct points (the
#' latter with a warning). Geometries along shapes that cross the antimeridian
#' are not handled correctly.
#'
#' @seealso [get_trip_length()], [get_shapes_geometry()]
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
#' # geometry along the shape from the first to the last stop of each trip
#' trip_geometry <- get_trip_geometry(gtfs)
#' head(trip_geometry)
#'
#' # straight lines between consecutive stops
#' trip_ids <- c("CPTM L07-0", "2002-10-0")
#' straight_geometry <- get_trip_geometry(
#'   gtfs,
#'   trip_id = trip_ids,
#'   method = "euclidean"
#' )
#' straight_geometry
#' plot(straight_geometry["trip_id"])
#'
#' @export
get_trip_geometry <- function(gtfs,
                              trip_id = NULL,
                              method = "shapes",
                              crs = 4326,
                              sort_sequence,
                              file = NULL) {
  if (!missing(sort_sequence)) {
    check_deprecated_sort_sequence(
      sort_sequence,
      "get_trip_geometry",
      c("stop_sequence", "shape_pt_sequence")
    )
  }

  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(trip_id, null.ok = TRUE, any.missing = FALSE)
  if (!is.null(file)) {
    method <- map_deprecated_file(
      file,
      "get_trip_geometry",
      details = paste0(
        "Geometries are now trimmed to the first and last stop of each trip. ",
        "Use {.fun get_shapes_geometry} for the geometry of the entire shapes."
      )
    )
  }
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

  assert_trip_stops_fields(gtfs)

  # geometries along the shapes require a 'shapes' table and a 'shape_id'
  # column in 'trips'. if any of them is missing, fall back to straight lines

  method <- resolve_shapes_method(
    gtfs,
    method,
    fallback = paste0(
      "Generating {.val euclidean} geometries (straight lines between ",
      "consecutive stops) instead of geometries along the shapes."
    )
  )

  if (method == "shapes") assert_shapes_fields(gtfs)

  st <- prepare_trip_stops(gtfs, trip_id)

  trip_ids <- unique(st$trip_id)
  trip_index <- data.table::chmatch(st$trip_id, trip_ids)

  # geometries are only built for the trips that have one. 'built_trip' lists
  # these trips and 'built_idx' the built geometry of each of them, so that a
  # geometry shared by many trips is built and transformed only once

  if (method == "euclidean") {
    has_coords <- !is.na(st$stop_lat) & !is.na(st$stop_lon)
    built_trip <- unique(trip_index[has_coords])
    built_idx <- seq_along(built_trip)

    # 'st' is a new table, so the trip index can be added to it by reference.
    # it's only subset when some stops don't have coordinates (usually none)

    data.table::set(st, j = "trip_index", value = trip_index)
    stop_points <- if (all(has_coords)) st else st[has_coords]
    built <- build_linestrings(
      stop_points,
      "trip_index",
      "stop_lon",
      "stop_lat"
    )
  } else {
    position <- locate_stops_along_shapes(gtfs, st, st$stop_lat, st$stop_lon)
    shape_points <- attr(position, "shape_points")

    # each trip is cut from its first to its last located stop. trips sharing
    # the same shape and the same cut (e.g. trips of the same pattern) share
    # the same geometry, so each cut is built only once

    is_located <- !is.na(position)
    located_trip <- trip_index[is_located]
    located_position <- position[is_located]
    is_first <- !duplicated(located_trip)
    is_last <- !duplicated(located_trip, fromLast = TRUE)

    built_trip <- located_trip[is_first]
    trip_cuts <- data.table::data.table(
      shape_id = shape_points$trip_shape_id[built_trip],
      from = located_position[is_first],
      to = located_position[is_last]
    )
    cuts <- unique(trip_cuts)
    built_idx <- cuts[trip_cuts, on = names(cuts), which = TRUE]

    built <- build_shape_cuts(cuts, shape_points)
  }

  if (crs != 4326 && crs != sf::st_crs(4326)) {
    built <- sf::st_transform(built, crs)
  }

  # trips without a built geometry get an empty one, appended after the built
  # geometries, so that the geometry of every trip is selected at once

  geometry_idx <- rep(length(built) + 1L, length(trip_ids))
  geometry_idx[built_trip] <- built_idx
  geometry <- sf::st_sfc(
    c(unclass(built), list(sf::st_linestring()))[geometry_idx],
    crs = sf::st_crs(built)
  )
  if (length(trip_ids) == 0) class(geometry)[1] <- "sfc_LINESTRING"

  trips_sf <- data.table::data.table(trip_id = trip_ids, geometry = geometry)
  trips_sf <- sf::st_as_sf(trips_sf)

  return(trips_sf)
}
