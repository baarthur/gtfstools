# the exported name is longer than lintr's 30-character limit
# nolint start: object_length_linter.

#' Interpolate the distances travelled along shapes
#'
#' Calculates the distance, in meters, from the start of each shape to each
#' stop and to each shape point, and writes it to the `shape_dist_traveled`
#' columns of `stop_times` and `shapes`, replacing any existing values.
#'
#' @template gtfs
#'
#' @return A GTFS object whose `stop_times` and `shapes` have a
#'   `shape_dist_traveled` column, in meters. The order of the rows and the
#'   other columns are kept. The given GTFS object is not modified.
#'
#' @section Details:
#' Each stop is projected onto the closest point of its trip's shape, with
#' stops advancing along the shape in `stop_sequence` order, as in
#' [get_trip_length()] (see there for the limitations of this method).
#' Distances are great-circle distances on the sphere used by `{s2}`.
#'
#' Existing `shape_dist_traveled` columns are entirely replaced, since their
#' values may be in any unit, so existing values are lost even where new ones
#' can't be calculated. Stops of trips not linked to a shape with at
#' least two distinct points (with a warning), stops without coordinates and
#' stops of trips with fewer than two stops with coordinates get `NA`, as do
#' shape points with missing coordinates. Unlike in [get_shape_length()],
#' these points are skipped, and the other points of the shape are still
#' measured.
#'
#' @seealso [get_trip_length()], [get_shape_length()]
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' gtfs <- interpolate_shape_dist_traveled(gtfs)
#' head(gtfs$stop_times[, c("trip_id", "stop_id", "shape_dist_traveled")])
#' head(gtfs$shapes)
#'
#' @export
interpolate_shape_dist_traveled <- function(gtfs) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)

  assert_trip_stops_fields(gtfs)

  if (!has_shapes(gtfs)) {
    cli::cli_abort(
      paste0(
        "The GTFS object doesn't have a {.file shapes} table, or its ",
        "{.file trips} table doesn't have a {.field shape_id} column."
      ),
      class = "gtfstools_shapes_unavailable"
    )
  }

  assert_shapes_fields(gtfs)

  # the position of each stop along its trip's shape, written back to the
  # stop_times rows they came from. rows without a trip_id are not located

  st <- prepare_trip_stops(gtfs, NULL, with_row = TRUE)

  stop_distance <- rep(NA_real_, nrow(gtfs$stop_times))

  if (nrow(st) > 0) {
    position <- locate_stops_along_shapes(gtfs, st, st$stop_lat, st$stop_lon)
    stop_distance[st$row] <- as.numeric(position)
  }

  # the distance from the start of each shape to each of its points. as in
  # locate_stops_along_shapes(), points without coordinates are skipped and
  # the others are sorted by shape_pt_sequence (a stable sort, so ties are
  # broken in the same way), so stops and points are measured alike. every
  # shape is measured, even if no trip uses it

  is_valid_point <- !is.na(gtfs$shapes$shape_id) &
    !is.na(gtfs$shapes$shape_pt_lat) &
    !is.na(gtfs$shapes$shape_pt_lon)

  shapes <- data.table::data.table(
    row = which(is_valid_point),
    shape_id = gtfs$shapes$shape_id[is_valid_point],
    shape_pt_sequence = gtfs$shapes$shape_pt_sequence[is_valid_point],
    shape_pt_lat = gtfs$shapes$shape_pt_lat[is_valid_point],
    shape_pt_lon = gtfs$shapes$shape_pt_lon[is_valid_point]
  )
  data.table::setorderv(shapes, c("shape_id", "shape_pt_sequence"))

  distance <- shape_point_distances(
    shapes$shape_id,
    shapes$shape_pt_lat,
    shapes$shape_pt_lon
  )
  is_first_point <- !duplicated(shapes$shape_id)

  # the points of each shape are contiguous, so the cumulative distances are
  # calculated per group of consecutive points

  cumulative_distance <- unlist(
    lapply(split(distance, cumsum(is_first_point)), cumsum),
    use.names = FALSE
  )

  shape_distance <- rep(NA_real_, nrow(gtfs$shapes))
  shape_distance[shapes$row] <- cumulative_distance

  # whole columns are replaced, so existing columns of other classes (e.g.
  # integer) become numeric. the tables are copied first, so the given gtfs is
  # not modified

  stop_times <- data.table::copy(gtfs$stop_times)
  data.table::set(stop_times, j = "shape_dist_traveled", value = stop_distance)

  shapes_table <- data.table::copy(gtfs$shapes)
  data.table::set(
    shapes_table,
    j = "shape_dist_traveled",
    value = shape_distance
  )

  gtfs$stop_times <- stop_times
  gtfs$shapes <- shapes_table

  return(gtfs)
}

# nolint end
