#' Check whether a GTFS object has shapes linked to its trips
#'
#' @template gtfs
#'
#' @return `TRUE` if the GTFS object has a `shapes` table and a `shape_id`
#'   column in its `trips` table, `FALSE` otherwise.
#'
#' @keywords internal
has_shapes <- function(gtfs) {
  has_table <- gtfsio::check_file_exists(gtfs, "shapes")
  has_field <- gtfsio::check_field_exists(gtfs, "trips", "shape_id")

  return(isTRUE(has_table) && isTRUE(has_field))
}



#' Assert the stop_times and stops fields needed to work with trip stops
#'
#' Asserts that `stop_times` has `trip_id`, `stop_id` and `stop_sequence`
#' and that `stops` has `stop_id`, `stop_lat` and `stop_lon`, with the
#' expected classes. `stop_times` is checked first, so the functions that
#' share these checks report errors in the same order.
#'
#' @param gtfs A GTFS object.
#'
#' @return Invisibly, `TRUE`.
#'
#' @keywords internal
assert_trip_stops_fields <- function(gtfs) {
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
  return(invisible(TRUE))
}



#' Assert the shapes (and trips) fields needed to work with shapes
#'
#' Asserts that `shapes` has `shape_id`, `shape_pt_lat`, `shape_pt_lon` and
#' `shape_pt_sequence`, with the expected classes, and, if `trips = TRUE`,
#' first that `trips` has `trip_id` and `shape_id`.
#'
#' @param gtfs A GTFS object.
#' @param trips Whether to also check the `trips` fields.
#'
#' @return Invisibly, `TRUE`.
#'
#' @keywords internal
assert_shapes_fields <- function(gtfs, trips = TRUE) {
  if (trips) {
    gtfsio::assert_field_class(
      gtfs,
      "trips",
      c("trip_id", "shape_id"),
      c("character", "character")
    )
  }
  gtfsio::assert_field_class(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"),
    c("character", "numeric", "numeric", "integer")
  )
  return(invisible(TRUE))
}



#' Fall back to the euclidean method when a GTFS object has no usable shapes
#'
#' If `method` is `"shapes"` but the GTFS object has no `shapes` table or its
#' `trips` table has no `shape_id` column (see [has_shapes()]), raises a
#' warning of class `gtfstools_shapes_unavailable` and returns
#' `"euclidean"`. Otherwise returns `method` unchanged.
#'
#' @param gtfs A GTFS object.
#' @param method A string, `"shapes"` or `"euclidean"`.
#' @param fallback A string. The "i" bullet of the warning, which says what
#'   is calculated instead. It is interpolated by cli in the helper, so it
#'   may use cli markup but not the caller's variables.
#'
#' @return A string, the method to use.
#'
#' @keywords internal
resolve_shapes_method <- function(gtfs, method, fallback) {
  if (method == "shapes" && !has_shapes(gtfs)) {
    cli::cli_warn(
      c(
        paste0(
          "The GTFS object doesn't have a {.file shapes} table, or its ",
          "{.file trips} table doesn't have a {.field shape_id} column."
        ),
        "i" = fallback
      ),
      class = "gtfstools_shapes_unavailable"
    )
    method <- "euclidean"
  }
  return(method)
}



#' Select and prepare the stop_times of the relevant trips
#'
#' @param gtfs A GTFS object.
#' @param trip_id The `trip_id`s whose stop_times should be selected, or `NULL`
#'   to select every trip.
#' @param with_departure Whether to also select the `departure_time`.
#' @param with_row Whether to also return the `row` of each selected stop_time
#'   in `gtfs$stop_times`.
#'
#' @return A new `data.table` with the `trip_id`, `stop_id` and
#'   `stop_sequence` (and, if `with_departure` is `TRUE`, the
#'   `departure_time`, and if `with_row` is `TRUE`, the `row`) of the selected
#'   stop_times, sorted by `trip_id` and `stop_sequence`, plus the `stop_lat`
#'   and `stop_lon` of each stop.
#'
#' @keywords internal
prepare_trip_stops <- function(gtfs,
                               trip_id,
                               with_departure = FALSE,
                               with_row = FALSE) {
  stop_times_cols <- c("trip_id", "stop_id", "stop_sequence")
  if (with_departure) stop_times_cols <- c(stop_times_cols, "departure_time")

  # select the relevant stop_times rows into a new table, so the tables of the
  # given gtfs are never modified. the selection index is created outside of
  # `[` so that {data.table} doesn't add an index to the original table

  if (!is.null(trip_id)) {
    warn_missing_ids(trip_id, gtfs$stop_times$trip_id, "stop_times", "trip_id")
    is_relevant <- gtfs$stop_times$trip_id %chin% trip_id
  } else {
    # rows without a trip_id (invalid GTFS, read as NA or "") can't belong to
    # any trip

    is_relevant <- !is.na(gtfs$stop_times$trip_id) &
      gtfs$stop_times$trip_id != ""
  }

  st <- gtfs$stop_times[is_relevant, .SD, .SDcols = stop_times_cols]
  if (with_row) data.table::set(st, j = "row", value = which(is_relevant))

  # the calculations below are vectorised over all rows, which requires the
  # rows of each trip to be contiguous, as sorting makes them

  data.table::setorderv(st, c("trip_id", "stop_sequence"))

  # stops' coordinates are looked up by their first match, instead of joined, so
  # duplicated stop_ids in 'stops' can't duplicate stop_times rows

  stop_idx <- data.table::chmatch(st$stop_id, gtfs$stops$stop_id)
  st[
    ,
    `:=`(
      stop_lat = gtfs$stops$stop_lat[stop_idx],
      stop_lon = gtfs$stops$stop_lon[stop_idx]
    )
  ]

  return(st[])
}



#' Select and sort the shape points
#'
#' @param gtfs A GTFS object.
#' @param shape_id The `shape_id`s whose points should be selected, or `NULL`
#'   to select every shape.
#'
#' @return A new `data.table` with the `shape_id`, `shape_pt_lat`,
#'   `shape_pt_lon` and `shape_pt_sequence` of the selected points, sorted by
#'   `shape_id` and `shape_pt_sequence`. Points with missing coordinates are
#'   kept; points with a missing `shape_id` are kept when every shape is
#'   selected (the callers never request a missing `shape_id`).
#'
#' @keywords internal
prepare_shape_points <- function(gtfs, shape_id = NULL) {
  shapes_cols <- c(
    "shape_id",
    "shape_pt_lat",
    "shape_pt_lon",
    "shape_pt_sequence"
  )

  # the selected columns are copied into a new table. the row selection is an
  # index created outside of `[`, so {data.table} doesn't add an index to the
  # given table; selecting every row through an index would be slower than
  # selecting the columns alone

  if (!is.null(shape_id)) {
    rows <- which(gtfs$shapes$shape_id %chin% shape_id)
    shapes <- gtfs$shapes[rows, .SD, .SDcols = shapes_cols]
  } else {
    shapes <- gtfs$shapes[, shapes_cols, with = FALSE]
  }
  data.table::setorderv(shapes, c("shape_id", "shape_pt_sequence"))

  return(shapes[])
}



#' Distance from each shape point to the previous one
#'
#' @param shape_id,lat,lon The `shape_id` and coordinates of the shape points,
#'   sorted by shape and sequence, so that the points of each shape are
#'   contiguous.
#'
#' @return A numeric vector of distances in meters, on the same sphere used by
#'   `{s2}`. The first point of each shape is 0 meters away from its
#'   "previous" point, whatever its coordinates. For any other point, a
#'   missing coordinate in it or in the previous point gives `NA`.
#'
#' @keywords internal
shape_point_distances <- function(shape_id, lat, lon) {
  distance <- rcpp_distance_haversine(
    data.table::shift(lat),
    data.table::shift(lon),
    lat,
    lon
  )
  distance[!duplicated(shape_id)] <- 0
  return(distance)
}



#' Locate stops along their trips' shapes
#'
#' @param gtfs A GTFS object.
#' @param st A `data.table` with the stop_times of the relevant trips, in which
#'   the rows of each trip are contiguous.
#' @param stop_lat,stop_lon The coordinates of the stops listed in `st`.
#'
#' @return A numeric vector with the position of each stop along its trip's
#'   shape, in meters from the start of the shape. Stops whose trip is not
#'   linked to a usable shape, or that don't have coordinates, get `NA`. Its
#'   `"shape_points"` attribute is a list with the shape points on which the
#'   positions were measured (`shapes`, without missing coordinates and
#'   sorted by `shape_pt_sequence`), the rows of each shape in `shapes`
#'   (`rows`) and the `shape_id` of each trip (`trip_shape_id`), in order of
#'   appearance in `st`.
#'
#' @keywords internal
locate_stops_along_shapes <- function(gtfs,
                                      st,
                                      stop_lat,
                                      stop_lon) {
  # each trip's first and last rows and its shape, looked up by first match

  trip_start <- which(!duplicated(st$trip_id))
  trip_end <- c(trip_start[-1L] - 1L, nrow(st))
  n_stops <- trip_end - trip_start + 1L

  trip_shape_id <- gtfs$trips$shape_id[
    data.table::chmatch(st$trip_id[trip_start], gtfs$trips$trip_id)
  ]

  # usable shape points, split by shape. shapes with fewer than two distinct
  # points can't be used (see get_usable_shapes())

  shapes_cols <- c(
    "shape_id",
    "shape_pt_lat",
    "shape_pt_lon",
    "shape_pt_sequence"
  )

  is_relevant_point <- gtfs$shapes$shape_id %chin% trip_shape_id &
    !is.na(gtfs$shapes$shape_pt_lat) &
    !is.na(gtfs$shapes$shape_pt_lon)
  shapes <- gtfs$shapes[is_relevant_point, .SD, .SDcols = shapes_cols]

  # shapes are looked up by their shape_ids as names below, which can't match
  # a blank shape_id. a blank shape_id only in 'trips' just means that the
  # trip isn't linked to a shape, but one also used in 'shapes' is ambiguous

  if ("" %chin% shapes$shape_id) {
    n_blank_trips <- sum(trip_shape_id %chin% "")
    cli::cli_abort(
      c(
        paste0(
          "{.file shapes} has points with a blank {.field shape_id}, used ",
          "by {n_blank_trips} trip{?s} in {.file trips}."
        ),
        "i" = "Give these shapes a non-blank {.field shape_id}."
      ),
      class = "gtfstools_blank_shape_id"
    )
  }

  data.table::setorderv(shapes, c("shape_id", "shape_pt_sequence"))

  shape_rows <- split(seq_len(nrow(shapes)), shapes$shape_id)
  usable_shapes <- get_usable_shapes(shapes)
  has_usable_shape <- trip_shape_id %chin% usable_shapes

  trips_without_shape <- st$trip_id[trip_start][!has_usable_shape & n_stops > 1]

  if (length(trips_without_shape) > 0) {
    warn_trips_without_shape(trips_without_shape)
  }

  # trips sharing the same shape and the same sequence of stops (a pattern)
  # have the same stop positions along the shape, so each pattern is located
  # only once. shapes and stops are described by integer codes, which are
  # compared by cpp_sequence_pattern_id()

  shape_code <- data.table::chmatch(trip_shape_id, unique(trip_shape_id))
  stop_code <- data.table::chmatch(st$stop_id, unique(st$stop_id))

  pattern_id <- cpp_sequence_pattern_id(
    n_stops,
    list(rep.int(shape_code, n_stops), stop_code)
  )
  pattern_trip <- which(!duplicated(pattern_id))

  # the stops of each pattern are those of its first trip. a pattern is located
  # only if its shape is usable and at least two of its stops have
  # coordinates. the others get NA positions, as do stops without coordinates.
  # all patterns are located at once, in parallel, in C++

  pattern_n_stops <- n_stops[pattern_trip]
  rows <- sequence(pattern_n_stops, from = trip_start[pattern_trip])
  row_pattern <- rep.int(seq_along(pattern_trip), pattern_n_stops)

  lat <- stop_lat[rows]
  lon <- stop_lon[rows]
  is_valid <- !is.na(lat) & !is.na(lon)
  n_valid <- tabulate(row_pattern[is_valid], nbins = length(pattern_trip))
  is_located <- has_usable_shape[pattern_trip] & n_valid >= 2
  to_locate <- is_valid & is_located[row_pattern]

  position <- rep(NA_real_, length(rows))

  if (any(to_locate)) {
    located_shape_id <- trip_shape_id[pattern_trip[is_located]]
    used_shapes <- unique(located_shape_id)
    shape_point <- unlist(shape_rows[used_shapes], use.names = FALSE)

    position[to_locate] <- cpp_locate_stops_on_shapes(
      shapes$shape_pt_lat[shape_point],
      shapes$shape_pt_lon[shape_point],
      lengths(shape_rows[used_shapes], use.names = FALSE),
      match(located_shape_id, used_shapes),
      lat[to_locate],
      lon[to_locate],
      n_valid[is_located],
      prune = TRUE
    )
  }

  pattern_positions <- split(position, row_pattern)

  # the positions of each trip are those of its pattern. since the rows of each
  # trip are contiguous, they can simply be concatenated in trip order

  position <- as.numeric(
    unlist(pattern_positions[pattern_id], use.names = FALSE)
  )
  attr(position, "shape_points") <- list(
    shapes = shapes,
    rows = shape_rows,
    trip_shape_id = trip_shape_id
  )

  return(position)
}



#' Warn about trips not linked to a usable shape
#'
#' @param trip_ids The `trip_id`s of the trips not linked to a shape with at
#'   least two distinct points.
#'
#' @return Invisibly returns `NULL`. Raises a warning of class
#'   `"gtfstools_trips_without_shape"`, whose `trip_id` field holds the given
#'   `trip_id`s.
#'
#' @keywords internal
warn_trips_without_shape <- function(trip_ids) {
  cli::cli_warn(
    paste0(
      "{length(trip_ids)} trip{?s} {?is/are} not linked to a ",
      "shape with at least two distinct points: ",
      "{.val {trip_ids}}."
    ),
    class = "gtfstools_trips_without_shape",
    trip_id = trip_ids
  )

  return(invisible(NULL))
}



#' Get the usable shapes of a shapes table
#'
#' Returns the shapes with at least two distinct points with coordinates. This
#' is the single definition of a usable shape, used by both
#' `convert_stops_to_shapes()` and `locate_stops_along_shapes()`.
#'
#' @param shapes A GTFS `shapes` table.
#'
#' @return A character vector with the `shape_id`s of the usable shapes.
#'
#' @keywords internal
get_usable_shapes <- function(shapes) {
  # distinct points are flagged instead of selected into a new table, to avoid
  # copying the shapes. a point with coordinates can only duplicate another
  # point with coordinates, so points without them don't affect the flags

  has_coords <- !is.na(shapes$shape_pt_lat) & !is.na(shapes$shape_pt_lon)
  is_distinct_point <- has_coords & !duplicated(
    shapes,
    by = c("shape_id", "shape_pt_lat", "shape_pt_lon")
  )

  n_distinct_points <- table(shapes$shape_id[is_distinct_point])
  usable_shapes <- names(n_distinct_points)[n_distinct_points >= 2]

  return(usable_shapes)
}



#' Build the geometries of parts of shapes
#'
#' @param cuts A `data.table` with the `shape_id` of each part and the
#'   positions, in meters from the start of the shape, where it starts
#'   (`from`) and ends (`to`).
#' @param shape_points The `"shape_points"` attribute of the output of
#'   `locate_stops_along_shapes()`, on which the positions were measured.
#'
#' @return A `sfc_LINESTRING` in WGS 84 with the geometry of each part.
#'
#' @keywords internal
build_shape_cuts <- function(cuts, shape_points) {
  # the cumulative distance of each point of the relevant shapes is calculated
  # in C++ with the same distances as cpp_locate_stops_on_shapes(), but summed
  # in long double, as R's cumsum() does, while cpp_locate_stops_on_shapes()
  # sums in double. so the positions of the last stops may slightly exceed the
  # cumulative distance of the last shape point. hence the interpolation
  # factors are clamped to [0, 1] below

  if (nrow(cuts) == 0) return(build_linestrings(cuts, "shape_id"))

  relevant_shapes <- unique(cuts$shape_id)
  shape_rows <- shape_points$rows[relevant_shapes]
  rows <- unlist(shape_rows, use.names = FALSE)

  lat <- shape_points$shapes$shape_pt_lat[rows]
  lon <- shape_points$shapes$shape_pt_lon[rows]

  # each part goes from the point at 'from' to the point at 'to', interpolated
  # along the shape, through the shape points in between ('inner' points,
  # whose cumulative distance is strictly between 'from' and 'to'). the shape
  # segments that contain 'from' and 'to' and the range of inner points are
  # found in C++ for all the parts at once. indices are global (into 'cum',
  # 'lat' and 'lon')

  n_cuts <- nrow(cuts)
  segments <- cpp_shape_cut_segments(
    lat,
    lon,
    lengths(shape_rows),
    match(cuts$shape_id, relevant_shapes),
    cuts$from,
    cuts$to
  )
  cum <- segments$cum
  from_segment <- segments$from_segment
  to_segment <- segments$to_segment
  first_inner <- segments$first_inner
  last_inner <- segments$last_inner

  interpolate <- function(position, segment) {
    segment_length <- cum[segment + 1L] - cum[segment]
    fraction <- (position - cum[segment]) / segment_length
    fraction[!(segment_length > 0)] <- 0
    fraction <- pmin(pmax(fraction, 0), 1)

    list(
      lon = lon[segment] + fraction * (lon[segment + 1L] - lon[segment]),
      lat = lat[segment] + fraction * (lat[segment + 1L] - lat[segment])
    )
  }
  start_point <- interpolate(cuts$from, from_segment)
  end_point <- interpolate(cuts$to, to_segment)

  # the points of each part: its start point, its inner points and its end
  # point

  n_inner <- pmax(last_inner - first_inner + 1L, 0L)
  n_cut_points <- n_inner + 2L
  cut_id <- rep.int(seq_len(n_cuts), n_cut_points)
  point_position <- sequence(n_cut_points)
  is_start <- point_position == 1L
  is_end <- point_position == n_cut_points[cut_id]

  point_idx <- first_inner[cut_id] + point_position - 2L
  point_idx[is_start | is_end] <- NA_integer_

  cut_lon <- lon[point_idx]
  cut_lat <- lat[point_idx]
  cut_lon[is_start] <- start_point$lon
  cut_lat[is_start] <- start_point$lat
  cut_lon[is_end] <- end_point$lon
  cut_lat[is_end] <- end_point$lat

  cut_points <- data.table::data.table(
    cut = cut_id,
    lon = cut_lon,
    lat = cut_lat
  )

  return(build_linestrings(cut_points, "cut"))
}



#' Build linestrings from sequences of points
#'
#' @param points A `data.table` with the coordinates of each point and an
#'   integer column identifying the linestring of each point. The points of
#'   each linestring must be contiguous and the ids increasing.
#' @param id_col The name of the column that identifies the linestrings.
#' @param x_col,y_col The names of the columns with the longitude and the
#'   latitude of each point.
#'
#' @return A `sfc_LINESTRING` in WGS 84, with one linestring per id.
#'
#' @keywords internal
build_linestrings <- function(points, id_col, x_col = "lon", y_col = "lat") {
  # the condition for nrow == 0 prevents an sfheaders error

  if (nrow(points) == 0) {
    built <- sf::st_sfc(crs = 4326)
    class(built)[1] <- "sfc_LINESTRING"
    return(built)
  }

  built <- sfheaders::sfc_linestring(
    points,
    x = x_col,
    y = y_col,
    linestring_id = id_col
  )
  built <- sf::st_set_crs(built, 4326)

  return(built)
}
