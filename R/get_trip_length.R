#' Get trip length
#'
#' Returns the length of each specified `trip_id`, measured either from the
#' first to the last stop of the trip or between each pair of consecutive
#' stops. Lengths can be measured along the trip's shape or as straight lines
#' between stops.
#'
#' @template gtfs
#' @param trip_id A character vector including the `trip_id`s to have their
#'   lengths calculated. If `NULL` (the default), the function calculates the
#'   lengths of every `trip_id` in the GTFS.
#' @template method
#' @param by A string, either `"trip"` (the default) or `"segment"`. `"trip"`
#'   returns the length from the first to the last stop of each trip, while
#'   `"segment"` returns the length between each pair of consecutive stops.
#' @param unit A string representing the unit in which lengths are desired.
#'   Either `"km"` (the default) or `"m"`.
#' @param sort_sequence Deprecated; do not supply. Timetables and shapes are
#'   now always sorted by `stop_sequence` and `shape_pt_sequence`. Passing
#'   `TRUE` raises a warning, and passing `FALSE` raises an error.
#' @param file Deprecated. Use `method` instead (`file = "stop_times"`
#'   corresponds to `method = "euclidean"`).
#' @param from A string, in the "HH:MM:SS" format. With `by = "trip"`, only
#'   trips that depart from their first stop at or after `from` are included.
#'   With `by = "segment"`, only segments that depart from their first stop at
#'   or after `from` are included. If `NULL` (the default), there is no lower
#'   limit.
#' @param to A string, in the "HH:MM:SS" format. Only trips (or segments) that
#'   depart from their first stop at or before `to` are included. If `NULL`
#'   (the default), there is no upper limit.
#'
#' @return With `by = "trip"`, a `data.table` with the `trip_id` and the
#'   `length` of each trip. With `by = "segment"`, a `data.table` with the
#'   `trip_id`, the `segment` number, the `from_stop_id` and `to_stop_id` that
#'   delimit the segment and its `length`.
#'
#' @section Details:
#' Segments are numbered as in [get_trip_segment_duration()], so both outputs
#' can be joined by `trip_id` and `segment`. Trips with a single stop have a
#' length of 0.
#'
#' With `method = "shapes"`, each stop is projected onto the closest point of
#' its trip's shape, with stops advancing along the shape in `stop_sequence`
#' order, so loops and shapes that pass by the same place more than once are
#' handled. Some limitations remain:
#'
#' - a stop slightly behind the previous one along the shape (e.g. due to
#'   imprecise coordinates) is placed at the previous stop's position, so their
#'   segment has a length of 0. On shapes that pass the same place twice in the
#'   same direction, such a stop may be matched to the later pass instead;
#' - on shapes whose outbound and return legs overlap exactly, the stops just
#'   before and after the turnaround may be placed at the same position. The
#'   trip length is unaffected, but the segment between them gets a length of
#'   0 and the next one is longer.
#'
#' `shape_dist_traveled` is ignored. Trips not linked to a shape with at least
#' two distinct points have `NA` lengths, with a warning. Use
#' [get_shape_length()] for the length of entire shapes.
#'
#' Lengths are great-circle (haversine) distances on the sphere used by
#' `{s2}`. Stops with missing coordinates, or not listed in `stops`, result in
#' `NA` lengths for their segments and trips.
#'
#' `from` and `to` are compared with the `departure_time` of the first stop of
#' each trip (in `stop_sequence` order) or segment, as written in
#' `stop_times`, so use times past `"24:00:00"` for departures after midnight.
#' For trips listed in `frequencies`, these are template times, not the times
#' of each departure. Trips and segments whose departure is blank are never
#' included when `from` or `to` is given.
#'
#' @seealso [get_shape_length()], [get_trip_speed()],
#'   [get_trip_segment_duration()]
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' # length along the shape from the first to the last stop of each trip
#' trip_length <- get_trip_length(gtfs)
#' head(trip_length)
#'
#' # length along the shape between consecutive stops
#' segment_length <- get_trip_length(gtfs, "CPTM L07-0", by = "segment")
#' head(segment_length)
#'
#' # straight-line distances between consecutive stops, in meters
#' straight_length <- get_trip_length(
#'   gtfs,
#'   "CPTM L07-0",
#'   method = "euclidean",
#'   by = "segment",
#'   unit = "m"
#' )
#' head(straight_length)
#'
#' @export
get_trip_length <- function(gtfs,
                            trip_id = NULL,
                            method = "shapes",
                            by = "trip",
                            unit = "km",
                            sort_sequence,
                            file = NULL,
                            from = NULL,
                            to = NULL) {
  if (!missing(sort_sequence)) {
    check_deprecated_sort_sequence(
      sort_sequence,
      "get_trip_length",
      c("stop_sequence", "shape_pt_sequence")
    )
  }

  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(trip_id, null.ok = TRUE, any.missing = FALSE)
  if (!is.null(file)) method <- map_deprecated_file(file, "get_trip_length")
  checkmate::assert(
    checkmate::check_string(method),
    checkmate::check_names(method, subset.of = c("shapes", "euclidean")),
    combine = "and"
  )
  checkmate::assert(
    checkmate::check_string(by),
    checkmate::check_names(by, subset.of = c("trip", "segment")),
    combine = "and"
  )
  checkmate::assert(
    checkmate::check_string(unit),
    checkmate::check_names(unit, subset.of = c("km", "m")),
    combine = "and"
  )
  window <- dwell_time_window(from, to)
  is_windowed <- any(is.finite(window))

  gtfsio::assert_field_class(
    gtfs,
    "stop_times",
    c("trip_id", "stop_id", "stop_sequence"),
    c("character", "character", "integer")
  )
  if (is_windowed) {
    gtfsio::assert_field_class(
      gtfs,
      "stop_times",
      "departure_time",
      "character"
    )
  }
  gtfsio::assert_field_class(
    gtfs,
    "stops",
    c("stop_id", "stop_lat", "stop_lon"),
    c("character", "numeric", "numeric")
  )

  # lengths along the shapes require a 'shapes' table and a 'shape_id' column
  # in 'trips'. if any of them is missing, fall back to euclidean distances

  if (method == "shapes" && !has_shapes(gtfs)) {
    cli::cli_warn(
      c(
        paste0(
          "The GTFS object doesn't have a {.file shapes} table, or its ",
          "{.file trips} table doesn't have a {.field shape_id} column."
        ),
        "i" = paste0(
          "Calculating {.val euclidean} (straight-line) lengths between ",
          "consecutive stops instead of lengths along the shapes."
        )
      ),
      class = "gtfstools_shapes_unavailable"
    )
    method <- "euclidean"
  }

  if (method == "shapes") {
    gtfsio::assert_field_class(
      gtfs,
      "trips",
      c("trip_id", "shape_id"),
      c("character", "character")
    )

    gtfsio::assert_field_class(
      gtfs,
      "shapes",
      c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"),
      c("character", "numeric", "numeric", "integer")
    )
  }

  if (is_windowed) {
    st <- prepare_trip_stops(gtfs, trip_id, with_departure = TRUE)
  } else {
    st <- prepare_trip_stops(gtfs, trip_id)
  }

  # with by = "trip", only the trips that depart from their first stop within
  # the time of day are measured. with by = "segment", the segments are
  # filtered after being numbered, below

  if (is_windowed && by == "trip") {
    kept_trips <- trips_departing_within(
      st$trip_id,
      st$stop_sequence,
      st$departure_time,
      window
    )
    st <- st[st$trip_id %chin% kept_trips]
  }

  n_rows <- nrow(st)
  if (n_rows == 0) return(empty_trip_length(by))

  is_last_stop <- c(st$trip_id[-1L] != st$trip_id[-n_rows], TRUE)
  stop_lat <- st$stop_lat
  stop_lon <- st$stop_lon

  # calculate the distance from each stop to the next stop of the same trip
  # (the last stop of each trip gets NA)

  if (method == "euclidean") {
    distance <- rcpp_distance_haversine(
      stop_lat,
      stop_lon,
      c(stop_lat[-1L], NA_real_),
      c(stop_lon[-1L], NA_real_)
    )
  } else {
    position <- locate_stops_along_shapes(gtfs, st, stop_lat, stop_lon)
    attr(position, "shape_points") <- NULL
    distance <- c(position[-1L], NA_real_) - position
  }

  distance[is_last_stop] <- NA_real_
  if (unit == "km") distance <- distance / 1000

  # build the output. the length of a trip is the sum of the lengths of its
  # segments, so trips with a single stop have a length of 0 and trips with
  # a missing segment length have NA

  if (by == "segment") {
    is_segment <- !is_last_stop

    lengths <- data.table::data.table(
      trip_id = st$trip_id[is_segment],
      segment = data.table::rowid(st$trip_id[is_segment]),
      from_stop_id = st$stop_id[is_segment],
      to_stop_id = st$stop_id[which(is_segment) + 1L],
      length = distance[is_segment]
    )

    if (is_windowed) {
      departure <- string_to_seconds(st$departure_time[is_segment])
      in_window <- !is.na(departure) &
        departure >= window[1] &
        departure <= window[2]
      lengths <- lengths[in_window]
    }
  } else {
    distance[is_last_stop] <- 0

    lengths <- data.table::data.table(
      trip_id = st$trip_id,
      length = distance
    )
    lengths <- lengths[, .(length = sum(length, na.rm = FALSE)), by = trip_id]
  }

  return(lengths[])
}



#' Create an empty output of `get_trip_length()`
#'
#' @param by Either `"trip"` or `"segment"`.
#'
#' @return An empty `data.table` with the columns of the output of
#'   `get_trip_length()`.
#'
#' @keywords internal
empty_trip_length <- function(by) {
  if (by == "segment") {
    empty <- data.table::data.table(
      trip_id = character(0),
      segment = integer(0),
      from_stop_id = character(0),
      to_stop_id = character(0),
      length = numeric(0)
    )
  } else {
    empty <- data.table::data.table(
      trip_id = character(0),
      length = numeric(0)
    )
  }

  return(empty)
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



#' Map the deprecated `file` argument to `method`
#'
#' Raises a deprecation warning and returns the `method` that corresponds to
#' the given `file`.
#'
#' @param file The value given to the deprecated `file` argument.
#' @param fn_name The name of the function whose argument is deprecated.
#' @param details A string describing how the results differ from those
#'   obtained with `file`.
#'
#' @return Either `"shapes"`, if `"shapes"` is included in `file`, or
#'   `"euclidean"`.
#'
#' @keywords internal
map_deprecated_file <- function(file,
                                fn_name,
                                details = paste0(
                                  "Lengths are now measured from the first ",
                                  "to the last stop of each trip. Use ",
                                  "{.fun get_shape_length} to calculate the ",
                                  "length of the entire shapes."
                                )) {
  checkmate::assert_character(file, min.len = 1, any.missing = FALSE)
  checkmate::assert_names(file, subset.of = c("shapes", "stop_times"))

  method <- ifelse("shapes" %in% file, "shapes", "euclidean")

  message <- c(
    "The {.arg file} argument of {.fun {fn_name}} is deprecated.",
    "i" = "Please use {.arg method} = {.val {method}} instead.",
    "i" = details
  )
  if (length(file) > 1) {
    message <- c(
      message,
      "i" = paste0(
        "Only {.val shapes} was used. To use both methods, call ",
        "{.fun {fn_name}} once with each of them."
      )
    )
  }

  cli::cli_warn(message, class = "deprecated_file")

  return(method)
}
