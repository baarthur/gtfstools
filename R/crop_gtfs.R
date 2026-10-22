#' Crop a GTFS object to a spatial extent
#'
#' Crops a GTFS object to a spatial extent (an `sf`, `sfc` or `bbox` object),
#' keeping only the stops inside it (and their parent stations) and the parts
#' of the trips that serve them. Unlike [filter_by_spatial_extent()], which
#' keeps or drops entire trips, `crop_gtfs()` cuts the trips that leave the
#' spatial extent, keeping only their stops inside it, and, by default, trims
#' their shapes accordingly.
#'
#' @template gtfs
#' @param geom An `sf`, `sfc` or `bbox` object, in WGS 84 (EPSG 4326).
#'   Describes the spatial extent used to crop the data. Stops on its boundary
#'   are considered inside it. A `bbox` is compared directly with the stops'
#'   coordinates, while `sf` and `sfc` objects are tested with
#'   [sf::st_intersects()], whose edges are geodesics when spherical geometry
#'   is used (the default in `{sf}`), so stops very close to their edges may be
#'   considered outside. Bounding boxes crossing the antimeridian are not
#'   supported.
#' @param trim_shapes A logical. Whether the shapes of the trips that leave the
#'   spatial extent should be trimmed to the part between their first and last
#'   stops inside it (defaults to `TRUE`). If `FALSE`, these trips keep their
#'   entire shapes.
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the cropping
#'   process.
#'
#' @section Details:
#' The `stop_times` entries at stops outside the spatial extent are dropped.
#' Trips left with fewer than two `stop_times` entries are dropped, as are the
#' entries related to them in other tables, as in [filter_by_trip_id()]. A trip
#' that leaves the spatial extent and enters it again keeps the stops inside
#' it, with gaps in their `stop_sequence`. The resulting `stops` table includes
#' the stops inside the spatial extent served by the remaining trips, the
#' entrances, generic nodes and boarding areas inside it related to these
#' stops, and their parent stations, even if these lie outside the spatial
#' extent. Stops without coordinates are considered outside of it, so
#' generic nodes and boarding areas without coordinates, and the pathways
#' that link to them, are dropped.
#'
#' GTFS requires times at the first and last stops of each trip. If the new
#' first or last stop of a cropped trip has blank times, the blank times of
#' the trip are interpolated with [interpolate_stop_times()] (along its shape,
#' or along straight lines between stops if the trip is not linked to a
#' shape), before the trip is cropped. Intermediate blank times of these
#' trips are thus filled too. Times that can't be interpolated (see
#' [interpolate_stop_times()]) are left blank, with a warning. If a cropped
#' trip is listed in `frequencies`, the `start_time` and `end_time` of its
#' entries are shifted by the time between the departure (or the arrival, if
#' the departure is blank) at its original first stop and at its new one,
#' since they refer to the first stop of the trip. Entries whose shift can't
#' be calculated (e.g. because of blank times) or is negative are kept
#' unchanged.
#'
#' With `trim_shapes = TRUE`, the stops of each cropped trip are located
#' along its shape, as in [get_trip_length()], and its shape is cut from its
#' first to its last remaining stop, keeping the parts of the shape outside the
#' spatial extent between them. A shape cut in only one way keeps its
#' `shape_id`. If a shape is cut in different ways, or is also used by trips
#' that are not cut, each cut gets a new `shape_id` (`"<shape_id>_1"`,
#' `"<shape_id>_2"`, etc., different from any `shape_id` already in use). The
#' `shape_dist_traveled` of the new shape points and of the `stop_times`
#' entries of the trips linked to them is set to `NA`, because the distance
#' unit used by the feed is unknown. Trips not linked to a usable shape keep
#' their shape unchanged.
#'
#' @seealso [filter_by_spatial_extent()], [interpolate_stop_times()]
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' bbox <- sf::st_bbox(
#'   c(xmin = -46.66, ymin = -23.57, xmax = -46.62, ymax = -23.53),
#'   crs = 4326
#' )
#' object.size(gtfs)
#'
#' # keeps only the stops inside the bounding box and trims the shapes
#' cropped_gtfs <- crop_gtfs(gtfs, bbox)
#' object.size(cropped_gtfs)
#'
#' # keeps the entire shapes of the cropped trips
#' cropped_gtfs <- crop_gtfs(gtfs, bbox, trim_shapes = FALSE)
#' object.size(cropped_gtfs)
#'
#' @export
crop_gtfs <- function(gtfs, geom, trim_shapes = TRUE) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert(
    checkmate::check_class(geom, "sf"),
    checkmate::check_class(geom, "sfc"),
    checkmate::check_class(geom, "bbox")
  )
  checkmate::assert_logical(trim_shapes, len = 1, any.missing = FALSE)

  gtfsio::assert_field_class(
    gtfs,
    "stops",
    c("stop_id", "stop_lat", "stop_lon"),
    c("character", "numeric", "numeric")
  )
  gtfsio::assert_field_class(
    gtfs,
    "stop_times",
    c("trip_id", "stop_id", "stop_sequence", "arrival_time", "departure_time"),
    c("character", "character", "integer", "character", "character")
  )

  should_trim <- trim_shapes && has_shapes(gtfs)

  if (should_trim) {
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

  if (sf::st_crs(geom) != sf::st_crs(4326)) {
    cli::cli_abort(
      "{.arg geom} CRS must be WGS 84 (EPSG 4326).",
      class = "gtfstools_bad_crs_error"
    )
  }

  # stops without coordinates can't be inside the spatial extent.
  #
  # a bounding box is compared directly with the stops' coordinates. converting
  # it to a polygon would, with spherical geometry ({s2}), turn its edges into
  # geodesics, which leave out stops inside the bounding box near its edges.
  # other geometries are unioned, as in filter_by_spatial_extent(), and tested
  # with st_intersects(). get_stops_geometry() doesn't skip stops without
  # coordinates, so only the others are passed to it

  has_coords <- !is.na(gtfs$stops$stop_lat) &
    !is.na(gtfs$stops$stop_lon) &
    !is.na(gtfs$stops$stop_id)

  if (inherits(geom, "bbox")) {
    is_valid_bbox <- isTRUE(
      geom[["xmin"]] <= geom[["xmax"]] && geom[["ymin"]] <= geom[["ymax"]]
    )

    if (!is_valid_bbox) {
      cli::cli_abort(
        paste0(
          "{.arg geom} must be a bounding box with non-missing limits, in ",
          "which {.field xmin} <= {.field xmax} and {.field ymin} <= ",
          "{.field ymax}."
        ),
        class = "gtfstools_bad_bbox_error"
      )
    }

    is_inside <- has_coords &
      gtfs$stops$stop_lon >= geom[["xmin"]] &
      gtfs$stops$stop_lon <= geom[["xmax"]] &
      gtfs$stops$stop_lat >= geom[["ymin"]] &
      gtfs$stops$stop_lat <= geom[["ymax"]]
    inside_stops <- unique(gtfs$stops$stop_id[which(is_inside)])
  } else {
    if (
      (inherits(geom, "sf") && nrow(geom) > 1) ||
        (inherits(geom, "sfc") && length(geom) > 1)
    ) {
      geom <- sf::st_union(geom)
    }

    stops_sf <- get_stops_geometry(gtfs, gtfs$stops$stop_id[has_coords])
    is_inside <- sf::st_intersects(geom, stops_sf, sparse = FALSE)[1, ]
    inside_stops <- unique(stops_sf$stop_id[is_inside])
  }

  # the stop_times of each trip, sorted by stop_sequence, in a new table, so
  # the given gtfs is never modified. the selection index is created outside of
  # `[` so that {data.table} doesn't add an index to the original table. rows
  # without a trip_id can't belong to any trip

  is_valid_row <- !is.na(gtfs$stop_times$trip_id) &
    gtfs$stop_times$trip_id != ""
  st <- gtfs$stop_times[
    is_valid_row,
    .SD,
    .SDcols = c(
      "trip_id",
      "stop_id",
      "stop_sequence",
      "arrival_time",
      "departure_time"
    )
  ]
  data.table::setorderv(st, c("trip_id", "stop_sequence"))

  # a row is kept if its stop is inside the spatial extent, and a trip is kept
  # if it has at least two kept rows. kept trips whose first or last row is not
  # kept are cropped

  trip_start <- which(!duplicated(st$trip_id))
  trip_end <- c(trip_start[-1L] - 1L, nrow(st))[seq_along(trip_start)]
  n_stops <- trip_end - trip_start + 1L
  trip_ids <- st$trip_id[trip_start]
  trip_index <- rep.int(seq_along(trip_start), n_stops)

  is_kept <- st$stop_id %chin% inside_stops
  kept_rows <- which(is_kept)
  kept_row_trip <- trip_index[kept_rows]
  n_kept <- tabulate(kept_row_trip, nbins = length(trip_start))

  first_kept <- last_kept <- rep(NA_integer_, length(trip_start))
  is_first <- !duplicated(kept_row_trip)
  is_last <- !duplicated(kept_row_trip, fromLast = TRUE)
  first_kept[kept_row_trip[is_first]] <- kept_rows[is_first]
  last_kept[kept_row_trip[is_last]] <- kept_rows[is_last]

  is_kept_trip <- n_kept >= 2L
  is_cropped <- is_kept_trip & (!is_kept[trip_start] | !is_kept[trip_end])
  kept_trips <- trip_ids[is_kept_trip]

  # cropped trips whose new first or last stop has blank times are
  # interpolated before cropping

  is_blank_end <- is_cropped & (
    is_blank_time(st$arrival_time[first_kept]) |
      is_blank_time(st$departure_time[first_kept]) |
      is_blank_time(st$arrival_time[last_kept]) |
      is_blank_time(st$departure_time[last_kept])
  )

  if (any(is_blank_end)) {
    gtfs <- interpolate_cropped_trips(
      gtfs,
      trip_id = trip_ids[is_blank_end],
      first_sequence = st$stop_sequence[first_kept[is_blank_end]],
      last_sequence = st$stop_sequence[last_kept[is_blank_end]]
    )

    # the times of the interpolated trips are updated in 'st', which is used
    # below to shift the frequencies

    interpolated_trip <- which(is_blank_end)
    st_rows <- sequence(
      n_stops[interpolated_trip],
      from = trip_start[interpolated_trip]
    )
    row <- match_stop_times_rows(
      gtfs$stop_times,
      st$trip_id[st_rows],
      st$stop_sequence[st_rows]
    )
    for (col in c("arrival_time", "departure_time")) {
      data.table::set(
        st,
        i = st_rows,
        j = col,
        value = gtfs$stop_times[[col]][row]
      )
    }
  }

  # frequencies' time windows refer to the first stop of each trip, so they
  # are shifted for the trips whose first stop is dropped, by the time between
  # the departures (or arrivals, if departures are blank) at the old and new
  # first stops

  is_new_first <- is_cropped & !is_kept[trip_start]

  if (
    gtfsio::check_file_exists(gtfs, "frequencies") &&
      any(is_new_first)
  ) {
    gtfsio::assert_field_class(
      gtfs,
      "frequencies",
      c("trip_id", "start_time", "end_time"),
      c("character", "character", "character")
    )

    stop_seconds <- function(row) {
      seconds <- string_to_seconds(st$departure_time[row])
      is_blank <- is.na(seconds)
      seconds[is_blank] <- string_to_seconds(st$arrival_time[row[is_blank]])
      return(seconds)
    }

    gtfs <- shift_cropped_frequencies(
      gtfs,
      trip_id = trip_ids[is_new_first],
      shift = stop_seconds(first_kept[is_new_first]) -
        stop_seconds(trip_start[is_new_first])
    )
  }

  # the stops of the cropped trips are located along their shapes before
  # filtering, using all their stops, so the positions match those used by
  # get_trip_length() and get_trip_geometry()

  shape_cuts <- NULL

  if (should_trim && any(is_cropped)) {
    shape_cuts <- locate_cropped_trip_cuts(
      gtfs,
      st,
      cropped_trip = which(is_cropped),
      trip_start = trip_start,
      n_stops = n_stops,
      is_kept = is_kept
    )
  }

  # the trip filter drops the trips with fewer than two kept rows, and the stop
  # filter drops the rows outside the spatial extent. children stops are not
  # included, because platforms of a station inside the spatial extent may lie
  # outside of it. parent stations are kept

  gtfs <- filter_by_trip_id(gtfs, kept_trips)
  gtfs <- filter_by_stop_id(gtfs, inside_stops, include_children = FALSE)

  if (!is.null(shape_cuts) && nrow(shape_cuts$cuts) > 0) {
    gtfs <- replace_cropped_shapes(gtfs, shape_cuts)
  }

  return(gtfs)
}



#' Check which time strings are blank
#'
#' @param x A character vector of time strings.
#'
#' @return A logical vector, `TRUE` where the time is `NA`, an empty or
#'   whitespace-only string, or `"NA"`, as in [interpolate_stop_times()].
#'
#' @keywords internal
is_blank_time <- function(x) {
  return(is.na(x) | trimws(x) %chin% c("", "NA"))
}



#' Match stop_times entries by trip and stop sequence
#'
#' @param stop_times The `stop_times` table.
#' @param trip_id,stop_sequence The `trip_id` and `stop_sequence` of each entry.
#'
#' @return An integer vector with the row of each entry in `stop_times`.
#'   Repeated `trip_id`/`stop_sequence` pairs in `stop_times` (invalid GTFS)
#'   are resolved by their first match.
#'
#' @keywords internal
match_stop_times_rows <- function(stop_times, trip_id, stop_sequence) {
  # only the rows of the given trips are joined, so the join doesn't sort the
  # whole table

  candidate_rows <- which(stop_times$trip_id %chin% trip_id)
  candidates <- stop_times[
    candidate_rows,
    .SD,
    .SDcols = c("trip_id", "stop_sequence")
  ]
  entries <- data.table::data.table(
    trip_id = trip_id,
    stop_sequence = stop_sequence
  )
  row <- candidates[
    entries,
    on = c("trip_id", "stop_sequence"),
    mult = "first",
    which = TRUE
  ]

  return(candidate_rows[row])
}



#' Interpolate the times of cropped trips
#'
#' @template gtfs
#' @param trip_id The `trip_id`s of the cropped trips with blank times at their
#'   new first or last stop.
#' @param first_sequence,last_sequence The `stop_sequence` of the new first and
#'   last stops of each trip.
#'
#' @return The GTFS object with the times of these trips interpolated, along
#'   their shapes when possible, and along straight lines otherwise.
#'
#' @keywords internal
interpolate_cropped_trips <- function(gtfs,
                                      trip_id,
                                      first_sequence,
                                      last_sequence) {
  muffle <- function(w) invokeRestart("muffleWarning")

  has_blank_end <- function(gtfs) {
    stop_times <- gtfs$stop_times
    ends <- c(
      match_stop_times_rows(stop_times, trip_id, first_sequence),
      match_stop_times_rows(stop_times, trip_id, last_sequence)
    )
    is_blank <- is_blank_time(stop_times$arrival_time[ends]) |
      is_blank_time(stop_times$departure_time[ends])
    n_trips <- length(trip_id)

    return(is_blank[seq_len(n_trips)] | is_blank[n_trips + seq_len(n_trips)])
  }

  # times are interpolated along the shapes first. the trips whose ends are
  # still blank (e.g. not linked to a usable shape) are then interpolated along
  # straight lines. the warnings about times left blank by the first attempt
  # are held: they are raised if no second attempt follows, and dropped
  # otherwise, because they may refer to trips filled by the second attempt
  # (so a trip whose ends were filled by the first attempt, but not some of
  # its intermediate times, gets no warning)

  to_interpolate <- rep(TRUE, length(trip_id))

  if (has_shapes(gtfs)) {
    held_warnings <- list()
    interpolated <- withCallingHandlers(
      interpolate_stop_times(gtfs, trip_id, method = "shapes"),
      gtfstools_trips_without_shape = muffle,
      gtfstools_times_not_interpolated = function(w) {
        # assigns to the local 'held_warnings' above, not to a global
        held_warnings[[length(held_warnings) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    )
    to_interpolate <- has_blank_end(interpolated)

    if (!any(to_interpolate)) {
      for (w in held_warnings) warning(w)
      return(interpolated)
    }

    gtfs <- interpolated
  }

  gtfs <- withCallingHandlers(
    interpolate_stop_times(gtfs, trip_id[to_interpolate], method = "euclidean"),
    gtfstools_trips_without_shape = muffle
  )

  return(gtfs)
}



#' Shift the frequencies of cropped trips
#'
#' @template gtfs
#' @param trip_id The `trip_id`s of the cropped trips whose first stop is
#'   dropped.
#' @param shift The shift of each trip, in seconds. Entries with an `NA` or
#'   negative shift are kept unchanged.
#'
#' @return The GTFS object with the `start_time` and `end_time` (and their
#'   `_secs` columns, if present) of the `frequencies` entries of these trips
#'   shifted.
#'
#' @keywords internal
shift_cropped_frequencies <- function(gtfs, trip_id, shift) {
  frequencies <- gtfs$frequencies
  freq_trip <- data.table::chmatch(frequencies$trip_id, trip_id)
  rows <- which(!is.na(freq_trip))

  if (length(rows) == 0) return(gtfs)

  row_shift <- shift[freq_trip[rows]]

  # a subset that selects every row may share columns with the original table
  # (and frequencies may be the caller's table), so it's copied before being
  # updated by reference

  frequencies <- data.table::copy(frequencies)

  for (col in c("start_time", "end_time")) {
    seconds <- string_to_seconds(frequencies[[col]][rows]) + row_shift
    is_shifted <- !is.na(seconds) & row_shift >= 0
    seconds <- as.integer(seconds[is_shifted])
    shifted_rows <- rows[is_shifted]

    data.table::set(
      frequencies,
      i = shifted_rows,
      j = col,
      value = seconds_to_string(seconds)
    )

    secs_col <- paste0(col, "_secs")
    if (secs_col %chin% names(frequencies)) {
      data.table::set(
        frequencies,
        i = shifted_rows,
        j = secs_col,
        value = seconds
      )
    }
  }

  gtfs$frequencies <- frequencies

  return(gtfs)
}



#' Locate the cuts of the shapes of cropped trips
#'
#' @template gtfs
#' @param st The sorted stop_times of every trip, in which the rows of each
#'   trip are contiguous.
#' @param cropped_trip The indices of the cropped trips.
#' @param trip_start,n_stops The first row of each trip in `st` and its number
#'   of rows.
#' @param is_kept Whether each row of `st` is kept.
#'
#' @return A list with `trip_cuts`, a `data.table` with the `trip_id` of each
#'   cropped trip with a cut, its `shape_id` and the index of its cut in
#'   `cuts`, which holds the unique `shape_id`, `from` and `to` of the cuts, and
#'   `shape_points`, the shape points on which the positions were measured.
#'
#' @keywords internal
locate_cropped_trip_cuts <- function(gtfs,
                                     st,
                                     cropped_trip,
                                     trip_start,
                                     n_stops,
                                     is_kept) {
  rows <- sequence(n_stops[cropped_trip], from = trip_start[cropped_trip])
  cropped_st <- st[rows, .SD, .SDcols = c("trip_id", "stop_id")]

  stop_idx <- data.table::chmatch(cropped_st$stop_id, gtfs$stops$stop_id)
  position <- withCallingHandlers(
    locate_stops_along_shapes(
      gtfs,
      cropped_st,
      gtfs$stops$stop_lat[stop_idx],
      gtfs$stops$stop_lon[stop_idx]
    ),
    gtfstools_trips_without_shape = function(w) invokeRestart("muffleWarning")
  )
  shape_points <- attr(position, "shape_points")

  # each trip is cut from its first to its last kept located stop. trips with
  # fewer than two kept located stops, or whose kept stops are all located at
  # the same point, are not cut

  row_trip <- rep.int(seq_along(cropped_trip), n_stops[cropped_trip])
  is_located <- is_kept[rows] & !is.na(position)
  located_trip <- row_trip[is_located]
  located_position <- position[is_located]
  is_first <- !duplicated(located_trip)
  is_last <- !duplicated(located_trip, fromLast = TRUE)

  cut_trip <- located_trip[is_first]
  from <- located_position[is_first]
  to <- located_position[is_last]
  is_cut <- to > from

  trip_cuts <- data.table::data.table(
    trip_id = cropped_st$trip_id[match(cut_trip[is_cut], row_trip)],
    shape_id = shape_points$trip_shape_id[cut_trip[is_cut]],
    from = from[is_cut],
    to = to[is_cut]
  )

  cuts <- unique(trip_cuts, by = c("shape_id", "from", "to"))
  cuts <- cuts[, .SD, .SDcols = c("shape_id", "from", "to")]
  cut_index <- cuts[trip_cuts, on = names(cuts), which = TRUE]

  trip_cuts <- trip_cuts[, .SD, .SDcols = c("trip_id", "shape_id")]
  data.table::set(trip_cuts, j = "cut", value = cut_index)

  return(
    list(trip_cuts = trip_cuts, cuts = cuts, shape_points = shape_points)
  )
}



#' Replace the shapes of cropped trips by their cuts
#'
#' @template gtfs
#' @param shape_cuts The output of `locate_cropped_trip_cuts()`.
#'
#' @return The GTFS object with the cut shapes added to `shapes`, the cropped
#'   trips linked to them and the `shape_dist_traveled` of their `stop_times`
#'   set to `NA`. Shapes no longer used by any trip are removed.
#'
#' @keywords internal
replace_cropped_shapes <- function(gtfs, shape_cuts) {
  cuts <- shape_cuts$cuts
  shape_points <- shape_cuts$shape_points
  trip_cuts <- shape_cuts$trip_cuts

  # a shape keeps its shape_id if it's cut in only one way and isn't used by
  # any trip that is not cut. otherwise each of its cuts gets a new shape_id,
  # "<shape_id>_<n>", different from those already in use

  uncut_shapes <- gtfs$trips$shape_id[
    !(gtfs$trips$trip_id %chin% trip_cuts$trip_id)
  ]
  n_shape_cuts <- table(cuts$shape_id)
  keeps_id <- as.vector(n_shape_cuts[cuts$shape_id]) == 1L &
    !(cuts$shape_id %chin% uncut_shapes)

  new_id <- cuts$shape_id
  needs_id <- which(!keeps_id)

  if (length(needs_id) > 0) {
    used_ids <- unique(c(gtfs$shapes$shape_id, gtfs$trips$shape_id))
    base_id <- cuts$shape_id[needs_id]
    counter <- data.table::rowid(base_id)
    candidate <- paste0(base_id, "_", counter)

    # counters of clashing candidates are increased until every candidate is
    # unique and not already in use

    repeat {
      is_clash <- candidate %chin% used_ids | duplicated(candidate)
      if (!any(is_clash)) break
      counter[is_clash] <- counter[is_clash] + length(needs_id)
      candidate[is_clash] <- paste0(base_id[is_clash], "_", counter[is_clash])
    }

    new_id[needs_id] <- candidate
  }

  # the points of the cuts

  built <- build_shape_cuts(cuts, shape_points)
  points <- sfheaders::sfc_to_df(built)
  new_shapes <- data.table::data.table(
    shape_id = new_id[points$linestring_id],
    shape_pt_lat = points$y,
    shape_pt_lon = points$x,
    shape_pt_sequence = data.table::rowid(points$linestring_id)
  )

  # the cut shapes no longer used whole are replaced by their cuts. new points
  # get NA shape_dist_traveled, filled by rbind()

  is_replaced <- gtfs$shapes$shape_id %chin% cuts$shape_id &
    !(gtfs$shapes$shape_id %chin% uncut_shapes)
  gtfs$shapes <- rbind(gtfs$shapes[!is_replaced], new_shapes, fill = TRUE)

  trips <- data.table::copy(gtfs$trips)
  trip_match <- data.table::chmatch(trips$trip_id, trip_cuts$trip_id)
  cut_rows <- which(!is.na(trip_match))
  data.table::set(
    trips,
    i = cut_rows,
    j = "shape_id",
    value = new_id[trip_cuts$cut[trip_match[cut_rows]]]
  )
  gtfs$trips <- trips

  if (gtfsio::check_field_exists(gtfs, "stop_times", "shape_dist_traveled")) {
    cut_rows <- which(gtfs$stop_times$trip_id %chin% trip_cuts$trip_id)

    if (length(cut_rows) > 0) {
      stop_times <- data.table::copy(gtfs$stop_times)
      data.table::set(
        stop_times,
        i = cut_rows,
        j = "shape_dist_traveled",
        value = stop_times$shape_dist_traveled[NA_integer_]
      )
      gtfs$stop_times <- stop_times
    }
  }

  return(gtfs)
}
