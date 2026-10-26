#' Get stop times patterns
#'
#' Identifies spatial and spatiotemporal patterns within the `stop_times`
#' table. Please see the details to understand what a "pattern" means in each of
#' these cases.
#'
#' @template gtfs
#' @param trip_id A character vector including the `trip_id`s to have their
#'   `stop_times` entries analyzed. If `NULL` (the default), the function
#'   analyses the pattern of every `trip_id` in the GTFS.
#' @param type A string specifying the type of patterns to be analyzed. Either
#'   `"spatial"` (the default) or "spatiotemporal".
#' @param sort_sequence Deprecated; do not supply. Timetables are now always
#'   sorted by `stop_sequence`. Passing `TRUE` raises a warning, and passing
#'   `FALSE` raises an error.
#'
#' @return A `data.table` associating each `trip_id` to a `pattern_id`.
#'
#' @section Details:
#' Two trips are assigned to the same spatial `pattern_id` if they travel along
#' the same sequence of stops. They are assigned to the same spatiotemporal
#' `pattern_id`, on the other hand, if they travel along the same sequence of
#' stops and they take the same time between stops. Please note that, in such
#' case, only the time between stops is taken into account, and the time that
#' the trip started is ignored (e.g. if two trips depart from stop A and follow
#' the same sequence of stops to arrive at stop B, taking both 1 hour to do so,
#' their spatiotemporal pattern will be considered the same, even if one
#' departed at 6 am and another at 7 am). Please also note that only the order
#' given by `stop_sequence` matters, not its values - e.g. if trip X lists stops
#' A followed by stop B with `stop_sequence`s 1 and 2, and trip Y lists stops A
#' followed by stop B with `stop_sequence`s 1 and 3, they are assigned to the
#' same `pattern_id`.
#'
#' Existing `_secs` columns in `stop_times` (e.g. created with
#' [convert_time_to_seconds()]) are used as-is, not recalculated from the time
#' strings.
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' data_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' patterns <- get_stop_times_patterns(gtfs)
#' head(patterns)
#'
#' # use the trip_id argument to control which trips are analyzed
#' patterns <- get_stop_times_patterns(
#'   gtfs,
#'   trip_id = c("143765658", "143765659", "143765660")
#' )
#' patterns
#'
#' # use the type argument to control the type of pattern analyzed
#' patterns <- get_stop_times_patterns(
#'   gtfs,
#'   trip_id = c("143765658", "143765659", "143765660"),
#'   type = "spatiotemporal"
#' )
#' patterns
#'
#' @export
get_stop_times_patterns <- function(gtfs,
                                    trip_id = NULL,
                                    type = "spatial",
                                    sort_sequence) {
  if (!missing(sort_sequence)) {
    check_deprecated_sort_sequence(
      sort_sequence,
      "get_stop_times_patterns",
      "stop_sequence"
    )
  }

  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(trip_id, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert(
    checkmate::check_string(type),
    checkmate::check_names(type, subset.of = c("spatial", "spatiotemporal")),
    combine = "and"
  )

  must_exist <- c("trip_id", "stop_id", "stop_sequence")
  classes <- c("character", "character", "integer")
  if (type == "spatiotemporal") {
    must_exist <- c(must_exist, "departure_time", "arrival_time")
    classes <- c(classes, rep("character", 2))
  }
  gtfsio::assert_field_class(
    gtfs,
    "stop_times",
    fields = must_exist,
    classes = classes
  )

  # select trips to check patterns of and raise warning if any of them doesn't
  # exist in stop_times

  if (!is.null(trip_id)) {
    relevant_trips <- trip_id

    warn_missing_ids(trip_id, gtfs$stop_times$trip_id, "stop_times", "trip_id")

    patterns <- gtfs$stop_times[trip_id %chin% relevant_trips]
  } else {
    patterns <- gtfs$stop_times
  }

  if (is.null(trip_id)) patterns <- data.table::copy(patterns)
  patterns <- data.table::setorderv(patterns, c("trip_id", "stop_sequence"))

  # each trip's sequence of stops is described by integer codes, which are
  # compared by cpp_sequence_pattern_id()

  stop_codes <- data.table::chmatch(patterns$stop_id, unique(patterns$stop_id))

  # the rows of each trip must be contiguous and the trips ordered as keyby
  # would order them, which setorderv() above already did

  tid <- patterns$trip_id
  trip_start <- which(!duplicated(tid))
  n_stops <- diff(c(trip_start, length(tid) + 1L))

  if (type == "spatial") {
    columns <- list(stop_codes)
  } else {
    dep <- time_secs(patterns, "departure_time")
    arr <- time_secs(patterns, "arrival_time")

    # times are compared relative to the first departure of each trip: its
    # smallest non-NA departure, found by sorting the departures within each
    # trip with NAs last. trips without departures keep NA (NaN included, as
    # it would otherwise be compared as a different value)

    trip_idx <- rep.int(seq_along(trip_start), n_stops)
    first_departure <- dep[
      order(trip_idx, dep, na.last = TRUE, method = "radix")
    ][trip_start]
    first_departure[is.na(first_departure)] <- NA
    first_departure <- rep.int(first_departure, n_stops)

    # non-integer offsets (from pre-existing double _secs columns) are turned
    # into integer codes. they are compared as text, as when the patterns were
    # identified by pasting the times together

    to_codes <- function(x) {
      if (is.integer(x)) return(x)
      x <- as.character(x)
      return(match(x, unique(x)))
    }

    columns <- list(
      stop_codes,
      to_codes(dep - first_departure),
      to_codes(arr - first_departure)
    )
  }

  patterns <- data.table::data.table(
    trip_id = tid[trip_start],
    pattern_id = cpp_sequence_pattern_id(n_stops, columns)
  )
  data.table::setkeyv(patterns, "trip_id")

  return(patterns[])
}
