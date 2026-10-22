#' Get trip segments' duration
#'
#' Returns the duration of segments between stops of each specified `trip_id`.
#'
#' @template gtfs
#' @param trip_id A string vector including the `trip_id`s to have their
#'   segments' duration calculated. If `NULL` (the default) the function
#'   calculates the segments' duration of every `trip_id` in the GTFS.
#' @param unit A string representing the time unit in which the duration is
#'   desired. One of `"s"` (seconds), `"min"` (minutes, the default),
#'   `"h"` (hours) or `"d"` (days).
#' @param sort_sequence Deprecated; do not supply. Timetables are now always
#'   sorted by `stop_sequence`. Passing `TRUE` raises a warning, and passing
#'   `FALSE` raises an error.
#' @param from A string, in the "HH:MM:SS" format. Only segments that depart
#'   from their first stop at or after `from` are included. If `NULL` (the
#'   default), there is no lower limit.
#' @param to A string, in the "HH:MM:SS" format. Only segments that depart from
#'   their first stop at or before `to` are included. If `NULL` (the default),
#'   there is no upper limit.
#'
#' @return A `data.table` containing the segments' duration of each specified
#'   trip.
#'
#' @section Details:
#' A trip segment is defined as the path between two subsequent stops in the
#' same trip. The  duration of a segment is defined as the time difference
#' between its arrival time and its departure time, as specified in the
#' `stop_times` file.
#'
#' Existing `_secs` columns in `stop_times` (e.g. created with
#' [convert_time_to_seconds()]) are used as-is, not recalculated from the time
#' strings.
#'
#' `from` and `to` are compared with the departure time of the first stop of
#' each segment, read as the durations are (from `departure_time_secs`, if
#' present), so use times past `"24:00:00"` for departures after midnight.
#' For trips listed in `frequencies`, these are template times, not the times
#' of each departure. Segments whose departure is blank are never included
#' when `from` or `to` is given.
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
#' trip_segment_dur <- get_trip_segment_duration(gtfs)
#' head(trip_segment_dur)
#'
#' # use the trip_id argument to control which trips are analyzed
#' trip_segment_dur <- get_trip_segment_duration(gtfs, trip_id = "CPTM L07-0")
#' trip_segment_dur
#'
#' # use the unit argument to control in which unit the durations are calculated
#' trip_segment_dur <- get_trip_segment_duration(gtfs, "CPTM L07-0", unit = "s")
#' trip_segment_dur
#'
#' # use the from and to arguments to keep only the segments that depart
#' # within a time of day
#' trip_segment_dur <- get_trip_segment_duration(
#'   gtfs,
#'   from = "07:00:00",
#'   to = "09:00:00"
#' )
#' head(trip_segment_dur)
#'
#' @export
get_trip_segment_duration <- function(gtfs,
                                      trip_id = NULL,
                                      unit = "min",
                                      sort_sequence,
                                      from = NULL,
                                      to = NULL) {
  if (!missing(sort_sequence)) {
    check_deprecated_sort_sequence(
      sort_sequence,
      "get_trip_segment_duration",
      "stop_sequence"
    )
  }

  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(trip_id, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert(
    checkmate::check_string(unit),
    checkmate::check_names(unit, subset.of = c("s", "min", "h", "d")),
    combine = "and"
  )
  window <- dwell_time_window(from, to)

  gtfsio::assert_field_class(
    gtfs,
    "stop_times",
    c("trip_id", "arrival_time", "departure_time", "stop_sequence"),
    c("character", "character", "character", "integer")
  )

  # select trip_ids to get segment's duration of and raise warning if a given
  # trip_id doesn't exist in 'stop_times'

  if (!is.null(trip_id)) {
    relevant_trips <- trip_id

    warn_missing_ids(trip_id, gtfs$stop_times$trip_id, "stop_times", "trip_id")

    durations <- gtfs$stop_times[trip_id %chin% relevant_trips]
  } else {
    durations <- gtfs$stop_times
  }

  # create auxiliary columns if needed

  if (!gtfsio::check_field_exists(gtfs, "stop_times", "departure_time_secs")) {
    durations[, departure_time_secs := string_to_seconds(departure_time)]
    created_departure_secs <- TRUE
  }

  if (!gtfsio::check_field_exists(gtfs, "stop_times", "arrival_time_secs")) {
    durations[, arrival_time_secs := string_to_seconds(arrival_time)]
    created_arrival_secs <- TRUE
  }

  if (is.null(trip_id)) durations <- data.table::copy(durations)
  durations <- data.table::setorderv(durations, c("trip_id", "stop_sequence"))

  # each stop but the last of each trip starts a segment, in the order the
  # segments are returned below. the rows of each trip are contiguous after
  # sorting, so the last stop is the last row with its trip_id (NAs included)

  is_windowed <- any(is.finite(window))

  if (is_windowed) {
    is_last_stop <- !duplicated(durations$trip_id, fromLast = TRUE)
    segment_departure <- durations$departure_time_secs[!is_last_stop]
  }

  # compute per trip, so rows of interleaved trips are never mixed up
  durations <- durations[
    ,
    .(
      segment = seq_len(max(.N - 1L, 0L)),
      duration = arrival_time_secs[-1L] - departure_time_secs[-.N]
    ),
    by = trip_id
  ]

  # keep only the segments that depart from their first stop within the time
  # of day

  if (is_windowed) {
    in_window <- !is.na(segment_departure) &
      segment_departure >= window[1] &
      segment_departure <= window[2]
    durations <- durations[in_window]
  }

  # select desired columns and convert duration to desired unit

  desired_cols <- c("trip_id", "segment", "duration")
  durations <- durations[, setdiff(names(durations), desired_cols) := NULL]
  data.table::setcolorder(durations, desired_cols)

  # {units} converts element by element, which is slow on long vectors, so we
  # get the conversion factor from a single value and multiply by it

  if (unit != "s") {
    unit_factor <- as.numeric(
      units::set_units(units::as_units(1, "s"), unit, mode = "standard")
    )
    durations[, duration := duration * unit_factor]
  }

  # the function may have created some colums to gtfs$stop_times if a copy
  # wasn't made when creating the durations table, so we need to clean them up

  if (
    gtfsio::check_field_exists(gtfs, "stop_times", "departure_time_secs") &&
    exists("created_departure_secs")
  ) {
    gtfs$stop_times[, departure_time_secs := NULL]
  }

  if (
    gtfsio::check_field_exists(gtfs, "stop_times", "arrival_time_secs") &&
    exists("created_arrival_secs")
  ) {
    gtfs$stop_times[, arrival_time_secs := NULL]
  }

  return(durations[])
}
