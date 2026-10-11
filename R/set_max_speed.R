#' Set trip maximum speed
#'
#' Caps the speed of the specified trips: each part of a trip that is faster
#' than `max_speed` is slowed down to `max_speed`, by changing the
#' `arrival_time` and `departure_time` columns in `stop_times`. Parts of the
#' trips that are already slower are left unchanged.
#'
#' @template gtfs
#' @param max_speed A number. The maximum speed allowed, in the unit given by
#'   `unit`.
#' @param route_id A character vector including the `route_id`s whose trips
#'   have their speeds capped. If both `route_id` and `trip_id` are `NULL`
#'   (the default), the speeds of every trip in the GTFS are capped.
#' @param trip_id A character vector including the `trip_id`s to have their
#'   speeds capped. When given together with `route_id`, the trips of both are
#'   capped.
#' @param unit A string representing the unit in which the speed is given. One
#'   of `"km/h"` (the default), `"m/s"` or `"mph"` (miles per hour).
#'
#' @return A GTFS object with the time columns of its `stop_times` adjusted.
#'
#' @section Details:
#' The speed of each span between two consecutive stops with times is its
#' length, as calculated by [get_trip_length()] along the trip's shape (or as
#' straight lines between stops, with a warning, if there are no shapes),
#' divided by the time between the departure from the first stop and the
#' arrival at the second. Stops with blank times inside a span are kept blank.
#' The `stops` table is required. Spans whose length is `NA` (e.g. in trips not
#' linked to a shape) are left unchanged.
#'
#' Each span faster than `max_speed` has its duration increased to its length
#' divided by `max_speed`, rounded up to the next second, so new times may
#' include seconds even if the original times were given in whole minutes.
#' Spans with zero duration are treated the same way. All the later times of
#' the trip are shifted by the accumulated extra time, so dwell times are kept.
#' Spans whose arrival is earlier than the previous departure are left
#' unchanged, with a warning. This usually happens when times after midnight
#' are written as `"00:30:00"` instead of `"24:30:00"`. Only `stop_times` is
#' modified.
#'
#' @seealso [set_trip_speed()], [get_trip_speed()]
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
#' # the trip is faster than 30 km/h between some of its stops
#' get_trip_speed(gtfs, "CPTM L07-0", by = "segment")
#'
#' gtfs_capped <- set_max_speed(gtfs, 30, trip_id = "CPTM L07-0")
#' get_trip_speed(gtfs_capped, "CPTM L07-0", by = "segment")
#'
#' # the later stops are shifted, so the trip ends later
#' gtfs$stop_times[trip_id == "CPTM L07-0"][.N]
#' gtfs_capped$stop_times[trip_id == "CPTM L07-0"][.N]
#'
#' # cap the speed of all the trips of a route, using another unit
#' gtfs_capped <- set_max_speed(gtfs, 8, route_id = "CPTM L07", unit = "m/s")
#'
#' @export
set_max_speed <- function(
  gtfs,
  max_speed,
  route_id = NULL,
  trip_id = NULL,
  unit = "km/h"
) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_number(
    max_speed,
    lower = .Machine$double.eps,
    finite = TRUE
  )
  checkmate::assert_character(route_id, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_character(trip_id, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert(
    checkmate::check_string(unit),
    checkmate::check_names(unit, subset.of = c("km/h", "m/s", "mph")),
    combine = "and"
  )

  gtfsio::assert_field_class(
    gtfs,
    "stop_times",
    c("trip_id", "arrival_time", "departure_time", "stop_sequence"),
    c("character", "character", "character", "integer")
  )

  # calculations use km/h. 1 international mile = 1.609344 km

  max_kmh <- max_speed * c("km/h" = 1, "m/s" = 3.6, "mph" = 1.609344)[[unit]]

  # select the trips. ids is NULL only when both arguments are NULL, so that
  # trip_ids that don't exist don't result in all trips being changed

  existing_trips <- gtfs$stop_times$trip_id
  ids <- NULL

  if (!is.null(trip_id) || !is.null(route_id)) {
    ids <- character(0)

    if (!is.null(trip_id)) {
      invalid_trip_id <- warn_missing_ids(
        trip_id,
        existing_trips,
        "stop_times",
        "trip_id"
      )
      ids <- setdiff(trip_id, invalid_trip_id)
    }

    if (!is.null(route_id)) {
      gtfsio::assert_field_class(
        gtfs,
        "trips",
        c("trip_id", "route_id"),
        c("character", "character")
      )

      invalid_route_id <- unique(
        route_id[!route_id %chin% gtfs$trips$route_id]
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

      # only trips with stop times, so get_trip_length() doesn't warn about
      # the others

      route_trips <- gtfs$trips$trip_id[gtfs$trips$route_id %chin% route_id]
      route_trips <- route_trips[route_trips %chin% existing_trips]
      ids <- unique(c(ids, route_trips))
    }
  }

  if (is.null(ids)) {
    rows <- seq_along(existing_trips)
  } else {
    rows <- which(existing_trips %chin% ids)
  }

  if (length(rows) == 0) {
    return(gtfs)
  }

  # calculate the length of each segment between consecutive stops. the rows
  # are ordered as in get_trip_length(), so that the segment starting at the
  # n-th stop of a trip is its n-th segment, whose length is kept in the
  # segment's first stop row

  segment_length <- get_trip_length(gtfs, ids, by = "segment", unit = "km")

  st <- gtfs$stop_times[
    rows,
    .(trip_id, stop_sequence, arrival_time, departure_time)
  ]
  st[, row := rows]
  data.table::setorderv(st, c("trip_id", "stop_sequence"))
  st[, pos := data.table::rowid(trip_id)]
  st[
    segment_length,
    on = c(trip_id = "trip_id", pos = "segment"),
    seg_length := i.length
  ]

  # a blank arrival or departure is replaced by the other time of the stop.
  # stops with any time are 'timed'. each span between consecutive timed stops
  # gets a label, the number of timed stops up to its first stop, so its length
  # is the sum of the lengths of the segments with this label. a span that
  # includes a segment with unknown length has an NA length

  st[, arr := string_to_seconds(arrival_time)]
  st[, dep := string_to_seconds(departure_time)]
  st[, t_dep := as.numeric(data.table::fcoalesce(dep, arr))]
  st[, label := cumsum(!is.na(t_dep))]

  span_length <- st[, .(span_len = sum(seg_length)), by = .(trip_id, label)]

  # each timed stop ends the span labelled one less than itself, which starts
  # at the previous timed stop of the trip. the first timed stop of each trip
  # ends no span, so its extra time is 0

  timed <- st[!is.na(t_dep)]
  timed[, t_arr := as.numeric(data.table::fcoalesce(arr, dep))]
  timed[, prev_label := label - 1L]
  timed[
    span_length,
    on = c(trip_id = "trip_id", prev_label = "label"),
    span_len := i.span_len
  ]

  is_first <- data.table::rowid(timed$trip_id) == 1L
  span_dur <- timed$t_arr - data.table::shift(timed$t_dep)
  min_dur <- ceiling(timed$span_len / max_kmh * 3600)
  extra <- pmax(min_dur - span_dur, 0)
  extra[is_first | is.na(extra)] <- 0

  # spans that end before they start have invalid times, which are not
  # replaced by made-up ones. spans whose length is unknown are left unchanged
  # anyway, so they are not reported

  is_backwards <- !is_first &
    !is.na(span_dur) &
    span_dur < 0 &
    !is.na(timed$span_len)
  extra[is_backwards] <- 0

  if (any(is_backwards)) {
    backwards_trips <- unique(timed$trip_id[is_backwards])
    abbreviated <- abbreviate_ids(backwards_trips)
    cli::cli_warn(
      c(
        paste0(
          "{length(backwards_trips)} trip{?s} {?has/have} stops whose ",
          "arrival is earlier than the previous departure, so {?its/their} ",
          "speed{?s} {?was/were} not capped between them: ",
          "{.val {abbreviated$shown}}{abbreviated$more}."
        ),
        "i" = paste0(
          "This usually happens when times after midnight are written as ",
          "{.val 00:30:00} instead of {.val 24:30:00}."
        )
      ),
      class = "gtfstools_backwards_times"
    )
  }

  # the delay of each stop is the extra time accumulated up to it in its trip.
  # shifting a span doesn't change the duration of the following ones, so the
  # delays don't depend on each other

  cum_extra <- cumsum(extra)
  trip_start <- cumsum(is_first)
  timed[, delay := cum_extra - (cum_extra - extra)[is_first][trip_start]]

  timed <- timed[delay > 0]

  if (nrow(timed) == 0) {
    return(gtfs)
  }

  # seconds_to_string() can't write times later than 9999:59:59

  is_invalid <- pmax(timed$arr, timed$dep, na.rm = TRUE) + timed$delay >
    max_time_secs

  if (any(is_invalid)) {
    invalid_trips <- unique(timed$trip_id[is_invalid])
    abbreviated <- abbreviate_ids(invalid_trips)
    cli::cli_abort(
      paste0(
        "Capping the speed results in times later than {.val 9999:59:59} ",
        "in the following {cli::qty(length(invalid_trips))}trip{?s}: ",
        "{.val {abbreviated$shown}}{abbreviated$more}."
      ),
      class = "gtfstools_time_out_of_range"
    )
  }

  # shift the non-blank times of the delayed stops

  stop_times <- data.table::copy(gtfs$stop_times)

  has_arr <- !is.na(timed$arr)
  stop_times[
    timed$row[has_arr],
    arrival_time := seconds_to_string(
      as.integer(timed$arr[has_arr] + timed$delay[has_arr])
    )
  ]

  has_dep <- !is.na(timed$dep)
  stop_times[
    timed$row[has_dep],
    departure_time := seconds_to_string(
      as.integer(timed$dep[has_dep] + timed$delay[has_dep])
    )
  ]

  # refresh pre-existing *_secs columns, which other functions use as-is

  for (time_col in c("departure_time", "arrival_time")) {
    secs_col <- paste0(time_col, "_secs")
    if (secs_col %chin% names(stop_times)) {
      stop_times[
        timed$row,
        (secs_col) := string_to_seconds(get(time_col))
      ]
    }
  }

  gtfs$stop_times <- stop_times
  return(gtfs)
}
