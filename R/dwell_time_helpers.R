# seconds in each time unit accepted by the dwell-time and duration functions

dwell_unit_factors <- c(s = 1, min = 60, h = 3600, d = 86400)



# the latest time that seconds_to_string() can write ("9999:59:59"). later
# times have their hours truncated, so set_dwell_time() doesn't create them

max_time_secs <- 9999L * 3600L + 59L * 60L + 59L



# "HH:MM:SS" (hours may exceed 23), as checked by every time-of-day argument

time_pattern <- "^\\d{2}:[0-5]\\d:[0-5]\\d$"



# 1 international mile = 1.609344 km

km_per_mile <- 1.609344



# km/h in each speed unit accepted by the speed functions: the exact decimal
# definitions (1 m/s = 3.6 km/h, 1 international mile = 1.609344 km)

speed_unit_factors <- c("km/h" = 1, "m/s" = 3.6, "mph" = km_per_mile)



# returns the rows of 'stop_times' of the given trips at the given stops (all
# of them, if NULL), warning about ids that are not in 'stop_times'. each
# column is scanned once, and its matches are reused to find the missing ids

select_dwell_time_calls <- function(stop_times, trip_id, stop_id) {
  is_selected <- NULL

  if (!is.null(trip_id)) {
    is_selected <- stop_times$trip_id %chin% trip_id
    warn_invalid_ids(
      trip_id,
      stop_times$trip_id[is_selected],
      "stop_times",
      "trip_id"
    )
  }

  if (!is.null(stop_id)) {
    in_stops <- stop_times$stop_id %chin% stop_id
    warn_invalid_ids(
      stop_id,
      stop_times$stop_id[in_stops],
      "stop_times",
      "stop_id"
    )

    if (is.null(is_selected)) {
      is_selected <- in_stops
    } else {
      is_selected <- is_selected & in_stops
    }
  }

  if (is.null(is_selected)) return(seq_len(nrow(stop_times)))

  return(which(is_selected))
}



# validates the 'from' and 'to' time-of-day arguments of the dwell time,
# duration, length and speed functions and returns them in seconds. a NULL
# end means no limit

dwell_time_window <- function(from, to) {
  checkmate::assert_string(from, pattern = time_pattern, null.ok = TRUE)
  checkmate::assert_string(to, pattern = time_pattern, null.ok = TRUE)

  from_secs <- if (is.null(from)) -Inf else string_to_seconds(from)
  to_secs <- if (is.null(to)) Inf else string_to_seconds(to)

  if (from_secs > to_secs) {
    cli::cli_abort(
      paste0(
        "{.arg from} ({.val {from}}) must not be later than {.arg to} ",
        "({.val {to}})."
      ),
      class = "gtfstools_invalid_time_of_day",
      call = parent.frame()
    )
  }

  return(c(from_secs, to_secs))
}



# returns the trip_ids whose departure from their first stop, in
# stop_sequence order, is within the time of day. 'departure' is either in
# seconds or a time string, in which case only the first stops' departures are
# converted. the stable sort, as in prepare_trip_stops(), picks the same first
# stop of trips with duplicated stop_sequences

trips_departing_within <- function(trip_id, stop_sequence, departure, window) {
  first_stops <- data.table::data.table(
    trip_id = trip_id,
    stop_sequence = stop_sequence,
    departure = departure
  )
  data.table::setorderv(first_stops, c("trip_id", "stop_sequence"))
  first_stops <- first_stops[!duplicated(first_stops$trip_id)]

  departure <- first_stops$departure
  if (is.character(departure)) departure <- string_to_seconds(departure)

  in_window <- !is.na(departure) &
    departure >= window[1] &
    departure <= window[2]

  return(first_stops$trip_id[in_window])
}



# cli formats every element of a vector, even those it doesn't show, which
# takes seconds for tens of thousands of ids. so messages list only the first
# ids, followed by a note on how many were left out. without the note, cli
# adds "and" before the last listed id

abbreviate_ids <- function(ids, n = 5) {
  n_more <- length(ids) - n

  if (n_more <= 0) return(list(shown = ids, more = ""))

  shown <- cli::cli_vec(utils::head(ids, n), list("vec-last" = ", "))
  return(list(shown = shown, more = paste0(" and ", n_more, " more")))
}
