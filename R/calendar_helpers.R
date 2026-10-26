# the weekday columns of 'calendar', in GTFS order

weekday_cols <- c(
  "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"
)



#' Get the weekday intervals of the calendar entries
#'
#' Returns one row per `calendar` entry and weekday on which it runs, with the
#' first and last days of the entry as integers (days since 1970-01-01).
#' Entries with missing or inverted dates are dropped.
#'
#' @param gtfs A GTFS object.
#' @param service_id The `service_id`s whose entries should be kept, or
#'   `NULL` to keep every entry.
#'
#' @return A `data.table` with the columns `service_id`, `weekday` (an
#'   integer, 1 is monday), `first_day` and `last_day`, empty if the GTFS
#'   object has no `calendar` table.
#'
#' @keywords internal
calendar_service_intervals <- function(gtfs, service_id = NULL) {
  service_intervals <- data.table::data.table(
    service_id = character(0),
    weekday = integer(0),
    first_day = integer(0),
    last_day = integer(0)
  )
  if (!gtfsio::check_file_exists(gtfs, "calendar")) return(service_intervals)

  calendar_cols <- c("service_id", weekday_cols, "start_date", "end_date")
  gtfsio::assert_field_class(
    gtfs,
    "calendar",
    calendar_cols,
    c("character", rep("integer", 7), "Date", "Date")
  )

  # the given services are selected before melting, which multiplies the rows
  # by seven. the row selection is an index created outside of `[`, so
  # {data.table} doesn't add an index to the given table; selecting every row
  # through an index would be slower than selecting the columns alone

  if (!is.null(service_id)) {
    rows <- which(gtfs$calendar$service_id %chin% service_id)
    calendar <- gtfs$calendar[rows, calendar_cols, with = FALSE]
  } else {
    calendar <- gtfs$calendar[, calendar_cols, with = FALSE]
  }

  service_intervals <- data.table::melt(
    calendar,
    id.vars = c("service_id", "start_date", "end_date"),
    measure.vars = weekday_cols,
    variable.name = "weekday",
    value.name = "runs"
  )
  service_intervals <- service_intervals[
    runs %in% 1L & start_date <= end_date,
    .(
      service_id,
      weekday = as.integer(weekday),
      first_day = as.integer(start_date),
      last_day = as.integer(end_date)
    )
  ]

  return(service_intervals)
}



#' Parse dates given as `Date`s or strings
#'
#' Validates a `date` argument given either as a `Date` vector or as a
#' character vector of dates in the `"YYYYMMDD"` or `"YYYY-MM-DD"` formats
#' (which may be mixed), and converts it to a `Date` vector.
#'
#' @param date The `date` argument to be parsed.
#' @param call The environment of the function whose `date` argument is
#'   parsed, in which errors are reported.
#' @param arg A string. The name of the argument, used in error messages.
#'
#' @return A `Date` vector.
#'
#' @keywords internal
parse_dates <- function(date, call = parent.frame(), arg = "date") {
  checkmate::assert(
    checkmate::check_date(date, any.missing = FALSE, min.len = 1),
    checkmate::check_character(date, any.missing = FALSE, min.len = 1),
    .var.name = arg
  )

  if (is.character(date)) {

    # the format of each string is chosen by its shape, and as.Date() alone
    # accepts trailing text (e.g. "2021-01-01x"), so strings must also be
    # formatted back to themselves

    date_format <- ifelse(grepl("^[0-9]{8}$", date), "%Y%m%d", "%Y-%m-%d")
    parsed_date <- as.Date(date, format = date_format)
    is_bad <- is.na(parsed_date) | format(parsed_date, date_format) != date
    is_bad[is.na(is_bad)] <- TRUE

    if (any(is_bad)) {
      bad_dates <- date[is_bad]
      cli::cli_abort(
        c(
          "{.arg {arg}} must be in the {.val YYYYMMDD} or {.val YYYY-MM-DD}
           formats.",
          "x" = "Invalid date{?s}: {.val {bad_dates}}."
        ),
        class = "gtfstools_bad_date_error",
        call = call
      )
    }

    date <- parsed_date
  }

  return(date)
}



#' Get the services that run on the given days
#'
#' Returns the services that run on each of the given days, according to the
#' `calendar` and `calendar_dates` tables (either may be missing).
#'
#' @template gtfs
#' @param days An integer vector of unique days, as the number of days since
#'   1970-01-01.
#'
#' @return A `data.table` with the columns `service_id` and `day` (as an
#'   integer), with one row for each service running on each day (rows may be
#'   duplicated). Services with a missing `service_id` are not included.
#'
#' @keywords internal
get_services_on_days <- function(gtfs, days) {
  # 1970-01-01 was a thursday, so (day + 3) %% 7 is 0 on mondays, which gives
  # the weekday of a day (1 is monday) regardless of the locale

  given_days <- data.table::data.table(
    day = days,
    weekday = (days + 3L) %% 7L + 1L
  )

  services_on_days <- data.table::data.table(
    service_id = character(0),
    day = integer(0)
  )

  # each calendar entry is matched to the given days that it spans and whose
  # weekday it flags (entries with missing or inverted dates are dropped)

  if (gtfsio::check_file_exists(gtfs, "calendar")) {
    service_weekdays <- calendar_service_intervals(gtfs)

    services_on_days <- service_weekdays[
      given_days,
      on = .(weekday, first_day <= day, last_day >= day),
      nomatch = NULL,
      .(service_id = x.service_id, day = i.day)
    ]
  }

  # the days removed from a service are dropped before the days added to it
  # are included, so that an addition wins over a removal on the same day

  if (gtfsio::check_file_exists(gtfs, "calendar_dates")) {
    gtfsio::assert_field_class(
      gtfs,
      "calendar_dates",
      c("service_id", "date", "exception_type"),
      c("character", "Date", "integer")
    )

    exceptions <- gtfs$calendar_dates[
      as.integer(date) %in% days,
      .(service_id, day = as.integer(date), exception_type)
    ]

    services_on_days <- services_on_days[
      !exceptions[exception_type == 2L],
      on = .(service_id, day)
    ]
    services_on_days <- rbind(
      services_on_days,
      exceptions[exception_type == 1L, .(service_id, day)]
    )
  }

  has_service_id <- !is.na(services_on_days$service_id)
  if (!all(has_service_id)) services_on_days <- services_on_days[has_service_id]

  return(services_on_days)
}



#' Get the service days of a GTFS object and their number of trips
#'
#' Returns the days on which at least one service used by a trip runs (any
#' service, if `trips` has no `service_id` field), according to the `calendar`
#' and `calendar_dates` tables (either may be missing), and the number of
#' trips that run on each of them.
#'
#' @template gtfs
#' @param count_trips A logical. Whether to count the trips of each day. If
#'   `FALSE`, `n_trips` is `NA`, which is faster when only the days are needed.
#'
#' @return A `data.table` with the columns `date` and `n_trips` (`NA` if
#'   `trips` has no `service_id` field or if `count_trips = FALSE`), sorted by
#'   date, with one row per service day.
#'
#' @keywords internal
get_service_days <- function(gtfs, count_trips = TRUE) {
  # each service weighs the number of trips it runs on a day. without trips,
  # or when they aren't counted, every service used by a trip (or every
  # service, without trips) weighs 1. services are referred to by their row in
  # 'service_weights' (sid), which makes the joins below faster than with
  # character ids

  has_trips <- gtfsio::check_field_exists(gtfs, "trips", "service_id")
  uses_trip_counts <- has_trips && count_trips

  if (uses_trip_counts) {
    service_weights <- get_service_weights(gtfs)
  } else {
    if (has_trips) {
      gtfsio::assert_field_class(gtfs, "trips", "service_id", "character")
      service_ids <- unique(gtfs$trips$service_id)
    } else {
      service_ids <- unique(
        c(gtfs[["calendar"]]$service_id, gtfs[["calendar_dates"]]$service_id)
      )
    }

    service_weights <- data.table::data.table(
      service_id = service_ids[!is.na(service_ids)],
      weight = 1
    )
  }

  # one row per service and weekday on which it runs, with the first and last
  # days of its calendar entry as integers (entries with missing or inverted
  # dates, and services without weight, are dropped)

  service_intervals <- calendar_service_intervals(
    gtfs,
    service_weights$service_id
  )
  service_intervals <- service_intervals[
    ,
    .(
      sid = match(service_id, service_weights$service_id),
      weekday,
      first_day,
      last_day
    )
  ]

  if (nrow(service_intervals) > 0L) {
    service_intervals <- merge_service_intervals(service_intervals)
  }

  # trips are counted on each day with a running sum by weekday, which adds
  # the weight of a service on the first day of its intervals and subtracts it
  # after the last. this splits each weekday into segments of constant weight,
  # whose days of that weekday are listed with their weight. listing the days
  # of each service instead could create millions of rows

  changes <- data.table::rbindlist(
    list(
      service_intervals[,
        .(weekday, day = first_day, change = service_weights$weight[sid])
      ],
      service_intervals[,
        .(weekday, day = last_day + 1L, change = -service_weights$weight[sid])
      ]
    )
  )
  changes <- changes[, .(change = sum(change)), keyby = .(weekday, day)]
  changes[,
    `:=`(
      weight = cumsum(change),
      next_day = data.table::shift(day, type = "lead")
    ),
    by = weekday
  ]
  segments <- changes[weight > 0]

  # each segment is expanded into its days of the given weekday only, starting
  # from the first of them. 1970-01-01 was a thursday, so (day + 3) %% 7 + 1
  # is the weekday of a day (1 is monday), regardless of the locale

  first_weekday <- (segments$day + 3L) %% 7L + 1L
  first_match <- segments$day + (segments$weekday - first_weekday) %% 7L
  n_matches <- (segments$next_day - 1L - first_match) %/% 7L + 1L
  segment_idx <- rep(seq_len(nrow(segments)), n_matches)

  day_weights <- data.table::data.table(
    date = first_match[segment_idx] + 7L * (sequence(n_matches) - 1L),
    weight = segments$weight[segment_idx]
  )

  # exceptions apply per service: on each of its dates, an exception adds the
  # weight of its service if the service is added and doesn't run on the day
  # through the calendar, and subtracts it if the service is removed and runs
  # on it. an addition wins over a removal of the same service and day

  if (gtfsio::check_file_exists(gtfs, "calendar_dates")) {
    gtfsio::assert_field_class(
      gtfs,
      "calendar_dates",
      c("service_id", "date", "exception_type"),
      c("character", "Date", "integer")
    )

    exceptions <- gtfs$calendar_dates[,
      .(
        sid = match(service_id, service_weights$service_id),
        date = as.integer(date),
        exception_type
      )
    ]
    exceptions <- exceptions[!is.na(sid) & !is.na(date)]

    if (nrow(exceptions) > 0L) {
      # max() over plain columns is used instead of any() because data.table
      # optimises it

      exceptions[,
        `:=`(
          is_added = exception_type %in% 1L,
          is_removed = exception_type %in% 2L
        )
      ]
      exception_days <- exceptions[,
        .(is_added = max(is_added), is_removed = max(is_removed)),
        keyby = .(sid, date)
      ]
      exception_days[, weekday := (date + 3L) %% 7L + 1L] # as above

      # the intervals of a service on a weekday don't overlap, so each exception
      # day falls within at most one of them

      covering_interval <- service_intervals[
        exception_days,
        on = .(sid, weekday, first_day <= date, last_day >= date),
        mult = "first",
        which = TRUE
      ]
      exception_days[, is_covered := !is.na(covering_interval)]

      exception_days[,
        weight := service_weights$weight[sid] *
          ((is_added | (is_covered & !is_removed)) - is_covered)
      ]

      day_weights <- rbind(day_weights, exception_days[, .(date, weight)])
    }
  }

  service_days <- day_weights[, .(weight = sum(weight)), keyby = date]
  service_days <- service_days[weight > 0]

  n_trips <- rep(NA_integer_, nrow(service_days))
  if (uses_trip_counts) {
    n_trips <- as.integer(service_days$weight)
  }

  service_days <- data.table::data.table(
    date = as.Date(service_days$date, origin = "1970-01-01"),
    n_trips = n_trips
  )

  return(service_days)
}



#' Get the service days of a GTFS object
#'
#' Returns the days on which at least one service used by a trip runs (any
#' service, if `trips` has no `service_id` field), according to the `calendar`
#' and `calendar_dates` tables (either may be missing).
#'
#' @template gtfs
#'
#' @return A sorted `Date` vector of unique days, empty if the feed has none.
#'
#' @keywords internal
get_service_period <- function(gtfs) {
  service_days <- get_service_days(gtfs, count_trips = FALSE)

  return(service_days$date)
}



#' Get the number of trips each service runs on a day
#'
#' Each trip counts once, except those listed in `frequencies`, which count
#' once per departure. A trip with an invalid `frequencies` entry counts once,
#' with a warning.
#'
#' @template gtfs
#'
#' @return A `data.table` with the columns `service_id` and `weight` (the
#'   number of trips, as a double).
#'
#' @keywords internal
get_service_weights <- function(gtfs) {
  gtfsio::assert_field_class(
    gtfs,
    "trips",
    c("trip_id", "service_id"),
    c("character", "character")
  )

  trips <- gtfs$trips[!is.na(service_id), .(trip_id, service_id)]
  if (anyDuplicated(trips$trip_id) > 0L) {
    trips <- unique(trips)
  }
  trips[, weight := 1]

  if (gtfsio::check_file_exists(gtfs, "frequencies")) {
    gtfsio::assert_field_class(
      gtfs,
      "frequencies",
      c("trip_id", "start_time", "end_time", "headway_secs"),
      c("character", "character", "character", "integer")
    )

    # the subset uses a logical vector, which, unlike %chin% inside the
    # brackets, doesn't add an index to the given table

    is_relevant <- !is.na(gtfs$frequencies$trip_id) &
      gtfs$frequencies$trip_id %chin% trips$trip_id
    frequencies <- unique(
      gtfs$frequencies[
        is_relevant,
        .(trip_id, start_time, end_time, headway_secs)
      ]
    )

    # malformed times are reported below, as invalid entries

    withCallingHandlers(
      frequencies[,
        `:=`(
          start_secs = string_to_seconds(start_time),
          end_secs = string_to_seconds(end_time)
        )
      ],
      gtfstools_malformed_time = function(w) invokeRestart("muffleWarning")
    )

    # the departures of an entry are every headway_secs from start_time until
    # (but not including) end_time, as in get_frequencies_departures(). an
    # entry whose start and end times are equal departs once

    frequencies[,
      is_invalid := is.na(start_secs) |
        is.na(end_secs) |
        is.na(headway_secs) |
        end_secs < start_secs |
        (headway_secs <= 0L & end_secs != start_secs)
    ]
    frequencies[
      is_invalid == FALSE,
      n_departures := data.table::fifelse(
        end_secs == start_secs,
        1,
        (end_secs - start_secs - 1) %/% headway_secs + 1
      )
    ]

    invalid_trips <- unique(frequencies[is_invalid == TRUE]$trip_id)
    if (length(invalid_trips) > 0L) {
      cli::cli_warn(
        c(
          paste0(
            "{.file frequencies} has invalid entries for ",
            "{length(invalid_trips)} trip_id{?s}: {.val {invalid_trips}}."
          ),
          "i" = "Each of these trips is counted once."
        ),
        class = "gtfstools_invalid_frequencies_warning"
      )
    }

    frequency_weights <- frequencies[
      !trip_id %chin% invalid_trips,
      .(weight = sum(n_departures)),
      by = trip_id
    ]
    # a trip_id may be listed under several services, so every row of a trip
    # gets its weight

    weight_idx <- match(trips$trip_id, frequency_weights$trip_id)
    has_weight <- which(!is.na(weight_idx))
    data.table::set(
      trips,
      i = has_weight,
      j = "weight",
      value = frequency_weights$weight[weight_idx[has_weight]]
    )
  }

  service_weights <- trips[, .(weight = sum(weight)), by = service_id]

  return(service_weights)
}



#' Merge the overlapping calendar intervals of each service and weekday
#'
#' @param service_intervals A `data.table` with the columns `sid`, `weekday`,
#'   `first_day` and `last_day` (integer days since 1970-01-01).
#'
#' @return A `data.table` with the same columns, in which the intervals of a
#'   service on a weekday don't overlap. `service_intervals` is reordered by
#'   reference.
#'
#' @keywords internal
merge_service_intervals <- function(service_intervals) {
  data.table::setorder(service_intervals, sid, weekday, first_day)

  # the intervals are merged in a single pass, without grouping. each group
  # (a service and weekday) is moved to its own range of a common axis, as
  # (group * span + day - base_day), so that a running max of the last days
  # never crosses groups. the keys are doubles, which are exact up to 2^53

  base_day <- min(service_intervals$first_day)
  span <- as.numeric(max(service_intervals$last_day)) - base_day + 2
  group_offset <- data.table::rleidv(service_intervals, c("sid", "weekday")) *
    span -
    base_day

  first_key <- group_offset + service_intervals$first_day
  last_key <- cummax(group_offset + service_intervals$last_day)

  starts <- which(first_key > data.table::shift(last_key, fill = -Inf))
  ends <- c(starts[-1L] - 1L, nrow(service_intervals))

  merged_intervals <- service_intervals[starts, .(sid, weekday, first_day)]
  merged_intervals[,
    last_day := as.integer(last_key[ends] - group_offset[ends])
  ]

  return(merged_intervals)
}
