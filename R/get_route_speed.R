#' Get route speed
#'
#' Returns summary statistics of the average speeds of the trips of each
#' specified `route_id`.
#'
#' @template gtfs
#' @param route_id A character vector including the `route_id`s to have their
#'   speeds summarised. If `NULL` (the default), the function summarises the
#'   speeds of every `route_id` in the GTFS. `route_id`s not listed in `trips`
#'   are ignored, with a warning.
#' @template method
#' @inheritParams get_trip_speed
#'
#' @return A `data.table` with the `route_id` and the minimum (`min_speed`),
#'   mean (`mean_speed`), median (`median_speed`), maximum (`max_speed`) and
#'   standard deviation (`sd_speed`) of the speeds of its trips, ordered by
#'   `route_id`.
#'
#' @section Details:
#' The speed of each trip is calculated with [get_trip_speed()] (with
#' `by = "trip"`), whose details also apply here. Each trip is linked to its
#' route by the `trips` table. `mean_speed` is the unweighted mean of the trip
#' speeds, not the total length of the trips divided by their total duration.
#' Trips listed in `frequencies` count once, regardless of their headways.
#'
#' Trips with `NA` speeds are ignored. Routes whose trips all have `NA` speeds
#' get `NA` statistics, and `sd_speed` is `NA` for routes with a single trip
#' with a known speed. Routes without any trip in `stop_times` are not
#' returned.
#'
#' @seealso [get_trip_speed()], [set_route_speed()], [set_trip_speed()]
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
#' route_speed <- get_route_speed(gtfs)
#' head(route_speed)
#'
#' route_ids <- c("CPTM L07", "2002-10")
#' route_speed <- get_route_speed(gtfs, route_ids)
#' route_speed
#'
#' route_speed <- get_route_speed(gtfs, route_ids, method = "euclidean")
#' route_speed
#'
#' route_speed <- get_route_speed(gtfs, route_ids, unit = "m/s")
#' route_speed
#'
#' @export
get_route_speed <- function(gtfs,
                            route_id = NULL,
                            method = "shapes",
                            unit = "km/h") {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(route_id, null.ok = TRUE, any.missing = FALSE)

  gtfsio::assert_field_class(
    gtfs,
    "trips",
    c("trip_id", "route_id"),
    c("character", "character")
  )

  # method, unit and the other required fields are validated by
  # get_trip_speed(), which is called even when no trip is selected

  trip_id <- NULL

  if (!is.null(route_id)) {
    invalid_route_id <- unique(
      route_id[! route_id %chin% gtfs$trips$route_id]
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

    # select the trips of the given routes that have stop times, so that
    # get_trip_speed() doesn't warn about trips without them. a non-character
    # trip_id column in stop_times is reported by get_trip_speed()

    trip_id <- gtfs$trips$trip_id[gtfs$trips$route_id %chin% route_id]
    if (is.character(gtfs$stop_times$trip_id)) {
      trip_id <- trip_id[trip_id %chin% gtfs$stop_times$trip_id]
    }
  }

  speeds <- get_trip_speed(gtfs, trip_id, method = method, unit = unit)

  # drops trips not listed in 'trips' (NA route_id) and, with route_id given,
  # routes not requested (a trip_id listed more than once in 'trips' gets the
  # route_id of its last entry)

  if (is.null(route_id)) {
    relevant_routes <- speeds$route_id[!is.na(speeds$route_id)]
  } else {
    relevant_routes <- route_id
  }

  speeds <- speeds[route_id %chin% relevant_routes]
  routes <- data.table::data.table(route_id = unique(speeds$route_id))

  # the statistics are calculated only from the non-NA speeds, so that routes
  # whose trips all have NA speeds don't result in -Inf and warnings. these
  # routes get NA statistics from the join below

  valid_speeds <- speeds[!is.na(speed)]

  if (nrow(valid_speeds) > 0) {
    speed_stats <- valid_speeds[
      ,
      .(
        min_speed = min(speed),
        mean_speed = mean(speed),
        median_speed = stats::median(speed),
        max_speed = max(speed),
        sd_speed = stats::sd(speed)
      ),
      by = "route_id"
    ]
  } else {
    speed_stats <- data.table::data.table(
      route_id = character(0),
      min_speed = numeric(0),
      mean_speed = numeric(0),
      median_speed = numeric(0),
      max_speed = numeric(0),
      sd_speed = numeric(0)
    )
  }

  result <- speed_stats[routes, on = "route_id"]
  data.table::setorderv(result, "route_id")

  return(result[])
}
