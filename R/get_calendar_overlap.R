#' Get calendar overlap
#'
#' Returns the periods in which all the given GTFS feeds have service, or the
#' number of trips each feed runs on each of its service days. Either result
#' can also be plotted. Useful to pick a date on which several feeds can be
#' analysed together (e.g. when routing on the networks of different
#' agencies).
#'
#' @param gtfs Either a character vector with the paths to GTFS `.zip` files
#'   or a list of GTFS objects, as created by [read_gtfs()] (a single GTFS
#'   object must be wrapped in a list).
#' @param resolution A string. `"daily"` (the default) returns the number of
#'   trips of each feed on each of its service days, and `"periods"` the
#'   periods in which all feeds have service.
#' @param plot A logical. Whether to return a plot of the result instead of
#'   the result itself (requires the `{ggplot2}` package). Defaults to `FALSE`.
#'
#' @return If `plot = FALSE` and `resolution = "periods"`, a `data.table` with
#'   one row per period of consecutive days on which all feeds have service,
#'   and the columns `start_date`, `end_date` (both included in the period) and
#'   `n_days`. It has no rows if there is no such day.
#'
#'   If `plot = FALSE` and `resolution = "daily"`, a `data.table` with one row
#'   per feed and service day, and the columns `feed`, `date`, `n_trips` (the
#'   number of trips that run on the day) and `overlap` (whether all feeds have
#'   service on the day).
#'
#'   If `plot = TRUE`, a `ggplot` object with dates on the x axis and one row
#'   per feed (top to bottom in the given order). With
#'   `resolution = "periods"`, it shows a bar spanning each feed's service
#'   days. With `resolution = "daily"`, it shows a box plot of when each
#'   feed's trips run: the box spans the days by which a quarter and three
#'   quarters of its trips have run, with a line on the day by which half
#'   have, and the whiskers reach its first and last service days. In both,
#'   the overlap periods are shaded.
#'
#' @section Details:
#' A feed has service on a day if at least one of its services runs on it. A
#' service runs on the days between the `start_date` and `end_date` of its
#' `calendar` entry whose weekday is flagged, minus the days removed from it
#' in `calendar_dates` (`exception_type` 2), plus the days added to it
#' (`exception_type` 1). If a service is both added and removed on a day, the
#' addition wins. Either table may be missing. If the feed has a `trips`
#' table with a `service_id` field, services not used by any trip are ignored.
#' `feed_info` dates are not used. A warning is raised for feeds without any
#' service day, and a message when two or more feeds have no day with service
#' in all of them.
#'
#' The number of trips on a day is the number of trips of the services that
#' run on it. A trip listed in `frequencies` counts once per departure, every
#' `headway_secs` from `start_time` until (but not including) `end_time` (an
#' entry whose `start_time` and `end_time` are equal departs once), and
#' the departures of overlapping entries of the same trip are added up
#' (identical entries count once), unlike in [frequencies_to_stop_times()],
#' which keeps only their unique departures. A trip with an invalid
#' `frequencies` entry counts once, with a warning. Trips count on their
#' service day, so departures after midnight (with times later than
#' `"24:00:00"`) count on the previous day. If the feed
#' has no `trips` table with a `service_id` field, `n_trips` is `NA`, and the
#' daily plot shows no box for it.
#'
#' Feeds are named after their file names (without the extension) or after the
#' list names. Unnamed feeds are named after their position (`"feed_1"`,
#' `"feed_2"`, etc.), and duplicated names are made unique. Plots of feeds
#' spanning long periods can be zoomed in with [ggplot2::coord_cartesian()].
#' The days on which all feeds have service are shaded in the plots only when
#' two or more feeds are given.
#'
#' @seealso [merge_gtfs()], [filter_by_service_id()]
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' poa_path <- system.file("extdata/poa_gtfs.zip", package = "gtfstools")
#'
#' # feeds may be given as paths to their .zip files
#' daily_trips <- get_calendar_overlap(c(spo_path, poa_path))
#' head(daily_trips)
#'
#' # or as a list of GTFS objects, whose names are used as feed names
#' feeds <- list(spo = read_gtfs(spo_path), poa = read_gtfs(poa_path))
#' get_calendar_overlap(feeds, resolution = "periods")
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   get_calendar_overlap(feeds, plot = TRUE)
#' }
#'
#' @export
get_calendar_overlap <- function(gtfs, resolution = "daily", plot = FALSE) {
  checkmate::assert(
    checkmate::check_character(gtfs, min.len = 1, any.missing = FALSE),
    checkmate::check_list(gtfs, types = "gtfs", min.len = 1),
    .var.name = "gtfs"
  )
  checkmate::assert_string(resolution)
  checkmate::assert_names(resolution, subset.of = c("periods", "daily"))
  checkmate::assert_flag(plot)

  if (plot && !requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "The {.pkg ggplot2} package is required when {.code plot = TRUE}.",
      class = "gtfstools_missing_ggplot2_error"
    )
  }

  # feeds given as paths are named after their files and read with only the
  # tables used below. 'files' errors when a listed file is missing, so it
  # lists only the tables each zip has (a long 'skip' list is slower). a path
  # that can't be listed lists none, and read_gtfs() raises its usual error

  if (is.character(gtfs)) {
    checkmate::assert_file_exists(gtfs)
    feed_names <- sub("\\.[^.]*$", "", basename(gtfs))
    needed_files <- c("calendar", "calendar_dates", "trips", "frequencies")
    feeds <- lapply(
      gtfs,
      function(path) {
        entries <- tryCatch(
          zip::zip_list(path)$filename,
          error = function(cnd) character(0)
        )
        tables <- sub("\\.txt$", "", entries[endsWith(entries, ".txt")])
        read_gtfs(path, files = tables[basename(tables) %chin% needed_files])
      }
    )
  } else {
    feed_names <- names(gtfs)
    if (is.null(feed_names)) {
      feed_names <- character(length(gtfs))
    }
    feeds <- lapply(gtfs, assert_and_assign_gtfs_object)
  }

  unnamed <- feed_names %in% c("", NA)
  feed_names[unnamed] <- paste0("feed_", which(unnamed))
  feed_names <- make.unique(feed_names, sep = "_")

  service_days <- lapply(
    feeds,
    get_service_days,
    count_trips = resolution == "daily"
  )
  names(service_days) <- feed_names

  no_service <- feed_names[vapply(service_days, nrow, integer(1)) == 0L]
  if (length(no_service) > 0L) {
    cli::cli_warn(
      "Feed{?s} {.val {no_service}} ha{?s/ve} no service days.",
      class = "gtfstools_no_service_days"
    )
  }

  # days are intersected as integers, which is much faster than as dates

  overlap_days <- Reduce(
    intersect,
    lapply(service_days, function(days) as.integer(days$date))
  )
  overlap <- dates_to_periods(as.Date(overlap_days, origin = "1970-01-01"))

  if (length(feeds) > 1L && nrow(overlap) == 0L) {
    cli::cli_inform(
      "No day has service in all feeds.",
      class = "gtfstools_no_calendar_overlap"
    )
  }

  if (resolution == "periods" && !plot) {
    return(overlap)
  }

  if (resolution == "daily") {
    daily_trips <- data.table::rbindlist(service_days, idcol = "feed")
    daily_trips[, overlap := as.integer(date) %in% overlap_days]

    if (!plot) return(daily_trips[])
  }

  # colours are okabe-ito orange (overlap) and blue (feeds). a single feed
  # overlaps with itself, so its overlap isn't shaded (the shading layer is
  # kept empty, so that the layers keep their positions)

  shaded_days <- overlap
  if (length(feeds) == 1L) {
    shaded_days <- overlap[0L]
  }

  if (resolution == "periods") {
    feed_periods <- data.table::rbindlist(
      lapply(service_days, function(days) dates_to_periods(days$date)),
      idcol = "feed"
    )

    # the first feed is plotted at the top. tiles span from the first day of
    # a period to the end of its last day, so that one-day periods are
    # visible

    overlap_plot <- ggplot2::ggplot() +
      ggplot2::geom_rect(
        data = shaded_days,
        ggplot2::aes(xmin = start_date, xmax = end_date + 1),
        ymin = -Inf,
        ymax = Inf,
        fill = "#E69F00",
        alpha = 0.4
      ) +
      ggplot2::geom_tile(
        data = feed_periods,
        ggplot2::aes(x = start_date + n_days / 2, y = feed, width = n_days),
        height = 0.6,
        fill = "#0072B2"
      ) +
      ggplot2::labs(x = "Date", y = NULL)
  } else {
    # each feed's box shows when its trips run: its quartiles are the days by
    # which a quarter, half and three quarters of its trips have run, and its
    # whiskers its first and last service days. they are computed here because
    # weighted boxes in ggplot2 require {quantreg}. days are sorted within
    # each feed, as returned by get_service_days()

    trip_counts <- daily_trips[!is.na(n_trips)]
    trip_counts[, trip_share := cumsum(n_trips) / sum(n_trips), by = feed]
    trip_quartiles <- trip_counts[,
      .(
        first_day = date[1L],
        lower_day = date[match(TRUE, trip_share >= 0.25)],
        median_day = date[match(TRUE, trip_share >= 0.5)],
        upper_day = date[match(TRUE, trip_share >= 0.75)],
        last_day = date[.N]
      ),
      by = feed
    ]

    overlap_plot <- ggplot2::ggplot() +
      ggplot2::geom_rect(
        data = shaded_days,
        ggplot2::aes(xmin = start_date - 0.5, xmax = end_date + 0.5),
        ymin = -Inf,
        ymax = Inf,
        fill = "#E69F00",
        alpha = 0.4
      ) +
      ggplot2::geom_boxplot(
        data = trip_quartiles,
        ggplot2::aes(
          y = feed,
          xmin = first_day,
          xlower = lower_day,
          xmiddle = median_day,
          xupper = upper_day,
          xmax = last_day
        ),
        stat = "identity",
        colour = "#0072B2",
        fill = "#0072B2",
        alpha = 0.3,
        width = 0.6
      ) +
      ggplot2::scale_x_date() +
      ggplot2::labs(x = "Date", y = NULL)
  }

  # feeds without trip counts or service days keep their place on the axis

  overlap_plot <- overlap_plot +
    ggplot2::scale_y_discrete(limits = rev(feed_names)) +
    ggplot2::theme_minimal()

  if (length(feeds) > 1L) {
    overlap_plot <- overlap_plot +
      ggplot2::labs(
        subtitle = "Shaded area in yellow: days on which all feeds have service"
      )
  }

  return(overlap_plot)
}


#' Collapse days into periods of consecutive days
#'
#' @param dates A sorted `Date` vector of unique days.
#'
#' @return A `data.table` with the columns `start_date`, `end_date` and
#'   `n_days`, with one row per period of consecutive days.
#'
#' @keywords internal
dates_to_periods <- function(dates) {
  # in sorted unique dates, the date minus its position is constant within
  # each period of consecutive days

  period_id <- as.integer(dates) - seq_along(dates)
  start_date <- dates[!duplicated(period_id)]
  end_date <- dates[!duplicated(period_id, fromLast = TRUE)]

  periods <- data.table::data.table(
    start_date = start_date,
    end_date = end_date,
    n_days = as.integer(end_date - start_date) + 1L
  )

  return(periods)
}
