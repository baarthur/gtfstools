#' Convert time string to seconds after midnight
#'
#' Converts strings in the "HH:MM:SS" format to seconds after midnight.
#'
#' @param string A string in "HH:MM:SS" format.
#'
#' @return The seconds after midnight of a given time string as an integer.
#'
#' @keywords internal
string_to_seconds <- function(string) {
  checkmate::assert_character(string)

  # time columns have few distinct values, so each one is converted only once
  # and the result is expanded back to the full length

  unique_strings <- unique(string)
  idx <- match(string, unique_strings)
  unique_seconds <- cpp_time_to_seconds(unique_strings)

  bad <- is.na(unique_seconds) & !is.na(unique_strings)
  if (any(bad)) bad[bad] <- !trimws(unique_strings[bad]) %in% c("", "NA")
  n_bad <- sum(bad[idx])
  if (n_bad > 0) {
    cli::cli_warn(
      "{n_bad} malformed time string{?s} (not H:MM:SS) converted to NA.",
      class = "gtfstools_malformed_time"
    )
  }

  seconds_from_midnight <- unique_seconds[idx]

  return(seconds_from_midnight)
}



#' Convert seconds after midnight to time string
#'
#' Converts seconds after midnight as integers to strings in the "HH:MM:SS"
#' format.
#'
#' @param seconds An integer.
#'
#' @return A time-representing string.
#'
#' @keywords internal
seconds_to_string <- function(seconds) {
  checkmate::assert_integer(seconds, lower = 0)

  # format each distinct value only once, then expand back to the full length

  unique_seconds <- unique(seconds)
  time_string <- cpp_seconds_to_string(unique_seconds)
  time_string <- time_string[match(seconds, unique_seconds)]

  return(time_string)
}



#' Make a GTFS copy without a given file
#'
#' Creates a copy of a GTFS object without a given file. Used for testing.
#'
#' @template gtfs
#' @param file File to be removed.
#'
#' @return A GTFS object without the given file.
#'
#' @keywords internal
copy_gtfs_without_file <- function(gtfs, file) {

  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_string(file)

  # check if file exists

  gtfsio::assert_file_exists(gtfs, file)

  # remove file

  gtfs_copy <- gtfs
  gtfs_copy[[file]] <- NULL

  return(gtfs_copy)

}



#' Make a GTFS copy without a given field from a given file
#'
#' Creates a copy of a GTFS object without a given field from a given file. Used
#' for testing.
#'
#' @template gtfs
#' @param file File from which the field must be removed.
#' @param field Field to be removed.
#'
#' @return A GTFS object without the given field.
#'
#' @keywords internal
copy_gtfs_without_field <- function(gtfs, file, field) {

  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_string(file)
  checkmate::assert_string(field)

  # check if field exists

  gtfsio::assert_field_exists(gtfs, file, field)

  # remove field

  gtfs_copy <- gtfs
  gtfs_copy[[file]] <- data.table::copy(gtfs[[file]])
  gtfs_copy[[file]][, (field) := NULL]

  return(gtfs_copy)

}



#' Make a GTFS copy with a field of a different class
#'
#' Creates a copy of a GTFS object while changing the class of a given field.
#' Used for testing.
#'
#' @template gtfs
#' @param file File whose field must have the class changed.
#' @param field Field to have the class changed.
#' @param class The desired class.
#'
#' @return A GTFS object with the field of desired class.
#'
#' @keywords internal
copy_gtfs_diff_field_class <- function(gtfs, file, field, class) {

  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_string(file)
  checkmate::assert_string(field)
  checkmate::assert_string(class)

  # check if field exists

  gtfsio::assert_field_exists(gtfs, file, field)

  # select the coercion function to use

  if (class == "factor")
    fn <- as.factor
  if (class == "character")
    fn <- as.character

  # change field class

  gtfs_copy <- gtfs
  gtfs_copy[[file]] <- data.table::copy(gtfs[[file]])
  gtfs_copy[[file]][, get("field") := fn(eval(parse(text = field)))]

  return(gtfs_copy)

}



#' @keywords internal
assert_and_assign_gtfs_object <- function(gtfs) {
  checkmate::assert_class(gtfs, "gtfs")

  if (!inherits(gtfs, "dt_gtfs")) gtfs <- as_dt_gtfs(gtfs)

  return(gtfs)
}



#' Warn about ids missing from a table
#'
#' Raises a warning listing the elements of `id` not found in `existing_id`.
#' The warning is attributed to the function that called this helper.
#'
#' @param id A character vector of ids given by the user.
#' @param existing_id A character vector of the ids that exist in the table.
#' @param table The name of the table, used in the warning message.
#' @param id_name The name of the id field, used in the warning message.
#'
#' @return The missing ids, invisibly.
#'
#' @keywords internal
warn_missing_ids <- function(id, existing_id, table, id_name) {
  invalid_id <- id[! id %chin% existing_id]

  if (length(invalid_id) > 0) {
    msg <- paste0(
      "'", table, "' doesn't contain the following ", id_name, "(s): ",
      paste0("'", invalid_id, "'", collapse = ", ")
    )
    warning(simpleWarning(msg, call = sys.call(-1)))
  }

  return(invisible(invalid_id))
}



#' Warn about ids missing from a table, with cli
#'
#' The cli counterpart of [warn_missing_ids()], whose base `warning()` text
#' the older callers keep. Raises a warning of class
#' `gtfstools_invalid_<id_name>` listing the distinct elements of `id` not
#' found in `existing_id`.
#'
#' @param id A character vector of ids given by the user, or `NULL`.
#' @param existing_id A character vector of the ids that exist in the table.
#' @param table The name of the table, used in the warning message.
#' @param id_name The name of the id field, used in the warning message and
#'   in the condition class.
#'
#' @return The distinct missing ids, invisibly (`NULL` if `id` is `NULL`).
#'
#' @keywords internal
warn_invalid_ids <- function(id, existing_id, table, id_name) {
  invalid_id <- unique(id[! id %chin% existing_id])

  # the table and field names are pasted in as text, not interpolated, so
  # that {?s} takes its quantity from the ids and not from these scalars

  if (length(invalid_id) > 0) {
    cli::cli_warn(
      paste0(
        "{.file ", table, "} doesn't contain the following ", id_name,
        "{?s}: {.val {invalid_id}}."
      ),
      class = paste0("gtfstools_invalid_", id_name)
    )
  }

  return(invisible(invalid_id))
}



#' Refresh the `*_secs` columns of a table after its time strings changed
#'
#' Functions that rewrite time strings keep any pre-existing `<time>_secs`
#' columns in sync, because other functions use these columns as-is (see
#' [convert_time_to_seconds()]). Columns that don't exist are skipped.
#'
#' @param dt A `data.table`, modified by reference.
#' @param time_cols A character vector with the names of the time string
#'   columns (e.g. `"departure_time"`).
#' @param rows An integer vector with the rows whose times changed, or `NULL`
#'   for all rows.
#'
#' @return The table, invisibly.
#'
#' @keywords internal
refresh_secs_columns <- function(dt, time_cols, rows = NULL) {
  for (time_col in time_cols) {
    secs_col <- paste0(time_col, "_secs")
    if (!secs_col %chin% names(dt)) next

    times <- dt[[time_col]]
    if (!is.null(rows)) times <- times[rows]

    data.table::set(
      dt,
      i = rows,
      j = secs_col,
      value = string_to_seconds(times)
    )
  }

  return(invisible(dt))
}

#' Raise the deprecation condition of the `sort_sequence` argument
#'
#' `sort_sequence` was deprecated after gtfstools 1.4.0: the functions that
#' took it now always sort by the sequence columns. Passing `TRUE` raises a
#' warning, and passing `FALSE` raises an error.
#'
#' @param sort_sequence The value passed by the user.
#' @param fn_name The name of the function the argument was passed to.
#' @param columns The sequence columns the function sorts by.
#'
#' @return Invisibly returns `NULL`, if it doesn't raise an error.
#'
#' @keywords internal
check_deprecated_sort_sequence <- function(sort_sequence, fn_name, columns) {
  checkmate::assert_flag(sort_sequence, .var.name = "sort_sequence")

  if (!sort_sequence) {
    cli::cli_abort(
      class = "gtfstools_defunct_sort_sequence_error",
      call = parent.frame(),
      message = c(
        "{.code sort_sequence = FALSE} is no longer supported.",
        "i" = paste0(
          "{.fn {fn_name}} now always orders the data by {.field {columns}}, ",
          "so {cli::qty(columns)}th{?is/ese} column{?s} {?is/are} required."
        ),
        "i" = "Please remove {.arg sort_sequence} from your call."
      )
    )
  }

  cli::cli_warn(
    class = "deprecated_sort_sequence",
    call = parent.frame(),
    message = c(
      paste0(
        "The {.arg sort_sequence} argument of {.fn {fn_name}} is deprecated ",
        "and ignored."
      ),
      "i" = "{.fn {fn_name}} now always orders the data by {.field {columns}}.",
      "i" = "Please remove {.arg sort_sequence} from your call."
    )
  )

  return(invisible(NULL))
}

#' Get the `route_id`s of the given `route_type`s
#'
#' Returns the `route_id`s listed in `routes` with any of the given
#' `route_type`s. Types not listed in `routes` are ignored. Also checks the
#' `trips` table, which the callers use to get the trips of these routes, so
#' that a feed without it raises an error instead of selecting nothing.
#'
#' @template gtfs
#' @param route_type An integer vector of `route_type`s.
#'
#' @return A character vector of `route_id`s.
#'
#' @keywords internal
route_type_ids <- function(gtfs, route_type) {
  gtfsio::assert_field_class(
    gtfs,
    "routes",
    c("route_id", "route_type"),
    c("character", "integer")
  )
  gtfsio::assert_field_class(
    gtfs,
    "trips",
    c("trip_id", "route_id"),
    c("character", "character")
  )

  return(gtfs$routes$route_id[gtfs$routes$route_type %in% route_type])
}
