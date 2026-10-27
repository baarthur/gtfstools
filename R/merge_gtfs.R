#' Merge GTFS files
#'
#' Combines many GTFS objects into a single one.
#'
#' @param ... GTFS objects to be merged. Each argument can either be a GTFS or
#' a list of GTFS objects.
#' @param files A character vector listing the GTFS tables to be merged. If
#' `NULL` (the default), all tables are merged. Elements that are not tables,
#' such as the `locations` of GTFS-Flex feeds (read from a geojson file), are
#' not merged and are left out of the result, with a warning.
#' @param prefix Either a logical or a character vector (defaults to `FALSE`).
#' Whether to add a prefix to the value of id fields that identify from which
#' GTFS object the value comes from. If `TRUE`, the prefixes will range from
#' `"1"` to `n`, where `n` is the number of objects passed to the function. If a
#' character vector, its elements, which must be unique, will be used to
#' identify the GTFS objects, and the length of the vector must equal the total
#' amount of objects passed in `...` (the first element will identify the first
#' GTFS, the second element the second GTFS, and so on). If `FALSE` and two or
#' more GTFS objects are merged, rows that are identical in every column are
#' kept only once (as in [remove_duplicates()], also when they come from the
#' same GTFS object; a column that a GTFS object lacks counts as empty), and
#' the GTFS objects may share ids (e.g. the same stop in two feeds) only if
#' their data are identical in all of them: rows are compared on all columns,
#' including columns that only some GTFS objects have, and a trip, for example,
#' must also have the same `stop_times` and `frequencies`. Otherwise an error is
#' raised; use `prefix = TRUE` in that case. This happens with versions of the
#' same feed, but also with unrelated feeds that use simple ids (e.g. both have
#' an `agency_id` `"1"`).
#'
#' @return A GTFS object in which each table is a combination (by row) of the
#' tables from the specified GTFS objects.
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
#'
#' spo_gtfs <- read_gtfs(spo_path)
#' names(spo_gtfs)
#'
#' ggl_gtfs <- read_gtfs(ggl_path)
#' names(ggl_gtfs)
#'
#' merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs)
#' names(merged_gtfs)
#'
#' # use a list() to programatically merge many GTFS objects
#' gtfs_list <- list(spo_gtfs, ggl_gtfs)
#' merged_gtfs <- merge_gtfs(gtfs_list)
#'
#' # 'prefix' helps disambiguating from which GTFS each id comes from.
#' # if TRUE, the ids range from 1:n, where n is the number of gtfs
#' merged_gtfs <- merge_gtfs(gtfs_list, prefix = TRUE)
#' merged_gtfs$agency
#'
#' # if a character vector, its elements will be used to identify the each gtfs
#' merged_gtfs <- merge_gtfs(gtfs_list, prefix = c("spo", "ggl"))
#' merged_gtfs$agency
#' @export
merge_gtfs <- function(..., files = NULL, prefix = FALSE) {
  checkmate::assert_character(files, null.ok = TRUE, any.missing = FALSE)

  # store gtfs objects in a list and unlist eventual lists of gtfs

  gtfs_objects <- list(...)

  is_list_of_gtfs <- vapply(gtfs_objects, is_gtfs_list, logical(1))

  gtfs_objects <- append(
    gtfs_objects[!is_list_of_gtfs],
    unlist(gtfs_objects[is_list_of_gtfs], recursive = FALSE)
  )
  gtfs_objects <- lapply(gtfs_objects, assert_and_assign_gtfs_object)

  # the objects are identified by their position in 'origin_gtfs', which
  # rbindlist() would fill with their names instead, if they had any

  gtfs_objects <- unname(gtfs_objects)

  # check if prefix is either a logical or a character vector with length equal
  # to the number of provided gtfs objects

  gtfs_count <- length(gtfs_objects)

  checkmate::assert(
    checkmate::check_logical(prefix, len = 1, any.missing = FALSE),
    checkmate::check_character(
      prefix,
      len = gtfs_count,
      any.missing = FALSE,
      unique = TRUE
    ),
    combine = "or"
  )

  if (is.logical(prefix)) {
    list_of_prefixes <- as.character(seq.int(gtfs_count))
  } else {
    list_of_prefixes <- prefix
  }

  # only merge the tables specified by 'files'
  # if 'files' is NULL, then merge all possible files.
  # * with the exception of the '.' element, that may exist in tidygtfs objects
  # if not, check if the GTFS to be merged contain the files.
  # * if some of them are missing, throw a warning.
  # * if all of them are missing, throw an error.

  files_within_gtfs <- unlist(lapply(gtfs_objects, names))
  files_within_gtfs <- unique(files_within_gtfs)
  files_to_merge <- setdiff(files_within_gtfs, ".")

  if (!is.null(files)) {
    invalid_files <- files[!files %chin% files_within_gtfs]
    files_to_merge <- files[!files %chin% invalid_files]
  }

  # elements that are not tables, such as the 'locations' of GTFS-Flex feeds
  # (read from a geojson file as a list), can't be merged with rbindlist(), so
  # they are left out of the merged feed

  is_table <- vapply(
    files_to_merge,
    function(file) {
      all(
        vapply(
          gtfs_objects,
          function(g) is.null(g[[file]]) || is.data.frame(g[[file]]),
          logical(1)
        )
      )
    },
    logical(1)
  )

  if (!all(is_table)) {
    unmerged_files <- files_to_merge[!is_table]
    files_to_merge <- files_to_merge[is_table]

    cli::cli_warn(
      paste0(
        "The following element{?s} {?is/are} not {?a table/tables} and ",
        "{?was/were} not merged: {.file {unmerged_files}}."
      ),
      class = "gtfstools_unmerged_files"
    )
  }

  if (!is.null(files)) {
    if (identical(files_to_merge, character(0))) {
      stop(
        "None of the GTFS to be merged contain the files passed to 'files'"
      )
    }

    if (!identical(invalid_files, character(0))) {
      warning(
        "None of the GTFS to be merged contain the following files: ",
        paste0("'", invalid_files, "'", collapse = ", ")
      )
    }
  }

  # merge specified files using rbindlist - loop over each file to merge, then
  # use rbindlist to bind all correspondent data.tables to a single data.table.

  merged_gtfs <- lapply(
    files_to_merge,
    function(file) {
      data.table::rbindlist(
        lapply(gtfs_objects, function(gtfs_object) gtfs_object[[file]]),
        fill = TRUE,
        idcol = "origin_gtfs"
      )
    }
  )

  # using fill = TRUE in data.table::rbindlist introduces some NA_character.
  # change those to "" to keep consistency (empty character entries are read as
  # "" by data.table::fread)

  for (dt in seq_along(merged_gtfs)) {
    # determine the character columns in each data.table

    charac_cols <- which(vapply(merged_gtfs[[dt]], is.character, logical(1)))

    # then substitute NA by ""

    for (col in charac_cols) {
      data.table::set(
        merged_gtfs[[dt]],
        i = which(is.na(merged_gtfs[[dt]][[col]])),
        j = col,
        value = ""
      )
    }
  }

  # without prefixes, rows that are identical in every column are kept only
  # once, and ids shared between gtfs objects must have exactly the same data in
  # all of them. otherwise the merged feed would be inconsistent (e.g. a trip_id
  # pointing to the stop_times of two different trips), so an error is raised.
  # each id is checked in the tables whose rows belong to it (e.g. a trip's
  # stop_times and frequencies), so that a trip whose stop_times differ, or
  # are a subset of another feed's, is caught. an id conflicts when one of its
  # rows is missing from some of the gtfs objects that have the id.
  # tables keyed by a combination of fields with no owning id (e.g. transfers,
  # translations) are deduplicated but not checked, and neither are blank ids,
  # as in previous versions. the check assumes plain (non-list) columns

  if (isFALSE(prefix) && gtfs_count > 1) {
    key_tables <- list(
      agency_id = "agency",
      stop_id = "stops",
      route_id = "routes",
      trip_id = c("trips", "stop_times", "frequencies"),
      service_id = c("calendar", "calendar_dates"),
      shape_id = "shapes",
      fare_id = c("fare_attributes", "fare_rules"),
      level_id = "levels",
      pathway_id = "pathways",
      attribution_id = "attributions",
      network_id = c("networks", "route_networks"),
      area_id = c("areas", "stop_areas"),
      fare_media_id = "fare_media",
      fare_product_id = "fare_products",
      rider_category_id = "rider_categories",
      timeframe_group_id = "timeframes",
      location_group_id = c("location_groups", "location_group_stops"),
      booking_rule_id = "booking_rules"
    )

    # count in how many gtfs objects each distinct row appears. identical rows
    # get the same dense rank, which is faster than grouping by every column
    # and also tells which rows to keep at the end. the counts are kept apart
    # from the tables, so that they can't clash with a column of the feeds

    # empty files are read as tables without columns, which have nothing to
    # deduplicate. their entries stay NULL: they are left out of the conflict
    # check and keep all of their rows

    n_origins <- vector("list", length(merged_gtfs))
    first_rows <- vector("list", length(merged_gtfs))

    for (i in seq_along(merged_gtfs)) {
      dt <- merged_gtfs[[i]]
      data_cols <- setdiff(names(dt), "origin_gtfs")
      if (length(data_cols) == 0) next

      row_group <- data.table::frankv(
        dt,
        cols = data_cols,
        ties.method = "dense",
        na.last = TRUE
      )
      in_origin <- !duplicated(
        data.table::data.table(dt$origin_gtfs, row_group)
      )
      n_origins[[i]] <- tabulate(row_group[in_origin], nrow(dt))[row_group]
      first_rows[[i]] <- !duplicated(row_group)
    }

    is_conflicting <- vapply(
      names(key_tables),
      function(field) {
        tables <- match(key_tables[[field]], files_to_merge, 0)
        tables <- tables[tables > 0]
        tables <- tables[
          vapply(
            tables,
            function(i) {
              !is.null(n_origins[[i]]) && field %chin% names(merged_gtfs[[i]])
            },
            logical(1)
          )
        ]
        if (length(tables) == 0) {
          return(FALSE)
        }

        # the id columns are bound before being converted to character, so
        # that ids stored as different types in different tables (e.g. integer
        # and double) are converted alike

        id <- data.table::rbindlist(
          lapply(tables, function(i) list(merged_gtfs[[i]][[field]]))
        )[[1]]
        id <- as.character(id)
        origin <- unlist(
          lapply(tables, function(i) merged_gtfs[[i]]$origin_gtfs)
        )
        n <- unlist(n_origins[tables])

        valid <- which(!is.na(id) & id != "")
        id <- id[valid]
        id_code <- data.table::chmatch(id, id)
        in_origin <- !duplicated(data.table::data.table(origin[valid], id_code))
        n_id <- tabulate(id_code[in_origin], length(id))

        any(n[valid] < n_id[id_code])
      },
      logical(1)
    )

    if (any(is_conflicting)) {
      shared_fields <- names(key_tables)[is_conflicting]

      cli::cli_abort(
        c(
          "Some ids are used by more than one of the GTFS objects to merge.",
          "x" = paste0(
            "Shared ids with different data found in ",
            "{.field {shared_fields}}."
          ),
          "i" = paste0(
            "Use {.code prefix = TRUE} (or a character vector of prefixes) to ",
            "make the ids of each GTFS object unique."
          )
        ),
        class = "gtfstools_duplicated_ids_error"
      )
    }

    # the rows to keep are computed outside the brackets: inside them,
    # data.table would look for names such as 'first_rows' among the columns
    # of the feeds first

    for (i in seq_along(merged_gtfs)) {
      if (!all(first_rows[[i]])) {
        keep <- which(first_rows[[i]])
        merged_gtfs[[i]] <- merged_gtfs[[i]][keep]
      }
    }
  }

  # if prefix is TRUE or a list of prefixes, then add the prefixes to the ids.
  # some fields that end with "_id" should not be changed (e.g. direction_id in
  # trips, which is actually an enum, not an id).

  unchanged_ids <- c("direction_id")

  if (!isFALSE(prefix)) {
    for (dt in merged_gtfs) {
      # determine the id columns (or columns that refer to ids) in each table.
      # direction_id, from trips, ends with _id but is an enum and should not be
      # changed. in translations, record_sub_id (a stop_sequence) and
      # field_value (a field's value) are not ids and should not be changed.

      id_cols <- names(dt)[grepl("_id$|parent_station", names(dt))]
      id_cols <- setdiff(id_cols, c("direction_id", "record_sub_id"))

      # then add the prefix to the id value. ids repeat across many rows (e.g.
      # trip_id in stop_times), so the prefixed id is pasted only once for
      # each distinct pair of gtfs object and id

      for (col in id_cols) {
        rows <- which(dt[[col]] != "")
        if (length(rows) == 0L) next

        ids <- dt[[col]][rows]
        origin <- dt$origin_gtfs[rows]
        id_group <- data.table::frankv(list(origin, ids), ties.method = "dense")
        group_first <- which(!duplicated(id_group))

        prefixed <- character(length(group_first))
        prefixed[id_group[group_first]] <- paste0(
          list_of_prefixes[origin[group_first]],
          "_",
          ids[group_first]
        )
        data.table::set(dt, i = rows, j = col, value = prefixed[id_group])
      }
    }
  }

  for (dt in merged_gtfs) {
    if ("origin_gtfs" %chin% names(dt)) dt[, origin_gtfs := NULL]
  }

  # create a gtfs object from the current list of gtfs tables in merged_gtfs

  names(merged_gtfs) <- files_to_merge
  merged_gtfs <- gtfsio::new_gtfs(merged_gtfs, "dt_gtfs")

  return(merged_gtfs)
}


is_gtfs_list <- function(x) {
  is_gtfs_object <- vapply(
    x,
    function(i) checkmate::test_class(i, "gtfs"),
    logical(1)
  )
  all(is_gtfs_object)
}
