spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")

spo_gtfs <- read_gtfs(spo_path)
ggl_gtfs <- read_gtfs(ggl_path)


# tests -------------------------------------------------------------------


test_that("raises errors due to incorrect input types", {
  expect_error(merge_gtfs("spo_gtfs", ggl_gtfs))
  expect_error(merge_gtfs(spo_gtfs, "ggl_gtfs"))
  expect_error(merge_gtfs(spo_gtfs, ggl_gtfs, files = 1))
  expect_error(merge_gtfs(spo_gtfs, ggl_gtfs, files = NA))
  expect_error(merge_gtfs(spo_gtfs, ggl_gtfs, prefix = 1))
  expect_error(merge_gtfs(spo_gtfs, ggl_gtfs, prefix = NA))
  expect_error(merge_gtfs(spo_gtfs, ggl_gtfs, prefix = "oi"))
  expect_error(merge_gtfs(spo_gtfs, ggl_gtfs, prefix = c("oi", NA)))
  expect_error(
    merge_gtfs(spo_gtfs, spo_gtfs, prefix = c("oi", "oi")),
    regexp = "duplicated"
  )
})

test_that("raises errors/warnings due to unavailable files passed to 'files'", {

  # should throw a warning if a specified file doesn't exist
  expect_warning(
    merge_gtfs(spo_gtfs, ggl_gtfs, files = c("shapes", "ola", "oie"))
  )

  # should throw an error when none of the specified files exist
  expect_error(merge_gtfs(spo_gtfs, ggl_gtfs, files = c("ola", "oie")))

  # should run silently otherwise
  expect_silent(merge_gtfs(spo_gtfs, ggl_gtfs))
})

test_that("results in a GTFS object", {
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")

  # should work when files = NULL
  expect_s3_class(merge_gtfs(spo_gtfs, ggl_gtfs), dt_gtfs_class, exact = TRUE)
  expect_s3_class(
    merge_gtfs(list(spo_gtfs, ggl_gtfs)),
    dt_gtfs_class,
    exact = TRUE
  )

  # and should also work when files = something else
  expect_s3_class(
    merge_gtfs(spo_gtfs, ggl_gtfs, files = "shapes"),
    dt_gtfs_class,
    exact = TRUE
  )
  expect_s3_class(
    merge_gtfs(list(spo_gtfs, ggl_gtfs), files = "shapes"),
    dt_gtfs_class,
    exact = TRUE
  )

  # even if a non-existent file is passed to 'files' (but not all of them)
  expect_s3_class(
    suppressWarnings(
      merge_gtfs(list(spo_gtfs, ggl_gtfs), files = c("shapes", "ola", "oie")),
    ),
    dt_gtfs_class,
    exact = TRUE
  )
})

test_that("merges the adequate 'files'", {

  # when files = NULL all files from all gtfs should be merged

  spo_names <- names(spo_gtfs)
  ggl_names <- names(ggl_gtfs)
  all_names <- c(spo_names, ggl_names)
  all_names <- unique(all_names[order(all_names)])

  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs)
  merged_gtfs_names <- names(merged_gtfs)[order(names(merged_gtfs))]

  expect_identical(all_names, merged_gtfs_names)

  # should also work if GTFS objects are passed as list

  merged_gtfs_list <- merge_gtfs(list(spo_gtfs, ggl_gtfs))
  merged_gtfs_lnames <- names(merged_gtfs_list)[order(names(merged_gtfs_list))]

  expect_identical(all_names, merged_gtfs_lnames)

  # when files = something else, only the specified files should be merged

  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs, files = "shapes")
  expect_identical(names(merged_gtfs), "shapes")

  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs, files = c("shapes", "stops"))
  expect_identical(names(merged_gtfs), c("shapes", "stops"))

  # should work even if a non-existant file is passed to 'files'

  expect_warning(
    merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs, files = c("shapes", "oie"))
  )
  expect_identical(names(merged_gtfs), "shapes")
})

test_that("bind the rows of each GTFS object adequately", {
  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs)
  merged_gtfs_names <- names(merged_gtfs)

  # each data.table in the final GTFS object should be the "sum" of the
  # data.tables of same name in the original GTFS objs (without identical rows,
  # which are kept only once when prefix = FALSE)

  prefixed_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs, prefix = TRUE)

  for (filename in merged_gtfs_names){
    expect_equal(
      nrow(prefixed_gtfs[[filename]]),
      nrow(rbind(spo_gtfs[[filename]], ggl_gtfs[[filename]], fill = TRUE))
    )
  }

  # but rbind.data.table() fill = TRUE results in some character columns with a
  # NA_character_ where a "" is expected
  # test if after replacing these values the data.tables are identical

  for (filename in merged_gtfs_names){
    merged_by_hand <- rbind(
      spo_gtfs[[filename]],
      ggl_gtfs[[filename]],
      fill = TRUE
    )
    col_classes  <- vapply(merged_by_hand, class, character(1))
    is_char <- which(col_classes == "character")

    for (col in is_char) {
      data.table::set(
        merged_by_hand,
        i = which(is.na(merged_by_hand[[col]])),
        j = col,
        value = ""
      )
    }

    expect_equal(merged_gtfs[[filename]], unique(merged_by_hand))
  }

})

test_that("does not change the original GTFS objects", {
  original_spo_gtfs <- read_gtfs(spo_path)
  original_ggl_gtfs <- read_gtfs(ggl_path)
  spo_gtfs <- read_gtfs(spo_path)
  ggl_gtfs <- read_gtfs(ggl_path)
  expect_identical(spo_gtfs, original_spo_gtfs)

  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs)
  merged_gtfs <- merge_gtfs(spo_gtfs, spo_gtfs)
  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs, prefix = TRUE)

  expect_equal(spo_gtfs, original_spo_gtfs, ignore_attr = TRUE)
  expect_equal(ggl_gtfs, original_ggl_gtfs, ignore_attr = TRUE)
})

test_that("prefix argument works correctly", {
  # retrieve id fields in both gtfs

  all_fields <- lapply(
    list(spo_gtfs, ggl_gtfs),
    function(gtfs) unlist(lapply(gtfs, names))
  )
  all_fields <- unique(unlist(all_fields))
  id_fields <- all_fields[grepl("_id$", all_fields)]
  id_fields <- setdiff(id_fields, "direction_id")

  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs, prefix = TRUE)

  # check if all id fields were edited

  tests <- lapply(
    merged_gtfs,
    function(gtfs_table) {
      table_ids <- id_fields[id_fields %chin% names(gtfs_table)]
      vapply(
        table_ids,
        FUN.VALUE = logical(1),
        FUN = function(id) {
          all(grepl("^\\d_", gtfs_table[[id]]) | gtfs_table[[id]] == "")
        }
      )
    }
  )
  tests <- unlist(tests)
  expect_true(all(tests))

  # check that direction_id wasn't changed
  expect_type(merged_gtfs$trips$direction_id, "integer")

  # check that correct prefixes were assigned to the values
  expect_identical(merged_gtfs$agency$agency_id, c("1_1", "1_1", "2_agency001"))

  # check that correct prefixes are assigned when prefix is a character vector
  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs, prefix = c("spo", "ggl"))
  expect_identical(
    merged_gtfs$agency$agency_id,
    c("spo_1", "spo_1", "ggl_agency001")
  )
})

test_that("removes identical rows when prefix = FALSE", {
  # merging a feed with itself keeps each row once, including the rows
  # duplicated within the feed

  merged_gtfs <- merge_gtfs(spo_gtfs, spo_gtfs)
  unique_gtfs <- remove_duplicates(spo_gtfs)
  expect_identical(names(merged_gtfs), names(unique_gtfs))
  for (table in names(unique_gtfs)) {
    expect_equal(
      merged_gtfs[[table]],
      unique_gtfs[[table]],
      ignore_attr = TRUE
    )
  }

  # empty files (read as tables without columns) and columns named like the
  # internal count don't get in the way

  empty_gtfs <- read_gtfs(spo_path)
  empty_gtfs$feed_info <- data.table::data.table()
  empty_gtfs$stops <- data.table::copy(empty_gtfs$stops)
  empty_gtfs$stops[, n_origins := 7L]
  merged_gtfs <- merge_gtfs(empty_gtfs, empty_gtfs)
  expect_identical(nrow(merged_gtfs$feed_info), 0L)
  expect_identical(merged_gtfs$stops$n_origins, empty_gtfs$stops$n_origins)

  # with prefixes, rows are not removed

  expect_identical(
    nrow(merge_gtfs(spo_gtfs, spo_gtfs, prefix = TRUE)$stops),
    2L * nrow(spo_gtfs$stops)
  )
})

test_that("errors when GTFS objects share ids with different data", {
  # spo + ber both have an agency_id "1", with different agencies

  ber_gtfs <- read_gtfs(
    system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  )
  expect_error(
    merge_gtfs(spo_gtfs, ber_gtfs),
    class = "gtfstools_duplicated_ids_error",
    regexp = "agency_id"
  )
  expect_error(
    merge_gtfs(spo_gtfs, ber_gtfs),
    class = "gtfstools_duplicated_ids_error",
    regexp = "prefix = TRUE"
  )
  expect_error(
    merge_gtfs(list(spo_gtfs, ggl_gtfs, ber_gtfs)),
    class = "gtfstools_duplicated_ids_error"
  )
  expect_s3_class(merge_gtfs(spo_gtfs, ber_gtfs, prefix = TRUE), "dt_gtfs")
  expect_s3_class(
    merge_gtfs(spo_gtfs, ber_gtfs, prefix = c("a", "b")),
    "dt_gtfs"
  )

  # a trip whose stop_times are a subset of another feed's conflicts too,
  # regardless of the order of the feeds

  truncated_gtfs <- read_gtfs(spo_path)
  truncated_trip <- truncated_gtfs$stop_times$trip_id[1]
  last_stop <- max(
    which(truncated_gtfs$stop_times$trip_id == truncated_trip)
  )
  truncated_gtfs$stop_times <- truncated_gtfs$stop_times[-last_stop]
  expect_error(
    merge_gtfs(spo_gtfs, truncated_gtfs),
    class = "gtfstools_duplicated_ids_error",
    regexp = "trip_id"
  )
  expect_error(
    merge_gtfs(truncated_gtfs, spo_gtfs),
    class = "gtfstools_duplicated_ids_error",
    regexp = "trip_id"
  )

  # named objects are identified by their position, not their names

  expect_error(
    merge_gtfs(x = spo_gtfs, x = ber_gtfs),
    class = "gtfstools_duplicated_ids_error"
  )
  expect_error(
    merge_gtfs(spo_gtfs, ggl_gtfs, list(a = ber_gtfs)),
    class = "gtfstools_duplicated_ids_error"
  )
  expect_identical(
    merge_gtfs(a = spo_gtfs, b = ggl_gtfs, prefix = c("spo", "ggl")),
    merge_gtfs(spo_gtfs, ggl_gtfs, prefix = c("spo", "ggl"))
  )

  # only the merged tables are checked

  expect_s3_class(
    merge_gtfs(spo_gtfs, truncated_gtfs, files = "frequencies"),
    "dt_gtfs"
  )

  # service_ids are pooled from 'calendar' and 'calendar_dates'

  calendar_gtfs <- gtfsio::new_gtfs(
    list(calendar = spo_gtfs$calendar[1]),
    "dt_gtfs"
  )
  calendar_dates_gtfs <- gtfsio::new_gtfs(
    list(
      calendar_dates = data.table::data.table(
        service_id = spo_gtfs$calendar$service_id[1],
        date = data.table::as.IDate("2020-01-01"),
        exception_type = 1L
      )
    ),
    "dt_gtfs"
  )
  expect_error(
    merge_gtfs(calendar_gtfs, calendar_dates_gtfs),
    class = "gtfstools_duplicated_ids_error",
    regexp = "service_id"
  )

  # blank and NA ids are not shared ids

  blank_gtfs <- gtfsio::new_gtfs(
    list(agency = data.table::data.table(agency_id = "", agency_name = "a")),
    "dt_gtfs"
  )
  expect_s3_class(merge_gtfs(blank_gtfs, blank_gtfs), "dt_gtfs")

  na_gtfs <- gtfsio::new_gtfs(
    list(agency = data.table::data.table(agency_id = NA_integer_)),
    "dt_gtfs"
  )
  expect_s3_class(merge_gtfs(na_gtfs, na_gtfs), "dt_gtfs")
})

test_that("works with non dt_gtfs objects", {
  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs)

  # gtfsio objects

  gtfsio_ggl <- gtfsio::import_gtfs(ggl_path)
  gtfsio_ggl$routes[
    ,
    route_desc := gsub("\"\"", "\"", route_desc, fixed = TRUE)
  ]
  expect_identical(merged_gtfs, merge_gtfs(spo_gtfs, gtfsio_ggl))

  # tidytransit objects
  # ggl_gtfs includes times in the H:MM:SS format, not HH:MM:SS. this causes
  # problems when checking if the objects are identical, because the conversion
  # procedure always convert times to HH:MM:SS. so we have to adjust ggl_gtfs
  # to make it work in this test

  adjusted_ggl_gtfs <- ggl_gtfs
  adjusted_ggl_gtfs$stop_times <- data.table::copy(ggl_gtfs$stop_times)
  adjusted_ggl_gtfs$stop_times[
    arrival_time != "",
    arrival_time := paste0("0", arrival_time)
  ]
  adjusted_ggl_gtfs$stop_times[
    departure_time != "",
    departure_time := paste0("0", departure_time)
  ]

  merged_gtfs <- merge_gtfs(spo_gtfs, adjusted_ggl_gtfs)

  tidy_ggl <- read_gtfs(ggl_path)
  tidy_ggl$stop_times[
    ,
    `:=`(
      arrival_time = string_to_seconds(arrival_time),
      departure_time = string_to_seconds(departure_time)
    )
  ]
  tidy_ggl$frequencies[
    ,
    `:=`(
      start_time = string_to_seconds(start_time),
      end_time = string_to_seconds(end_time)
    )
  ]
  invisible(lapply(tidy_ggl, data.table::setDF))
  tidy_ggl$. <- list(internal_table = data.frame(a = 1:2, b = 2:3))
  class(tidy_ggl) <- c("tidygtfs", "gtfs")

  expect_identical(merged_gtfs, merge_gtfs(spo_gtfs, tidy_ggl))
})

test_that("elements that are not tables are not merged, with a warning", {
  locations <- list(type = "FeatureCollection", features = list())
  flex_spo <- read_gtfs(spo_path)
  flex_spo$locations <- locations
  flex_ggl <- read_gtfs(ggl_path)
  flex_ggl$locations <- locations

  expect_warning(
    result <- merge_gtfs(flex_spo, flex_ggl),
    class = "gtfstools_unmerged_files"
  )
  expect_null(result$locations)
  expect_identical(result, merge_gtfs(spo_gtfs, ggl_gtfs))

  expect_error(
    suppressWarnings(merge_gtfs(flex_spo, flex_ggl, files = "locations")),
    regexp = "None of the GTFS to be merged contain the files"
  )
})

test_that("prefix leaves blank ids unchanged", {
  blank_gtfs <- read_gtfs(spo_path)
  blank_gtfs$trips <- data.table::copy(blank_gtfs$trips)
  blank_gtfs$trips[, block_id := ""]

  merged_gtfs <- merge_gtfs(blank_gtfs, blank_gtfs, prefix = TRUE)

  expect_true(all(merged_gtfs$trips$block_id == ""))

  # with blank and non-blank ids, only the non-blank ones get the prefix of
  # their gtfs object

  blank_gtfs$trips[, block_id := rep(c("", "b1"), length.out = .N)]
  mixed_gtfs <- merge_gtfs(blank_gtfs, blank_gtfs, prefix = c("a", "b"))
  expect_identical(
    mixed_gtfs$trips$block_id,
    c(
      ifelse(blank_gtfs$trips$block_id == "", "", "a_b1"),
      ifelse(blank_gtfs$trips$block_id == "", "", "b_b1")
    )
  )
  prefixes <- rep(c("1_", "2_"), each = nrow(blank_gtfs$trips))
  expect_identical(
    merged_gtfs$trips$trip_id,
    paste0(prefixes, blank_gtfs$trips$trip_id)
  )
})
