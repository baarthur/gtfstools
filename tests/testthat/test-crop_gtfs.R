spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
poa_path <- system.file("extdata/poa_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)
poa_gtfs <- read_gtfs(poa_path)

bbox <- sf::st_bbox(
  c(xmin = -46.66, ymin = -23.57, xmax = -46.62, ymax = -23.53),
  crs = 4326
)
polygon <- sf::st_as_sf(sf::st_as_sfc(bbox))

tester <- function(gtfs = spo_gtfs, geom = bbox, trim_shapes = TRUE) {
  crop_gtfs(gtfs, geom, trim_shapes)
}

# whether each of the given stops is inside the given geometry

is_inside <- function(gtfs, stop_id, geom = polygon) {
  stops_sf <- get_stops_geometry(gtfs, unique(stop_id))
  inside <- sf::st_intersects(geom, stops_sf, sparse = FALSE)[1, ]
  return(unique(stop_id) %chin% stops_sf$stop_id[inside])
}

# tests -------------------------------------------------------------------

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(spo_gtfs)))

  expect_error(tester(geom = unclass(bbox)))
  expect_error(
    tester(geom = sf::st_transform(polygon, 4674)),
    class = "gtfstools_bad_crs_error"
  )

  bad_bbox <- bbox
  bad_bbox[c("xmin", "xmax")] <- bbox[c("xmax", "xmin")]
  expect_error(tester(geom = bad_bbox), class = "gtfstools_bad_bbox_error")
  bad_bbox <- bbox
  bad_bbox["ymax"] <- NA
  expect_error(tester(geom = bad_bbox), class = "gtfstools_bad_bbox_error")

  expect_error(tester(trim_shapes = "TRUE"))
  expect_error(tester(trim_shapes = c(TRUE, TRUE)))
  expect_error(tester(trim_shapes = NA))
})

test_that("results in a dt_gtfs object", {
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")
  cropped_gtfs <- tester()
  expect_s3_class(cropped_gtfs, dt_gtfs_class)
  expect_type(cropped_gtfs, "list")
  invisible(lapply(cropped_gtfs, expect_s3_class, "data.table"))
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  cropped_gtfs <- tester(gtfs)
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)

  # a spatial extent covering the whole feed selects every row of some tables

  whole_bbox <- sf::st_bbox(get_stops_geometry(gtfs))
  whole_bbox <- whole_bbox + c(-0.01, -0.01, 0.01, 0.01)
  cropped_gtfs <- tester(gtfs, whole_bbox)
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
  expect_identical(nrow(cropped_gtfs$stop_times), nrow(gtfs$stop_times))
  expect_identical(cropped_gtfs$shapes$shape_id, gtfs$shapes$shape_id)
})

test_that("supports sf, sfc and bbox objects", {
  result1 <- tester(geom = polygon)
  result2 <- tester(geom = sf::st_geometry(polygon))
  result3 <- tester(geom = bbox)

  expect_identical(result1, result2)
  expect_identical(result1, result3)
})

test_that("keeps only stops inside the spatial extent", {
  cropped_gtfs <- tester()

  expect_true(nrow(cropped_gtfs$stop_times) > 0)
  expect_true(all(is_inside(cropped_gtfs, cropped_gtfs$stop_times$stop_id)))

  # trips with fewer than two stops are dropped

  n_stops <- table(cropped_gtfs$stop_times$trip_id)
  expect_true(all(n_stops >= 2))
  expect_setequal(cropped_gtfs$trips$trip_id, names(n_stops))

  # a spatial extent without stops results in empty tables

  far_bbox <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 1, ymax = 1), crs = 4326)
  empty_gtfs <- tester(geom = far_bbox)
  expect_identical(nrow(empty_gtfs$stop_times), 0L)
  expect_identical(nrow(empty_gtfs$trips), 0L)
  expect_identical(nrow(empty_gtfs$shapes), 0L)
})

test_that("keeps parent stations, but not children stops outside the extent", {
  # stop 'inside_stop' is inside the extent and 'outside_stop' is outside of
  # it. both are made children of a new station inside the extent

  gtfs <- read_gtfs(spo_path)
  cropped_gtfs <- tester(gtfs)
  inside_stop <- cropped_gtfs$stop_times$stop_id[1]
  served_stops <- unique(gtfs$stop_times$stop_id)
  outside_stop <- served_stops[!is_inside(gtfs, served_stops)][1]

  stops <- data.table::copy(gtfs$stops)
  stops[, parent_station := ""]
  stops[stop_id %chin% c(inside_stop, outside_stop), parent_station := "stn"]
  station <- stops[stop_id == inside_stop]
  station[, `:=`(stop_id = "stn", parent_station = "", location_type = 1L)]
  gtfs$stops <- rbind(stops, station, fill = TRUE)

  cropped_gtfs <- tester(gtfs)
  expect_true("stn" %chin% cropped_gtfs$stops$stop_id)
  expect_true(inside_stop %chin% cropped_gtfs$stop_times$stop_id)
  expect_false(outside_stop %chin% cropped_gtfs$stop_times$stop_id)
  expect_false(outside_stop %chin% cropped_gtfs$stops$stop_id)
})

test_that("keeps stops on the edges of a bounding box", {
  # with spherical geometry, the edges of a polygon made from a wide bounding
  # box are geodesics, which leave out stops on its northern edge. bounding
  # boxes are compared directly with the coordinates

  stop <- tester()$stop_times$stop_id[1]
  stop_lat <- spo_gtfs$stops[stop_id == stop]$stop_lat
  wide_bbox <- sf::st_bbox(
    c(xmin = -47.2, ymin = -24, xmax = -46.2, ymax = stop_lat),
    crs = 4326
  )

  cropped_gtfs <- tester(geom = wide_bbox)
  expect_true(stop %chin% cropped_gtfs$stop_times$stop_id)
})

test_that("considers stops without coordinates outside the extent", {
  gtfs <- read_gtfs(spo_path)
  inside_stop <- tester(gtfs)$stop_times$stop_id[1]

  stops <- data.table::copy(gtfs$stops)
  stops[stop_id == inside_stop, `:=`(stop_lat = NA_real_, stop_lon = NA_real_)]
  gtfs$stops <- stops

  cropped_gtfs <- tester(gtfs)
  expect_false(inside_stop %chin% cropped_gtfs$stop_times$stop_id)
  expect_true(all(is_inside(cropped_gtfs, cropped_gtfs$stop_times$stop_id)))
})

test_that("trims shapes to the first and last remaining stops", {
  cropped_gtfs <- tester()

  # the length of each trimmed shape matches the length of its trip, measured
  # from its first to its last stop

  cropped_trips <- c("METRÔ L1-0", "2105-10-0", "5290-10-1")
  trip_length <- get_trip_length(cropped_gtfs, cropped_trips, unit = "m")
  shape_id <- cropped_gtfs$trips[match(trip_length$trip_id, trip_id)]$shape_id
  shape_length <- function(gtfs) {
    shapes_sf <- get_shapes_geometry(gtfs, shape_id)
    length <- as.numeric(sf::st_length(shapes_sf))
    return(length[match(shape_id, shapes_sf$shape_id)])
  }
  expect_equal(shape_length(cropped_gtfs), trip_length$length, tolerance = 1e-6)

  # every shape used by the trips is listed in shapes, and vice versa

  expect_setequal(cropped_gtfs$trips$shape_id, cropped_gtfs$shapes$shape_id)

  # the trimmed shapes are shorter than the original ones

  expect_true(all(shape_length(cropped_gtfs) < shape_length(spo_gtfs)))
})

test_that("gives new shape_ids to shapes cut in different ways", {
  # 'METRÔ L1-0' is cropped at both ends. 'copy_1' uses the same shape, but
  # without its first two stops inside the extent, so it's cut differently.
  # 'copy_2' only has stops inside the extent, so it's not cut, and uses a copy
  # of the shape whose shape_id clashes with the first candidate new shape_id

  gtfs <- read_gtfs(spo_path)
  trip <- "METRÔ L1-0"
  trip_shape <- gtfs$trips[trip_id == trip]$shape_id
  clashing_id <- paste0(trip_shape, "_1")

  trip_rows <- gtfs$stop_times[trip_id == trip]
  inside_rows <- trip_rows[
    is_inside(gtfs, stop_id)[match(stop_id, unique(stop_id))]
  ]

  copy_1 <- trip_rows[!stop_id %chin% inside_rows$stop_id[1:2]]
  copy_1[, trip_id := "copy_1"]
  copy_2 <- data.table::copy(inside_rows)[, trip_id := "copy_2"]
  gtfs$stop_times <- rbind(gtfs$stop_times, copy_1, copy_2)

  new_trips <- gtfs$trips[rep(which(trip_id == trip), 2)]
  new_trips[
    ,
    `:=`(
      trip_id = c("copy_1", "copy_2"),
      shape_id = c(trip_shape, clashing_id)
    )
  ]
  gtfs$trips <- rbind(gtfs$trips, new_trips)

  clashing_shape <- gtfs$shapes[shape_id == trip_shape]
  clashing_shape[, shape_id := clashing_id]
  gtfs$shapes <- rbind(gtfs$shapes, clashing_shape)

  cropped_gtfs <- tester(gtfs)
  trip_ids <- c(trip, "copy_1", "copy_2")
  shapes <- cropped_gtfs$trips$shape_id[
    match(trip_ids, cropped_gtfs$trips$trip_id)
  ]

  expect_identical(shapes[3], clashing_id)
  expect_false(any(shapes[1:2] %chin% c(trip_shape, clashing_id)))
  expect_false(shapes[1] == shapes[2])
  expect_true(all(startsWith(shapes[1:2], paste0(trip_shape, "_"))))

  # the uncut trip keeps its entire shape, and the original shape, no longer
  # used whole, is replaced by its cuts

  expect_equal(
    cropped_gtfs$shapes[shape_id == clashing_id],
    clashing_shape,
    ignore_attr = TRUE
  )
  expect_false(trip_shape %chin% cropped_gtfs$shapes$shape_id)

  # each cut matches the length of its trip

  trip_length <- get_trip_length(cropped_gtfs, trip_ids[1:2], unit = "m")
  shape_length <- sf::st_length(get_shapes_geometry(cropped_gtfs, shapes[1:2]))
  shape_length <- shape_length[
    match(shapes[1:2], get_shapes_geometry(cropped_gtfs, shapes[1:2])$shape_id)
  ]
  expect_equal(
    as.numeric(shape_length),
    trip_length$length[match(trip_ids[1:2], trip_length$trip_id)],
    tolerance = 1e-6
  )
})

test_that("keeps the parts of shapes outside the extent between stops", {
  # the extent is made of two areas around the second and the second to last
  # stops of a trip, so the stops in between are dropped

  trip <- "2002-10-0"
  trip_stops <- spo_gtfs$stop_times[trip_id == trip]$stop_id
  stops_sf <- get_stops_geometry(spo_gtfs, trip_stops)
  stops_sf <- stops_sf[match(trip_stops, stops_sf$stop_id), ]
  ends <- stops_sf[c(2, length(trip_stops) - 1), ]
  areas <- sf::st_buffer(ends, 50)

  cropped_gtfs <- tester(geom = areas)
  cropped_stops <- cropped_gtfs$stop_times[trip_id == trip]$stop_id
  expect_identical(cropped_stops, ends$stop_id)

  shape_id <- cropped_gtfs$trips[trip_id == trip]$shape_id
  shape_sf <- get_shapes_geometry(cropped_gtfs, shape_id)
  original_length <- get_trip_length(spo_gtfs, trip, unit = "m")$length
  first_stop_length <- get_trip_length(
    filter_by_stop_id(spo_gtfs, trip_stops[1:2]),
    trip,
    unit = "m"
  )$length
  last_stop_length <- get_trip_length(
    filter_by_stop_id(spo_gtfs, utils::tail(trip_stops, 2)),
    trip,
    unit = "m"
  )$length

  expect_equal(
    as.numeric(sf::st_length(shape_sf)),
    original_length - first_stop_length - last_stop_length,
    tolerance = 1e-3
  )
})

test_that("trim_shapes = FALSE keeps entire shapes", {
  cropped_gtfs <- tester(trim_shapes = FALSE)
  used_shapes <- cropped_gtfs$trips$shape_id
  expected_shapes <- spo_gtfs$shapes[shape_id %chin% used_shapes]
  expect_equal(cropped_gtfs$shapes, expected_shapes, ignore_attr = TRUE)
  expect_identical(
    cropped_gtfs$trips$shape_id,
    spo_gtfs$trips[match(cropped_gtfs$trips$trip_id, trip_id)]$shape_id
  )
})

test_that("works without shapes", {
  no_shapes <- copy_gtfs_without_file(spo_gtfs, "shapes")
  expect_silent(cropped_gtfs <- tester(no_shapes))
  expect_true(all(is_inside(cropped_gtfs, cropped_gtfs$stop_times$stop_id)))
  expect_null(cropped_gtfs$shapes)

  no_shape_id <- copy_gtfs_without_field(spo_gtfs, "trips", "shape_id")
  expect_silent(cropped_gtfs <- tester(no_shape_id))
  expect_true(all(is_inside(cropped_gtfs, cropped_gtfs$stop_times$stop_id)))
})

test_that("works with other feeds and without optional files and fields", {
  for (path in c("ber_gtfs.zip", "ggl_gtfs.zip")) {
    gtfs <- read_gtfs(system.file("extdata", path, package = "gtfstools"))
    stops_bbox <- sf::st_bbox(get_stops_geometry(gtfs))
    stops_bbox["xmax"] <- (stops_bbox["xmin"] + stops_bbox["xmax"]) / 2

    original_gtfs <- read_gtfs(
      system.file("extdata", path, package = "gtfstools")
    )
    cropped_gtfs <- suppressWarnings(tester(gtfs, stops_bbox))
    expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
    expect_s3_class(cropped_gtfs, "dt_gtfs")

    served_stops <- cropped_gtfs$stop_times$stop_id
    kept_stops <- cropped_gtfs$stops[stop_id %chin% served_stops]
    expect_true(all(kept_stops$stop_lon <= stops_bbox[["xmax"]]))
  }

  no_frequencies <- copy_gtfs_without_file(spo_gtfs, "frequencies")
  expect_silent(cropped_gtfs <- tester(no_frequencies))
  expect_null(cropped_gtfs$frequencies)

  # spo doesn't have a parent_station column, so the other tests already cover
  # feeds without it
  expect_false("parent_station" %chin% names(spo_gtfs$stops))
})

test_that("interpolates blank times at the new ends of cropped trips", {
  # poa only has times at the first and last stops of each trip. the extent
  # covers the middle stops of a trip, whose new ends have blank times

  trip <- "T2-1@1#520"
  trip_rows <- poa_gtfs$stop_times[trip_id == trip][order(stop_sequence)]
  middle_stops <- trip_rows$stop_id[5:(nrow(trip_rows) - 5)]
  middle_bbox <- sf::st_bbox(get_stops_geometry(poa_gtfs, middle_stops))
  expect_true(all(trip_rows$departure_time[5:6] == ""))

  cropped_gtfs <- suppressWarnings(tester(poa_gtfs, middle_bbox))
  cropped_rows <- cropped_gtfs$stop_times[trip_id == trip][order(stop_sequence)]
  expect_true(nrow(cropped_rows) >= 2)
  expect_true(cropped_rows$stop_sequence[1] > trip_rows$stop_sequence[1])

  ends <- cropped_rows[c(1, .N)]
  expect_false(any(ends$arrival_time == "" | is.na(ends$arrival_time)))
  expect_false(any(ends$departure_time == "" | is.na(ends$departure_time)))
})

test_that("interpolates along straight lines or warns when it can't", {
  trip <- "T2-1@1#520"
  trip_gtfs <- filter_by_trip_id(poa_gtfs, trip)
  trip_rows <- trip_gtfs$stop_times[order(stop_sequence)]
  middle_stops <- trip_rows$stop_id[5:(nrow(trip_rows) - 5)]
  middle_bbox <- sf::st_bbox(get_stops_geometry(trip_gtfs, middle_stops))

  # a trip not linked to a usable shape is interpolated along straight lines,
  # without warnings

  no_shape <- trip_gtfs
  no_shape$trips <- data.table::copy(trip_gtfs$trips)[, shape_id := "missing"]
  expect_silent(cropped_gtfs <- tester(no_shape, middle_bbox))
  ends <- cropped_gtfs$stop_times[order(stop_sequence)][c(1, .N)]
  expect_false(any(ends$arrival_time == "" | ends$departure_time == ""))

  # a trip with a single timepoint can't be interpolated, so its new ends are
  # left blank, with a warning

  one_timepoint <- trip_gtfs
  stop_times <- data.table::copy(trip_gtfs$stop_times)
  last_row <- which.max(stop_times$stop_sequence)
  stop_times[last_row, `:=`(arrival_time = "", departure_time = "")]
  one_timepoint$stop_times <- stop_times
  expect_warning(
    cropped_gtfs <- tester(one_timepoint, middle_bbox),
    class = "gtfstools_times_not_interpolated"
  )
  expect_true(any(cropped_gtfs$stop_times$departure_time == ""))
})

test_that("shifts the frequencies of trips whose first stop is dropped", {
  gtfs <- read_gtfs(spo_path)
  trip <- "METRÔ L1-0"
  frequencies <- data.table::copy(gtfs$frequencies)
  first_row <- which(frequencies$trip_id == trip)[1]
  frequencies[first_row, `:=`(start_time = "23:50:00", end_time = "24:30:00")]
  gtfs$frequencies <- frequencies

  cropped_gtfs <- tester(gtfs)

  st <- gtfs$stop_times[trip_id == trip][order(stop_sequence)]
  new_first <- cropped_gtfs$stop_times[trip_id == trip][order(stop_sequence)]
  shift <- string_to_seconds(new_first$departure_time[1]) -
    string_to_seconds(st$departure_time[1])
  expect_true(shift > 0)

  original <- frequencies[trip_id == trip]
  shifted <- cropped_gtfs$frequencies[trip_id == trip]
  expect_identical(
    string_to_seconds(shifted$start_time),
    string_to_seconds(original$start_time) + shift
  )
  expect_identical(
    string_to_seconds(shifted$end_time),
    string_to_seconds(original$end_time) + shift
  )
  expect_true(all(string_to_seconds(shifted$end_time)[1] > 24 * 3600))

  # the arrival is used when the departure at the first stop is blank

  arrival_gtfs <- read_gtfs(spo_path)
  stop_times <- data.table::copy(arrival_gtfs$stop_times)
  first_sequence <- min(stop_times[trip_id == trip]$stop_sequence)
  first_row <- which(
    stop_times$trip_id == trip & stop_times$stop_sequence == first_sequence
  )
  stop_times[first_row, departure_time := ""]
  arrival_gtfs$stop_times <- stop_times
  cropped_gtfs <- tester(arrival_gtfs)
  expect_identical(
    string_to_seconds(cropped_gtfs$frequencies[trip_id == trip]$start_time),
    string_to_seconds(arrival_gtfs$frequencies[trip_id == trip]$start_time) +
      shift
  )

  # _secs columns are kept consistent

  secs_gtfs <- convert_time_to_seconds(gtfs, "frequencies")
  cropped_gtfs <- tester(secs_gtfs)
  shifted <- cropped_gtfs$frequencies[trip_id == trip]
  expect_identical(
    shifted$start_time_secs,
    string_to_seconds(shifted$start_time)
  )
})
