spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
poa_path <- system.file("extdata/poa_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)
poa_gtfs <- read_gtfs(poa_path)

tester <- function(gtfs = spo_gtfs) {
  interpolate_shape_dist_traveled(gtfs)
}

# meters per degree of longitude on the equator, on the sphere used by {s2}

meters_per_degree <- 6371010 * pi / 180

# creates a feed with a single trip that serves stops on the equator, where
# great-circle distances are proportional to the difference in longitude. the
# trip's shape is a straight line, and a second shape is used by no trip

synthetic_feed <- function(stop_lon = c(0, 0.01, 0.03),
                           shape_lon = c(0, 0.02, 0.03)) {
  stop_ids <- paste0("s", seq_along(stop_lon))

  feed <- list(
    trips = data.table::data.table(trip_id = "t1", shape_id = "sh1"),
    shapes = data.table::data.table(
      shape_id = c(rep("sh1", length(shape_lon)), "sh2", "sh2"),
      shape_pt_lat = 0,
      shape_pt_lon = c(shape_lon, 1, 1.5),
      shape_pt_sequence = c(seq_along(shape_lon), 1L, 2L)
    ),
    stops = data.table::data.table(
      stop_id = stop_ids,
      stop_lat = 0,
      stop_lon = stop_lon
    ),
    stop_times = data.table::data.table(
      trip_id = "t1",
      stop_id = stop_ids,
      stop_sequence = seq_along(stop_ids)
    )
  )

  return(gtfsio::new_gtfs(feed, "dt_gtfs"))
}

basic_feed <- synthetic_feed()

# tests ---

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(basic_feed)))

  wrong_class <- copy_gtfs_diff_field_class(
    basic_feed,
    "stop_times",
    "stop_sequence",
    "character"
  )
  expect_error(tester(wrong_class))

  wrong_class <- copy_gtfs_diff_field_class(
    basic_feed,
    "shapes",
    "shape_pt_lat",
    "character"
  )
  expect_error(tester(wrong_class))
})

test_that("raises error if the gtfs has no shapes", {
  no_shapes <- copy_gtfs_without_file(basic_feed, "shapes")
  expect_error(tester(no_shapes), class = "gtfstools_shapes_unavailable")

  no_shape_id <- copy_gtfs_without_field(basic_feed, "trips", "shape_id")
  expect_error(tester(no_shape_id), class = "gtfstools_shapes_unavailable")
})

test_that("results in a dt_gtfs object", {
  result <- tester(basic_feed)
  expect_s3_class(result, "dt_gtfs")
  expect_type(result$stop_times$shape_dist_traveled, "double")
  expect_type(result$shapes$shape_dist_traveled, "double")
})

test_that("calculates the distances along the shapes", {
  result <- tester(basic_feed)

  expect_equal(
    result$stop_times$shape_dist_traveled,
    c(0, 0.01, 0.03) * meters_per_degree
  )

  # the shape that no trip uses is measured too

  expect_equal(
    result$shapes$shape_dist_traveled,
    c(0, 0.02, 0.03, 0, 0.5) * meters_per_degree
  )
})

test_that("stop and shape distances are consistent", {
  for (gtfs in list(spo_gtfs, poa_gtfs)) {
    result <- suppressWarnings(tester(gtfs))

    shapes <- result$shapes[order(shape_id, shape_pt_sequence)]
    last_point <- shapes[
      ,
      .(length = data.table::last(shape_dist_traveled)),
      by = shape_id
    ]
    shape_length <- get_shape_length(gtfs, unit = "m")
    shape_length <- shape_length[!is.na(length)]
    expect_equal(
      last_point[shape_length, on = "shape_id"]$length,
      shape_length$length
    )

    # stops advance along the shape and never go past its end

    st <- result$stop_times[order(trip_id, stop_sequence)]
    st <- st[!is.na(shape_dist_traveled)]
    expect_true(all(st[, all(diff(shape_dist_traveled) >= 0), by = trip_id]$V1))

    trip_shape <- gtfs$trips[st, on = "trip_id", shape_id]
    expect_true(
      all(
        st$shape_dist_traveled <=
          last_point[trip_shape, on = "shape_id"]$length + 1e-6
      )
    )
  }
})

test_that("keeps the order of the rows of unsorted tables", {
  result <- tester(spo_gtfs)

  reversed <- spo_gtfs
  reversed$stop_times <- spo_gtfs$stop_times[rev(seq_len(.N))]
  reversed$shapes <- spo_gtfs$shapes[rev(seq_len(.N))]
  reversed_result <- tester(reversed)

  expect_identical(
    reversed_result$stop_times$shape_dist_traveled,
    rev(result$stop_times$shape_dist_traveled)
  )
  expect_identical(
    reversed_result$shapes$shape_dist_traveled,
    rev(result$shapes$shape_dist_traveled)
  )
})

test_that("replaces existing shape_dist_traveled columns", {
  feed <- synthetic_feed()
  feed$stop_times[, shape_dist_traveled := c(0L, 5L, 10L)]
  data.table::setcolorder(feed$stop_times, "shape_dist_traveled")

  result <- tester(feed)
  expect_identical(names(result$stop_times)[1], "shape_dist_traveled")
  expect_equal(
    result$stop_times$shape_dist_traveled,
    c(0, 0.01, 0.03) * meters_per_degree
  )
})

test_that("gives NA to stops that can't be located", {
  feed <- synthetic_feed()

  # a trip whose shape doesn't exist, and a trip with a single stop

  feed$trips <- data.table::data.table(
    trip_id = c("t1", "t2", "t3"),
    shape_id = c("sh1", "missing", "sh1")
  )
  feed$stop_times <- rbind(
    feed$stop_times,
    data.table::data.table(
      trip_id = c("t2", "t2", "t3"),
      stop_id = c("s1", "s2", "s1"),
      stop_sequence = c(1L, 2L, 1L)
    )
  )

  expect_warning(
    result <- tester(feed),
    class = "gtfstools_trips_without_shape"
  )
  expect_equal(
    result$stop_times$shape_dist_traveled,
    c(c(0, 0.01, 0.03) * meters_per_degree, NA, NA, NA)
  )

  # stops without coordinates

  feed <- synthetic_feed()
  feed$stops[stop_id == "s2", stop_lat := NA]
  result <- tester(feed)
  expect_equal(
    result$stop_times$shape_dist_traveled,
    c(0, NA, 0.03 * meters_per_degree)
  )
})

test_that("measures shapes even if stop_times is empty", {
  feed <- synthetic_feed()
  feed$stop_times <- feed$stop_times[0]

  result <- tester(feed)
  expect_identical(result$stop_times$shape_dist_traveled, numeric(0))
  expect_equal(
    result$shapes$shape_dist_traveled,
    c(0, 0.02, 0.03, 0, 0.5) * meters_per_degree
  )
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  result <- tester(gtfs)
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
})
