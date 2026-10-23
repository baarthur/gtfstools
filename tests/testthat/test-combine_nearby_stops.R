spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)

tester <- function(gtfs = spo_gtfs, max_dist = 100) {
  combine_nearby_stops(gtfs, max_dist)
}

# distances along a meridian, in degrees of latitude, with the earth radius
# used by the distance kernel

meters_to_degrees <- function(meters) meters / 6371010 * 180 / pi

new_feed <- function(stops, ...) {
  gtfsio::new_gtfs(list(stops = data.table::data.table(stops), ...), "dt_gtfs")
}

# clusters calculated from all pairs of nodes, to check the grid that selects
# the pairs to be compared

brute_force_clusters <- function(lat, lon, max_dist) {
  n <- length(lat)
  from <- rep.int(seq_len(n), n - seq_len(n))
  to <- sequence(n - seq_len(n)) + from
  distance <- cpp_stop_distances(lat, lon, from, to)
  is_close <- !is.na(distance) & distance <= max_dist

  pairs <- data.table::data.table(
    from = from[is_close],
    to = to[is_close],
    distance = distance[is_close]
  )
  data.table::setorderv(pairs, c("distance", "from", "to"))

  cpp_merge_stop_clusters(n, pairs$from, pairs$to)
}

# a feed with a station whose platforms are farther apart than max_dist, a
# platform that is the parent of another platform, a boarding area, an
# entrance, a station without platforms and references to stops in other
# files

new_station_feed <- function(transfers_rows = 1:4) {
  new_feed(
    stops = data.table::data.table(
      stop_id = c("S1", "P1", "P2", "P3", "P4", "BA", "E1", "S2", "X1", "Y1"),
      stop_name = c(
        "Central", "Central 1", "Central 2", "Central 3", "Park", "Area",
        "Entrance", "Empty", "Market", "Market"
      ),
      stop_lat = meters_to_degrees(
        c(0, 0, 500, 0, 3000, 3000, 0, 0, 1000, 1030)
      ),
      stop_lon = 0,
      location_type = c(1L, 0L, 0L, 0L, 0L, 4L, 2L, 1L, 0L, NA),
      parent_station = c("", "S1", "S1", "P1", "", "P4", "S1", "", "", "")
    ),
    stop_times = data.table::data.table(
      trip_id = "T1",
      stop_id = c("P1", "P2", "X1", "Y1", "P4"),
      stop_sequence = 1:5
    ),
    transfers = data.table::data.table(
      from_stop_id = c("S1", "X1", "Y1", "P1"),
      to_stop_id = c("X1", "Y1", "X1", "P2"),
      transfer_type = c(2L, 2L, 0L, 1L)
    )[transfers_rows],
    pathways = data.table::data.table(
      pathway_id = "PW1",
      from_stop_id = "E1",
      to_stop_id = "P1"
    ),
    stop_areas = data.table::data.table(
      area_id = "A1",
      stop_id = c("X1", "Y1")
    ),
    location_group_stops = data.table::data.table(
      location_group_id = "G1",
      stop_id = "P2"
    ),
    fare_leg_join_rules = data.table::data.table(
      from_network_id = "N1",
      to_network_id = "N1",
      from_stop_id = "P4",
      to_stop_id = "Y1"
    )
  )
}


# tests -------------------------------------------------------------------


test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(spo_gtfs)))
  for (bad_dist in list(-1, NA_real_, c(1, 2), "1", Inf, NULL)) {
    expect_error(tester(max_dist = bad_dist))
  }
  expect_error(tester(copy_gtfs_without_field(spo_gtfs, "stops", "stop_lat")))
  expect_error(tester(copy_gtfs_without_file(spo_gtfs, "stops")))
  expect_error(
    tester(copy_gtfs_diff_field_class(spo_gtfs, "stops", "stop_lat", "factor"))
  )
  ggl_gtfs <- read_gtfs(ggl_path)
  expect_error(
    tester(
      copy_gtfs_diff_field_class(ggl_gtfs, "stops", "parent_station", "factor")
    )
  )

  # the c++ kernel checks its indices

  expect_error(cpp_merge_stop_clusters(2L, 1L, 3L))
  expect_error(cpp_merge_stop_clusters(2L, 1L, 1L))
  expect_error(cpp_merge_stop_clusters(2L, 1L, c(2L, 1L)))
  expect_identical(
    cpp_merge_stop_clusters(0L, integer(0), integer(0)),
    integer(0)
  )

  expect_error(
    tester(new_feed(data.table::data.table(stop_id = "a", stop_lat = 95,
                                           stop_lon = 0))),
    class = "gtfstools_invalid_coordinates"
  )

  # cycles of even and odd length

  for (parents in list(c("b", "a", ""), c("b", "c", "a"))) {
    expect_error(
      tester(
        new_feed(
          data.table::data.table(
            stop_id = c("a", "b", "c"),
            stop_lat = c(0, 1, 2),
            stop_lon = 0,
            parent_station = parents
          )
        )
      ),
      class = "gtfstools_parent_station_cycle"
    )
  }
  expect_error(
    tester(
      new_feed(
        data.table::data.table(
          stop_id = "a",
          stop_lat = 0,
          stop_lon = 0,
          location_type = 1L
        )
      )
    ),
    class = "gtfstools_no_stops_to_merge"
  )
})

test_that("results in a dt_gtfs object", {
  expect_s3_class(tester(), c("dt_gtfs", "gtfs", "list"))
  expect_s3_class(tester(new_station_feed()), c("dt_gtfs", "gtfs", "list"))
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)
  result <- tester(gtfs, 300)
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)

  # a one-row table may share its columns with a subset of it

  original_gtfs <- new_station_feed(transfers_rows = 1)
  gtfs <- new_station_feed(transfers_rows = 1)
  result <- tester(gtfs)
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
})

test_that("merges stops so that all merged stops are within max_dist", {
  # a-b and b-c are within max_dist, but a-c isn't: only the closest pair is
  # merged

  gtfs <- new_feed(
    data.table::data.table(
      stop_id = c("c", "a", "b"),
      stop_name = c("C St", "A St", "B St"),
      stop_lat = meters_to_degrees(c(410, 0, 200)),
      stop_lon = 0
    )
  )
  result <- tester(gtfs, 300)

  expect_identical(result$stops$stop_id, c("a-b", "c"))
  expect_identical(result$stops$stop_name, c("A St-B St", "C St"))
  expect_equal(result$stops$stop_lat, meters_to_degrees(c(100, 410)))
  expect_equal(result$stops$stop_lon, c(0, 0))
})

test_that("compares every pair of stops within max_dist", {
  stops <- unique(spo_gtfs$stops, by = "stop_id")
  data.table::setorderv(stops, "stop_id")

  # a lattice at 70 degrees of latitude, with points on both sides of the
  # cell edges, and two points exactly max_dist apart along a meridian

  lattice <- expand.grid(i = 0:24, j = 0:24)
  lattice_lat <- 70 + meters_to_degrees(lattice$i * 37)
  lattice_lon <- lattice$j * meters_to_degrees(41) / cos(70 * pi / 180)
  lattice_lat <- c(lattice_lat, 71, 71 + meters_to_degrees(100))
  lattice_lon <- c(lattice_lon, 5, 5)

  for (max_dist in c(50, 300)) {
    expect_identical(
      cluster_stop_nodes(stops$stop_lat, stops$stop_lon, max_dist),
      brute_force_clusters(stops$stop_lat, stops$stop_lon, max_dist)
    )
  }
  for (max_dist in c(60, 100)) {
    expect_identical(
      cluster_stop_nodes(lattice_lat, lattice_lon, max_dist),
      brute_force_clusters(lattice_lat, lattice_lon, max_dist)
    )
  }

  # every pair of stops merged together is within max_dist

  max_dist <- 300
  cluster <- cluster_stop_nodes(stops$stop_lat, stops$stop_lon, max_dist)
  expect_true(any(duplicated(cluster)))
  for (members in split(seq_along(cluster), cluster)) {
    if (length(members) < 2) next
    pairs <- utils::combn(members, 2)
    distance <- cpp_stop_distances(
      stops$stop_lat,
      stops$stop_lon,
      pairs[1, ],
      pairs[2, ]
    )
    expect_true(all(distance <= max_dist))
  }

  # the result doesn't depend on the order of the stops

  expect_false(anyDuplicated(spo_gtfs$stops$stop_id) > 0)
  shuffled_gtfs <- spo_gtfs
  shuffled_gtfs$stops <- spo_gtfs$stops[rev(seq_len(nrow(spo_gtfs$stops)))]
  expect_equal(
    tester(shuffled_gtfs, max_dist)$stops,
    tester(spo_gtfs, max_dist)$stops,
    ignore_attr = TRUE
  )
})

test_that("doesn't merge clusters with a pair farther than max_dist", {
  # two blobs of 300 stops in which only the outermost pair, 79.9 m apart, is
  # farther than max_dist. repeatedly checking every pair of the two blobs
  # would take very long

  gtfs <- new_feed(
    data.table::data.table(
      stop_id = c(sprintf("a%03d", 0:299), sprintf("b%03d", 0:299)),
      stop_lat = meters_to_degrees(c(0:299 / 10, 50 + 0:299 / 10)),
      stop_lon = 0
    )
  )
  result <- tester(gtfs, 79.85)
  expect_gt(nrow(result$stops), 1)
})

test_that("merges stops with the same parent station first", {
  gtfs <- new_station_feed()
  result <- tester(gtfs, 100)

  # P1, P2 and P3 are S1, though P2 is 500 m from P1. P4 is the parent of a
  # boarding area only, so it's not merged with it

  expect_identical(result$stops$stop_id, c("P4", "S1", "X1-Y1"))
  expect_identical(result$stops$stop_name, c("Park", "Central", "Market"))
  expect_identical(result$stops$location_type, c(0L, 0L, 0L))
  expect_identical(result$stops$parent_station, c("", "", ""))
  expect_equal(
    result$stops$stop_lat,
    meters_to_degrees(c(3000, 500 / 3, 1015))
  )

  expect_identical(
    result$stop_times$stop_id,
    c("S1", "S1", "X1-Y1", "X1-Y1", "P4")
  )
  expect_null(result$pathways)

  # station transfers are updated and the transfers between X1 and Y1 become
  # one transfer within X1-Y1

  expect_identical(result$transfers$from_stop_id, c("S1", "X1-Y1", "S1"))
  expect_identical(result$transfers$to_stop_id, c("X1-Y1", "X1-Y1", "S1"))
  expect_identical(result$transfers$transfer_type, c(2L, 2L, 1L))

  expect_identical(result$stop_areas$stop_id, "X1-Y1")
  expect_identical(result$location_group_stops$stop_id, "S1")
  expect_identical(result$fare_leg_join_rules$from_stop_id, "P4")
  expect_identical(result$fare_leg_join_rules$to_stop_id, "X1-Y1")

  # references to removed stops, like a station without platforms, are kept

  gtfs$transfers <- data.table::data.table(
    from_stop_id = "S2",
    to_stop_id = "P1",
    transfer_type = 2L
  )
  result <- tester(gtfs, 100)
  expect_false("S2" %chin% result$stops$stop_id)
  expect_identical(result$transfers$from_stop_id, "S2")
  expect_identical(result$transfers$to_stop_id, "S1")

  # a stop that is its own parent is a stop without parent

  gtfs$stops[stop_id == "P4", parent_station := "P4"]
  expect_identical(tester(gtfs, 100)$stops$stop_id, c("P4", "S1", "X1-Y1"))

  # ber's parent stations are not listed in stops, so stops are named after
  # their platforms

  ber_gtfs <- read_gtfs(ber_path)
  expect_false(any(ber_gtfs$stops$parent_station %chin% ber_gtfs$stops$stop_id))
  result <- tester(ber_gtfs, 0)
  expect_setequal(result$stops$stop_id, unique(ber_gtfs$stops$parent_station))

  parent <- result$stops$stop_id[1]
  children <- ber_gtfs$stops[parent_station == parent]
  data.table::setorderv(children, "stop_id")
  expect_identical(
    result$stops$stop_name[1],
    paste(unique(children$stop_name), collapse = "-")
  )
  expect_true(all(result$stop_times$stop_id %chin% result$stops$stop_id))
  expect_identical(nrow(result$stop_times), nrow(ber_gtfs$stop_times))
})

test_that("raises an error when all stops are merged into one", {
  # the bounding box shows that all stops are within max_dist, so they are
  # not clustered

  local({
    local_mocked_bindings(cpp_merge_stop_clusters = function(...) stop("late"))
    expect_error(tester(spo_gtfs, 1e7), class = "gtfstools_single_stop_error")
  })

  # here the bounding box doesn't, and the stops are merged by clustering

  gtfs <- new_feed(
    data.table::data.table(
      stop_id = c("a", "b"),
      stop_lat = c(0, meters_to_degrees(100)),
      stop_lon = c(meters_to_degrees(100), 0)
    )
  )
  expect_error(tester(gtfs, 150), class = "gtfstools_single_stop_error")
  expect_identical(nrow(tester(gtfs, 140)$stops), 2L)

  # stops merged by parent station only, and feeds with one stop, are fine

  one_stop_gtfs <- new_feed(
    data.table::data.table(stop_id = "a", stop_lat = 0, stop_lon = 0)
  )
  expect_identical(tester(one_stop_gtfs, 1e7)$stops$stop_id, "a")

  parent_gtfs <- new_feed(
    data.table::data.table(
      stop_id = c("a", "b"),
      stop_lat = c(0, 1),
      stop_lon = 0,
      parent_station = c("S", "S")
    )
  )
  expect_identical(tester(parent_gtfs, 0)$stops$stop_id, "S")

  # a max_dist of 0 merges only stops at the same place

  gtfs <- new_feed(
    data.table::data.table(
      stop_id = c("a", "b", "c"),
      stop_lat = c(0, 0, meters_to_degrees(1)),
      stop_lon = 0
    )
  )
  expect_identical(tester(gtfs, 0)$stops$stop_id, c("a-b", "c"))

  local_mocked_bindings(max_stop_pairs_without_warning = function() 10)
  expect_warning(tester(spo_gtfs, 300), class = "gtfstools_many_stops_warning")
})

test_that("handles duplicated ids, missing coordinates and optional fields", {
  gtfs <- new_feed(
    data.table::data.table(
      stop_id = c("A", "B", "A-B"),
      stop_lat = c(0, 0, 10),
      stop_lon = 0
    )
  )
  expect_error(tester(gtfs, 10), class = "gtfstools_duplicated_stop_id")

  gtfs <- new_feed(
    data.table::data.table(
      stop_id = c("a", "b", "c"),
      stop_lat = c(0, 0, NA),
      stop_lon = c(0, 0, 0)
    )
  )
  result <- tester(gtfs, 10)
  expect_identical(result$stops$stop_id, c("a-b", "c"))
  expect_identical(result$stops$stop_lat, c(0, NA))

  # missing names are left out of merged names, and a stop with one missing
  # coordinate has no position

  gtfs <- new_feed(
    data.table::data.table(
      stop_id = c("a", "b", "c", "d"),
      stop_name = c(NA, "B", NA, "D"),
      stop_lat = c(0, 0, 1, NA),
      stop_lon = c(0, 0, NA, 1)
    )
  )
  result <- tester(gtfs, 10)
  expect_identical(result$stops$stop_id, c("a-b", "c", "d"))
  expect_identical(result$stops$stop_name, c("B", NA, "D"))
  expect_identical(result$stops$stop_lat, c(0, NA, NA))
  expect_identical(result$stops$stop_lon, c(0, NA, NA))

  # a parent station without name is named after its platforms

  gtfs <- new_feed(
    data.table::data.table(
      stop_id = c("S", "a", "b"),
      stop_name = c(NA, "A", "B"),
      stop_lat = c(0, 0, 1),
      stop_lon = 0,
      location_type = c(1L, 0L, 0L),
      parent_station = c("", "S", "S")
    )
  )
  expect_identical(tester(gtfs, 10)$stops$stop_name, "A-B")

  ggl_gtfs <- read_gtfs(ggl_path)
  ggl_gtfs <- copy_gtfs_without_field(ggl_gtfs, "stops", "parent_station")
  ggl_gtfs <- copy_gtfs_without_field(ggl_gtfs, "stops", "location_type")
  ggl_gtfs <- copy_gtfs_without_file(ggl_gtfs, "transfers")
  result <- tester(ggl_gtfs, 10)
  expect_s3_class(result, "dt_gtfs")
  expect_false("location_type" %in% names(result$stops))
  expect_true("F12-F12N-F12S" %chin% result$stops$stop_id)
})
