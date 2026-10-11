ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
ggl_gtfs <- read_gtfs(ggl_path)

test_that("filter_fare_rules_from_zone_id() works correctly", {
  test_gtfs <- ggl_gtfs
  test_gtfs$fare_rules <- data.table::copy(ggl_gtfs$fare_rules)

  test_gtfs$fare_rules <- rbind(
    test_gtfs$fare_rules,
    data.table::data.table(
      fare_id = "d",
      route_id = "GRT",
      origin_id = "",
      destination_id = "",
      contains_id = c("2", "4")
    )
  )

  test_gtfs$fare_rules <- rbind(
    test_gtfs$fare_rules,
    data.table::data.table(
      fare_id = "e",
      route_id = "GRT",
      origin_id = "",
      destination_id = "",
      contains_id = c("1", "2", "4")
    )
  )

  filtered_gtfs_keeping <- filter_fare_rules_from_zone_id(
    test_gtfs,
    c("2", "4"),
    `%chin%`
  )
  expect_identical(
    filtered_gtfs_keeping$fare_rules,
    data.table::data.table(
      fare_id = c("a", "a", "d", "d"),
      route_id = c("GRT", "GRJ", "GRT", "GRT"),
      origin_id = c("2", "4", "", ""),
      destination_id = c("4", "2", "", ""),
      contains_id = c("", "", "2", "4")
    )
  )

  filtered_gtfs_dropping <- filter_fare_rules_from_zone_id(
    test_gtfs,
    c("2", "4"),
    Negate(`%chin%`)
  )
  expect_identical(
    filtered_gtfs_dropping$fare_rules,
    data.table::data.table(
      fare_id = c(rep("a", 6), "b", "c"),
      route_id = c("TSW", "TSE", "GRT", "GRJ", "SVJ", "JSV", "GRT", "GRT"),
      origin_id = c(rep("1", 6), "3", ""),
      destination_id = c(rep("1", 6), "3", ""),
      contains_id = c(rep("", 7), "6")
    )
  )
})

test_that("fare_attributes are kept when the feed has no fare_rules", {
  no_rules_gtfs <- ggl_gtfs
  no_rules_gtfs$fare_rules <- NULL
  trip_ids <- ggl_gtfs$trips$trip_id
  route_ids <- ggl_gtfs$routes$route_id

  # single-agency feed: every fare is kept by every filter

  filtered <- list(
    filter_by_trip_id(no_rules_gtfs, trip_ids),
    filter_by_route_id(no_rules_gtfs, route_ids),
    filter_by_service_id(no_rules_gtfs, unique(ggl_gtfs$trips$service_id)),
    filter_by_stop_id(no_rules_gtfs, ggl_gtfs$stops$stop_id)
  )
  for (result in filtered) {
    expect_identical(result$fare_attributes, ggl_gtfs$fare_attributes)
  }

  # multi-agency feed: fares are kept if their agency still has routes, or if
  # they don't have an agency

  two_agencies_gtfs <- no_rules_gtfs
  two_agencies_gtfs$agency <- rbind(
    ggl_gtfs$agency,
    data.table::copy(ggl_gtfs$agency)[, agency_id := "agency002"]
  )
  two_agencies_gtfs$routes <- data.table::copy(ggl_gtfs$routes)[
    ,
    agency_id := data.table::fifelse(route_id == "A", "agency001", "agency002")
  ]
  two_agencies_gtfs$fare_attributes <- data.table::copy(
    ggl_gtfs$fare_attributes
  )[
    ,
    agency_id := c("agency001", "agency002", "", NA, "agency002")
  ]

  result <- filter_by_route_id(two_agencies_gtfs, "A")
  expect_identical(result$agency$agency_id, "agency001")
  expect_identical(result$fare_attributes$fare_id, c("1", "3", "4"))

  # an empty selection doesn't bring the other agencies back

  result <- filter_by_route_id(two_agencies_gtfs, character(0))
  expect_identical(nrow(result$agency), 0L)
  expect_identical(result$fare_attributes$fare_id, c("3", "4"))
})
