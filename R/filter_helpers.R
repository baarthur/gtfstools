# keeps (or drops, through `%ffilter%`) the rows of 'file' whose 'field' is in
# 'ids'. the file and the field are optional: when either is absent the feed
# is returned as is. with keep_blank, rows whose field is "" are always kept,
# because a blank key means "applies to all"

filter_table_from_ids <- function(gtfs,
                                  file,
                                  field,
                                  ids,
                                  `%ffilter%`,
                                  keep_blank = FALSE) {
  if (!gtfsio::check_field_exists(gtfs, file, field)) return(gtfs)

  gtfsio::assert_field_class(gtfs, file, field, "character")

  values <- gtfs[[file]][[field]]
  is_kept <- values %ffilter% ids
  if (keep_blank) is_kept <- is_kept | values %chin% ""

  gtfs[[file]] <- gtfs[[file]][is_kept]

  return(gtfs)
}

filter_agency_from_agency_id <- function(gtfs, ids, `%ffilter%`) {
  # since agency_id is optional in many tables when the feed is only composed by
  # one agency, we may have situations in which ids is NULL (e.g. when trying
  # to get gtfs$routes$agency_id from a routes table without an agency_id
  # column). so if ids is NULL, we keep the gtfs intact. otherwise, we would
  # wrongfully subset agency down to an empty table.

  if (is.null(ids)) return(gtfs)

  return(filter_table_from_ids(gtfs, "agency", "agency_id", ids, `%ffilter%`))
}

filter_agency_from_derived_agency_id <- function(gtfs, relevant_agencies) {
  # agency_id may be blank or absent in single-agency feeds, so we keep
  # 'agency' intact in such cases

  if (NROW(gtfs$agency) <= 1L) return(gtfs)

  return(filter_agency_from_agency_id(gtfs, relevant_agencies, `%chin%`))
}

filter_attributions_from_agency_id <- function(gtfs, ids, `%ffilter%`) {
  gtfs <- filter_table_from_ids(
    gtfs,
    "attributions",
    "agency_id",
    ids,
    `%ffilter%`,
    keep_blank = TRUE
  )

  return(gtfs)
}

filter_fare_attr_from_agency_id <- function(gtfs,
                                            relevant_agencies,
                                            `%ffilter%`) {
  if (gtfsio::check_field_exists(gtfs, "fare_attributes", "agency_id")) {
    gtfsio::assert_field_class(
      gtfs,
      "fare_attributes",
      "agency_id",
      "character"
    )
    # fares without an agency_id can't be matched to an agency, so they're
    # always kept

    gtfs$fare_attributes <- gtfs$fare_attributes[
      is.na(agency_id) |
        agency_id %chin% "" |
        agency_id %ffilter% relevant_agencies
    ]
  }

  return(gtfs)
}

filter_fare_attr_from_fare_id <- function(gtfs, ids, `%ffilter%`) {
  # fares of feeds without a 'fare_rules' table apply to every route of their
  # agency, so they are kept if their agency still has routes (or if they
  # have no agency). the callers filter 'fare_rules' before calling this
  # function, but never remove the table, so an absent table means that the
  # feed had none (unlike remove_unused_ids(), an empty table doesn't count,
  # since it may have been emptied by the filtering). 'routes' is already
  # filtered by the callers, while 'agency' isn't yet

  if (is.null(gtfs$fare_rules)) {
    if (NROW(gtfs$agency) <= 1L) {
      return(gtfs)
    }

    # multi-agency feeds must list the agency_id of each route

    gtfs <- filter_fare_attr_from_agency_id(
      gtfs,
      c(NA_character_, "", unique(gtfs$routes$agency_id)),
      `%chin%`
    )

    return(gtfs)
  }

  gtfs <- filter_table_from_ids(
    gtfs,
    "fare_attributes",
    "fare_id",
    ids,
    `%ffilter%`
  )

  return(gtfs)
}

filter_routes_from_agency_id <- function(gtfs, ids, `%ffilter%`) {
  return(filter_table_from_ids(gtfs, "routes", "agency_id", ids, `%ffilter%`))
}

filter_fare_rules_from_route_id <- function(gtfs, ids, `%ffilter%`) {
  gtfs <- filter_table_from_ids(
    gtfs,
    "fare_rules",
    "route_id",
    ids,
    `%ffilter%`,
    keep_blank = TRUE
  )

  return(gtfs)
}

filter_fare_rules_from_zone_id <- function(gtfs, ids, `%ffilter%`) {
  for (zone_field in c("origin_id", "destination_id")) {
    gtfs <- filter_table_from_ids(
      gtfs,
      "fare_rules",
      zone_field,
      ids,
      `%ffilter%`,
      keep_blank = TRUE
    )
  }

  # the spec mentions that if the same fare_id is associated to many zones in
  # contains_id, all contains_ids must be matched for the fare to apply.
  # using their example: (https://gtfs.org/schedule/reference/#fare_rulestxt)
  #
  # fare_id,route_id,...,contains_id
  # c,GRT,...,5
  # c,GRT,...,6
  # c,GRT,...,7
  #
  # an itinerary that passes through zones 5 and 6 but not zone 7 would not have
  # fare class "c".
  #
  # in this case, when filtering fare_rules by contains_id we have to check if
  # all referred contains_ids exist in stops (or if all ids are not in stops,
  # when dropping ids), otherwise we can drop the fare class entirely

  if (gtfsio::check_field_exists(gtfs, "fare_rules", "contains_id")) {
    gtfsio::assert_field_class(gtfs, "fare_rules", "contains_id", "character")

    .all_but_contains <- setdiff(names(gtfs$fare_rules), "contains_id")

    # the flags are kept in a separate vector, instead of a column, because
    # 'fare_rules' may still be the table of the original gtfs object, which
    # must not be modified

    contains_ids <- gtfs$fare_rules$contains_id
    .flagged <- contains_ids %chin% "" | contains_ids %ffilter% ids

    gtfs$fare_rules <- gtfs$fare_rules[
      gtfs$fare_rules[, .I[all(.flagged[.I])], by = .all_but_contains]$V1
    ]
  }

  return(gtfs)
}

filter_trips_from_route_id <- function(gtfs, ids, `%ffilter%`) {
  return(filter_table_from_ids(gtfs, "trips", "route_id", ids, `%ffilter%`))
}

filter_trips_from_shape_id <- function(gtfs, relevant_shapes, `%ffilter%`) {
  if (gtfsio::check_field_exists(gtfs, "trips", "shape_id")) {
    gtfsio::assert_field_class(gtfs, "trips", "shape_id", "character")
    gtfs$trips <- gtfs$trips[shape_id %ffilter% relevant_shapes]
  } else if (gtfsio::check_file_exists(gtfs, "trips")) {
    warning(
      "'trips' table missing 'shape_id' column, therefore kept intact during ",
      "the filtering process.",
      call. = FALSE
    )
  }

  return(gtfs)
}

filter_trips_from_service_id <- function(gtfs, ids, `%ffilter%`) {
  return(filter_table_from_ids(gtfs, "trips", "service_id", ids, `%ffilter%`))
}

filter_shapes_from_shape_id <- function(gtfs, ids, `%ffilter%`) {
  return(filter_table_from_ids(gtfs, "shapes", "shape_id", ids, `%ffilter%`))
}

filter_calendar_from_service_id <- function(gtfs, ids, `%ffilter%`) {
  gtfs <- filter_table_from_ids(
    gtfs,
    "calendar",
    "service_id",
    ids,
    `%ffilter%`
  )

  return(gtfs)
}

filter_calend_dates_from_service_id <- function(gtfs, ids, `%ffilter%`) {
  gtfs <- filter_table_from_ids(
    gtfs,
    "calendar_dates",
    "service_id",
    ids,
    `%ffilter%`
  )

  return(gtfs)
}

filter_frequencies_from_trip_id <- function(gtfs, ids, `%ffilter%`) {
  gtfs <- filter_table_from_ids(
    gtfs,
    "frequencies",
    "trip_id",
    ids,
    `%ffilter%`
  )

  return(gtfs)
}

filter_stop_times_from_trip_id <- function(gtfs, ids, `%ffilter%`) {
  return(filter_table_from_ids(gtfs, "stop_times", "trip_id", ids, `%ffilter%`))
}

filter_stop_times_from_stop_id <- function(gtfs, ids, `%ffilter%`) {
  return(filter_table_from_ids(gtfs, "stop_times", "stop_id", ids, `%ffilter%`))
}

filter_transfers_from_stop_id <- function(gtfs, relevant_stops, `%ffilter%`) {
  from_to_stop_id <- c("from_stop_id", "to_stop_id")

  if (gtfsio::check_field_exists(gtfs, "transfers", from_to_stop_id)) {
    gtfsio::assert_field_class(
      gtfs,
      "transfers",
      from_to_stop_id,
      rep("character", 2)
    )
    gtfs$transfers <- gtfs$transfers[
      from_stop_id %chin% "" | from_stop_id %ffilter% relevant_stops
    ]
    gtfs$transfers <- gtfs$transfers[
      to_stop_id %chin% "" | to_stop_id %ffilter% relevant_stops
    ]
  }

  return(gtfs)
}

filter_transfers_from_route_id <- function(gtfs, ids, `%ffilter%`) {
  for (field in c("from_route_id", "to_route_id")) {
    gtfs <- filter_table_from_ids(
      gtfs,
      "transfers",
      field,
      ids,
      `%ffilter%`,
      keep_blank = TRUE
    )
  }

  return(gtfs)
}

filter_transfers_from_trip_id <- function(gtfs, ids, `%ffilter%`) {
  for (field in c("from_trip_id", "to_trip_id")) {
    gtfs <- filter_table_from_ids(
      gtfs,
      "transfers",
      field,
      ids,
      `%ffilter%`,
      keep_blank = TRUE
    )
  }

  return(gtfs)
}

filter_stops_from_stop_id <- function(gtfs, ids, `%ffilter%`) {
  return(filter_table_from_ids(gtfs, "stops", "stop_id", ids, `%ffilter%`))
}

filter_pathways_from_stop_id <- function(gtfs, relevant_stops, `%ffilter%`) {
  from_to_stop_id <- c("from_stop_id", "to_stop_id")

  if (gtfsio::check_field_exists(gtfs, "pathways", from_to_stop_id)) {
    gtfsio::assert_field_class(
      gtfs,
      "pathways",
      from_to_stop_id,
      rep("character", 2)
    )
    gtfs$pathways <- gtfs$pathways[from_stop_id %ffilter% relevant_stops]
    gtfs$pathways <- gtfs$pathways[to_stop_id %ffilter% relevant_stops]
  }

  return(gtfs)
}

filter_levels_from_level_id <- function(gtfs, ids, `%ffilter%`) {
  return(filter_table_from_ids(gtfs, "levels", "level_id", ids, `%ffilter%`))
}

filter_trips_from_trip_id <- function(gtfs, ids, `%ffilter%`) {
  return(filter_table_from_ids(gtfs, "trips", "trip_id", ids, `%ffilter%`))
}

filter_routes_from_route_id <- function(gtfs, ids, `%ffilter%`) {
  return(filter_table_from_ids(gtfs, "routes", "route_id", ids, `%ffilter%`))
}




get_stops_and_parents <- function(gtfs) {
  relevant_stops <- unique(gtfs$stop_times$stop_id)

  # get_parent_station() raises a warning if a stop is present in 'stop_times'
  # but not in 'stops', but we don't need to worry about it

  if (gtfsio::check_field_exists(gtfs, "stops", "parent_station")) {
    suppressWarnings(
      stops_with_parents <- get_parent_station(gtfs, relevant_stops)
    )
    relevant_stops <- stops_with_parents$stop_id

    # also keep the entrances, generic nodes and boarding areas (location_type
    # 2, 3 and 4) whose parents are kept
    if (gtfsio::check_field_exists(gtfs, "stops", "location_type")) {
      children <- gtfs$stops$stop_id[
        gtfs$stops$location_type %in% 2:4 &
          gtfs$stops$parent_station %chin% relevant_stops
      ]
      relevant_stops <- unique(c(relevant_stops, children))
    }
  }

  return(relevant_stops)
}

# the derived steps shared by the filter_by_*() cascades (not every filter
# uses all three, and seed steps may come between them): once 'stop_times' is
# final, the stop tables follow from it; the zones and levels follow from the
# stops; the fares and agencies follow from the routes and fare rules. each
# helper filters with %chin%, as the ids are derived from the feed

# keeps the stops served by the remaining 'stop_times' and their parents (and
# the entrances, nodes and boarding areas of kept stations), and filters
# 'transfers', 'pathways' and 'stops' by them

filter_stops_from_stop_times <- function(gtfs) {
  relevant_stops <- get_stops_and_parents(gtfs)

  gtfs <- filter_transfers_from_stop_id(gtfs, relevant_stops, `%chin%`)
  gtfs <- filter_pathways_from_stop_id(gtfs, relevant_stops, `%chin%`)
  gtfs <- filter_stops_from_stop_id(gtfs, relevant_stops, `%chin%`)

  return(gtfs)
}

# filters 'fare_rules' by the zones and 'levels' by the levels of the
# remaining 'stops'

filter_zones_levels_from_stops <- function(gtfs) {
  relevant_zones <- unique(gtfs$stops$zone_id)
  relevant_levels <- unique(gtfs$stops$level_id)

  gtfs <- filter_fare_rules_from_zone_id(gtfs, relevant_zones, `%chin%`)
  gtfs <- filter_levels_from_level_id(gtfs, relevant_levels, `%chin%`)

  return(gtfs)
}

# keeps the fares that the remaining 'fare_rules' refer to (or, in feeds
# without 'fare_rules', the fares of the remaining routes' agencies), then the
# agencies that the remaining routes and fares refer to: agency can be reached
# via routes -> agency_id and via fare_rules -> fare_id -> fare_attributes ->
# agency_id, so both paths are used (feeds with one agency always keep it,
# and also keep all their fares when they have no 'fare_rules' table, see
# filter_fare_attr_from_fare_id()). 'routes' and 'fare_rules' must already be
# filtered

filter_fares_and_agency <- function(gtfs) {
  relevant_fares <- unique(gtfs$fare_rules$fare_id)
  gtfs <- filter_fare_attr_from_fare_id(gtfs, relevant_fares, `%chin%`)

  relevant_agencies <- unique(
    c(gtfs$routes$agency_id, gtfs$fare_attributes$agency_id)
  )
  gtfs <- filter_agency_from_derived_agency_id(gtfs, relevant_agencies)

  return(gtfs)
}
