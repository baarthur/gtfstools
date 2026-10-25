#' Combine nearby stops
#'
#' Merges the stops that are within a given distance of each other into a
#' single stop, and updates the tables that refer to them. Useful to build
#' routing networks or to analyse the feed by station rather than by
#' platform.
#'
#' @template gtfs
#' @param max_dist A number. The maximum distance, in meters, between two
#'   stops merged together.
#'
#' @return The GTFS object with the merged stops.
#'
#' @section Details:
#' Stops with a `parent_station` are always merged into one stop with the
#' `stop_id` of the parent station, regardless of the distance between them.
#' This stop is named after the parent station if it's listed in `stops`, or
#' else after its platforms. Then stops are merged by distance, so
#' that every pair of stops merged together is at most `max_dist` apart. The
#' closest pairs are merged first. Stops that were already merged by their
#' parent station are compared by the mean of their coordinates, so their own
#' platforms may be farther apart. Distances are great-circle distances, as in
#' [get_stop_distances()]. Stops without coordinates are only merged by their
#' parent station.
#'
#' A stop merged from several stops has a `stop_id` and a `stop_name` made of
#' the ids and names of the merged stops, separated by `"-"` (repeated names
#' are listed only once), and the mean of their coordinates. Its other fields,
#' such as `zone_id` and `level_id`, are those of the merged stop whose
#' `stop_id` comes first. If a `stop_id` is listed more than once in `stops`,
#' only its first entry is used.
#'
#' The result has only plain stops (`location_type` 0) without
#' `parent_station`: stations are replaced by the stop merged from their
#' platforms, and stations without platforms, entrances, generic nodes,
#' boarding areas and `pathways` are removed. The `stop_id`s of merged stops
#' and stations are updated in `stop_times`, `transfers`, `stop_areas`,
#' `location_group_stops` and `fare_leg_join_rules`, but references to
#' removed stops are left as they are. Transfers between stops that were
#' merged together become transfers within the merged stop, and when several
#' transfers end up with the same stops, routes and trips, only the first one
#' is kept, even if their `transfer_type` or `min_transfer_time` differ. The
#' other fields of transfers are not changed, so a `min_transfer_time`
#' between platforms that were merged now applies within the merged stop, and
#' may no longer be accurate. Since `transfers` is optional, it can be removed
#' (`gtfs$transfers <- NULL`) when these rules are not needed. Duplicated
#' rows of `stop_areas` and `location_group_stops` are removed. Translations
#' of merged stops are not updated. References to
#' removed stops and their translations can be removed with
#' [remove_unused_ids()].
#'
#' An error is raised if `max_dist` is so large that all stops would be merged
#' into a single one. Large values of `max_dist` in dense feeds compare many
#' pairs of stops, which is slow and raises a warning when it may use a lot of
#' memory, or an error when there are too many pairs to compare.
#'
#' Merged `stop_id`s can be long at large terminals, and are ambiguous when
#' the original `stop_id`s include `"-"`. Feeds that cross the antimeridian
#' are not supported.
#'
#' @seealso [get_stop_distances()], [merge_gtfs()]
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#' nrow(gtfs$stops)
#'
#' combined_gtfs <- combine_nearby_stops(gtfs, max_dist = 100)
#' nrow(combined_gtfs$stops)
#' @export
combine_nearby_stops <- function(gtfs, max_dist) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_number(max_dist, lower = 0, finite = TRUE)
  gtfsio::assert_field_class(
    gtfs,
    "stops",
    c("stop_id", "stop_lat", "stop_lon"),
    c("character", "numeric", "numeric")
  )

  has_parent <- gtfsio::check_field_exists(gtfs, "stops", "parent_station")
  has_location_type <- gtfsio::check_field_exists(
    gtfs,
    "stops",
    "location_type"
  )
  has_name <- gtfsio::check_field_exists(gtfs, "stops", "stop_name")

  if (has_parent) {
    gtfsio::assert_field_class(gtfs, "stops", "parent_station", "character")
  }
  if (has_location_type) {
    gtfsio::assert_field_class(gtfs, "stops", "location_type", "integer")
  }

  # copied so that sorting it doesn't change the given gtfs. sorting by
  # stop_id makes the result independent of the order of the rows

  stops <- unique(data.table::copy(gtfs$stops), by = "stop_id")
  data.table::setorderv(stops, "stop_id")

  location_type <- if (has_location_type) {
    stops$location_type
  } else {
    rep(NA_integer_, nrow(stops))
  }
  is_plain <- is.na(location_type) | location_type == 0L
  is_station <- !is_plain & location_type == 1L

  plain <- stops[is_plain]

  if (nrow(plain) == 0L) {
    cli::cli_abort(
      c(
        "{.file stops} has no stops to merge.",
        "i" = "Only stops with {.field location_type} 0 or empty are merged."
      ),
      class = "gtfstools_no_stops_to_merge"
    )
  }

  is_invalid <- (!is.na(plain$stop_lat) & abs(plain$stop_lat) > 90) |
    (!is.na(plain$stop_lon) & abs(plain$stop_lon) > 180)

  if (any(is_invalid)) {
    invalid_stops <- plain$stop_id[is_invalid]
    cli::cli_abort(
      c(
        "Stops can't have invalid coordinates.",
        "x" = "{.field stop_lat} must be between -90 and 90, and
               {.field stop_lon} between -180 and 180, but not for
               {.val {invalid_stops}}."
      ),
      class = "gtfstools_invalid_coordinates"
    )
  }

  # stops are first grouped by parent station. parents that are entrances,
  # generic nodes or boarding areas are not stations, so they are ignored

  plain_names <- if (has_name) plain$stop_name else rep("", nrow(plain))

  # a stop with one missing coordinate has no position

  has_coords <- !is.na(plain$stop_lat) & !is.na(plain$stop_lon)

  members <- data.table::data.table(
    node_key = get_stop_node_keys(
      plain$stop_id,
      if (has_parent) plain$parent_station else NULL,
      stops$stop_id[!is_plain & !is_station]
    ),
    stop_name = plain_names,
    stop_lat = data.table::fifelse(has_coords, plain$stop_lat, NA_real_),
    stop_lon = data.table::fifelse(has_coords, plain$stop_lon, NA_real_)
  )

  nodes <- members[
    ,
    .(
      node_lat = mean(stop_lat, na.rm = TRUE),
      node_lon = mean(stop_lon, na.rm = TRUE),
      node_name = paste_stop_names(stop_name)
    ),
    keyby = node_key
  ]
  nodes[is.nan(node_lat) | is.nan(node_lon), c("node_lat", "node_lon") := NA]

  # nodes named after a stop in the feed (a parent station or a platform that
  # other platforms point to) take its name

  if (has_name) {
    key_stop <- data.table::chmatch(nodes$node_key, stops$stop_id)
    is_listed <- !is.na(key_stop) & !is.na(stops$stop_name[key_stop])
    nodes[is_listed, node_name := stops$stop_name[key_stop[is_listed]]]
  }

  nodes[, cluster := cluster_stop_nodes(node_lat, node_lon, max_dist)]

  if (nrow(nodes) > 1L && all(nodes$cluster == 1L)) {
    formatted_dist <- format(max_dist, big.mark = ",", scientific = FALSE)
    cli::cli_abort(
      c(
        "A {.arg max_dist} of {formatted_dist} m merges all {nrow(nodes)}
         stops or stations into a single stop.",
        "i" = "Please use a smaller {.arg max_dist}."
      ),
      class = "gtfstools_single_stop_error"
    )
  }

  clusters <- nodes[
    ,
    .(
      new_id = paste(node_key, collapse = "-"),
      new_name = paste_stop_names(node_name)
    ),
    keyby = cluster
  ]

  stop_cluster <- nodes$cluster[
    data.table::chmatch(members$node_key, nodes$node_key)
  ]
  members[, cluster := stop_cluster]
  coordinates <- members[
    ,
    .(
      stop_lat = mean(stop_lat, na.rm = TRUE),
      stop_lon = mean(stop_lon, na.rm = TRUE)
    ),
    keyby = cluster
  ]
  coordinates[
    is.nan(stop_lat) | is.nan(stop_lon),
    c("stop_lat", "stop_lon") := NA
  ]

  # the other fields of a merged stop come from the stop whose stop_id comes
  # first, which is its first row since stops are sorted

  is_first <- !duplicated(stop_cluster)
  first_cluster <- stop_cluster[is_first]
  new_stops <- plain[is_first]
  new_stops[, stop_id := clusters$new_id[first_cluster]]
  if (has_name) new_stops[, stop_name := clusters$new_name[first_cluster]]
  new_stops[, stop_lat := coordinates$stop_lat[first_cluster]]
  new_stops[, stop_lon := coordinates$stop_lon[first_cluster]]
  if (has_location_type) new_stops[, location_type := 0L]
  if (has_parent) new_stops[, parent_station := ""]
  data.table::setorderv(new_stops, "stop_id")

  duplicated_ids <- unique(new_stops$stop_id[duplicated(new_stops$stop_id)])

  if (length(duplicated_ids) > 0) {
    cli::cli_abort(
      c(
        "Merging the stops creates duplicated {.field stop_id}s.",
        "x" = "{.val {duplicated_ids}} would be listed more than once.",
        "i" = "Merged {.field stop_id}s join the ids of their stops with
               {.val -}, so they may match existing ids."
      ),
      class = "gtfstools_duplicated_stop_id"
    )
  }

  # stations are mapped to the stop merged from their platforms

  old_id <- c(plain$stop_id, nodes$node_key)
  new_id <- c(
    clusters$new_id[stop_cluster],
    clusters$new_id[nodes$cluster]
  )
  is_unique <- !duplicated(old_id)
  old_id <- old_id[is_unique]
  new_id <- new_id[is_unique]

  gtfs$stops <- new_stops
  gtfs$pathways <- NULL

  references <- stop_id_references()

  for (file in names(references)) {
    if (!data.table::is.data.table(gtfs[[file]])) next

    fields <- intersect(references[[file]], names(gtfs[[file]]))
    if (length(fields) == 0) next

    table <- data.table::copy(gtfs[[file]])

    for (field in fields) {
      gtfsio::assert_field_class(gtfs, file, field, "character")
      data.table::set(
        table,
        j = field,
        value = replace_stop_ids(table[[field]], old_id, new_id)
      )
    }

    if (file == "transfers") {
      transfer_keys <- intersect(
        c(
          "from_stop_id",
          "to_stop_id",
          "from_route_id",
          "to_route_id",
          "from_trip_id",
          "to_trip_id"
        ),
        names(table)
      )
      table <- unique(table, by = transfer_keys)
    } else if (file %chin% c("stop_areas", "location_group_stops")) {
      table <- unique(table)
    }

    gtfs[[file]] <- table
  }

  return(gtfs)
}



#' Fields that refer to stop_ids, by file
#'
#' @return A named list of character vectors.
#'
#' @keywords internal
stop_id_references <- function() {
  list(
    stop_times = "stop_id",
    transfers = c("from_stop_id", "to_stop_id"),
    stop_areas = "stop_id",
    location_group_stops = "stop_id",
    fare_leg_join_rules = c("from_stop_id", "to_stop_id")
  )
}



#' Replace stop_ids
#'
#' @param x A character vector of `stop_id`s.
#' @param old_id,new_id Character vectors with the old and the new `stop_id`
#'   of each stop.
#'
#' @return `x`, with the ids found in `old_id` replaced by their `new_id`.
#'
#' @keywords internal
replace_stop_ids <- function(x, old_id, new_id) {
  replaced <- data.table::fcoalesce(new_id[data.table::chmatch(x, old_id)], x)

  return(replaced)
}



#' Paste stop names
#'
#' @param x A character vector of stop names.
#'
#' @return The unique, non-missing names joined by `"-"`, or `NA` if there are
#'   none.
#'
#' @keywords internal
paste_stop_names <- function(x) {
  x <- unique(x[!is.na(x)])
  if (length(x) == 0L) return(NA_character_)

  return(paste(x, collapse = "-"))
}



#' Get the node of each stop
#'
#' Groups stops by parent station: the node of a stop is its topmost parent.
#' Parents that are other stops are followed up to a stop without parent, by
#' pointer doubling, and the parent of that stop, if any, is the node.
#'
#' @param stop_id A character vector of `stop_id`s, without duplicates.
#' @param parent_station A character vector with the parent of each stop, or
#'   `NULL`.
#' @param ignored_parents A character vector of `stop_id`s that are not
#'   considered parents.
#'
#' @return A character vector with the node of each stop.
#'
#' @keywords internal
get_stop_node_keys <- function(stop_id, parent_station, ignored_parents) {
  if (is.null(parent_station)) return(stop_id)

  parent <- parent_station
  has_no_parent <- is.na(parent) |
    parent == "" |
    parent == stop_id |
    parent %chin% ignored_parents
  parent[has_no_parent] <- NA_character_

  # each pass doubles the number of levels skipped, so ceiling(log2(n))
  # passes reach the top of any chain. the top of a chain has no parent among
  # the stops, so a stop whose top still has one is in, or leads to, a cycle
  # (doubling may also stop early on a cycle of even length)

  up <- data.table::chmatch(parent, stop_id)
  top <- seq_along(stop_id)
  top[!is.na(up)] <- up[!is.na(up)]

  max_passes <- ceiling(log2(max(length(top), 2))) + 2

  for (pass in seq_len(max_passes)) {
    next_top <- top[top]
    if (identical(next_top, top)) break
    top <- next_top
  }

  in_cycle <- !is.na(up[top])

  if (any(in_cycle)) {
    cycle_stops <- stop_id[in_cycle]
    cli::cli_abort(
      c(
        "{.field parent_station} can't form cycles.",
        "x" = "The parents of {.val {cycle_stops}} lead back to themselves."
      ),
      class = "gtfstools_parent_station_cycle"
    )
  }

  node_key <- stop_id[top]
  top_parent <- parent[top]
  has_top_parent <- !is.na(top_parent)
  node_key[has_top_parent] <- top_parent[has_top_parent]

  return(node_key)
}



# the sphere used by the haversine distances (as in {s2}), shared by the
# clustering bound and the pair search so that they agree

earth_radius <- 6371010
to_radians <- pi / 180



#' Cluster stop nodes by distance
#'
#' Finds the pairs of nodes within `max_dist` of each other with
#' `find_stop_pairs_within()` and merges them with `cpp_merge_stop_clusters()`.
#'
#' @param lat,lon Numeric vectors with the coordinates of each node, in
#'   degrees.
#' @param max_dist A number, in meters.
#'
#' @return An integer vector with the cluster of each node. When every pair
#'   of nodes is surely within `max_dist`, all nodes are in cluster 1 and the
#'   pairs are not calculated.
#'
#' @keywords internal
cluster_stop_nodes <- function(lat, lon, max_dist) {
  n_nodes <- length(lat)
  has_coords <- !is.na(lat) & !is.na(lon)
  node <- which(has_coords)
  lat_rad <- lat[node] * to_radians
  lon_rad <- lon[node] * to_radians

  if (length(node) < 2) return(seq_len(n_nodes))

  # a path along a meridian and then along a parallel is never shorter than
  # the great-circle distance, so this bounds every pairwise distance. the
  # margin keeps rounding from making it disagree with the distances below

  if (all(has_coords)) {
    lat_range <- range(lat_rad)
    closest_to_equator <- if (lat_range[1] <= 0 && lat_range[2] >= 0) {
      0
    } else {
      min(abs(lat_range))
    }
    max_possible_dist <- earth_radius * (
      diff(lat_range) + diff(range(lon_rad)) * cos(closest_to_equator)
    )

    if (max_possible_dist * (1 + 1e-9) <= max_dist) {
      return(rep(1L, n_nodes))
    }
  }

  # the errors are raised from this function's frame, so that they mention it
  # rather than the closure that checks the number of pairs

  env <- environment()

  check_n_pairs <- function(n_pairs) {
    check_stop_pair_count(n_pairs, max_dist, "max_dist", "Merging stops", env)
  }

  pairs <- find_stop_pairs_within(lat, lon, max_dist, check_n_pairs)
  data.table::setorderv(pairs, c("distance", "from", "to"))

  cluster <- cpp_merge_stop_clusters(n_nodes, pairs$from, pairs$to)

  return(cluster)
}



#' Check the number of pairs of stops before comparing them
#'
#' Raises an error if the pairs can't be indexed, and a warning if comparing
#' them may use a lot of memory (each pair takes about 64 bytes at the peak).
#'
#' @param n_pairs A number, the pairs of stops to be compared.
#' @param max_dist A number, the distance given by the user, in meters.
#' @param arg_name The name of the user-facing distance argument.
#' @param action A string that starts the warning, e.g. `"Merging stops"`.
#' @param call The environment of the user-facing function, so that the error
#'   mentions it.
#'
#' @return Invisibly returns `NULL`, if it doesn't raise an error.
#'
#' @keywords internal
check_stop_pair_count <- function(n_pairs, max_dist, arg_name, action, call) {
  if (n_pairs > .Machine$integer.max) {
    cli::cli_abort(
      c(
        "Too many pairs of stops to compare.",
        "x" = "{format(n_pairs, big.mark = ',', scientific = FALSE)} pairs
               would have to be compared.",
        "i" = "Please use a smaller {.arg {arg_name}}."
      ),
      class = "gtfstools_too_many_stops",
      call = call
    )
  }

  if (n_pairs > max_stop_pairs_without_warning()) {
    memory_gb <- signif(n_pairs * 64 / 1e9, 2)

    cli::cli_warn(
      c(
        paste0(
          action,
          " with a {.arg {arg_name}} of {max_dist} m may use a lot of memory."
        ),
        "i" = "{format(n_pairs, big.mark = ',', scientific = FALSE)} pairs of
               stops need at least about {memory_gb} GB.",
        "i" = "Consider using a smaller {.arg {arg_name}}."
      ),
      class = "gtfstools_many_stops_warning"
    )
  }

  return(invisible(NULL))
}



#' Find the pairs of stops within a distance of each other
#'
#' Finds the pairs of points within `max_dist` of each other with a grid of
#' cells at least `max_dist` wide, so that only points in neighbouring cells
#' are compared.
#'
#' @param lat,lon Numeric vectors with the coordinates of each point, in
#'   degrees. Points without coordinates are ignored.
#' @param max_dist A number, in meters.
#' @param check_n_pairs A function called with the number of pairs of points
#'   in neighbouring cells before they are listed, to check that they can be
#'   compared.
#'
#' @return A `data.table` with the indices of the two points of each pair
#'   (`from` smaller than `to`) and the `distance` between them, in meters.
#'
#' @keywords internal
find_stop_pairs_within <- function(lat, lon, max_dist, check_n_pairs) {
  has_coords <- !is.na(lat) & !is.na(lon)
  node <- which(has_coords)
  lat_rad <- lat[node] * to_radians
  lon_rad <- lon[node] * to_radians

  if (length(node) < 2) {
    empty_pairs <- data.table::data.table(
      from = integer(0),
      to = integer(0),
      distance = numeric(0)
    )
    return(empty_pairs)
  }

  # with these cell sizes, two points within dist are in the same or in
  # neighbouring cells: their latitudes differ by at most dist / R, and the
  # haversine formula bounds the difference of their longitudes by the width

  dist <- max(max_dist, 1)
  max_abs_lat <- max(abs(lat_rad))
  cell_height <- dist / earth_radius * (1 + 1e-9)
  cell_width <- 2 * asin(
    min(1, sin(dist / (2 * earth_radius)) / cos(max_abs_lat))
  ) * (1 + 1e-9)

  points <- data.table::data.table(
    node = node,
    row = as.integer(floor(lat_rad / cell_height)),
    col = as.integer(floor(lon_rad / cell_width))
  )

  # each pair of cells is visited once: the cell itself and 4 of its 8
  # neighbours

  row_offsets <- c(0L, 0L, 1L, 1L, 1L)
  col_offsets <- c(0L, 1L, -1L, 0L, 1L)

  cells <- points[, .N, by = c("row", "col")]
  n_pairs <- 0

  for (k in seq_along(row_offsets)) {
    neighbours <- data.table::data.table(
      row = cells$row - row_offsets[k],
      col = cells$col - col_offsets[k],
      neighbour_n = cells$N
    )
    matched <- cells[neighbours, on = c("row", "col"), nomatch = NULL]

    n_pairs <- n_pairs + if (k == 1) {
      sum(as.numeric(matched$N) * (matched$N - 1) / 2)
    } else {
      sum(as.numeric(matched$N) * matched$neighbour_n)
    }
  }

  check_n_pairs(n_pairs)

  pairs <- lapply(
    seq_along(row_offsets),
    function(k) {
      neighbours <- data.table::data.table(
        row = points$row - row_offsets[k],
        col = points$col - col_offsets[k],
        neighbour = points$node
      )
      matched <- points[
        neighbours,
        on = c("row", "col"),
        nomatch = NULL,
        allow.cartesian = TRUE
      ]
      if (k == 1) matched <- matched[matched$node < matched$neighbour]

      data.table::data.table(
        from = pmin(matched$node, matched$neighbour),
        to = pmax(matched$node, matched$neighbour)
      )
    }
  )
  pairs <- data.table::rbindlist(pairs)

  pairs[, distance := cpp_stop_distances(lat, lon, from, to)]
  pairs <- pairs[distance <= max_dist]

  return(pairs)
}
