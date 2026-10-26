#' Get the `shapes` table with spatial geometry
#'
#' Converts the `shapes` table to a `LINESTRING sf` object.
#'
#' @template gtfs
#' @param shape_id A character vector including the `shape_id`s to be converted.
#'   If `NULL` (the default), all shapes are converted.
#' @param crs The CRS of the resulting object, either as an EPSG code or as an
#'   `crs` object. Defaults to 4326 (WGS 84).
#'
#' @return A `LINESTRING sf` object.
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' # read gtfs
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' shapes_sf <- get_shapes_geometry(gtfs)
#' head(shapes_sf)
#'
#' shapes_sf <- get_shapes_geometry(gtfs, shape_id = "17846")
#' shapes_sf
#'
#' @export
get_shapes_geometry <- function(
  gtfs,
  shape_id = NULL,
  crs = 4326
) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(shape_id, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert(
    checkmate::check_number(crs),
    checkmate::check_class(crs, "crs"),
    combine = "or"
  )

  assert_shapes_fields(gtfs, trips = FALSE)

  # select the relevant shape points into a new table, so the given gtfs is not
  # modified, and raise a warning if given shape_ids don't exist in shapes

  if (!is.null(shape_id)) {
    warn_missing_ids(shape_id, gtfs$shapes$shape_id, "shapes", "shape_id")
  }
  shapes <- prepare_shape_points(gtfs, shape_id)

  # create an empty LINESTRING sf if shapes is empty

  if (nrow(shapes) == 0) {
    empty_linestring <- sf::st_sfc()
    class(empty_linestring)[1] <- "sfc_LINESTRING"

    shapes_sf <- sf::st_sf(
      shape_id = character(),
      geometry = empty_linestring,
      stringsAsFactors = FALSE
    )
  } else {
    shapes_sf <- sfheaders::sf_linestring(
      shapes,
      x = "shape_pt_lon",
      y = "shape_pt_lat",
      linestring_id = "shape_id"
    )
  }

  shapes_sf <- sf::st_set_crs(shapes_sf, 4326)

  # transform crs from 4326 to the one passed to 'crs'

  if (crs != 4326 && crs != sf::st_crs(4326)) {
    shapes_sf <- sf::st_transform(shapes_sf, crs)
  }

  return(shapes_sf)
}
