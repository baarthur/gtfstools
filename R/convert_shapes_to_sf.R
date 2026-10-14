#' Convert `shapes` table to simple feature object (deprecated)
#'
#' @description
#' This function is deprecated: calling it raises a warning. Please use
#' [get_shapes_geometry()] instead, which takes the same arguments (except
#' `sort_sequence`) and returns the same result.
#'
#' @template gtfs
#' @param shape_id A character vector including the `shape_id`s to be converted.
#'   If `NULL` (the default), all shapes are converted.
#' @param crs The CRS of the resulting object, either as an EPSG code or as an
#'   `crs` object. Defaults to 4326 (WGS 84).
#' @param sort_sequence Deprecated; do not supply. Shapes are now always sorted
#'   by `shape_pt_sequence`. Passing `TRUE` raises a warning, and passing
#'   `FALSE` raises an error.
#'
#' @return A `LINESTRING sf` object.
#'
#' @seealso [get_shapes_geometry()], which replaces this function.
#'
#' @export
convert_shapes_to_sf <- function(
  gtfs,
  shape_id = NULL,
  crs = 4326,
  sort_sequence
) {
  cli::cli_warn(
    class = "deprecated_convert_shapes_to_sf",
    message = c(
      "{.fn convert_shapes_to_sf} is deprecated.",
      "i" = paste0(
        "Please use {.fn get_shapes_geometry} instead, which takes the same ",
        "arguments."
      )
    )
  )

  if (!missing(sort_sequence)) {
    check_deprecated_sort_sequence(
      sort_sequence,
      "convert_shapes_to_sf",
      "shape_pt_sequence"
    )
  }

  shapes_sf <- get_shapes_geometry(
    gtfs = gtfs,
    shape_id = shape_id,
    crs = crs
  )

  return(shapes_sf)
}
