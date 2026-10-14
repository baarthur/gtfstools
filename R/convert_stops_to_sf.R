#' Convert `stops` table to simple feature object (deprecated)
#'
#' @description
#' This function is deprecated: calling it raises a warning. Please use
#' [get_stops_geometry()] instead, which takes the same arguments and returns
#' the same result.
#'
#' @template gtfs
#' @param stop_id A character vector including the `stop_id`s to be converted.
#'   If `NULL` (the default), all stops are converted.
#' @param crs The CRS of the resulting object, either as an EPSG code or as an
#'   `crs` object. Defaults to 4326 (WGS 84).
#'
#' @return A `POINT sf` object.
#'
#' @seealso [get_stops_geometry()], which replaces this function.
#'
#' @export
convert_stops_to_sf <- function(gtfs, stop_id = NULL, crs = 4326) {
  cli::cli_warn(
    class = "deprecated_convert_stops_to_sf",
    message = c(
      "{.fn convert_stops_to_sf} is deprecated.",
      "i" = paste0(
        "Please use {.fn get_stops_geometry} instead, which takes the same ",
        "arguments."
      )
    )
  )

  stops_sf <- get_stops_geometry(
    gtfs = gtfs,
    stop_id = stop_id,
    crs = crs
  )

  return(stops_sf)
}
