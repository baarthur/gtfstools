# reading a fixture takes a few seconds (mostly unzipping to a temp dir), and
# the tests read the same four fixtures hundreds of times. this wrapper reads
# each inst/extdata fixture once and hands out deep copies, so tests can still
# modify what they get. calls with extra arguments and non-fixture paths
# (e.g. files written by the tests) go straight to gtfstools::read_gtfs().
#
# this is a setup file, not a helper, because helpers are not visible to test
# code under devtools::test(). to test the real reader in a file, add
# read_gtfs <- gtfstools::read_gtfs at its top. tests of warnings or messages
# raised while reading must use the real reader, since they fire only once.

fixture_cache <- new.env(parent = emptyenv())
fixture_dir <- normalizePath(
  system.file("extdata", package = "gtfstools"),
  winslash = "/"
)

read_gtfs <- function(path, ...) {
  norm_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  is_fixture <- nzchar(fixture_dir) &&
    startsWith(norm_path, paste0(fixture_dir, "/"))

  if (...length() > 0L || !is_fixture) {
    return(gtfstools::read_gtfs(path, ...))
  }

  if (is.null(fixture_cache[[norm_path]])) {
    fixture_cache[[norm_path]] <- gtfstools::read_gtfs(path)
  }

  # always copy: tests modify what they get and check that inputs are left
  # unchanged (and without indices), which the shared cache must never see

  return(data.table::copy(fixture_cache[[norm_path]]))
}
