gtfs1 <- (system.file("extdata/spo_gtfs.zip", package = "gtfstools"))

gtfs2 <- gtfstools::read_gtfs(system.file(
  "extdata/poa_gtfs.zip",
  package = "gtfstools"
))

get_summary(gtfs2, unit = "mph")

a <- build_graph(
  gtfs2,
  from = "08:00:00",
  to = "10:00:00",
  format = "tidygraph"
)

library(tidygraph)
library(ggraph)
library(sfnetworks)

ggraph(a, layout = 'fr') +
  geom_edge_link() +
  geom_node_point()

ggraph(a, layout = "manual", x = x_coord, y = y_coord) +
  geom_edge_link() +
  geom_node_point()


net <- as_sfnetwork(a)
plot(net)


# Update documentation
devtools::document(pkg = ".")


##### Coverage ------------------------

#  ERROR in shapeid 52936

library(gtfs2gps)
library(covr)
library(testthat)

function_coverage(
  fun = gtfs2gps::filter_day_period,
  test_file("tests/testthat/test_filter_day_period.R")
)
function_coverage(
  fun = gtfs2gps::test_gtfs_freq,
  test_file("./tests/testthat/test_test_gtfs_freq.R")
)
function_coverage(
  fun = gtfs2gps::gps_as_sflinestring,
  test_file("./tests/testthat/test_gps_as_sflinestring.R")
)
function_coverage(
  fun = gtfs2gps::gps_as_sfpoints,
  test_file("./tests/testthat/test_gps_as_sfpoints.R")
)


a <- function_coverage(
  fun = gtfs2gps::gtfs2gps,
  test_file("./tests/testthat/test_gtfs2gps.R")
)


a <- covr::package_coverage(path = ".", type = "tests")


##### Profiling function ------------------------
p <- profvis(update_newstoptimes("T2-1@1#2146"))

p <- profvis(b <- corefun("T2-1"))


# checks spelling
library(spelling)
devtools::spell_check(pkg = ".", vignettes = TRUE, use_wordlist = TRUE)

# Update documentation
devtools::document(pkg = ".")


### tests ----------------

devtools::test(stop_on_failure = T)

### CMD Check ----------------
# Check package errors

# LOCAL
Sys.setenv(NOT_CRAN = "true")
devtools::check(pkg = ".", cran = FALSE, env_vars = c(NOT_CRAN = "true"))

# CRAN
Sys.setenv(NOT_CRAN = "false")
devtools::check(pkg = ".", cran = TRUE, env_vars = c(NOT_CRAN = "false"))

# submit to CRAN -----------------
usethis::use_cran_comments('teste 2222, , asdadsad')

urlchecker::url_check()
devtools::check(remote = TRUE, manual = TRUE)
devtools::check_win_release()
devtools::check_mac_release()
rhub::check_for_cran(show_status = FALSE)


devtools::submit_cran()
