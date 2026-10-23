available_versions <- c(
  "latest",
  "6.0.0",
  "5.0.1",
  "5.0.0",
  "4.2.0",
  "4.1.0",
  "4.0.0",
  "3.1.1",
  "3.1.0",
  "3.0.1",
  "3.0.0"
)
tmpdir <- tempfile("gtfsvalidator")
dir.create(tmpdir)

tester <- function(path = tmpdir,
                   version = "latest",
                   force = FALSE,
                   quiet = TRUE) {
  download_validator(path, version, force, quiet)
}

test_that("raises error due to incorrect input types", {
  expect_error(tester(path = "non_existent_dir"))
  expect_error(tester(path = 1))
  expect_error(tester(path = c(tmpdir, tmpdir)))

  expect_error(tester(version = 3))
  expect_error(tester(version = c("latest", "latest")))
  expect_error(tester(version = "a"))

  expect_error(tester(force = 1))
  expect_error(tester(force = c(TRUE, TRUE)))
  expect_error(tester(force = NA))

  expect_error(tester(quiet = 1))
  expect_error(tester(quiet = c(TRUE, TRUE)))
  expect_error(tester(quiet = NA))
})

new_tmpdir <- function() {
  dir <- tempfile("gtfsvalidator")
  dir.create(dir)
  return(dir)
}

# writes a dummy file to 'path' and then raises the real httr2 error for the
# given status, mimicking httr2, which keeps the error body in 'path'

failing_download <- function(status) {
  function(req, path) {
    writeLines("error body", path)
    httr2::with_mocked_responses(
      function(req) httr2::response(status),
      httr2::req_perform(req)
    )
  }
}

test_that("network problems when getting the latest version don't error", {
  httr2::local_mocked_responses(function(req) httr2::response(503))

  # the message is shown even with quiet = TRUE (tester()'s default)

  cnd <- expect_message(
    result <- tester(path = new_tmpdir()),
    class = "gtfstools_validator_download_failure"
  )
  expect_null(result)
  expect_match(conditionMessage(cnd), "releases/latest")
  expect_match(conditionMessage(cnd), "Try again later")
})

test_that("unexpected redirects of the latest version don't error", {
  httr2::local_mocked_responses(
    function(req) {
      httr2::response(
        200,
        url = "https://github.com/MobilityData/gtfs-validator/releases"
      )
    }
  )

  cnd <- expect_message(
    result <- tester(path = new_tmpdir()),
    class = "gtfstools_validator_download_failure"
  )
  expect_null(result)
  expect_match(conditionMessage(cnd), "latest validator version")
})

test_that("retry-after waits are capped", {
  resp <- httr2::response(429, headers = list(`Retry-After` = "3600"))
  expect_identical(cap_retry_after(resp), 30)
  expect_true(is.na(cap_retry_after(httr2::response(503))))
})

test_that("network problems when downloading don't error nor leave files", {
  dir <- new_tmpdir()
  local_mocked_bindings(perform_download = failing_download(503))

  expect_message(
    result <- tester(path = dir, version = "6.0.0"),
    class = "gtfstools_validator_download_failure"
  )
  expect_null(result)
  expect_identical(list.files(dir), character(0))
})

test_that("connection failures report their cause", {
  dir <- new_tmpdir()

  # a closed local port fails right away, without the network

  local_mocked_bindings(
    perform_download = function(req, path) {
      httr2::req_perform(httr2::request("http://127.0.0.1:9"))
    }
  )

  cnd <- expect_message(
    result <- tester(path = dir, version = "6.0.0"),
    class = "gtfstools_validator_download_failure"
  )
  expect_null(result)
  expect_match(conditionMessage(cnd), "internet connection")
  expect_match(conditionMessage(cnd), "127.0.0.1")
})

test_that("non-existent versions raise a version error", {
  dir <- new_tmpdir()
  local_mocked_bindings(perform_download = failing_download(404))

  cnd <- expect_error(
    tester(path = dir, version = "9.9.9"),
    class = "gtfstools_invalid_validator_version"
  )
  expect_identical(conditionCall(cnd)[[1]], quote(download_validator))
  expect_match(conditionMessage(cnd), "list_validator_versions")
  expect_identical(list.files(dir), character(0))
})

test_that("successful downloads are saved to path", {
  dir <- new_tmpdir()
  local_mocked_bindings(
    perform_download = function(req, path) writeLines("jar", path)
  )

  result <- tester(path = dir, version = "9.9.9")
  expect_identical(
    result,
    normalizePath(file.path(dir, "gtfs-validator-v9.9.9.jar"))
  )
  expect_identical(list.files(dir), "gtfs-validator-v9.9.9.jar")
})

test_that("path defaults to tempdir()", {
  local_mocked_bindings(
    perform_download = function(req, path) writeLines("jar", path)
  )
  expected <- file.path(tempdir(), "gtfs-validator-v9.9.9.jar")
  on.exit(unlink(expected), add = TRUE)

  result <- download_validator(version = "9.9.9", force = TRUE)
  expect_identical(result, normalizePath(expected))
})

test_that("previously downloaded validators are used without going online", {
  dir <- new_tmpdir()
  cached <- file.path(dir, "gtfs-validator-v6.0.0.jar")
  writeLines("jar", cached)
  local_mocked_bindings(perform_download = failing_download(503))
  httr2::local_mocked_responses(function(req) stop("went online"))

  expect_identical(tester(path = dir, version = "6.0.0"), normalizePath(cached))
})

test_that("list_validator_versions() doesn't error on network problems", {
  httr2::local_mocked_responses(function(req) httr2::response(503))

  expect_message(
    result <- list_validator_versions(),
    class = "gtfstools_validator_download_failure"
  )
  expect_null(result)
})

test_that("latest version is found", {
  skip_on_cran()
  skip_if_offline()

  validator_url <- get_validator_url("latest", quiet = TRUE)
  expect_match(names(validator_url), "^\\d+\\.\\d+\\.\\d+$")
})

testthat::skip() # calls skip_on_cran()

test_that("validator is saved to path", {
  expect_identical(list.files(tmpdir), character(0))
  tester()
  expect_true(length(list.files(tmpdir)) == 1)
})

test_that("correct version is downloaded", {
  # latest defaults to highest version, else downloads specified version
  tester(version = "latest")
  expect_true(grepl("gtfs-validator-v6\\.0\\.0\\.jar$", list.files(tmpdir)))

  tester(version = "3.0.0")
  expect_true(
    any(grepl("gtfs-validator-v3\\.0\\.0\\.jar$", list.files(tmpdir)))
  )
})

test_that("returns normalized path to validator", {
  normalized_path <- tester(version = "3.1.0")
  manually_built_path <- file.path(tmpdir, "gtfs-validator-v3.1.0.jar")
  expect_identical(normalized_path, normalizePath(manually_built_path))
})

test_that("force argument works", {
  download_time <- system.time(tester(force = TRUE))
  cache_time <- system.time(tester())
  expect_true(download_time["elapsed"] > cache_time["elapsed"])
})

test_that("quiet argument is respected", {
  # first when not hitting the cache

  expect_silent(tester(force = TRUE))

  download_messages <- tempfile("download_messages", fileext = ".txt")
  expect_message(
    utils::capture.output(
      tester(force = TRUE, quiet = FALSE),
      file = download_messages,
      type = "message"
    ),
    label = "Downloading https:\\/\\/.*\\.jar to .*\\.jar\\."
  )

  curl_messages <- readLines(download_messages)
  expect_true(any(grepl("\\[\\d*%\\] Downloaded \\d* bytes", curl_messages)))

  # and then when using cached validator

  expect_silent(tester())
  expect_message(
    tester(quiet = FALSE),
    label = "^Using previously downloaded validator"
  )
})

test_that("all versions can be downloaded", {
  paths <- vapply(
    available_versions,
    function(v) tester(tempdir(), version = v),
    character(1)
  )
  expect_true(paths["latest"] == paths["6.0.0"])

  non_latest <- paths[-1]
  manually_built <- file.path(
    tempdir(),
    paste0("gtfs-validator-v", names(non_latest), ".jar")
  )
  expect_true(all(normalizePath(manually_built) == non_latest))
})
