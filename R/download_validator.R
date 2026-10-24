#' Download MobilityData's GTFS validator
#'
#' Downloads MobilityData's command line tool to validate GTFS feeds.
#'
#' @param path A string. The directory where the validator should be saved to.
#'   By default, the function downloads the validator to a temporary
#'   directory, which is removed when the R session ends. Use a persistent
#'   directory to reuse the validator across sessions.
#' @param version A string. The version of the validator that should be
#'   downloaded. Defaults to `"latest"`, but accepts version numbers as strings
#'   (i.e. to download version v6.0.0 please enter `"6.0.0"`). Please check
#'   [MobilityData/gtfs-validator
#'   releases](https://github.com/MobilityData/gtfs-validator/releases) for the
#'   full set of available versions.
#' @param force A logical. Whether to overwrite a previously downloaded
#'   validator in `path`. Defaults to `FALSE`.
#' @param quiet A logical. Whether to hide log messages and progress bars.
#'   Defaults to `TRUE`.
#'
#' @return Invisibly returns the normalized path to the downloaded validator.
#'   If the validator can't be downloaded due to network problems (e.g. no
#'   internet connection or GitHub being unavailable), it informs that with a
#'   message and invisibly returns `NULL`.
#'
#' @family validation
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true") && curl::has_internet()
#' path <- tempdir()
#'
#' download_validator(path)
#'
#' # specifying a specific version
#' download_validator(path, version = "6.0.0")
#'
#' @export
download_validator <- function(
  path = tempdir(),
  version = "latest",
  force = FALSE,
  quiet = TRUE
) {
  call <- environment()

  checkmate::assert(
    checkmate::check_string(path),
    checkmate::check_directory_exists(path),
    checkmate::check_access(path, access = "w"),
    combine = "and"
  )
  checkmate::assert(
    checkmate::check_string(version, pattern = "\\d.\\d.\\d"),
    version == "latest",
    combine = "or"
  )
  checkmate::assert_logical(force, any.missing = FALSE, len = 1)
  checkmate::assert_logical(quiet, any.missing = FALSE, len = 1)

  validator_url <- get_validator_url(version, quiet)
  if (is.null(validator_url)) {
    return(invisible(NULL))
  }

  validator_basename <- paste0("gtfs-validator-v", names(validator_url), ".jar")
  output_file <- file.path(path, validator_basename)

  if (file.exists(output_file) && (!force)) {
    if (!quiet) {
      message(
        "Using previously downloaded validator found at ",
        output_file,
        "."
      )
    }
  } else {
    if (!quiet) {
      message("Downloading ", validator_url, " to ", output_file, ".")
    }

    # download to a temporary file first, so that a failed download never
    # leaves a broken validator at output_file

    tmp_file <- tempfile("gtfs-validator", tmpdir = path, fileext = ".jar")
    on.exit(unlink(tmp_file), add = TRUE)

    req <- validator_request(validator_url)
    if (!quiet) {
      req <- httr2::req_progress(req)
    }

    downloaded <- tryCatch(
      {
        quietly_if(perform_download(req, tmp_file), quiet)
        TRUE
      },
      httr2_http_404 = function(cnd) {
        cli::cli_abort(
          c(
            "Validator version {.val {names(validator_url)}} not found.",
            "i" = paste0(
              "Run {.fn gtfstools::list_validator_versions} to see the ",
              "available versions."
            )
          ),
          parent = cnd,
          class = "gtfstools_invalid_validator_version",
          call = call
        )
      },
      httr2_error = function(cnd) {
        inform_download_failure(validator_url, cnd)
        FALSE
      }
    )
    if (!downloaded) {
      return(invisible(NULL))
    }

    if (!file.rename(tmp_file, output_file)) {
      cli::cli_abort(
        "Could not save the validator to {.file {output_file}}.",
        call = call
      )
    }
  }

  return(invisible(normalizePath(output_file)))
}

# returns NULL, with a message, if the latest version can't be determined due
# to network problems

get_validator_url <- function(version, quiet) {
  base_url <- "https://github.com/MobilityData/gtfs-validator/releases/"

  if (version == "latest") {
    latest_url <- paste0(base_url, "latest")
    req <- tryCatch(
      quietly_if(
        validator_request(latest_url) |>
          httr2::req_method("HEAD") |>
          httr2::req_perform(),
        quiet
      ),
      httr2_error = function(cnd) {
        inform_download_failure(latest_url, cnd)
        NULL
      }
    )
    if (is.null(req)) {
      return(NULL)
    }

    version <- regmatches(req$url, regexpr("([^/v])+$", req$url))

    if (!isTRUE(grepl("^\\d+\\.\\d+\\.\\d+$", version))) {
      cli::cli_inform(
        c(
          "x" = paste0(
            "Could not determine the latest validator version from ",
            "{.url {req$url}}."
          )
        ),
        class = "gtfstools_validator_download_failure"
      )
      return(NULL)
    }
  }

  release_url <- paste0(base_url, "download/v", version, "/")
  cli_basename <- paste0("gtfs-validator-", version, "-cli.jar")

  if (numeric_version(version) < numeric_version("3.1.0")) {
    cli_basename <- sub("gtfs-validator-", "gtfs-validator-v", cli_basename)
    cli_basename <- sub("-cli", "_cli", cli_basename)
  }

  validator_url <- paste0(release_url, cli_basename)
  names(validator_url) <- version

  return(validator_url)
}

# connect timeout and stall detection, instead of a total timeout, so that a
# slow but progressing download of the validator is not killed

validator_request <- function(url) {
  req <- httr2::request(url) |>
    httr2::req_options(
      connecttimeout = 30,
      low_speed_limit = 1,
      low_speed_time = 60
    ) |>
    httr2::req_retry(
      max_tries = 3,
      max_seconds = 120,
      retry_on_failure = TRUE,
      after = cap_retry_after
    )

  return(req)
}

# a separate function so that tests can mock the download, since httr2's
# mocked responses are never written to 'path'

perform_download <- function(req, path) {
  httr2::req_perform(req, path = path)
}

# max_seconds doesn't limit the wait requested by a server's Retry-After header
# (which GitHub may send when rate limiting), so it's capped here. NA falls
# back to the default backoff

cap_retry_after <- function(resp) {
  return(min(httr2::resp_retry_after(resp), 30))
}

# httr2 announces retry backoffs with messages, which quiet must hide too. the
# failure messages below are emitted outside of it, so they are always shown

quietly_if <- function(expr, quiet) {
  if (quiet) {
    return(suppressMessages(expr))
  }
  return(expr)
}

# network problems must not raise errors (CRAN policy on internet resources),
# so they are reported with a message instead

inform_download_failure <- function(url, cnd) {
  cause <- conditionMessage(if (is.null(cnd$parent)) cnd else cnd$parent)

  hint <- if (inherits(cnd, "httr2_http")) {
    paste0(
      "GitHub may be temporarily unavailable or limiting requests. ",
      "Try again later."
    )
  } else {
    "Check your internet connection and try again."
  }

  cli::cli_inform(
    c("x" = "Could not reach {.url {url}}.", "!" = "{cause}", "i" = hint),
    class = "gtfstools_validator_download_failure"
  )
}


# list_validator_versions ---------------------------------------------------------------------

#' List MobilityData's GTFS validator versions
#'
#' Lists available versions of MobilityData's command line tool to validate GTFS feeds.
#'
#' @return A data.table with each CLI version, published date, and download URL.
#'   If the versions can't be listed due to network problems, it informs that
#'   with a message and invisibly returns `NULL`.
#'
#' @family validation
#'
#' @export

list_validator_versions <- function() {
  releases_url <- paste0(
    "https://api.github.com/repos/MobilityData/gtfs-validator/",
    "releases"
  )
  req <- tryCatch(
    validator_request(releases_url) |>
      httr2::req_headers(`User-Agent` = "httr2") |>
      httr2::req_perform(),
    httr2_error = function(cnd) {
      inform_download_failure(releases_url, cnd)
      NULL
    }
  )
  if (is.null(req)) {
    return(invisible(NULL))
  }

  data <- httr2::resp_body_json(req)

  versions <- lapply(
    data,
    function(x) {
      asset_nr <- x$assets |>
        lapply(\(x) grepl("jar$", x$name) & grepl("cli", x$name)) |>
        unlist() |>
        which()
      versions <- data.frame(
        version = x$tag_name,
        published_at = x$published_at,
        cli_download_url = ifelse(
          length(asset_nr) == 0,
          NA_character_,
          x$assets[[asset_nr]]$browser_download_url
        )
      )
      return(versions)
    }
  ) |>
    data.table::rbindlist()

  versions <- versions[!is.na(versions$cli_download_url), ]

  return(versions)
}
