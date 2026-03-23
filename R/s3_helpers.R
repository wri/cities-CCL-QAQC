# Retry raster reads because remote S3/HTTP access can fail transiently.
rast_retry <- function(path, attempts = 6, base_sleep = 0.5, quiet = FALSE) {
  last_err <- NULL

  for (i in seq_len(attempts)) {
    rast <- tryCatch(
      terra::rast(path),
      error = function(e) {
        last_err <<- e
        NULL
      }
    )

    if (!is.null(rast)) {
      return(rast)
    }

    sleep_time <- base_sleep * (1.6 ^ (i - 1)) + stats::runif(1, 0, 0.25)
    if (!quiet) {
      message(sprintf("rast() failed (%d/%d). Retrying in %.2fs: %s", i, attempts, sleep_time, path))
    }
    Sys.sleep(sleep_time)
  }

  stop(
    sprintf(
      "[rast_retry] Failed after %d attempts:\n%s\n%s",
      attempts,
      path,
      conditionMessage(last_err)
    ),
    call. = FALSE
  )
}

# Retry raster merges because large multi-tile merges can fail intermittently.
merge_retry <- function(rasters, attempts = 6, base_sleep = 0.5, quiet = FALSE) {
  last_err <- NULL

  for (i in seq_len(attempts)) {
    merged <- tryCatch(
      do.call(terra::merge, rasters),
      error = function(e) {
        last_err <<- e
        NULL
      }
    )

    if (!is.null(merged)) {
      return(merged)
    }

    sleep_time <- base_sleep * (1.6 ^ (i - 1)) + stats::runif(1, 0, 0.25)
    if (!quiet) {
      message(sprintf("merge() failed (%d/%d). Retrying in %.2fs", i, attempts, sleep_time))
    }
    Sys.sleep(sleep_time)
  }

  stop(
    sprintf("[merge_retry] Failed after %d attempts:\n%s", attempts, conditionMessage(last_err)),
    call. = FALSE
  )
}

# Read one or more rasters and merge them into a single SpatRaster.
load_and_merge <- function(paths, quiet = FALSE) {
  stopifnot(length(paths) > 0)

  rasters <- lapply(paths, function(path) rast_retry(path, quiet = quiet))

  if (length(rasters) == 1) {
    return(rasters[[1]])
  }

  merge_retry(rasters, quiet = quiet)
}

# Decode the small set of XML entities returned by the S3 list endpoint.
xml_decode <- function(x) {
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  gsub("&apos;", "'", x, fixed = TRUE)
}

# Pull repeated XML tag values from a short S3 list response without adding an
# extra XML package dependency.
extract_xml_values <- function(xml_text, pattern) {
  matches <- regmatches(xml_text, gregexpr(pattern, xml_text, perl = TRUE))[[1]]

  if (!length(matches) || identical(matches, character(0))) {
    return(character(0))
  }

  xml_decode(sub(pattern, "\\1", matches, perl = TRUE))
}

# List public S3 bucket contents through the ListObjectsV2 HTTP endpoint.
s3_list_bucket_public <- function(bucket, prefix, delimiter = NULL) {
  prefix <- sub("^/+", "", prefix)
  results <- list(prefixes = character(0), keys = character(0))
  continuation_token <- NULL

  repeat {
    query <- c(
      "list-type=2",
      paste0("prefix=", URLencode(prefix, reserved = TRUE))
    )

    if (!is.null(delimiter)) {
      query <- c(query, paste0("delimiter=", URLencode(delimiter, reserved = TRUE)))
    }

    if (!is.null(continuation_token) && nzchar(continuation_token)) {
      query <- c(
        query,
        paste0("continuation-token=", URLencode(continuation_token, reserved = TRUE))
      )
    }

    request_url <- sprintf("https://%s.s3.us-east-1.amazonaws.com/?%s", bucket, paste(query, collapse = "&"))
    response <- tryCatch(
      paste(readLines(request_url, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      error = function(e) {
        stop(sprintf("Failed to list public S3 contents at %s\n%s", request_url, conditionMessage(e)), call. = FALSE)
      }
    )

    results$prefixes <- c(
      results$prefixes,
      extract_xml_values(response, "(?s)<CommonPrefixes>\\s*<Prefix>(.*?)</Prefix>\\s*</CommonPrefixes>")
    )
    results$keys <- c(
      results$keys,
      extract_xml_values(response, "<Key>(.*?)</Key>")
    )

    is_truncated <- extract_xml_values(response, "<IsTruncated>(.*?)</IsTruncated>")
    if (!length(is_truncated) || !identical(tolower(is_truncated[[1]]), "true")) {
      break
    }

    next_token <- extract_xml_values(response, "<NextContinuationToken>(.*?)</NextContinuationToken>")
    if (!length(next_token) || !nzchar(next_token[[1]])) {
      break
    }

    continuation_token <- next_token[[1]]
  }

  results$prefixes <- unique(results$prefixes)
  results$keys <- unique(results$keys)
  results
}

# List tile folders directly under a scenario prefix in a public S3 bucket.
list_tiles <- function(folder_s3_url) {
  folder_s3_url <- sub("/?$", "/", folder_s3_url)
  stripped <- sub("^s3://", "", folder_s3_url)
  bucket <- sub("/.*$", "", stripped)
  prefix <- sub("^[^/]+/?", "", stripped)

  listed <- s3_list_bucket_public(bucket = bucket, prefix = prefix, delimiter = "/")
  tile_prefixes <- grep("tile_[0-9]+/$", listed$prefixes, value = TRUE)
  sort(unique(sub("^.*(tile_[0-9]+)/$", "\\1", tile_prefixes)))
}
