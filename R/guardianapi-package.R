

#' Tge `guardianapi` package
#'
#' Access to 'The Guardian' open API
#' <https://open-platform.theguardian.com/>, containing all articles, video and
#' images published in the 'Guardian' from 1999 to the present. Users must
#' [register](https://open-platform.theguardian.com/access/) and use an API key,
#' which can be saved with the [gu_api_key()] function, or as the `GU_API_KEY`
#' environmental variable. Free users can make up
#' to 5,000 calls per day and 12 calls per second, and access all article text
#' and associated metadata.
#' Images and video require a commercial subscription.
#'
#'
#' @docType package
#' @name guardianapi
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble enframe
#' @importFrom httr http_type GET http_error status_code
#' @importFrom dplyr bind_rows
#' @importFrom utils menu
#' @importFrom rlang list2
#' @aliases NULL guardianapi-package
NULL

# Checking for API key on package load
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("gu.API.key"))) {
    key <- Sys.getenv("GU_API_KEY")
    if (key != "") options("gu.API.key" = key)
  }

  invisible()
}
