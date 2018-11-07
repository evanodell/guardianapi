

#' guardianapi
#'
#'
#' @docType package
#' @name guardianapi
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble enframe
#' @importFrom httr http_type GET http_error status_code
#' @importFrom dplyr bind_rows
#' @importFrom utils menu
#' @importFrom rsdmx readSDMX
#' @importFrom rlang list2
#' @aliases NULL nomisr-package
NULL

# Checking for API key on package load
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("nomisr.API.key"))) {
    key <- Sys.getenv("NOMIS_API_KEY")
    if (key != "") options("nomisr.API.key" = key)
  }

  invisible()
}
