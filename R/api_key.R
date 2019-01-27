#' API Key
#'
#' @param check_env If `TRUE`
#'
#' @export
#'

gu_api_key <- function(check_env = FALSE) {
  if (check_env) {
    key <- Sys.getenv("GU_API_KEY")
    if (key != "") {
      message("Updating GU_API_KEY environment variable...")
      options("gu.API.key" = key)
      return(invisible())
    } else {
      warning("Couldn't find environment variable 'GU_API_KEY'")
    }
  }

  if (interactive()) {
    key <- readline("Please enter your API key and press enter: ")
  } else {
    cat("Please enter your API key and press enter: ")
    key <- readLines(con = "stdin", n = 1)
  }

  if (identical(key, "")) {
    stop("Nomis API key entry failed", call. = FALSE)
  }

  message("Updating GU_API_KEY environment variable...")
  options("gu.API.key" = key)
  invisible()
}
