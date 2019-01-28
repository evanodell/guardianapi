


#' API Key
#'
#' @description A function to assign or re-assign the API key. Register for
#' an API key on the
#' [Guardian Open Platform](https://open-platform.theguardian.com/access/) site.
#'
#' @description By default, `guardianapi` will look for the `GU_API_KEY`
#' environmental variable when the package is loaded. If found the key is stored
#' in the session option `gu.API.key`. This function can be used to set the key
#' for a single session. To avoid having to use this function, use the .Renviron
#' file to store you key as `GU_API_KEY`.
#'
#'
#' @param check_env If `TRUE`, will check the environment variable `GU_API_KEY`
#' first before asking for user input. If found, assigns the API key to the
#' `gu.API.key` variable.
#'
#' @export

gu_api_key <- function(check_env = FALSE) {
  if (check_env) {
    key <- Sys.getenv("GU_API_KEY")
    if (key != "") {
      message("Updating gu.API.key session variable...")
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

  message("Updating gu.API.key session variable...")
  options("gu.API.key" = key)
  invisible()
}
