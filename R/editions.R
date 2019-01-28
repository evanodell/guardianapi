
#' Editions
#'
#' The different main pages of the Guardian. As of January 2019 they are the
#' United Kingdom ("uk"), the United States ("us"), Australia ("au") and
#' an International ("international") front page.
#'
#' @param query A string, which will return editions based on that string.
#' Defaults to `NULL` and returns all editions. Strings are not case sensitive.
#' @param ... Pass additional options to API. There are no additional
#' options as of this writing. See the
#' [endpoint docs](https://open-platform.theguardian.com/documentation/edition)
#' @inheritParams gu_content
#'
#' @return A tibble with details of the given edition.
#' @export
#'
#' @examples
#' \dontrun{
#' uk <- gu_editions(query = "uk")
#' }
#' 
gu_editions <- function(query = NULL, ..., verbose = TRUE,
                        tidy = TRUE, tidy_style = "snake_case") {
  if (!is.null(query)) {
    search_query <- paste0("editions?q=", utils::URLencode(tolower(query)), "&")
  } else {
    search_query <- "editions?"
  }

  dots <- rlang::list2(...) ## eval the dots
  names(dots) <- toupper(names(dots))
  dots_vector <- c()

  for (i in seq_along(dots)) { # retrieve the dots
    dots_vector[i] <- ifelse(length(dots[[i]]) > 0,
      paste0(
        "&", toupper(names(dots[i])), "=",
        paste0(dots[[i]], collapse = ",")
      ),
      ""
    )
  }

  dots_query <- paste0(dots_vector, collapse = "")

  search_query_url <- paste0(
    base_url, search_query, "api-key=", getOption("gu.API.key"), dots_query
  )

  df <- gu_data_grabber(search_query_url, verbose)

  if (tidy == TRUE) {
    df <- gu_tidy(df, tidy_style)
  }
  df
}
