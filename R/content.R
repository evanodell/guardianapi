

#' Content
#'
#' See the
#' [API docs](https://open-platform.theguardian.com/documentation/search) for
#' full details on the query options available for this API.
#'
#' @param query Search query string.
#' @param ... Use to pass any other parameters to the API.
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples



gu_content <- function(query, ..., verbose = TRUE) {
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

  query <- paste0(
    query, "?api-key=", getOption("gu.API.key"), dots_query
  )

}
