gu_data_grabber <- function(search_query_url, verbose) {
  query_params <- httr::parse_url(search_query_url)$query
  single_page <- any(c("page", "page-size") %in% names(query_params))

  if (is.null(query_params[["page-size"]])) {
    search_query_url <- paste0(search_query_url, "&page-size=50")
  }

  if (verbose == TRUE && !single_page) {
    message("Retrieving page 1")
  }

  first_df <- jsonlite::fromJSON(search_query_url,
    flatten = TRUE
  )

  jpage <- first_df$response$pages

  jpage <- ifelse(is.null(jpage), 1, jpage)

  if (verbose == TRUE && single_page) {
    page <- first_df$response$currentPage
    if (is.null(page) || jpage == 0L) {
      message("Returned ", NROW(first_df$response$results), " results")
    } else {
      message("Returned page ", page, " of ", jpage)
    }
  }

  if (single_page) {
    return(tibble::as_tibble(first_df$response$results))
  }

  pages <- list()

  if (jpage >= 2) {
    seq_list <- seq(from = 2, to = jpage, by = 1)

    for (i in seq_along(seq_list)) {
      if (verbose == TRUE) {
        message("Retrieving additional page ", seq_list[[i]], " of ", jpage)
      }

      mydata <- jsonlite::fromJSON(
        paste0(search_query_url, "&page=", seq_list[[i]]),
        flatten = TRUE
      )

      pages[[seq_list[[i]]]] <- mydata$response$results
    }

    df <- tibble::as_tibble(dplyr::bind_rows(first_df$response$results, pages))
  } else {
    df <- tibble::as_tibble(first_df$response$results)
  }

  df
}
