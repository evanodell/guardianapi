
gu_data_grabber <- function(search_query_url, verbose) {
  if (verbose == TRUE) {
    message("Retrieving page 1")
  }

  first_df <- jsonlite::fromJSON(paste0(search_query_url, "&page-size=50"),
    flatten = TRUE
  )

  jpage <- first_df$response$pages

  jpage <- ifelse(is.null(jpage), 1, jpage)

  pages <- list()

  if (jpage >= 2) {
    seq_list <- seq(from = 2, to = jpage, by = 1)

    for (i in seq_along(seq_list)) {
      if (verbose == TRUE) {
        message("Retrieving additional page ", seq_list[[i]], " of ", jpage)
      }

      mydata <- jsonlite::fromJSON(
        paste0(search_query_url, "&page-size=50&page=", seq_list[[i]]),
        flatten = TRUE
      )

      pages[[seq_list[[i]] ]] <- mydata$response$results
    }

    df <- tibble::as_tibble(dplyr::bind_rows(first_df$response$results, pages))
  } else {
    df <- tibble::as_tibble(first_df$response$results)
  }

  df
}
