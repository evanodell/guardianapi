

#' Sections
#'
#' @description Returns details on the sections and subsections used to
#' organise content.
#'
#' @description See the
#' [API docs](https://open-platform.theguardian.com/documentation/section) for
#' full details on the query options available for the sections endpoint.
#'
#' @param query A string, containing the search query. Defaults to `NULL`,
#' which returns all available sections subject to other parameters.
#' Supports AND, OR and NOT operators, and exact phrase queries
#' using double quotes. E.g. `'"football" OR "politics"'`. Also accepts a
#' character vector of section names and returns those sections.
#' @inheritParams gu_content
#'
#' @export
#' @examples
#' \dontrun{
#' business <- gu_section(query = "business")
#' 
#' foot_pol <- gu_section(query = c("politics", "business", "football"))
#' }
#' 
gu_section <- function(query = NULL, ..., verbose = TRUE,
                       tidy = TRUE, tidy_style = "snake_case") {
  if (!is.null(query)) {
    search_query <- paste0(
      "sections?q=",
      utils::URLencode(paste0(query, collapse = ",")),
      "&"
    )
  } else {
    search_query <- "sections?"
  }

  search_query_url <- paste0(
    base_url, search_query, "api-key=", getOption("gu.API.key")
  )

  df <- gu_data_grabber(search_query_url, verbose)

  if (tidy == TRUE) {
    df <- gu_tidy(df, tidy_style)
    if ("editions" %in% colnames(df)) {
      for (i in seq_along(df$editions)) {
        df$editions[[i]] <- gu_tidy(df$editions[[i]], tidy_style)
      }
    }
  }
  df
}
