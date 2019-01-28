

#' Content
#'
#' @description Query and return all available content in the API.
#'
#' @description See the
#' [API docs](https://open-platform.theguardian.com/documentation/search) for
#' full details on the query options available for this content endpoint.
#'
#' @param query A string, containing the search query. Defaults to `NULL`,
#' which returns all available content subject to other parameters.
#' Supports AND, OR and NOT operators, and exact phrase queries
#' using double quotes. E.g. `'"football" OR "politics"'`.
#' @param show_fields A string or character vector of fields to include in
#' the returned data. Defaults to `"all"`. See details for a list of options.
#' @param show_tags A string or character vector of tags to include in
#' the returned data. Defaults to `"all"`. See details for a list of options.
#' @param tag A string or character vector of tags to filter
#' the returned data. Defaults to `NULL`.
#' @param from_date Accepts character values in `'YYYY-MM-DD'`
#' format, and objects of class `Date`, `POSIXt`, `POSIXct`,
#' `POSIXlt` or anything else that can be coerced to a date with
#' `as.Date()`. Defaults to `NULL`.
#' @param to_date Accepts character values in `'YYYY-MM-DD'`
#' format, and objects of class `Date`, `POSIXt`, `POSIXct`,
#' `POSIXlt` or anything else that can be coerced to a date with
#' `as.Date()`. Defaults to `NULL`.
#' @param use_date The date type to use for the `from_date` and `to_date`
#'  parameters. One of `"published"`, `"first-publication"`,
#'  `"newspaper-edition"` or `"last-modified"`. Defaults to `"published"`.
#' @param ... Use to pass any other parameters to the API. See the
#' [docs](https://open-platform.theguardian.com/documentation/search) for a
#' full list of options.
#' @param verbose Prints messages to console. Defaults to `TRUE`.
#' @param tidy Convert variable names to snake_case, remove some `"<NA>"`
#' strings. Defaults to `TRUE`.
#' @param tidy_style Style to variable names with.
#'
# @details Parameter details
#'
#' @section Fields options:
#'
#' The following are the options for the `show_fields` parameter:
#'
#' - "`all`" Includes all the fields (default)
#' - "`trailText`"
#' - "`headline`"
#' - "`showInRelatedContent`" Whether this content can appear in automatically
#' generated Related Content
#' - "`body`"
#' - "`lastModified`"
#' - "`hasStoryPackage`" Has related content selected by editors
#' - "`score`" A relevance score based on the search query used
#' - "`standfirst`"
#' - "`shortUrl`"
#' - "`thumbnail`"
#' - "`wordcount`"
#' - "`commentable`"
#' - "`isPremoderated`" Comments will be checked by a moderator prior to
#' publication if true.
#' - "`allowUgc`" May have associated User Generated Content. This typically
#' means the content has an associated Guardian Witness assignment
#' which can be accessed by querying "`show-references=witness-assignment`",
#' using the `query` parameter.
#' - "`byline`"
#' - "`publication`"
#' - "`internalPageCode`"
#' - "`productionOffice`"
#' - "`shouldHideAdverts`" Adverts will not be displayed if true
#' - "`liveBloggingNow`" Content is currently live blogged if true
#' - "`commentCloseDate`" The date the comments have been closed
#' - "`starRating`"
#'
#' @section #' The following are the options for the `show_tags` parameter:
#'
#' - "`blog`"
#' - "`contributor`"
#' - "`keyword`"
#' - "`newspaper-book`"
#' - "`newspaper-book-section`"
#' - "`publication`"
#' - "`series`"
#' - "`tone`"
#' - "`type`"
#' - "`all`": The default option.
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- gu_content(query = "films")
#' 
#' y <- gu_content(
#'   query = "relationships",
#'   from_date = "2018-11-30", to_date = "2018-12-30"
#' )
#' }
#' 
gu_content <- function(query = NULL, show_fields = "all", show_tags = "all",
                       tag = NULL, from_date = NULL, to_date = NULL,
                       use_date = "published", ..., verbose = TRUE,
                       tidy = TRUE, tidy_style = "snake_case") {
  if (!is.null(query)) {
    search_query <- paste0("search?q=", utils::URLencode(query), "&")
  } else {
    search_query <- "search?"
  }

  if (is.null(show_fields)) {
    show_fields <- "all"
  }

  if (is.null(show_tags)) {
    show_tags <- "all"
  }

  if (!is.null(from_date)) {
    from_date_q <- paste0("&from-date=", from_date)
  } else {
    from_date_q <- ""
  }

  if (!is.null(to_date)) {
    to_date_q <- paste0("&to-date=", to_date)
  } else {
    to_date_q <- ""
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

  fields_query <- paste0("&show-fields=", paste0(show_fields, collapse = ","))

  show_tags_query <- paste0("&show-tags=", paste0(show_tags, collapse = ","))

  tag_query <- ifelse(!is.null(tag),
    paste0("&tag=", paste0(tag, collapse = ",")), ""
  )

  search_query_url <- paste0(
    base_url, search_query, "api-key=", getOption("gu.API.key"),
    fields_query, dots_query, show_tags_query, tag_query, from_date_q,
    to_date_q
  )

  df <- gu_data_grabber(search_query_url, verbose)

  if (tidy == TRUE) {
    df <- gu_tidy(df, tidy_style)
  }
  df
}
