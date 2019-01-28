
#' Tags
#'
#' All the tags used on the Guardian website. See the
#' [API docs](https://open-platform.theguardian.com/documentation/tag) on this
#' endpoint for more details.
#'
#' @param query A string, which will return all tags containing that string.
#' @param tag_type One of "`keyword`", "`series`", "`contributor`",
#' "`tone`", "`type`" or "`blog`". Defaults to `NULL` and does not filter by
#' tag type.
#' @param section Return only tags of a given section.
#' @param references Return only tags with those references
#' @param reference_type Return only tags with those reference types.
#' @param show_references Show associated reference data such as ISBNs.
#' Defaults to `"all"` and shows all available references. Accepts character
#' vectors of one or more references (see details for options).
#' @param ... Use to pass any other parameters to the API. See the
#' [docs](https://open-platform.theguardian.com/documentation/tag) for a
#' full list of options.
#' @inheritParams gu_content
#'
#' @section References options:
#'
#' The following are the options for the `show_references` parameter:
#'
#' - "`all`" Includes all the fields (default)
#' - "`author`"
#' - "`bisac-prefix`"
#' - "`esa-cricket-match`"
#' - "`esa-football-match`"
#' - "`esa-football-team`"
#' - "`esa-football-tournament`"
#' - "`isbn`"
#' - "`imdb`"
#' - "`musicbrainz`"
#' - "`musicbrainzgenre`"
#' - "`opta-cricket-match`"
#' - "`opta-football-match`"
#' - "`opta-football-team`"
#' - "`opta-football-tournament`"
#' - "`pa-football-competition`"
#' - "`pa-football-match`"
#' - "`pa-football-team`"
#' - "`r1-film`"
#' - "`reuters-index-ric`"
#' - "`reuters-stock-ric`"
#' - "`witness-assignment`"
#'
#' @return A tibble with details on tags.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return all tags containing "apple"
#' apple1 <- gu_tags(query = "apple")
#' 
#' # Return all tags containing "apple" in the technology section
#' apple2 <- gu_tags(query = "apple", section = "technology")
#' 
#' # Return all contributor tags in the life and style section
#' tag_sec_type <- gu_tags(section = "lifeandstyle", tag_type = "contributor")
#' }
#' 
gu_tags <- function(query = NULL, tag_type = NULL,
                    section = NULL, references = NULL, reference_type = NULL,
                    show_references = "all", ..., verbose = TRUE,
                    tidy = TRUE, tidy_style = "snake_case") {
  if (!is.null(query)) {
    search_query <- paste0("tags?q=", utils::URLencode(query), "&")
  } else {
    search_query <- "tags?"
  }

  if (is.null(show_references)) {
    show_references <- "all"
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

  if (!is.null(references)) {
    references_query <- paste0(
      "&reference=",
      paste0(references, collapse = ",")
    )
  } else {
    references_query <- ""
  }


  if (!is.null(reference_type)) {
    references_type_query <- paste0(
      "&reference-type=",
      paste0(reference_type, collapse = ",")
    )
  } else {
    references_type_query <- ""
  }

  if (!is.null(show_references)) {
    show_references_query <- paste0(
      "&show-references=",
      paste0(show_references, collapse = ",")
    )
  } else {
    show_references_query <- ""
  }

  if (!is.null(section)) {
    section_query <- paste0("&section=", paste0(section, collapse = ","))
  } else {
    section_query <- ""
  }

  if (!is.null(tag_type)) {
    tag_type <- tolower(tag_type)

    if (tag_type %in% c(
      "keyword", "series", "contributor",
      "tone", "type", "blog"
    )) {
      tag_type_query <- paste0("&type=", tag_type)
    } else {
      warning("tag_type not set, please check your parameters.")
      tag_type_query <- ""
    }
  } else {
    tag_type_query <- ""
  }

  search_query_url <- paste0(
    base_url, search_query, "api-key=", getOption("gu.API.key"),
    show_references_query, dots_query, tag_type_query,
    references_query, references_type_query, section_query
  )

  df <- gu_data_grabber(search_query_url, verbose)

  if (tidy == TRUE) {
    df <- gu_tidy(df, tidy_style)
  }
  df
}
