


# tidy function

gu_tidy <- function(df, tidy_style) {
  names(df) <- gsub("fields\\.", "", names(df), perl = TRUE)

  if ("standfirst" %in% names(df)) {
    df$standfirst <- gsub("<br>", " ", df$standfirst)
    df$standfirst <- gsub("<.*?>", "", df$standfirst)
    df$standfirst <- trimws(df$standfirst)
  }

  # converting date info to POSIXct or Date
  if (any(names(df) == "commentCloseDate")) {
    df$commentCloseDate <- as.POSIXct(gsub("T", " ", df$commentCloseDate))
  }

  if (any(names(df) == "webPublicationDate")) {
    df$webPublicationDate <- as.POSIXct(gsub("T", " ", df$webPublicationDate))
  }

  if (any(names(df) == "firstPublicationDate")) {
    df$firstPublicationDate <- as.POSIXct(gsub(
      "T", " ",
      df$firstPublicationDate
    ))
  }

  if (any(names(df) == "newspaperEditionDate")) {
    df$newspaperEditionDate <- as.Date(gsub("T", " ", df$newspaperEditionDate))
  }

  if (tidy_style == "camelCase") {

  } else if (tidy_style == "period.case") {
    names(df) <- gsub(
      "([[:lower:]])([[:upper:]])", "\\1.\\2", names(df),
      perl = TRUE
    )

    names(df) <- tolower(names(df))
  } else {
    names(df) <- gsub(
      "([[:lower:]])([[:upper:]])", "\\1_\\2", names(df),
      perl = TRUE
    )

    names(df) <- tolower(names(df))
  }

  df
}
