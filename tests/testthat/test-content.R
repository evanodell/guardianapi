context("test-content")


test_that("content testing works works", {
  skip_on_cran()
  options("gu.API.key" = "test")

  a <- gu_content(
    from_date = "2018-11-30", to_date = "2018-12-04",
    tag = "tone/reviews",
    show_fields = "all", "show-refinements" = "all"
  )
  expect_equal(length(a), 41)
  expect_true(tibble::is_tibble(a))
  expect_equal(as.Date(mean(a$web_publication_date)), as.Date("2018-12-02"))

  b <- gu_content(
    query = '"football" OR "politics"',
    from_date = "2018-11-30", to_date = "2018-11-30"
  )
  expect_equal(nrow(b), 73)
  expect_length(names(b), 45)
  expect_true("Emma Brockes" %in% b$byline)

  c <- gu_content(
    query = '"football" AND "politics"',
    from_date = "2018-11-30", to_date = "2018-11-30"
  )
  expect_equal(nrow(c), 1)
  expect_length(names(c), 34)
  expect_true(
    "Friday briefing: Who's not meeting who at the G20" %in% c$web_title
    )


})
