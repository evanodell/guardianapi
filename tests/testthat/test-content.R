context("test-content")

test_that("content testing works works", {
  options("gu.API.key" = "test")

  a <- gu_content(from_date = "2018-11-30", to_date = "2018-12-04",
                  tag = "tone/reviews",
                  show_fields = "all", "show-refinements" = "all")

  b <- gu_content(query = '"football" OR "politics"',
                  from_date = "2018-11-30", to_date = "2018-11-30")
  expect_equal(nrow(b), 73)
  expect_length(names(b), 45)
  expect_true("Emma Brockes" %in% b$byline)


})
