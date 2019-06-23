context("test-content")


test_that("content testing works works", {
  skip_on_cran()
  options("gu.API.key" = "test")

  a <- gu_content(
    from_date = "2018-11-30", to_date = "2018-12-04",
    tag = "tone/reviews",
    show_fields = "all", "show-refinements" = "all"
  )
  #expect_equal(length(a), 42)
  expect_true(tibble::is_tibble(a))
  expect_equal(as.Date(mean(a$web_publication_date)), as.Date("2018-12-02"))

  b <- gu_content(
    query = '"football" OR "politics"',
    from_date = "2018-11-30", to_date = "2018-11-30"
  )
  #expect_equal(nrow(b), 73)
  #expect_length(names(b), 45)
  expect_true("Emma Brockes" %in% b$byline)

  c <- gu_content(
    query = '"football" AND "politics"',
    from_date = "2018-11-20", to_date = "2018-11-30"
  )
  #expect_equal(nrow(c), 14)
  #expect_length(names(c), 34)
  expect_true(
    "Momentum is giving people of colour a chance to shape Labour" %in% c$web_title
  )


  rel_sex <- gu_content(
    query = "relationships", from_date = "2018-11-30",
    to_date = "2018-12-30", tag = "lifeandstyle/sex"
  )
  expect_true(tibble::is_tibble(rel_sex))
  expect_length(rel_sex, 40)
  expect_equal(nrow(rel_sex), 5)
  expect_true("lifeandstyle" %in% rel_sex$section_id)
})
