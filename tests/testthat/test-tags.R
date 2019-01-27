context("test-tags")

test_that("tags work", {
  skip_on_cran()
  options("gu.API.key" = "test")

  tags1 <- gu_tags(query = "apple")
  expect_length(tags1, 13)
  expect_true(tibble::is_tibble(tags1))
  expect_true("web_title" %in% colnames(tags1))
})
