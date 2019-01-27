context("test-tags")

test_that("multiplication works", {

  tags1 <- gu_tags(query = "apple")
  expect_length(tags1, 13)
  expect_true(tibble::is_tibble(tags1))
  expect_true("web_title" %in% colnames(tags1))

})
