context("test-single-item")

test_that("items works", {
  skip_on_cran()
  options("gu.API.key" = "test")

  item1 <- gu_items("profile/jeremy-cliffe")
  expect_true(tibble::is_tibble(item1))
  expect_true("Opinion" %in% item1$section_name)
})
