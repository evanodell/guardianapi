context("test-sections")

test_that("sections works", {
  skip_on_cran()
  options("gu.API.key" = "test")

  sec1 <- gu_section()
  expect_true(tibble::is_tibble(sec1))
  expect_true("artanddesign" %in% sec1$id)
})
