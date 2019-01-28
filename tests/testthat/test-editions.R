context("test-editions")

test_that("editions works", {
  skip_on_cran()
  options("gu.API.key" = "test")

  eds <- gu_editions()
  expect_true(tibble::is_tibble(eds))
  expect_true("AU" %in% eds$edition)

  uk_ed <- gu_editions("UK")
  expect_true(tibble::is_tibble(uk_ed))
  expect_true("UK" %in% uk_ed$edition)
  expect_equal(nrow(uk_ed), 1)

  uk_ed2 <- gu_editions("uk")
  expect_equal(uk_ed, uk_ed2)

  int_ed <- gu_editions("international")
  expect_true(tibble::is_tibble(int_ed))
  expect_true("International" %in% int_ed$edition)
  expect_equal(nrow(int_ed), 1)
})
