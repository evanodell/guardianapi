context("test-tags")

test_that("tags work", {
  skip_on_cran()
  options("gu.API.key" = "test")

  tags1 <- gu_tags(query = "apple")
  expect_length(tags1, 13)
  expect_true(tibble::is_tibble(tags1))
  expect_true("web_title" %in% colnames(tags1))

  tag_regs <- gu_tags(reference_type = "isbn")
  expect_length(tag_regs, 14)
  expect_true(tibble::is_tibble(tag_regs))
  expect_true("web_title" %in% colnames(tag_regs))
  expect_true("books/the-female-eunuch" %in% tag_regs$id)

  tag_isbn <- gu_tags(references = "isbn/9780349108391")
  expect_length(tag_isbn, 8)
  expect_true(tibble::is_tibble(tag_isbn))
  expect_true("web_title" %in% colnames(tag_isbn))
  expect_equal("Generation X", tag_isbn$web_title)


  tag_sec_type <- gu_tags(section = "lifeandstyle", tag_type = "contributor")
  expect_length(tag_sec_type, 13)
  expect_true("Yotam Ottolenghi" %in% tag_sec_type$web_title)
  expect_equal(tag_sec_type$type[[1]], "contributor")
})
