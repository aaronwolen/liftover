context("Chain files")

test_that("chain file URLs are valid", {
  url1 <- chain_url(from = "hg18", to = "hg19")
  url2 <- chain_url(from = "mm8",  to = "mm9")
  url3 <- chain_url(from = "mm9",  to = "hg19")
  
  expect_true(url.exists(url1))
  expect_true(url.exists(url2))
  expect_true(url.exists(url3))
})
