library(testthat)

test_that("create_triangle works", {
  df <- data.frame(
    accident = c(1,1,2),
    dev = c(1,2,1),
    value = c(10, 20, 30)
  )
  tri <- create_triangle(df, "accident", "dev", "value")
  expect_equal(dim(tri), c(2,2))
  expect_equal(tri[1,1], 10)
})
