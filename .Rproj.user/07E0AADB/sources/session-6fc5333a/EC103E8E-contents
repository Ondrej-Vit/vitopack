library(testthat)
library(vitopack)

# Test data
df <- data.frame(
  accident = c(1,1,1,2,2,3,1),
  dev      = c(1,2,3,1,2,1,1),
  value    = c(10, 20, 30, 40, 50, 60, 60)
)

test_that("create_triangle works correctly", {
  tri <- create_triangle(df, "accident", "dev", "value")

  # Check dimensions
  expect_equal(dim(tri), c(3,3))

  # Check values manually
  expect_equal(tri[1,1], 70)
  expect_equal(tri[1,2], 20)
  expect_equal(tri[1,3], 30)
  expect_equal(tri[2,1], 40)
  expect_equal(tri[2,2], 50)
  expect_equal(tri[3,1], 60)
})

test_that("create_cumulative_triangle works correctly", {
  tri <- create_triangle(df, "accident", "dev", "value")
  cum_tri <- create_cumulative_triangle(tri)

  # Check cumulative sum along rows
  expect_equal(cum_tri[1,2], tri[1,1] + tri[1,2])
  expect_equal(cum_tri[2,2], tri[2,1] + tri[2,2])
})

test_that("create_decumulative_triangle works correctly", {
  tri <- create_triangle(df, "accident", "dev", "value")
  cum_tri <- create_cumulative_triangle(tri)
  decum_tri <- create_decumulative_triangle(cum_tri)

  # Should recover original triangle
  expect_equal(decum_tri, tri)
})
