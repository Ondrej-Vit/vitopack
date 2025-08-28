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


test_that("create_cumulative_triangle is non-decreasing across development", {
  tri <- create_triangle(df, "accident", "dev", "value")
  cum_tri <- create_cumulative_triangle(tri)
  # pro každý řádek: cum[,j] >= cum[,j-1]
  for (j in 2:ncol(cum_tri)) {
    expect_true(all(cum_tri[, j] >= cum_tri[, j - 1], na.rm = TRUE))
  }
})

test_that("create_decumulative_triangle recovers original and has non-negative diffs", {
  tri <- create_triangle(df, "accident", "dev", "value")
  cum_tri <- create_cumulative_triangle(tri)
  decum_tri <- create_decumulative_triangle(cum_tri)
  expect_equal(decum_tri, tri)
  # decum by definition nesmí být záporný
  expect_true(all(decum_tri[!is.na(decum_tri)] >= 0))
})

test_that("create_chl_coef_triangle has NA in first column and equals ratios", {
  tri <- create_triangle(df, "accident", "dev", "value")
  cum_tri <- create_cumulative_triangle(tri)
  chl_trg <- create_chl_coef_triangle(cum_tri)

  expect_true(all(is.na(chl_trg[, 1])))
  # Zkontroluj několik validních poměrů (kde nejsou NA)
  rows <- nrow(cum_tri)
  for (i in 1:rows) {
    for (j in 2:ncol(cum_tri)) {
      if (i + j <= rows + 1 && !is.na(cum_tri[i, j]) && !is.na(cum_tri[i, j - 1])) {
        expect_equal(chl_trg[i, j], cum_tri[i, j] / cum_tri[i, j - 1])
      }
    }
  }
})

test_that("create_chl_coefs returns expected shape, names and NA in age 0", {
  tri <- create_triangle(df, "accident", "dev", "value")
  cum_tri <- create_cumulative_triangle(tri)

  res_full <- create_chl_coefs(cum_tri, chl_length = "full")
  expect_s3_class(res_full, "data.frame")
  expect_true("CH_L_lengths" %in% names(res_full))
  expect_true("0" %in% colnames(res_full))
  expect_true(all(is.na(res_full[ , "0"])))  # věk 0 musí být NA

  res_multi <- create_chl_coefs(cum_tri, chl_length = c("full", 2, 3))
  expect_equal(nrow(res_multi), 3L)
  expect_setequal(res_multi$CH_L_lengths, c("chain_ladder - full", "chain_ladder - 2", "chain_ladder - 3"))
})

test_that("create_product_coefs computes ultimate multipliers correctly", {
  # vytvoř jednoduché CHL koeficienty pro 1 řadu (hlavička jako create_chl_coefs)
  chl_df <- structure(
    data.frame(CH_L_lengths = "chain_ladder - full", `0` = NA, `1` = 1.2, `2` = 1.1, `3` = 1.05),
    class = "data.frame"
  )
  # očekávané produkty (od daného věku do konce): age3=1.05, age2=1.1*1.05, age1=1.2*1.1*1.05
  expected <- c("Product", NA, 1.2 * 1.1 * 1.05, 1.1 * 1.05, 1.05)

  prod_vec <- create_product_coefs(chl_df, name = "Product")
  expect_equal(as.vector(prod_vec), as.vector(expected))
})

test_that("create_avg_coefs averages over diagonals correctly", {
  tri <- create_triangle(df, "accident", "dev", "value")
  cum_tri <- create_cumulative_triangle(tri)
  chl_trg <- create_chl_coef_triangle(cum_tri)

  avg_df <- create_avg_coefs(chl_trg, avg_length = "full")
  expect_s3_class(avg_df, "data.frame")
  expect_true("Average_DF_lengths" %in% names(avg_df))

  # Pro sloupec 2 (věk 1) zkontroluj, že odpovídá průměru relevantních diagonálních prvků
  # Vyber všechny nenulové/nena hodnoty v chl_trg[,2] pod trojúhelníkem
  col_idx <- 2L
  vals <- chl_trg[, col_idx]
  vals <- vals[!is.na(vals)]
  expect_equal(avg_df[1, as.character(col_idx - 1)], mean(vals))
})

test_that("triangle_namer sets row and col names as expected", {
  tri <- create_triangle(df, "accident", "dev", "value")
  rn <- c("Y1", "Y2", "Y3", "Y4")  # delší než nrow(tri)
  tri_named <- triangle_namer(tri, rn)

  expect_equal(rownames(tri_named), rn[1:nrow(tri)])
  expect_equal(colnames(tri_named), as.character(0:(ncol(tri) - 1)))
})

test_that("diag_sums returns correct diagonal sums on simple triangle", {
  tri_simple <- matrix(c(1, NA, NA,
                         1,  1, NA,
                         1,  1,  1), nrow = 3, byrow = TRUE)
  s <- diag_sums(tri_simple, order = "main_to_last")
  expect_equal(s, c(3, 2, 1))
})

test_that("diag_writer merges last diagonal from new triangle", {
  # old 2x2, new 3x3 kumulativně kompatibilní matice
  old_trg <- matrix(c(10, NA,
                      20, 30), nrow = 2, byrow = TRUE)
  new_trg <- matrix(c(10, NA, NA,
                      20, 30, NA,
                      40, 50, 60), nrow = 3, byrow = TRUE)

  merged <- diag_writer(old_trg, new_trg)
  expect_equal(dim(merged), dim(new_trg))
  # poslední diagonála (3., prvky [1,3], [2,2], [3,1]) by měla odpovídat new_trg
  expect_equal(merged[1,3], new_trg[1,3])
  expect_equal(merged[2,2], new_trg[2,2])
  expect_equal(merged[3,1], new_trg[3,1])
})

test_that("create_annual_triangle aggregates over specified period", {
  tri <- create_triangle(df, "accident", "dev", "value")
  cum_tri <- create_cumulative_triangle(tri)

  # period 2 ⇒ years = floor(nrow/2) = 1 (pro náš data frame 3 řádky ⇒ 1 rok)
  ann2 <- create_annual_triangle(cum_tri, period = 2)
  expect_equal(dim(ann2), c(1, 1))
  expect_true(!is.na(ann2[1,1]))
})

test_that("create_run_off_check returns numeric vector of correct length", {
  tri <- create_triangle(df, "accident", "dev", "value")
  cum_tri <- create_cumulative_triangle(tri)
  ro <- create_run_off_check(cum_tri)
  # délka = nrow(cum_tri) - 1 dle implementace
  expect_type(ro, "double")
  expect_equal(length(ro), nrow(cum_tri) - 1)
})

test_that("plotly visualizations return plotly objects and validate columns", {
  tri <- create_triangle(df, "accident", "dev", "value")
  cum_tri <- create_cumulative_triangle(tri)
  chl_trg <- create_chl_coef_triangle(cum_tri)

  # pojmenuj řádky, aby osa X měla labels
  tri_named <- triangle_namer(chl_trg, paste0("t", seq_len(nrow(chl_trg))))

  # valid columns (existují)
  p1 <- create_chl_trg_visualization(tri_named, columns = c("1", "2"))
  expect_s3_class(p1, "plotly")

  p2 <- create_chl_trg_visualization_pvzp(tri_named, columns = c("1"))
  expect_s3_class(p2, "plotly")

  # neexistující sloupec vyvolá chybu
  expect_error(create_chl_trg_visualization(tri_named, columns = c("999")))
  expect_error(create_chl_trg_visualization_pvzp(tri_named, columns = c("foo")))
})
