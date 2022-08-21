suppressWarnings(suppressMessages({
  library(brms)
  library(dplyr)
  library(magrittr)
}))

test_that("get_cell_definitions() throws expected errors", {
  fit <- readRDS(test_path('models/fit_gaussian_dummy.rds'))

  expect_error(compare_groups(fit, hdi = '0.89'), "Argument 'hdi' must be a single value between 0 and 1.")
  expect_error(compare_groups(fit, hdi = NULL), "Argument 'hdi' must be a single value between 0 and 1.")
  expect_error(compare_groups(fit, hdi = NA), "Argument 'hdi' must be a single value between 0 and 1.")
  expect_error(compare_groups(fit, hdi = c(0.8, 0.95)), "Argument 'hdi' must be a single value between 0 and 1.")
  expect_error(compare_groups(fit, hdi = 0), "Argument 'hdi' must be a single value between 0 and 1.")
  expect_error(compare_groups(fit, hdi = -0.5), "Argument 'hdi' must be a single value between 0 and 1.")
  expect_error(compare_groups(fit, hdi = 1), "Argument 'hdi' must be a single value between 0 and 1.")
  expect_error(compare_groups(fit, hdi = 1.5), "Argument 'hdi' must be a single value between 0 and 1.")
})


test_that("get_cell_definitions() returns correct output", {
  fit <- readRDS(test_path('models/fit_gaussian_dummy.rds'))

  expect_equal(inherits(compare_groups(fit), "faintCompare"), TRUE)
  expect_equal(compare_groups(fit, congruency == "con", hdi = 0.89)$hdi, 0.89)
  expect_equal(compare_groups(fit, color == "red", congruency != "con")$comparison$higher, "color == \"red\"")
  expect_equal(compare_groups(fit, color == "red", congruency != "con")$comparison$lower, "congruency != \"con\"")
  expect_equal(compare_groups(fit, lower = color == "green")$comparison$higher, "grand mean")
  expect_equal(compare_groups(fit, lower = congruency != "con" | color == "red")$comparison$lower,
               "congruency != \"con\" | color == \"red\"")
})

