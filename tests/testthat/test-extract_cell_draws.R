suppressWarnings(suppressMessages({
  library(brms)
  library(dplyr)
  library(magrittr)
}))

test_that("extract_cell_draws() throws expected errors", {
  fit <- readRDS(test_path('models/fit_gaussian_dummy.rds'))

  expect_error(extract_cell_draws(fit, color == 'blue'), "Level specification 'color == \"blue\"' unknown.")
})


test_that("extract_cell_draws() returns correct output", {
  fit <- readRDS(test_path('models/fit_gaussian_dummy.rds'))

  expect_equal(nrow(extract_cell_draws(fit)), 50)
  expect_equal(inherits(extract_cell_draws(fit), 'data.frame'), TRUE)
  expect_equal(colnames(extract_cell_draws(fit, congruency == "con", 'cell_con')), 'cell_con')

  out_single_row <- extract_cell_draws(fit, congruency == "incon" & color == "green") %>%
    pull() %>%
    sort()

  expected_single_row <- as_draws_df(fit) %>%
    mutate(draws = b_Intercept + b_congruencyincon + b_colorgreen + `b_congruencyincon:colorgreen`) %>%
    pull(draws) %>%
    sort()

  expect_equal(out_single_row, expected_single_row)

  out_multiple_rows <- extract_cell_draws(fit, color == "red") %>% pull() %>% sort()
  expected_multiple_rows <- as_draws_df(fit) %>%
    mutate(draws = 0.5*(b_Intercept + b_Intercept + b_congruencyincon)) %>%
    pull(draws) %>% sort()

  expect_equal(out_multiple_rows, expected_multiple_rows)
})
