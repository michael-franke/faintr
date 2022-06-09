suppressWarnings(suppressMessages({
  library(brms)
  library(stats)
  library(dplyr)
  library(magrittr)
}))


test_that("get_cell_definitions() throws expected errors with mock backend", {

  set.seed(123)
  response <- data.frame(y1 = rnorm(12), y2 = rnorm(12),
                         y3 = rbinom(12, 8, 0.5), y4 = rbinom(12, 8, 0.7)) %>%
              mutate(size = y3+y4, y3 = y3/size, y4 = y4/size)

  factors <- expand.grid(cat1 = c("A", "B"), cat2 = c("cond1", "cond2"),
                         cat3 = c("one", "two", "three"))

  data <- cbind(response, factors)

  # wrong model class
  expect_error(get_cell_definitions(lm(y1 ~ cat1 * cat2, data)),
               "The object passed to argument 'fit' is not of class 'brmsfit'. See '\\?brmsfit' for details.")

  # multivariate models
  bf_mv1 <- bf(mvbind(y1, y2) ~ cat1 + cat2 + cat3) + set_rescor(TRUE)
  mockfit_mv1 <- brm(bf_mv1, data, mock_fit = 1, backend = "mock", rename = FALSE)
  expect_error(get_cell_definitions(mockfit_mv1),
               "Multivariate models are currently not supported.")

  bf_mv2 <- mvbf(bf(y1 ~ cat1), bf(y2 ~ cat2), rescor = TRUE)
  mockfit_mv2 <- brm(bf_mv2, data, mock_fit = 1, backend = "mock", rename = FALSE)
  expect_error(get_cell_definitions(mockfit_mv2),
               "Multivariate models are currently not supported.")

  # categorical models
  mockfit_cat <- brm(cat3 ~ cat1 * cat2, data, family = categorical(link = 'logit'),
                     mock_fit = 1, backend = "mock", rename = FALSE)
  expect_error(get_cell_definitions(mockfit_cat),
               "Models using family 'categorical' are currently not supported.")

  # logistic normal models
  mockfit_lm <- brm(cbind(y3, y4) ~ cat3, data, logistic_normal(link = 'identity'),
                    mock_fit = 1, backend = "mock", rename = FALSE)
  expect_error(get_cell_definitions(mockfit_lm),
               "Models using family 'logistic_normal' are currently not supported.")

  # normal model using special effect terms
  mockfit_mo <- brm(y1 ~ cat2 * mo(cat3), data %>% mutate(cat3 = factor(cat3, ordered = TRUE)),
                    mock_fit = 1, backend = "mock", rename = FALSE)
  expect_error(get_cell_definitions(mockfit_mo),
               "Models using special effect terms 'mo', 'me', 'mi', and 'cs' are currently not supported.")

  mockfit_cs <- brm(cat3 ~ cs(cat1) + cat2, data %>% mutate(cat3 = factor(cat3, ordered = TRUE)),
                    family = acat(link = 'probit'), mock_fit = 1, backend = "mock", rename = FALSE)
  expect_error(get_cell_definitions(mockfit_cs),
               "Models using special effect terms 'mo', 'me', 'mi', and 'cs' are currently not supported.")

  # normal models with intercept as population-level parameter
  mockfit_intercept1 <- brm(y1 ~ 0 + Intercept + cat2 + cat2, data,
                            mock_fit = 1, backend = "mock", rename = FALSE)
  expect_error(get_cell_definitions(mockfit_intercept1),
               'Models with the intercept as population-level effect are currently not supported.')

  mockfit_intercept2 <- suppressWarnings(brm(y1 ~ 0 + intercept + cat2 + cat2, data,
                                             mock_fit = 1, backend = "mock", rename = FALSE))
  expect_error(get_cell_definitions(mockfit_intercept2),
               'Models with the intercept as population-level effect are currently not supported.')
})


test_that("get_cell_definitions() returns correct output", {

  fit <- readRDS(test_path('models/fit_gaussian_dummy.rds'))
  cell_defs <- get_cell_definitions(fit)

  expect_equal(dim(cell_defs), c(4, 7))
  expect_equal(inherits(cell_defs, 'tbl_df'), TRUE)
  expect_equal('cell' %in% colnames(cell_defs), TRUE)
  expect_equal(cell_defs$cell, seq(1, 4))
  expect_equal('congruency' %in% colnames(cell_defs), TRUE)
  expect_equal('color' %in% colnames(cell_defs), TRUE)
  expect_equal(cell_defs %>% select(congruency, color),
               fit$data %>% select(congruency, color) %>% unique(), ignore_attr = TRUE)

  expect_equal(cell_defs %>%
                 subset(congruency == "con" & color == "red", select = -c(1:3)) %>%
                 as.numeric(),
               c(1, 0, 0, 0))

  expect_equal(cell_defs %>%
                 subset(congruency == "incon" & color == "red", select = -c(1:3)) %>%
                 as.numeric(),
               c(1, 1, 0, 0))

  expect_equal(cell_defs %>%
                 subset(congruency == "con" & color == "green", select = -c(1:3)) %>%
                 as.numeric(),
               c(1, 0, 1, 0))

  expect_equal(cell_defs %>%
                 subset(congruency == "incon" & color == "green", select = -c(1:3)) %>%
                 as.numeric(),
               c(1, 1, 1, 1))
})
