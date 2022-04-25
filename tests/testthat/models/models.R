
response <- data.frame(RT = rnorm(36, 300),
                       correctness = sample(c(0, 1), replace = TRUE, 36))

factors <- expand.grid(shape = c("square", "circle", "triangle"),
                       congruency = c("con", "incon"),
                       color = c("red", "green"),
                       ID = seq(1, 3))

data <- cbind(response, factors)

fit_gaussian_dummy <- brms::brm(RT ~ congruency * color + (1 | ID), data,
                                chains = 1, iter = 100, seed = 1234)


saveRDS(fit_gaussian_dummy, 'fit_gaussian_dummy.rds')

