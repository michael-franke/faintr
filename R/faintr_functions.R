#' Obtain information about factors in regression models
#'
#' This function takes a \code{\link[brms]{brms}} model fit for a
#' factorial design and returns all design cells along with their encoding in
#' the regression model.
#'
#' @param fit An object of class \code{\link[brms]{brmsfit}}.
#'
#' @return A \code{\link[tibble]{tibble}} containing the minimal design matrix.
#'
#' @note
#' The \pkg{faintr} package currently does not support multivariate models and
#' models that use families \code{categorical}, \code{dirichlet}, \code{multinomial},
#' and \code{logistic_normal}. Furthermore, models must not include special effect
#' terms \code{mo()}, \code{mi()}, \code{me()}, and \code{cs()} for fixed effects.
#' Also note that \pkg{faintr} currently does not support models where the intercept
#' is a population-level parameter (class \code{b}), as is the case when using the
#' \code{0 + Intercept} syntax in the \code{brm} function call.
#'
#' @references
#' Bürkner, P.-C. (2017). brms: An R Package for Bayesian Multilevel Models
#' Using Stan. \emph{Journal of Statistical Software}, \emph{80}(1), 1-28.
#' \doi{10.18637/jss.v080.i01}
#'
#' @examples
#' \dontrun{
#' # fit a linear mixed effects model using 'brms'
#' # regressing voice pitch against gender and context and including random effects
#' fit <- brms::brm(formula = pitch ~ gender * context + (1 | subject + sentence),
#'                  data = politeness)
#'
#' # check encoding scheme used in the model
#' get_cell_definitions(fit)
#'}
#'
#' @export
get_cell_definitions <- function(fit) {

  check_model(fit)

  # get fixed effects names
  fixef <- all.vars(brms::brmsterms(stats::formula(fit))$dpars$mu$fe)

  # stop if the intercept is a population-level parameter
  if ("Intercept" %in% fixef || "intercept" %in% fixef) {
    stop('Models with the intercept as population-level effect are currently not supported.', call. = FALSE)
  }

  # concatenate design matrix and actual data
  cell_defs <- dplyr::bind_cols(
    fit$data %>% dplyr::select(dplyr::all_of(fixef)),
    as.data.frame(brms::standata(fit)$X)
  ) %>% unique() %>%
    tibble::rowid_to_column(var = 'cell')

  return(tibble::as_tibble(cell_defs))
}


#' Extract posterior draws for one subset of factorial design cells
#'
#' This function takes as input a \code{\link[brms]{brms}} model fit for a
#' factorial design and a group specification (one subset of the design cells)
#' and returns the posterior draws for that group. If no group is specified,
#' the returned draws are grand means.
#'
#' @param fit An object of class \code{\link[brms]{brmsfit}}.
#' @param group An expression specifying the group to filter the draws for.
#' @param colname A string specifying the column name of the returned data frame;
#' defaults to 'draws'.
#'
#' @return A \code{\link[posterior]{draws_df}} object containing posterior draws
#' for the specified group, as well as additional metadata.
#'
#' @note
#' The \pkg{faintr} package currently does not support multivariate models and
#' models that use families \code{categorical}, \code{dirichlet}, \code{multinomial},
#' and \code{logistic_normal}. Furthermore, models must not include special effect
#' terms \code{mo()}, \code{mi()}, \code{me()}, and \code{cs()} for fixed effects.
#' Also note that \pkg{faintr} currently does not support models where the intercept
#' is a population-level parameter (class \code{b}), as is the case when using the
#' \code{0 + Intercept} syntax in the \code{brm} function call.
#'
#' @references
#' Bürkner, P.-C. (2017). brms: An R Package for Bayesian Multilevel Models
#' Using Stan. \emph{Journal of Statistical Software}, \emph{80}(1), 1-28.
#' \doi{10.18637/jss.v080.i01}
#'
#' @examples
#' \dontrun{
#' # fit a linear mixed effects model
#' # regressing voice pitch against gender and context and including random effects
#' fit <- brms::brm(formula = pitch ~ gender * context + (1 | subject + sentence),
#'                  data = politeness)
#'
#' # extract draws for female speakers in informal contexts
#' extract_cell_draws(fit, gender == "F" & context == "inf")
#'
#' # extract draws for male speakers or informal contexts
#' extract_cell_draws(fit, gender == "M" | context == "inf")
#'
#' # averaged over gender, extract draws for all but polite contexts
#' extract_cell_draws(fit, context != "pol")
#'
#' # extract posterior draws averaged over all factors (grand mean)
#' extract_cell_draws(fit)
#' }
#'
#' @importFrom rlang .data
#'
#' @export
extract_cell_draws <- function(fit, group=NULL, colname='draws') {

  ## extract draws for each design cell ----

  # get minimal design matrix as tibble with row numbers in column
  design_matrix <- get_cell_definitions(fit)

  # get fixed effects names
  fixef <- all.vars(brms::brmsterms(stats::formula(fit))$dpars$mu$fe)

  # extract coefficient names of fixed effects
  coeff_names <- paste0('b_', brms::standata(fit)$X %>% colnames())

  # extract posterior draws
  draws <- posterior::as_draws_df(fit, variable = coeff_names)

  # store meta information
  .chain     <- draws$.chain
  .iteration <- draws$.iteration
  .draw      <- draws$.draw

  # re-extract minimal design matrix as matrix
  X <- design_matrix %>%
    dplyr::select(!dplyr::all_of(c(fixef,'cell'))) %>%
    as.matrix()

  # extract relevant draws as matrix
  Y <- tibble::as_tibble(draws) %>%
    dplyr::select(dplyr::all_of(coeff_names)) %>%
    as.matrix()

  # use matrix product to get draws for each cell
  draws_for_cells <- Y %*% t(X)

  ## extract draws for factor level combinations ----

  group_spec <- rlang::enquo(group)

  if (rlang::quo_is_null(group_spec)) {
    cell_numbers <- design_matrix$cell
  } else {
    # get cell numbers based on specification
    cell_numbers <- design_matrix %>%
      dplyr::filter(!!group_spec) %>%
      dplyr::select(.data$cell) %>%
      dplyr::pull()
  }

  if (length(cell_numbers) == 1) {
    out <- draws_for_cells[,cell_numbers] %>% as.data.frame()
  } else if (length(cell_numbers) > 1) {
    out <- draws_for_cells[,cell_numbers] %>% rowMeans() %>% as.data.frame()
  } else {
    stop("Level specification '", rlang::quo_get_expr(group_spec) %>% deparse(), "' unknown.")
  }

  # add column name
  colnames(out) <- colname

  # attach meta information
  out[c(".chain", ".iteration", ".draw")] <- c(.chain, .iteration, .draw)

  # convert to 'draws_df'
  out <- posterior::as_draws_df(out)

  out
}


#' Compare two subsets of factorial design cells
#'
#' Convenience function for comparing two subsets of factorial design cells.
#' It takes a \code{\link[brms]{brms}} model fit, two group specifications
#' (two subsets of design cells), and the probability mass within the highest
#' density interval of the difference in groups. It outputs the posterior mean
#' of the 'higher' minus the 'lower' group, a credible interval of the mean difference,
#' as well as the posterior probability and odds that the mean estimate of the 'higher'
#' group is higher than that of the 'lower' group. A comparison of one group against
#' the grand mean can be obtained by leaving out one of the two group specifications
#' in the function call.
#'
#' @param fit An object of class \code{\link[brms]{brmsfit}}.
#' @param higher An expression specifying the 'higher' group to filter the draws for.
#' @param lower An expression specifying the 'lower' group to filter the draws for.
#' @param hdi A single value (0, 1) defining the probability mass within the
#' highest density interval; defaults to 0.95.
#' @param include_bf A Boolean flag indicating whether Bayes Factors should be
#' approximated (required additional sampling); defaults to FALSE.
#'
#' @return An object of class 'faintCompare' containing summary statistics of the comparison.
#'
#' @note
#' The \pkg{faintr} package currently does not support multivariate models and
#' models that use families \code{categorical}, \code{dirichlet}, \code{multinomial},
#' and \code{logistic_normal}. Furthermore, models must not include special effect
#' terms \code{mo()}, \code{mi()}, \code{me()}, and \code{cs()} for fixed effects.
#' Also note that \pkg{faintr} currently does not support models where the intercept
#' is a population-level parameter (class \code{b}), as is the case when using the
#' \code{0 + Intercept} syntax in the \code{brm} function call.
#'
#' @references
#' Bürkner, P.-C. (2017). brms: An R Package for Bayesian Multilevel Models
#' Using Stan. \emph{Journal of Statistical Software}, \emph{80}(1), 1-28.
#' \doi{10.18637/jss.v080.i01}
#'
#' @examples
#' \dontrun{
#' # fit a linear mixed effects model
#' # regressing voice pitch against gender and context and including random effects
#' fit <- brms::brm(formula = pitch ~ gender * context + (1 | subject + sentence),
#'                  data = politeness)
#'
#' # compare female speakers in informal contexts against male speakers in polite contexts
#' compare_groups(
#'  fit  = fit,
#'  higher = gender == "F" & context == "inf",
#'  lower  = gender == "M" & context == "pol"
#' )
#'
#' # compare informal contexts against polite contexts, averaged over gender
#' compare_groups(
#'  fit  = fit,
#'  higher = context == "inf",
#'  lower  = context == "pol"
#' )
#'
#' # compare female speakers against the grand mean
#' compare_groups(
#'  fit  = fit,
#'  higher = gender == "F",
#'  hdi = 0.8
#' )
#' }
#'
#' @export
compare_groups <- function(fit, higher=NULL, lower=NULL, hdi=0.95, include_bf=FALSE) {

  # check for invalid 'hdi' input
  if(!is.numeric(hdi) || length(hdi) != 1 || hdi <= 0 || hdi >= 1) {
    stop("Argument 'hdi' must be a single value between 0 and 1.")
  }

  higher <- rlang::enquo(higher)
  lower  <- rlang::enquo(lower)

  # extract cell draws for both group specifications
  post_samples_higher <- extract_cell_draws(fit = fit, !!higher)
  post_samples_lower  <- extract_cell_draws(fit = fit, !!lower)

  # get names of group specification
  get_group_names <- function(group){
    if (rlang::quo_is_null(group)) {
      return('grand mean')
    }
    rlang::quo_get_expr(group) %>% deparse()
  }

  # compute summary statistics
  diff <- post_samples_higher$draws - post_samples_lower$draws
  mean_diff <- mean(diff)
  ci <- HDInterval::hdi(diff, credMass = hdi)
  post_prob <- mean(post_samples_higher$draws > post_samples_lower$draws)
  post_odds <- post_prob / (1 - post_prob)

  # if BF is requested, get prior-only fit, compute
  if (include_bf) {

    suppressMessages(
      fit_prior_only <- stats::update(
        fit,
        silent = TRUE,
        refresh = 0,
        sample_prior = "only"
      ))

    prior_samples_higher <- extract_cell_draws(fit = fit_prior_only, !!higher)
    prior_samples_lower  <- extract_cell_draws(fit = fit_prior_only, !!lower)

    prior_prob <- mean(prior_samples_higher$draws > prior_samples_lower$draws)
    prior_odds <- prior_prob / (1 - prior_prob)
    bayes_factor = post_odds / prior_odds
  } else {
    bayes_factor = NA
  }

  outlist <- list(
    hdi = hdi,
    higher = get_group_names(higher),
    lower = get_group_names(lower),
    mean_diff = mean_diff,
    l_ci = as.vector(ci[1]),
    u_ci = as.vector(ci[2]),
    post_prob = post_prob,
    post_odds = post_odds,
    include_bf = include_bf,
    bayes_factor = bayes_factor
  )

  class(outlist) <- 'faintCompare'
  return(outlist)
}

#' Print group comparison object

#' @param x An object containing the summary statistics of a group comparison.
#' @param ... Ignored.
#'
#' @export
print.faintCompare <- function(x, ...) {
  cat("Outcome of comparing groups: \n")
  cat(" * higher: ", x$higher, "\n")
  cat(" * lower:  ", x$lower, "\n")
  cat("Mean 'higher - lower': ", signif(x$mean_diff, 4), "\n")
  cat(paste0(x$hdi*100, "% HDI: "), "[", signif(x$l_ci, 4), ";", signif(x$u_ci, 4), "]\n")
  cat("P('higher - lower' > 0): ", signif(x$post_prob, 4), "\n")
  cat("Posterior odds: ", signif(x$post_odds, 4), "\n")
  if (x$include_bf) {
    cat("Bayes factor: ", signif(x$bayes_factor, 4), "\n")
  }
}
