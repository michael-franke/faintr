check_model <- function(x) {

  # check whether the object is of class 'brmsfit'
  if (!brms::is.brmsfit(x)) {
    stop("The object passed to argument '", deparse(substitute(x)),
         "' is not of class 'brmsfit'. See '?brmsfit' for details.", call. = FALSE)
  }

  # check whether the model is multivariate
  if (brms::is.mvbrmsformula(x$formula)) {
    stop("Multivariate models are currently not supported.", call. = FALSE)
  }

  # check whether the models family is supported
  if (x$family$family %in% c('categorical', 'dirichlet', 'multinomial', 'logistic_normal')) {
    stop("Models using family '", x$family$family, "' are currently not supported.", call. = FALSE)
  }

  # check whether the model contains special effect terms
  if ('sp' %in% attributes(brms::brmsterms(x$formula)$dpars$mu)$names ||
      'cs' %in% attributes(brms::brmsterms(x$formula)$dpars$mu)$names) {
    stop("Models using special effect terms 'mo', 'me', 'mi', and 'cs' are currently not supported.", call. = FALSE)
  }
}

