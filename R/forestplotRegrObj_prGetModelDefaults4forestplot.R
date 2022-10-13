#' @importFrom stats model.offset family
prGetModelDefaults4foresplot <- function(regr.obj,
                                         xlab = NULL,
                                         xlog = NULL,
                                         zero = NULL,
                                         estimate.txt = NULL,
                                         exp = NULL) {
  ret <- mget(names(formals()), sys.frame(sys.nframe()))
  ret <- ret[sapply(ret, Negate(is.null))]
  ret$regr.obj <- NULL
  for (n in names(ret)) {
    if (is.symbol(ret[[n]])) {
      ret[[n]] <- NULL
    }
  }
  
  if (is.null(ret$xlab)) {
    if (isFitLogit(regr.obj)) {
      ret$xlab <- "Odds ratio"
    } else if (inherits(regr.obj, "coxph") ) {
      ret$xlab <- "Hazard ratio"
    } else if (family(regr.obj)$family == "poisson" && !is.null(model.offset(regr.obj))) {
      ret$xlab <- "Relative risk"
    } else {
      ret$xlab <- "Estimate"
    }
  }
  
  if (is.null(estimate.txt)) {
    if (isFitLogit(regr.obj)) {
      ret$estimate.txt <- "OR"
    } else if (inherits(regr.obj, "coxph")) {
      ret$xlab <- "HR"
    } else if (family(regr.obj)$family == "poisson" && !is.null(model.offset(regr.obj))) {
      ret$estimate.txt <- "RR"
    } else {
      ret$estimate.txt <- "Est."
    }
  }
  
  exponential <- isFitLogit(regr.obj) || 
    inherits(regr.obj, "coxph") ||
    family(regr.obj)$family == "poisson"
  if (is.null(ret$xlog)) {
    if (exponential) {
      ret$xlog <- TRUE
    } else {
      ret$xlog <- FALSE
    }
  }
  
  if (is.null(ret$zero)) {
    if (exponential) {
      ret$zero <- 1
    } else {
      ret$zero <- 0
    }
  }
  
  if (is.null(ret$exp)) {
    if (exponential) {
      ret$exp <- TRUE
    } else {
      ret$exp <- FALSE
    }
  }
  
  return(ret)
}