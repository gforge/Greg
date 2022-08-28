prGetModelDefaults4foresplot <- function(regr.obj,
                                         xlab,
                                         xlog,
                                         zero,
                                         estimate.txt,
                                         exp) {
  ret <- mget(names(formals()), sys.frame(sys.nframe()))
  ret$regr.obj <- NULL
  for (n in names(ret)) {
    if (is.symbol(ret[[n]])) {
      ret[[n]] <- NULL
    }
  }
  
  if (is.null(ret$xlab)) {
    if (isFitLogit(regr.obj)) {
      ret$xlab <- "Odds ratio"
    } else if (family(regr.obj)$family == "poisson" && !is.null(model.offset(regr.obj))) {
      ret$xlab <- "Relative risk"
    }
  }
  
  if (is.null(estimate.txt)) {
    if (isFitLogit(regr.obj)) {
      ret$xlab <- "OR"
    } else if (family(regr.obj)$family == "poisson" && !is.null(model.offset(regr.obj))) {
      ret$xlab <- "RR"
    }
  }
  
  exponential <- isFitLogit(regr.obj) || family(regr.obj)$family == "poisson"
  if (is.null(ret$xlog)) {
    if (exponential) {
      ret$xlog <- TRUE
    }
  }
  
  if (is.null(ret$zero)) {
    if (exponential) {
      ret$zero <- 1
    }
  }
  
  if (is.null(ret$exp)) {
    if (exponential) {
      ret$exp <- TRUE
    }
  }
  
  return(ret)
}