#' Forest plot for multiple models
#'
#' Plot different model fits with similar variables in order to
#' compare the model's estimates and confidence intervals. Each
#' model is represented by a separate line on top of eachother
#' and are therefore ideal for comparing different models. This
#' extra appealing when you have lots of variables included in
#' the models.
#'
#' @param regr.obj A regression model object. It should be of coxph, crr or glm class.
#'   Warning: The glm is not fully tested.
#' @param postprocess_estimates.fn A function that takes the regression outputs and returns
#'   the same data with modifications. The input columns are:
#'   
#'   * `Rowname`
#'   * `Coef`
#'   * `Lower`
#'   * `Upper`
#'   * `Sort`
#' @param get_box_size A function for extracting the box sizes
#' @param rowname The name of the variables
#' @param estimate.txt The text above the estimate, e.g. Est, HR
#' @param ci.txt The text above the confidence interval, defaults to `"CI"`
#' @param ci.glue The string used for [glue::glue()] the `lower` and `higher`
#'  confidence intervals together.
#' @param digits The number of digits to round presented values to
#'
#' @example inst/examples/forestplotRegrObj_example.R
#'
#' @importFrom Gmisc insertRowAndKeepAttr
#'
#' @inheritParams forestplotCombineRegrObj
#' @inheritParams forestplot::forestplot
#'
#' @family forestplot wrappers
#' @rdname forestplotRegrObj
#' @import forestplot
#' @export
forestplotRegrObj <- function(regr.obj,
                              postprocess_estimates.fn = \(x) x,
                              rowname = "Variable",
                              ci.txt = "CI",
                              ci.glue = "{lower} to {higher}",
                              digits = 1,
                              get_box_size = fpBoxSize,
                              ...) 
{
  UseMethod("forestplotRegrObj")
}

#' @rdname forestplotRegrObj
#' @export
forestplotRegrObj.default <- function(regr.obj,
                                      postprocess_estimates.fn = \(x) x,
                                      rowname = "Variable",
                                      ci.txt = "CI",
                                      ci.glue = "{lower} to {higher}",
                                      digits = 1,
                                      get_box_size = fpBoxSize,
                                      ...) {
  stop("Method not implemented for this class: ", class(regr.obj))
}

prFixForestplotArgs <- function(regr.obj, args, extra, already_as_list = FALSE) {
  args$regr.obj <- NULL
  if (already_as_list) {
    args$regressions <- regr.obj
  } else {
    args$regressions <- list(regr.obj)
  }
  args[["..."]] <- NULL
  for (n in names(extra)) {
    args[[n]] <- extra[[n]]
  }
  
  # Add original defaults so we don't have to add them to each S3 manually
  org_defaults <- formals(forestplotRegrObj)[sapply(formals(forestplotRegrObj), 
                                                    Negate(\(x) is.symbol(x) || is.null(x)))]
  for (n in names(org_defaults)) {
    if (is.null(args[[n]])) {
      args[[n]] <- org_defaults[[n]]
    }
  }
  do.call(prForestPlotPrep, args)
}

#' @rdname forestplotRegrObj
#' @export
forestplotRegrObj.coxph <- function(regr.obj,
                                    postprocess_estimates.fn = \(x) x,
                                    rowname = "Variable",
                                    ci.txt = "CI",
                                    ci.glue = "{lower} to {higher}",
                                    digits = 1,
                                    get_box_size = fpBoxSize,
                                    xlab = "Hazard Ratio",
                                    estimate.txt = "HR",
                                    xlog = TRUE,
                                    zero = 1,
                                    exp = TRUE,
                                    ...) {
  prFixForestplotArgs(regr.obj = regr.obj,
                      args = mget(names(formals()), sys.frame(sys.nframe())),
                      extra = list(...))
}


#' @rdname forestplotRegrObj
#' @export
forestplotRegrObj.lrm <- function(regr.obj,
                                  postprocess_estimates.fn = \(x) x,
                                  rowname = "Variable",
                                  ci.txt = "CI",
                                  ci.glue = "{lower} to {higher}",
                                  digits = 1,
                                  get_box_size = fpBoxSize,
                                  xlab = "Odds ratio",
                                  estimate.txt = "HR",
                                  xlog = TRUE,
                                  zero = 1,
                                  exp = TRUE,
                                  ...) {
  prFixForestplotArgs(regr.obj = regr.obj,
                      args = mget(names(formals()), sys.frame(sys.nframe())),
                      extra = list(...))
}


#' @rdname forestplotRegrObj
#' @export
forestplotRegrObj.lm <- function(regr.obj,
                                 postprocess_estimates.fn = \(x) x,
                                 rowname = "Variable",
                                 ci.txt = "CI",
                                 ci.glue = "{lower} to {higher}",
                                 digits = 1,
                                 get_box_size = fpBoxSize,
                                 xlab = "Effect",
                                 estimate.txt = "Coef",
                                 xlog = FALSE,
                                 zero = 0,
                                 exp = FALSE,
                                 ...) {
  prFixForestplotArgs(regr.obj = regr.obj,
                      args = mget(names(formals()), sys.frame(sys.nframe())),
                      extra = list(...))
}

#' @rdname forestplotRegrObj
#' @export
forestplotRegrObj.glm <- function(regr.obj,
                                  postprocess_estimates.fn = \(x) x,
                                  rowname = "Variable",
                                  ci.txt = "CI",
                                  ci.glue = "{lower} to {higher}",
                                  digits = 1,
                                  get_box_size = fpBoxSize,
                                  xlab = NULL,
                                  xlog = NULL,
                                  zero = NULL,
                                  estimate.txt = NULL,
                                  exp = NULL,
                                  ...) {
  defaults <- prGetModelDefaults4foresplot(regr.obj = regr.obj,
                                           xlab = xlab,
                                           xlog = xlog,
                                           zero = zero,
                                           estimate.txt = estimate.txt,
                                           exp = exp)

  args <- mget(names(formals()), sys.frame(sys.nframe()))
  for (n in names(defaults)) {
    if (is.symbol(args[[n]]) || is.null(args[[n]])) {
      args[[n]] <- defaults[[n]]
    }
  }
  
  prFixForestplotArgs(regr.obj = regr.obj,
                      args = args,
                      extra = list(...))
}

#' @rdname forestplotRegrObj
#' @export
forestplotRegrObj.list <- function(regr.obj,
                                   postprocess_estimates.fn = \(x) x,
                                   rowname = "Variable",
                                   ci.txt = "CI",
                                   ci.glue = "{lower} to {higher}",
                                   digits = 1,
                                   get_box_size = fpBoxSize,
                                   xlab = NULL,
                                   xlog = NULL,
                                   zero = NULL,
                                   estimate.txt = NULL,
                                   exp = NULL,
                                   ...) {
  defaults <- prGetModelDefaults4foresplot(regr.obj = regr.obj[[1]],
                                           xlab = xlab,
                                           xlog = xlog,
                                           zero = zero,
                                           estimate.txt = estimate.txt,
                                           exp = exp)
  
  args <- mget(names(formals()), sys.frame(sys.nframe()))
  for (n in names(defaults)) {
    if (is.symbol(args[[n]]) || is.null(args[[n]])) {
      args[[n]] <- defaults[[n]]
    }
  }
  
  prFixForestplotArgs(regr.obj = regr.obj,
                      args = args,
                      extra = list(...),
                      already_as_list = TRUE)
}

#' @param p_values The p-values that will work as the foundation for the box size
#' @param variable_count The number of variables
#' @param boxsize The default box size
#' @param significant Level of significance .05
#' @rdname forestplotRegrObj
#' @export
fpBoxSize <- function(p_values,
                      variable_count,
                      boxsize,
                      significant = .05) {
  b_size <- c(NA, rep(boxsize, variable_count))
  b_size[p_values < significant] <- boxsize * 1.5
  return(b_size)
}