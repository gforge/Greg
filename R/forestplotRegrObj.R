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
                              box.default.size,
                              postprocess_estimates.fn = NULL,
                              xlab,
                              xlog,
                              exp,
                              estimate.txt = xlab,
                              zero,
                              get_box_size = fpBoxSize,
                              ...) 
{
  UseMethod("forestplotRegrObj")
}

#' @rdname forestplotRegrObj
#' @export
forestplotRegrObj.default <- function(regr.obj,
                                      skip.variables,
                                      add.empty_row,
                                      order.regexps,
                                      order.addrows,
                                      box.default.size,
                                      rowname.fn,
                                      xlab,
                                      xlog,
                                      exp,
                                      estimate.txt = xlab,
                                      zero,
                                      get_box_size = fpBoxSize,
                                      ...) {
  stop("Method not implemented for this class: ", class(regr.obj))
}

prFixForestplotArgs <- function(regr.obj, args, extra) {
  args$regr.obj <- NULL
  args$regressions <- list(regr.obj)
  args[["..."]] <- NULL
  for (n in names(extra)) {
    args[[n]] <- extra[[n]]
  }
  do.call(prForestPlotPrep, args)
}

#' @rdname forestplotRegrObj
#' @export
forestplotRegrObj.coxph <- function(regr.obj,
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
                                  xlab,
                                  xlog,
                                  zero,
                                  estimate.txt,
                                  exp,
                                  ...) {
  defaults <- prGetModelDefaults4Foresplot(regr.obj = regr.obj,
                                           xlab = xlab,
                                           xlog = xlog,
                                           zero = zero,
                                           estimate.txt = estimate.txt,
                                           exp = exp)

  args <- mget(names(formals()), sys.frame(sys.nframe()))
  for (n in names(defaults)) {
    if (is.symbol(args[[n]])) {
      args[[n]] <- defaults[[n]]
    }
  }
  
  prFixForestplotArgs(regr.obj = regr.obj,
                      args = args,
                      extra = list(...))
}

#' @param p_values The p-values that will work as the foundation for the box size
#' @param variable_count The number of variables
#' @param box.default.size The default box size
#' @param significant Level of significance .05
#' @rdname forestplotRegrObj
#' @export
fpBoxSize <- function(p_values,
                      variable_count,
                      box.default.size,
                      significant = .05) {
  b_size <- c(NA, rep(box.default.size, variable_count))
  b_size[p_values < significant] <- box.default.size * 1.5
  return(b_size)
}