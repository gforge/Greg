#' Compares different scores in different regression objects.
#'
#' Creates a composite from different regression objects into
#' one forestplot where you can choose the variables of interest
#' to get an overview and easier comparison.
#'
#' @param regr.obj A list with all the fits that have variables that are to
#'   be identified through the regular expression
#' @param variablesOfInterest.regexp A regular expression identifying the variables
#'   that are of interest of comparing. For instance it can be "(score|index|measure)"
#'   that finds scores in different models that should be compared.
#' @param post_process_data A function that takes the data frame just prior to calling `forestplot`
#'   and allows you to manipulate it. Primarily used for changing the `column_label`
#'   that has the names shown in the final plot.
#' @param estimate.txt The text of the estimate, usually HR for hazard ratio, OR for
#'  odds ratio
#' @param zero Indicates what is zero effect. For survival/logistic fits the zero is
#'   1 while in most other cases it's 0.
#' @param exp Report in exponential form. Default true since the function was built for
#'   use with survival models.
#' @param add_first_as_ref If you want that the first variable should be reference for
#'   that group of variables. The ref is a variable with the estimate 1 or 0 depending
#'   if exp() and the confidence interval 0.
#' @param ref_txt Text instead of estimate number
#' @param digits Number of digits to use for the estimate output
#' @param ... Passed to \code{\link[forestplot]{forestplot}()}
#'
#' @example inst/examples/forestplotCombineRegrObj_example.R
#'
#' @inheritParams forestplot::forestplot
#' @import forestplot
#' @family forestplot wrappers
#' @export
forestplotCombineRegrObj <- function(
  regr.obj,
  variablesOfInterest.regexp = NULL,
  estimate.txt = NULL,
  add_first_as_ref = FALSE,
  ref_txt = "ref.",
  digits = 1,
  post_process_data = \(x) x,
  is.summary = NULL,
  xlab = NULL,
  zero = NULL,
  xlog = NULL,
  exp  = xlog,
  ...)
{
  if (length(regr.obj) < 2)
    stop("This function combines several fits so please provide more than one model_fits")

  if (is.object(regr.obj) == TRUE)
    stop("The model_fits need to be a list of fits")

  # Initiate some standard values if the user
  # hasn't supplied any
  if (is.null(xlab)) {
    if (isFitCoxPH(regr.obj[[1]])) {
      xlab <- "Hazard Ratio"
    } else if (isFitLogit(regr.obj[[1]])) {
      xlab <- "Odds Ratio"
    }
  }

  if (is.null(estimate.txt)) {
    if (isFitCoxPH(regr.obj[[1]])) {
      estimate.txt <- "HR"
    } else if (isFitLogit(regr.obj[[1]])) {
      estimate.txt <- "OR"
    }
  }

  if (is.null(xlog)) {
    if (isFitCoxPH(regr.obj[[1]]) ||
          isFitLogit(regr.obj[[1]])) {
      xlog <- TRUE
      if (is.null(zero))
        zero <- 1
      if (is.null(exp))
        exp <- TRUE
    }else{
      xlog <- FALSE
      if (is.null(zero))
        zero <- 0
      if (is.null(exp))
        exp <- FALSE
    }
  }

  models_fit_fp_data <- getModelData4Forestplot(regr.obj = regr.obj,
                                                exp = exp,
                                                variablesOfInterest.regexp = variablesOfInterest.regexp,
                                                add_first_as_ref = add_first_as_ref)
  if (!is.null(is.summary)) {
    models_fit_fp_data$is.summary <- is.summary
  }
  
  models_fit_fp_data |> 
    mutate(est_txt = htmlTable::txtRound(estimate, digits = digits)) |> 
    post_process_data() |> 
    forestplot::forestplot(labeltext = c(column_label, est_txt),
                           mean = estimate,
                           lower = conf.low,
                           upper = conf.high,
                           # TODO: Fix this bug in forestplot
                           is.summary = is.summary,
                           xlog = xlog,
                           xlab = xlab,
                           ...) |> 
    forestplot::fp_add_header(est_txt = estimate.txt)
}
