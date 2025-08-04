#' Regression Helper Functions
#'
#' This package provides utilities that streamline working with regression
#' models. Standard regression output offers a wealth of information, but for
#' publication—especially in journals such as *NEJM* and *BMJ*—results often
#' need additional simplification. The functions here help you get there.
#'
#' @section Output functions:
#'
#' The package contains functions that automatically print crude (unadjusted)
#' estimates alongside adjusted estimates, a presentation style expected in
#' many medical papers.
#'
#' Forest-plot wrappers let you visualise regression estimates—handy when
#' you’re dealing with a long list of covariates. Utilities are also included
#' for comparing models (e.g., patient subsets or different regression types).
#'
#' @section Time splitter:
#'
#' Cox models sometimes violate proportional-hazards assumptions. When the
#' `tt()` approach becomes unwieldy on large data sets, you can time-split the
#' data and use start time as an interaction term. See
#' \code{\link{timeSplitter}()} and \code{vignette("timeSplitter")}.
#'
#' @section Other regression functions:
#'
#' The package extends linear regression by enabling robust covariance
#' matrices through the \pkg{sandwich} framework for
#' \code{rms::\link[rms]{ols}()}. Additional helpers target \pkg{stats} and
#' \pkg{survival} workflows, including \code{\link[survival]{coxph}()}.
#'
#' @section Important notice:
#'
#' Extensive tests guard against regressions, but always verify results in your
#' own context. If you use the package with other model classes—and have tests
#' to share—please let me know.
#'
#' @author Max Gordon
#' @name Greg-package
"_PACKAGE"
