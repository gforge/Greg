#' The \emph{isFitLogit} is a simple check if object inherits either "lrm" 
#' class or inherits the  "glm" class together with a "logit" link 
#' function indicating that it is a logistic function.
#'
#' @rdname isFitFn
#' 
#' @example inst/examples/isFitLogit_example.R
#' 
#' @export
isFitLogit <- function(fit){
  if ("lrm" %in% class(fit)) {
    return(TRUE)
  }
  
  if (inherits(fit, "glm") &&
    family(fit)$family == "binomial") {
    return(TRUE)
  }
  
  return(FALSE)  
}
