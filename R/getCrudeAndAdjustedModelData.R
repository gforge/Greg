#' This function helps with printing regression models
#' 
#' This function is used for getting the adjusted and unadjusted values
#' for a regression model. It takes a full model and walks through each
#' variable, removes in the regression all variables except one then
#' reruns that variable to get the unadjusted value. This functions not
#' intended for direct use, it's better to use \code{\link{printCrudeAndAdjustedModel}}
#' that utilizes this function.
#' 
#' This function saves a lot of time creating tables since it compiles a fully
#' unadjusted list of all your used covariates.
#' 
#' If the model is an exponential poisson/logit/cox regression model then it automatically
#' reports the exp() values instead of the original values
#' 
#' The function skips by default all spline variables since this becomes very complicated
#' and there is no simple \deqn{\beta}{beta} to display. For the same reason it skips
#' any interaction variables since it's probably better to display these as a contrast table. 
#' 
#' Note that the rms regression has a separate function that uses the rms:::summaryrms function
#' that returns a matrix that is then pruned.
#' 
#' @param model The regression model
#' @param level The confidence interval level
#' @param remove_interaction_vars Removes the interaction terms as this makes no sense in the output
#' @param ... Not used
#' @return Returns a matrix with the columns: 
#'   \code{c("Crude", "2.5 \%", "97.5 \%", "Adjusted", "2.5 \%", "97.5 \%")}.
#'   The row order is not changed from the original model. The percentages can vary depending
#'   on the set level.
#'
#' @seealso \code{\link{printCrudeAndAdjustedModel}}
#' 
#' @example inst/examples/getCrudeAndAdjustedModelData_example.R
#' 
#' @importFrom stringr str_split
#' 
#' @rdname getCrudeAndAdjustedModelData
#' @author max
#' @export
getCrudeAndAdjustedModelData <- function(model, level, ...)
  UseMethod("getCrudeAndAdjustedModelData")

#' @author max
#' @rdname getCrudeAndAdjustedModelData
#' @method getCrudeAndAdjustedModelData default
#' @export
#' @keywords internal
getCrudeAndAdjustedModelData.default <- function(model, level=.95, remove_interaction_vars = TRUE, ...){
  
  # Just a prettifier for the output an alternative could be:
  # paste(round(x[,1],1), " (95% CI ", min(round(x[,2:3])), "-", max(round(x[,2:3])), ")", sep="") 
  get_coef_and_ci <- function(model, skip_intercept=FALSE){
    # Get the coefficients
    if (inherits(model, "lme")){
      tmp <- intervals(model, level=level)$fixed
      my_coefficients <- tmp[,"est."]
      coef_names <- rownames(tmp)
      
      ci <- tmp[,c("lower", "upper")]
    }else{
      my_coefficients <- coef(model)
      coef_names <- names(my_coefficients)
      
      ci <- suppressMessages(confint(model, level=level))
    }
    
    if (skip_intercept){
      intercept <- grep("intercept", coef_names, 
                        ignore.case = TRUE)
      if (length(intercept) > 0){
        my_coefficients <- my_coefficients[-intercept]
        ci <- ci[-intercept,]
        coef_names <- coef_names[-intercept]
      }
    }
    
    # Use the exp() if logit or cox regression
    if (inherits(model, "coxph") ||
          (!is.null(model$family$link) &&
             model$family$link %in% c("logit", "log"))){
      my_coefficients <- exp(my_coefficients)
      ci <- exp(ci)
    }
    
    if (length(my_coefficients) > 1)
      ret_val <- cbind(my_coefficients, ci)
    else
      ret_val <- matrix(c(my_coefficients, ci), nrow=1)
    
    colnames(ret_val) <- c("", 
      sprintf("%.1f%%", 100*(1-level)/2),
      sprintf("%.1f%%", 100*(level + (1-level)/2)))
    rownames(ret_val) <- coef_names
    return(ret_val)
  }
  
  var_names <- prGetModelVariables(model, 
                                   remove_interaction_vars = remove_interaction_vars,
                                   add_intercept = TRUE)
  if (length(var_names) == 0)
    stop("You have no variables that can be displayed as adjusted/unadjusted.",
      " They are most likely all are part of an interaction, spline, I(),",
      " strata, or some other function.")
  
  # Get the adjusted variables
  adjusted <- get_coef_and_ci(model)
  
  # Map rows to variables
  var_rows <- prMapVariable2Name(var_names = var_names, 
                                 available_names = rownames(adjusted),
                                 data = prGetModelData(model))
  keep <- c()
  for (vars in var_rows){
    keep <- c(keep,
              vars$location)
  }
  
  if (length(keep) == 0)
    stop("Error when trying to extract the variable names",
      " from the adjusted values. These names: ", paste(var_names, collapse=", "),
      "\n seem not to exist within the rownames of: ", paste(rownames(adjusted), collapse=", "))
  
  # Sort in order to keep the order
  adjusted <- adjusted[sort(keep), ,drop=FALSE]
  
  unadjusted <- c()
  for(variable in var_names){
    if (!grepl("intercept", variable, 
               ignore.case = TRUE)){
      # Run the same model but with only one variable
      model_only1 <- update(model, paste(".~", variable))
      
      # Get the coefficients processed with some advanced
      # round part()
      new_vars <- get_coef_and_ci(model_only1, skip_intercept = TRUE)
      
      # Add them to the previous
      unadjusted <- rbind(unadjusted, new_vars)
    }else{
      # Run the same model but without any variables
      model_only1 <- update(model, ".~ 1")
      
      # Get the coefficients
      new_vars <- get_coef_and_ci(model_only1, skip_intercept = FALSE)
      
      # Add
      unadjusted <- rbind(new_vars, unadjusted)
      
      # Change name back to the original
      rownames(unadjusted)[1] <- variable[grepl("intercept", variable, 
                                                ignore.case = TRUE)]
    }
  }

  if (any(rownames(adjusted) != rownames(unadjusted)))
    stop("The rownames of the adjusted don't match:", 
         "\n\t a:", rownames(adjusted),
         "\n\tUa: ", rownames(unadjusted))
  
  # If just one variable it's not a proper matrix
  if (is.null(dim(adjusted))){
    both <- matrix(c(unadjusted, adjusted), nrow=1)
  }else{
    both <- cbind(unadjusted, adjusted)
  }
  
  levels_str <- c(sprintf("%.1f %%", 100*(1-level)/2),
    sprintf("%.1f %%", 100*(level + (1-level)/2)))
  
  colnames(both) <- c("Crude", levels_str,
                      "Adjusted", levels_str)
  
  attr(both, "model") <- model
  return(both)
}

