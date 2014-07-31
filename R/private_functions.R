# This file contains all the helper funcitons that the outer exported
# functions utilize. I try to have a pr at the start of the name for all
# the private functions.
#
# Author: max
###############################################################################

#' Looks for unique rowname match without grep
#' 
#' Since a rowname may contain characters reserved by regular
#' expressions I've found it easier to deal with the rowname
#' finding by just checking for matching strings at the beginning
#' of the name while at the same time excluding names that have the
#' same stem, i.e. DM and DM_COMP will cause an issue since DM will
#' match both rows.
#' 
#' @param rnames A vector with the rownames that are looked for
#' @param vn The variable name that is of interest
#' @param vars A vector with all the names and the potentially competing names
#' @return integer A vector containing the position of the matches
#' 
#' TODO: remove this function in favor of the more powerful prMapVariable2Name
#' @keywords internal
prFindRownameMatches <- function(rnames, vn, vars){
  # Find the beginning of the string that matches exactly to the var. name
  name_stub <- substr(rnames, 1, nchar(vn))
  matches <- which(name_stub == vn)

  # Since the beginning of the name may not be unique we need to
  # check for other "competing matches"
  # TODO: make this fix more elegant
  vars_name_stub <- substr(vars, 1, nchar(vn))
  if (sum(vars_name_stub == vn) > 1){
    competing_vars <- vars[vars != vn & 
                             vars_name_stub == vn]
    
    competing_matches <- NULL
    for(comp_vn in competing_vars){
      competing_name_stub <- substr(rnames, 1, nchar(comp_vn))
      competing_matches <- 
        c(competing_matches,
          which(competing_name_stub == comp_vn))
    }
    
    # Clean out competing matches
    matches <- matches[!matches %in% competing_matches]
  }
  
  return(matches)
}

#' Get model outcome
#' 
#' Uses the model to extract the outcome variable. Throws
#' error if unable to find the outcome.
#' 
#' @param model The fitted model.1
#' @return vector
#' 
#' @author max
#' @keywords internal
prExtractOutcomeFromModel <- function(model){
  outcome_formula <- formula(model)[[2]]
  outcome <- NULL
  
  if (length(all.vars(outcome_formula)) != 1){
    # We can probably figure this one out if we have
    # the Surv() call to work with
    if (grepl("^Surv", outcome_formula)[1]){
      ls <- as.list(outcome_formula)
      if(is.null(names(ls))){
        if (length(ls) %in% 3:4){
          outcome_var_name <- as.character(tail(ls, 1))
        }else{
          stop("Could not identify the survival outcome from the elements:",
               paste(ls, collapse=", "))
        }
      }else if ("event" %in% names(ls))
        outcome_var_name <- ls$event
      else if(sum("" == names(ls)) >= 4)
        outcome_var_name <- ls[[which("" ==  names(ls))[4]]]
      else if(sum("" == names(ls)) <= 3)
        outcome_var_name <- ls[[tail(which("" ==  names(ls)), 1)]]
      else
        stop("Could not figure out in your Surv() call what parameter to pick for the descriptive column")
    }else{
      outcome <- try({
        if (is.null(model$call$data)){
          eval(outcome_formula)
        }else{
          with(eval(model$call$data), 
               eval(outcome_formula))
        }})
      
      if (!inherits(outcome, "try-error"))
        return(outcome)
      
      stop(paste("You seem to have some kind of complex outcome:", 
                 deparse(outcome_formula),
                 "\n In order for this function to work in a predictive way",
                 "you can only have one outcome - sorry"))
    }
  }else if (length(outcome_formula) > 1){
    # A complex outcome formula
    if (is.null(model$call$data))
      stop("You need to use the regression model together with the data= option",
        " when you have complex outcomes that consist out of more than one thing or",
        " the software will get confused on how to extract that information.")
    ds <- eval(model$call$data)
    outcome <- with(ds, eval(outcome_formula))
  }else{
    outcome_var_name <- all.vars(outcome_formula)[1]
  }
  
  if (length(outcome) == 0){
    if (is.null(model$call$data)){
      if (grepl("==", outcome_var_name)){
        outcome <- get(gsub("[ ]*==.+$", "", outcome_var_name))
        eq_str <- gsub("^.*==[ ]*", "", outcome_var_name)
        if(grepl("^[0-9]*\\.{0,1}[0-9]*$", eq_str))
          eq_str <- as.numeric(eq_str)
        outcome <- eval(substitute(outcome == a, 
                                   list(a = eq_str)))
      }else{
        outcome <- get(outcome_var_name)
      }
    }else{
      ds <- eval(model$call$data)
      # Remove any $ prior to the call if there is
      # a dataset that was used
      if (grepl("$", outcome_var_name, fixed=TRUE)){
        outcome_var_name <- gsub(".+\\$", "", outcome_var_name)
      }
      
      if (grepl("==", outcome_var_name, fixed=TRUE)){
        o_vars <- strsplit(outcome_var_name, "[ ]*==[ ]*")
        if (o_vars[[1]][1] %in% colnames(ds))
          outcome <- ds[,o_vars[[1]][1]]
        else
          outcome <- get(o_vars[[1]][1])
        outcome <- outcome == gsub("\\\"", "", o_vars[[1]][2])
      }else if (grepl("%in%", outcome_var_name, fixed=TRUE)){
        stop("The %in% is not yet implemented, try creating a new variable")
      }else{
        if (outcome_var_name %in% colnames(ds))
          outcome <- ds[,outcome_var_name]
        else
          outcome <- get(outcome_var_name)
      }
    }
  }

  # I was thinking of using the following
  #     outcome <- model$y
  # but it causes issues if there are missing
  # as the predictors with missing will be included
  # while the outcome with missing are unfortunately not
  # inlcuded - hence we don't know how to match them
  
  if (is.null(outcome))
    stop(paste("Could not find outcome variable:", outcome_var_name))
  
  return(outcome)
}

#' Get model predictors
#' 
#' Uses the model to extract the dataset. Throws
#' error if unable to find the data
#' 
#' @param model The fitted model.  
#' @return data.frame 
#' 
#' @author max
#' @keywords internal
prExtractPredictorsFromModel <- function(model){
  vars <- prGetModelVariables(model, remove_splines=FALSE)
  if (is.null(model$call$data)){
    # Try to create the data frame from the environment
    ds <- data.frame(get(vars[1]))
    colnames(ds) <- c(vars[1])
    if (length(vars) > 1){
      for (i in 2:length(vars)){
        ds[vars[i]] <- get(vars[i])
      }
    }
  }else{
    ds <- eval(model$call$data)
    if (any(vars %nin% colnames(ds)))
      stop(paste("Could not find all the variables in the model in the dataset specified,",
          "these were missing:",
          paste(vars[vars %nin% colnames(ds)]), collapse=", "))
    
    # The drop is needed in case we only have one variable
    ds <- ds[,vars, drop=FALSE]
  }

  # Also tried using the rms package x=TRUE functionality but it unfortunately
  # converts everything to dummy variables making it difficult for regenerating
  # the original dataset
  
  if (is.matrix(ds) & is.data.frame(ds))
    ds <- as.data.frame(ds)
  
  if (is.data.frame(ds) == FALSE)
    stop(paste("Failed to get the original data that was used for generating the model."))
  
  return(ds)
}

#' Get model data.frame
#' 
#' Combines the \code{\link{prExtractPredictorsFromModel}}
#' with the \code{\link{prExtractOutcomeFromModel}} to generate
#' a full model dataset
#' 
#' @param x The fitted model.  
#' @return data.frame 
#' 
#' TODO: Check if this cannot be replaced by model.frame()
#' @author max
#' @keywords internal
prGetModelData <- function(x){
  data <- prExtractPredictorsFromModel(x)
  data <- cbind(data, prExtractOutcomeFromModel(x))
  outcome_name <- as.character(formula(x)[[2]])
  if (length(outcome_name) > 1){
    # A cox regression model can give > 1, Surv(ftime, fstatus) will result in three rows
    colnames(data)[ncol(data)] <- "Outcome"
  }else{
    colnames(data)[ncol(data)] <- outcome_name
  }
  return(data)
}

#' Get the models variables
#'  
#' This function extract the modelled variables. Any interaction
#' terms are removed as those should already be represented by
#' the individual terms.
#'  
#' @param model A model fit
#' @param remove_splines If splines, etc. should be cleaned 
#'  from the variables as these no longer are "pure" variables
#' @param remove_interaction_vars If interaction variables are
#'  not interesting then these should be removed. Often in
#'  the case of \code{\link{printCrudeAndAdjustedModel}} it is impossible
#'  to properly show interaction variables and it's better to show
#'  these in a separate table
#' @param add_intercept Adds the intercept if it exists
#' @return vector with names 
#' 
#' @importFrom stringr str_split
#' @keywords internal
prGetModelVariables <- function(model, 
    remove_splines = TRUE, remove_interaction_vars=FALSE,
    add_intercept = FALSE){
  if (inherits(model, "nlme")){
    vars <- attr(fit$fixDF$terms, "names")
  }else{
    vars <- attr(model$terms, "term.labels")
  }
  
  # Remove I() as these are not true variables
  # and their names can probably have lots of variants
  unwanted_vars <- grep("^I\\(.*$", vars)
  if (length(unwanted_vars) > 0){
    attr(vars, "I() removed") <- vars[unwanted_vars]
    vars <- vars[-unwanted_vars]
  }
  
  pat <- "^[[:alpha:]\\.]+[^(]+\\(.*$"
  fn_vars <- grep(pat, vars)
  if(length(fn_vars) > 0){
    if (remove_splines){
      # Remove splines and other functions
      attr(vars, "functions removed") <- vars[fn_vars]
      vars <- vars[-fn_vars]
    }else{
      # Cleane the variable names into proper names
      # the assumption here is that the real variable
      # name is the first one in the parameters
      pat <- "^[[:alpha:]\\.]+.*\\(([^,)]+).*$" 
      vars[fn_vars] <- sub(pat, "\\1", vars[fn_vars])
    }
  }

  # Remove interaction terms as these are not variables
  int_term <- "^.+:.+$"
  in_vars <- grep(int_term, vars)
  if (length(in_vars) > 0){
    if (remove_interaction_vars){
      in_vn <- unlist(str_split(vars[in_vars], ":"))
      in_vars <- unique(c(in_vars, which(vars %in% in_vn)))
    }
    attr(vars, "interactions removed") <- vars[in_vars]
    vars <- vars[-in_vars]
  }
  
  if (add_intercept && 
        grepl("intercept", names(coef(model))[1], ignore.case = TRUE)){
    vars <- c(names(coef(model))[1],
              vars)
  }
  
  clean_vars <- unique(vars)
  attributes(clean_vars) <- attributes(vars)
  return(clean_vars)
}

#' Get statistics according to the type
#' 
#' A simple function applied by the \code{\link[Gmisc]{getDescriptionStatsBy}}
#' for the total column. This function is also used by \code{\link{printCrudeAndAdjustedModel}}
#' in case of a basic linear regression is asked for a raw stat column
#'  
#' @param x The variable that we want the statistics for 
#' @param show_perc If this is a factor/proportion variable then we
#'  might want to show the percentages 
#' @param html If the output should be in html or LaTeX formatting
#' @param digits Number of decimal digits
#' @param numbers_first If number is to be prior to the percentage
#' @param show_missing If missing should be included 
#' @param show_all_values This is by default false as for instance if there is
#'  no missing and there is only one variable then it is most sane to only show 
#'  one option as the other one will just be a complement to the first. For instance
#'  sex - if you know gender then automatically you know the distribution of the 
#'  other sex as it's 100 \% - other \%. 
#' @param continuous_fn A function for describing continuous variables
#'  defaults to \code{\link{describeMean}} 
#' @param prop_fn A function for describing proportions, defaults to
#'  the factor function
#' @param factor_fn A function for describing factors, defaults to
#'  \code{\link{describeFactors}}
#' @param percentage_sign If you want to suppress the percentage sign you
#'  can set this variable to FALSE. You can also choose something else that
#'  the default \% if you so wish by setting this variable.
#' @return A matrix or a vector depending on the settings
#' 
#' TODO: Use the Gmisc function instead of this copy
#' 
#' @keywords internal
prGetStatistics <- function(x, 
  show_perc = FALSE, 
  html = TRUE, 
  digits = 1, 
  numbers_first = TRUE, 
  show_missing = TRUE, 
  show_all_values = FALSE,
  continuous_fn = describeMean, 
  factor_fn = describeFactors,
  prop_fn = factor_fn,
  percentage_sign = percentage_sign)
{
  show_missing <- prConvertShowMissing(show_missing)
  if (is.factor(x) || 
        is.logical(x) ||
        is.character(x)){
    if (length(unique(x)) == 2){
      if (show_perc){
        total_table <- prop_fn(x, 
            html=html, 
            digits=digits,
            number_first=numbers_first, 
            show_missing = show_missing,
            percentage_sign = percentage_sign)
      }else{
        total_table <- table(x, useNA=show_missing)
        names(total_table)[is.na(names(total_table))] <- "Missing"
        # Choose only the reference level
        if (show_all_values == FALSE)
          total_table <- total_table[names(total_table) %in% c(levels(x)[1], "Missing")]
      }
      
    } else {
      if (show_perc)
        total_table <- factor_fn(x, 
            html=html, 
            digits=digits,
            number_first=numbers_first, 
            show_missing = show_missing,
            percentage_sign = percentage_sign)
      else{
        total_table <- table(x, useNA=show_missing)
        names(total_table)[is.na(names(total_table))] <- "Missing"
      }
    }
  }else{
    total_table <- continuous_fn(x, 
      html=html, digits=digits, 
      number_first=numbers_first, 
      show_missing = show_missing)
    
    # If a continuous variable has two rows then it's assumed that the second is the missing
    if (length(total_table) == 2 &&
      show_perc == FALSE)
      total_table[2] <- sum(is.na(x))
  }
  return(total_table)
}

#' Gets the boundaries for a survival fit
#' 
#' @param fit A survival model of either competing risk regression or cox regression type 
#' @param conf.int The interval of interest 0-1, see levels in confint()
#' @param exp If the value should be in exponential form (default)
#' @return A matrix with the columns: 
#' \item{beta}{The estimated coefficient}
#' \item{p_val}{P-value}
#' \item{low}{The lower confidence interval}
#' \item{high}{The upper confidence interval}
#' \item{order}{A column that later can be used in ordering}
#' 
#' @keywords internal
prGetFpDataFromSurvivalFit <- function (fit, 
  conf.int = 0.95,
  exp      = TRUE){
  # Get the p-value, I use the method in the
  # print.cph -> prModFit from the rms package
  Z <- coef(fit)/sqrt(diag(fit$var))
  p_val <- signif(1 - pchisq(Z^2, 1), 5)
  order <- rep(-1, length(beta))
  ci <- confint(fit, level=conf.int)
  
  if (exp){
    ret_matrix <- cbind(
      beta=exp(coef(fit)),
      p_val=p_val,
      low=exp(ci[,1]),
      high=exp(ci[,2]),
      order=order)
  }else{
    ret_matrix <- cbind(
      beta=coef(fit),
      p_val=p_val,
      low=ci[,1],
      high=ci[,2],
      order=order)
  }
  
  # Set the names of the rows
  rownames(ret_matrix) <- names(fit$coef)
  
  
  return(ret_matrix)
}

#' Gets the boundaries for a GLM fit that is poisson or quasipoisson based
#' 
#' @param glm.fit A regression model 
#' @param conf.int The interval of interest 0-1, see levels in confint()
#' @param exp If the value should be in exponential form (default)
#' @return A matrix with the columns: 
#' \item{beta}{The estimated coefficient}
#' \item{p_val}{P-value}
#' \item{low}{The lower confidence interval}
#' \item{high}{The upper confidence interval}
#' \item{order}{A column that later can be used in ordering}
#' 
#' @keywords internal
prGetFpDataFromGlmFit <- function(glm.fit, 
  conf.int = 0.95,
  exp      = TRUE){
  warning("The GLM part has not been properly tested. Please check that it seems right")
  
  summary_glm <- summary.glm(glm.fit)
  
  # Extract the summary values of interest
  summary_se <- summary_glm$coefficients[,colnames(summary_glm$coefficients) == "Std. Error"]
  if ("quasipoisson" %in% glm.fit$family){
    summary_p_val <- summary_glm$coefficients[,colnames(summary_glm$coefficients) == "Pr(>|t|)"]
  }else if ("poisson" %in% glm.fit$family){
    summary_p_val <- summary_glm$coefficients[,colnames(summary_glm$coefficients) == "Pr(>|z|)"]
  }else{
    stop("Type of analysis not prepared!")
  }
  
  order = rep(-1, length(glm.fit$coefficients))
  ci <- confint(glm.fit, level=conf.int)
  
  if (exp){
    ret_matrix <- cbind(
      beta=exp(coef(glm.fit)),
      p_val=summary_p_val,
      low=exp(ci[,1]),
      high=exp(ci[,2]),
      order=order)
  }else{
    ret_matrix <- cbind(
      beta=coef(glm.fit),
      p_val=summary_p_val,
      low=ci[,1],
      high=ci[,2],
      order=order)
  }
  
  # Set the names of the rows
  rownames(ret_matrix) <- names(glm.fit$coefficients)
  
  # Remove the intercept
  ret_matrix <- ret_matrix[names(glm.fit$coefficients) != "(Intercept)", ]
  
  return(ret_matrix)
}


#' Gets the confidence interval, p-values,
#' coefficients from a survival object
#' 
#' @param model_fit A regression fit from CRR, coxph, cph object 
#' @param conf.int The interval of interest 0-1, see levels in confint()
#' @param exp If the value should be in exponential form (default)
#' @return A matrix with the columns: 
#' \item{beta}{The estimated coefficient}
#' \item{p_val}{P-value}
#' \item{low}{The lower confidence interval}
#' \item{high}{The upper confidence interval}
#' \item{order}{A column that later can be used in ordering}
#' 
#' @keywords internal
prGetFpDataFromFit <- function(model_fit, 
  conf.int = 0.95,
  exp = TRUE){
  # Get the estimates, confidence intervals and the p_values
  if (any(class(model_fit) %in% "coxph") || 
    any(class(model_fit) %in% "crr")){
    sd <- prGetFpDataFromSurvivalFit(fit = model_fit, conf.int = conf.int, exp = exp)
  } else if (any(class(model_fit) %in% "glm")){
    sd <- prGetFpDataFromGlmFit(glm.fit = model_fit, conf.int = conf.int, exp = exp)
  } else {
    stop(paste("Unknown fit class type:", class(model_fit)))
  }
  
  return(sd)
}

#' A functuon for converting a show_missing variable
#' 
#' The variable is suppose to be directly compatible with
#' table(..., useNA=show_missing). It throughs an error
#' if not compatible
#' 
#' @param show_missing Boolean or "no", "ifany", "always" 
#' @return string 
#' 
#' @keywords internal
prConvertShowMissing <- function(show_missing){
  if (show_missing == FALSE || show_missing == "no")
    show_missing <- "no"
  else if (show_missing == TRUE)
    show_missing <- "ifany"
  
  if (show_missing %nin% c("no", "ifany", "always"))
    stop(sprintf("You have set an invalid option for show_missing variable, '%s' ,it should be boolean or one of the options: no, ifany or always.", show_missing))
  
  return(show_missing)
}

#' A function that tries to resolve what variable corresponds to what row
#' 
#' As both the \code{\link{getCrudeAndAdjustedModelData}} and the 
#' \code{\link{printCrudeAndAdjustedModel}} need to now exactly
#' what name from the \code{\link[stats]{coef}}/\code{\link[rms]{summary}}
#' correspond to we for generalizeability this rather elaborate function.
#' 
#' @param var_names The variable names that are saught after
#' @param available_names The names that are available to search through
#' @param data The data set that is sught after
#' @return \code{list} Returns a list with each element has the corresponding
#'  variable name and a subsequent list with the parameters \code{no_rows}
#'  and \code{location} indiciting the number of rows corresponding to that
#'  element and where those rows are located. For factors the list also contains
#'  \code{lvls} and \code{no_lvls}.
#' @keywords internal
prMapVariable2Name <- function(var_names, available_names, data){
  if (any(duplicated(available_names)))
    stop("You have non-unique names. You probably need to adjust",
         " (1) variable names or (2) factor labels.")
  
  # Start with figuring out how many rows each variable
  var_data <- list()
  for (name in var_names){
    if (grepl("intercept", name, ignore.case = TRUE)){
      var_data[[name]] <- 
        list(no_rows = 1)
    }else if (is.factor(data[,name])){
      var_data[[name]] <- 
        list(lvls = levels(data[,name]))
      # Sometimes due to subsetting some factors don't exist
      # we therefore need to remove those not actually in the dataset
      var_data[[name]]$lvls <- 
        var_data[[name]]$lvls[var_data[[name]]$lvls %in%
                                as.character(unique(data[, name][!is.na(data[, name])]))]
      var_data[[name]][["no_lvls"]] <- length(var_data[[name]]$lvls)
      var_data[[name]][["no_rows"]] <- length(var_data[[name]]$lvls) - 1
    }else{
      var_data[[name]] <- 
        list(no_rows = 1)
    }
  }
  
  # A function for stripping the name and the additional information
  # from the available name in order to get the cleanest form
  getResidualCharacters <- function(search, conflicting_name){
    residual_chars <- substring(conflicting_name, nchar(search) + 1)
    if (!is.null(var_data[[search]]$lvls)){
      best_resid <- residual_chars
      
      for (lvl in var_data[[search]]$lvls){
        new_resid <- sub(lvl, "", residual_chars, 
                         fixed = TRUE)
        if (nchar(new_resid) < nchar(best_resid)){
          best_resid <- new_resid
          if (nchar(new_resid) == 0)
            break;
        }
      }
      residual_chars <- best_resid
    }
    return(residual_chars)
  }

  matched_names <- c()
  matched_numbers <- c()
  org_available_names <- available_names
  # Start with simple non-factored variables as these should give a single-line match
  # then continue with the longest named variable
  for (name in var_names[order(sapply(var_data, function(x) is.null(x$lvls)), 
                               nchar(var_names), decreasing = TRUE)]){
    matches <- which(name == substr(available_names, 1, nchar(name)))
    if(length(matches) == 1){
      if (var_data[[name]]$no_rows != 1)
        stop("Expected more than one match for varible '", name, "'",
             " the only positive match was '", available_names[matches], "'")
      
    }else if (length(var_names) > length(matched_names) + 1){
      if (is.null(var_data[[name]]$lvls) &&
            sum(name == available_names) == 1){
        # Check if the searched for variable is a non-factor variable
        # if so then match if there is a perfect match
        
        matches <- which(name == available_names)
        
      }else if (length(var_names) > length(matched_names) + 1){

        # Check that there is no conflicting match
        conflicting_vars <- var_names[var_names != name &
                                        !var_names %in% matched_names]
        possible_conflicts <- c()
        for (conf_var in conflicting_vars){
          possible_conflicts <- 
            union(possible_conflicts,
                   which(substr(available_names, 1, nchar(conflicting_vars)) %in% 
                           conflicting_vars))
        }
        conflicts <- intersect(possible_conflicts, matches)
        if (length(conflicts) > 0){
          
          conflicting_vars <- conflicting_vars[sapply(conflicting_vars,
                 function(search)
                   any(search == substr(available_names, 1, nchar(search))))]
          
          for (conflict in conflicts){
            # We will try to find a better match that leaves fewer "residual characters"
            # than what we started with
            start_res_chars <- getResidualCharacters(name, available_names[conflict])
            
            best_match <- NULL
            best_conf_name <- NULL
            for (conf_name in conflicting_vars){
              resid_chars <- getResidualCharacters(conf_name, available_names[conflict])
              if (is.null(best_match) ||
                    nchar(best_match) > nchar(resid_chars)){
                best_match <- resid_chars
                best_conf_name <- conf_name
              }
            }
            
            if (nchar(start_res_chars) == nchar(best_match)){
              stop("The software can't decide which name belongs to which variable.",
                   " The variable that is searched for is '", name, "'",
                   " and there is a conflict with the variable '", best_conf_name ,"'.",
                   " The best match for '", name, "' leaves: '", start_res_chars, "'",
                   " while the conflict '", best_conf_name ,"' leaves: '", best_match ,"'",
                   " when trying to match the name: '", available_names[conflict] ,"'")
              
            }else if(nchar(start_res_chars) > nchar(best_match)){
              # Now remove the matched row if we actually found a better match
              matches <- matches[matches != conflict]
            }
          }
        }
      }
      if (length(matches) == 0)
        stop("Could not identify the rows corresponding to the variable '", name ,"'",
             " this could possibly be to similarity between different variable names",
             " and factor levels. Try to make sure that all variable names are unique",
             " the variables that are currently looked for are:",
             " '", paste(var_names, 
                         collapse="'', '"),
             "'.")
    }

    # Check that multiple matches are continuous, everything else is suspicious
    if (length(matches) > 1){
      matches <- matches[order(matches)]
      if (any(1 != tail(matches, length(matches) - 1) - 
                head(matches, length(matches) -1)))
        stop("The variable '", name, "' failed to provide an adequate",
             " consequent number of matches, the names matched are located at:",
             " '", paste(matches, collapse="', '"), "'")
    }
    
    # Since we remove the matched names we need to look back at the original and
    # find the exact match in order to deduce the true number
    true_matches <- which(org_available_names %in% 
                            available_names[matches])
    # Avoid accidentally rematching
    true_matches <- setdiff(true_matches, matched_numbers)
    var_data[[name]][["location"]] <- true_matches
    # Update the loop vars
    available_names <- available_names[-matches]
    matched_names <- c(matched_names, name)
    matched_numbers <- c(matched_numbers, true_matches)
    
    if (length(var_data[[name]][["location"]]) !=
          var_data[[name]][["no_rows"]]){
      warning("Expected the variable '", name ,"'",
              " to contain '",var_data[[name]][["no_rows"]],"' no. rows",
              " but got '", length(var_data[[name]][["location"]]), "' no. rows.")
      var_data[[name]][["no_rows"]] <- length(var_data[[name]][["location"]])
    }
  }
  
  return(var_data)
}