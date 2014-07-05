#' Output crude and adjusted model data
#' 
#' Prints table for a fitted object. It prints by default a latex table but can 
#' also be converted into a HTML table that should be more compatible with common
#' word processors.
#' 
#' A word of warning: if you call this function and you've changed any of the variables
#' used in the original call, i.e. the premises are changed, this function will not
#' remember the original values and the statistics will be faulty! 
#' 
#' @param model A regression model or the output from \code{\link{getCrudeAndAdjusted}}
#' @param order A vector with regular expressions for each group.
#' @param digits The number of digits to round to
#' @param ci_max A number that specifies if any values should be 
#'   abbreviated above this value, for instance a value of 1000 
#'   would give a value of \deqn{> -1000}{> -1000} for a value of 1001. This gives
#'   a prettier table when you have very wide confidence intervals. 
#' @param ci_min A number that specifies if any values should be 
#'   abbreviated above this value, for instance a value of -1000 
#'   would give a value of \deqn{< -1000}{< -1000} for a value of -1001. 
#'   This gives a prettier table when you have very wide confidence intervals.
#' @param sprintf_ci_str A string according to \code{\link{sprintf}} to 
#'   write the confidence interval where the first \%s is the lower and 
#'   the second the upper. 
#' @param add_references True if it should use the data set to look for 
#'   references, otherwise supply the function with a vector with names. 
#'   Sometimes you want to indicate the reference row for each group. 
#'   This needs to be just as many as the  groups as the order identified. 
#'   Use NA if you don't want to have a reference for that particular group.
#' @param add_references_pos The position where a reference should be added. 
#'   Sometimes you don't want the reference to be at the top, for instance 
#'   if you have age groups then you may have < 25, 25-39, 40-55, > 55 and
#'   you have the reference to be 25-39 then you should set the reference 
#'   list for \code{age_groups} as \code{add_references_pos = list(age_groups = 2)}
#'   so that you have the second group as the position for the reference.
#' @param reference_zero_effect Used with references, tells if zero effect 
#'   is in exponential form, i.e. \code{exp(0) = 1}, or in regular format, 
#'   i.e. \code{0 = 0} (can be set to any value)
#' @param groups Only used together with regular expression for ordering and 
#'   grouping. Should be a vector with group names if you want to have groups
#'   to some of the identified order groups. If you wish to skip one just
#'   us NA for that instance.
#' @param rowname.fn A function that takes a row name and sees if it needs
#'   beautifying. The function has only one parameter the coefficients name and 
#'   should return a string or expression.  
#' @param use_labels If the rowname.fn function doesn't change the name then 
#'   the label should be used instead of the name, that is if there is a
#'   label and it isn't a factor. 
#' @param desc_column Add descriptive column to the crude and adjusted table
#' @param desc_show_tot_perc Show percentages for the total column
#' @param desc_numb_first Whether to show the number before the percentages
#' @param desc_continuous_fn Stat function used for the descriptive statistics, 
#'   defaults to \code{\link{describeMean}}
#' @param desc_prop_fn Stat function used for the descriptive statistics, 
#'   defaults to \code{\link{describeProp}}
#' @param desc_factor_fn Stat function used for the descriptive statistics, 
#'   defaults to \code{\link{describeFactors}}
#' @param desc_show_missing Show missing variables in the descriptive columns
#' @param desc_digits Number of digits to use in the descriptive columns. 
#'   Defaults to the general digits if not specified.
#' @param desc_colnames The names of the two descriptive columns. By default
#'   Total and Event.
#' @param impute_args A list with additional arguments if the provided input is
#'   a imputed object. Currently the list options \code{coef_change} and
#'   \code{variance.inflation} are supported. If you want both columns then
#'   the simplest way is to provide the list: 
#'   \code{list(coef_change=TRUE, variance.inflation=TRUE)}.
#'   The \code{coef_change} adds a column with the change in coefficients due to 
#'   the imputation, the the "raw" model is subtracted from the imputed results.
#'   The "raw" model is the unimputed model, \code{coef(imputed_model) - coef(raw_model)}.
#'   The \code{variance.inflation} adds the \code{variance.inflation.impute} from the 
#'   \code{\link[Hmisc]{fit.mult.impute}} to a separate column. See the description
#'   for the \code{variance.inflation.impute} in in the \code{\link[Hmisc]{fit.mult.impute}}
#'   description.
#'   Both arguments can be customized by providing a \code{list}. The list can have
#'   the elements \code{type}, \code{name}, \code{out_str}, and/or \code{digits}.
#'   The \code{type} can for \code{coef_change}/\code{variance.impute} be either 
#'   "percent" or "ratio", note that \code{variance.inflation.impute} was not
#'   originally intended to be interpreted as \%. The default for \code{coef_change} is to 
#'   have "diff", that gives the absolute difference in the coefficient.
#'   The \code{name} provides the column name, the \code{out_str} should be a string
#'   that is compatible with \code{\link[base]{sprintf}} and also contains an argument
#'   for accepting a float value, e.g. "%.0f%%" is used by default iun the coef_change
#'   column. The \code{digits} can be used if you are not using the \code{out_str}
#'   argument, it simply specifies the number of digits to show. See the example
#'   for how for a working example.
#'   \emph{Note} that currently only the \code{\link[Hmisc]{fit.mult.impute}}
#'   is supported by this option.
#' @param ... Passed onto the Hmisc::\code{\link{latex}} function, or to 
#'   the \code{\link{htmlTable}} via the print call
#' @return \code{matrix} Returns a matrix of class printCrudeAndAdjusted that 
#'   has a default print method associated with
#' 
#' @importFrom Gmisc insertRowAndKeepAttr
#' 
#' @example inst/examples/printCrudeAndAdjustedModel_example.R
#' 
#' @rdname printCrudeAndAdjustedModel
#' @author max
#' @export
printCrudeAndAdjustedModel <- function(model, 
  order                 = FALSE,
  digits                = 2,
  ci_max                = Inf, 
  ci_min                = -Inf,  
  sprintf_ci_str        = "%s to %s",
  add_references        = FALSE, 
  add_references_pos    = list(),
  reference_zero_effect,
  groups                = NULL,
  rowname.fn            = NULL,
  use_labels            = TRUE,
  desc_column           = FALSE,
  desc_show_tot_perc    = FALSE,
  desc_numb_first       = TRUE,
  desc_continuous_fn    = describeMean,
  desc_prop_fn          = describeProp,
  desc_factor_fn        = describeFactors,
  desc_show_missing     = FALSE,
  desc_digits           = digits,
  desc_colnames         = c("Total", "Event"),
  impute_args,
  ...)
{
  if (missing(reference_zero_effect))
    reference_zero_effect <- ifelse(all("lm" %in% class(model)) ||
        "ols" %in% class(model) ||
        (inherits(model, "glm") && model$family$link == "identity"), 0, 1)
  
  # Just to simplify the checks below
  if (length(add_references) == 1 && add_references == FALSE)
    add_references <- NULL
  
  # You need references if you're going to have a descriptive column
  if (is.null(add_references) &&
        desc_column)
    add_references <- TRUE
  
  if(!"matrix" %in% class(model)){
    # Convert the x that should be a model into a matrix that
    # originally was expected
    x <- getCrudeAndAdjustedModelData(model = model)
    
    ds <- prGetModelData(model)
  }else{
    x <- model
    model <- attr(model, "model")
    
    ds <- prGetModelData(model)
  }
  
  if (!missing(impute_args) && 
        !inherits(model, "fit.mult.impute")){
    stop("You aim to use the arguments aimed for imputed results but unfortunately",
         " the provided model type that you have provided does not support this feature.",
         " The only compatible imputation is the one based upon the fit.mult.impute",
         " at this model and your model does not carry that class name:", 
         " '", paste(class(model), collapse="', '"), "'")
  }else if(!missing(impute_args) &&
             any(!names(impute_args) %in% c("coef_change", 
                                            "variance.inflation"))) {
    invalid_args <- names(impute_args)[!names(impute_args) %in% c("coef_change", 
                                                                  "variance.inflation")]
    warning("The imputation arguments (impute_args):",
            "'", paste(invalid_args, collapse="', '"), "'",
            " provided are invalid and will be ignored by the function.",
            " Currently only arguments coef_change and variance.inflation",
            " are accepted.")
  }
  
  # The rms doesn't getCrudeAndAdjusted doesn't handle the intercept
  intercept <- ifelse(inherits(model, "rms"), FALSE, TRUE)
  var_order <- 
    prMapVariable2Name(var_names = prGetModelVariables(model = model, 
                                                       add_intercept = intercept, 
                                                       remove_interaction_vars = TRUE,
                                                       remove_splines = TRUE),
                       available_names = rownames(x),
                       data = ds)
  x <- prCaPrepareCrudeAndAdjusted(x = x, 
                                   ci_max = ci_max,
                                   ci_min = ci_min,
                                   digits = digits,
                                   sprintf_ci_str = sprintf_ci_str)
  
  if (length(order) > 1 || is.character(order)){
    greps <- prCaGetOrderVariables(names = names(var_order), 
                                   order = order)
    var_order <- var_order[greps]
    row_reorder <- c()
    for (i in 1:length(var_order)){
      last_pos <- length(row_reorder)
      row_reorder <- c(row_reorder,
                       var_order[[i]]$location)
      var_order[[i]]$location <- last_pos + 1:var_order[[i]]$no_rows
    }
    reordered_groups <- x[row_reorder, ,drop=FALSE]
    
    if (any(!rownames(reordered_groups) %in% rownames(x))){
      groups_not_fount <-
        rownames(reordered_groups)[!rownames(reordered_groups) %in% rownames(x)]
      stop("An error occurred when reordering, there are now more",
           " variables than initially found, the following new",
           " vars exist: '", paste(groups_not_fount, collapse="', '"), "'")
    }else if (any(!rownames(x) %in% rownames(reordered_groups))){
      rows_not_used <-
        rownames(x)[!rownames(x) %in% rownames(reordered_groups)]
      warning("Not all variables selected from the model when re-ordering,",
              " the following were not included:",
              " '", paste(rows_not_used, collapse="', '"), "'")
    }
    
    if (length(add_references) == 1 && 
      add_references == TRUE){
      reordered_groups <- 
        prCaAddRefAndStat(model = model,
                          var_order = var_order,
                          add_references = add_references,
                          add_references_pos = add_references_pos,
                          reference_zero_effect = reference_zero_effect, 
                          values = reordered_groups, 
                          ds = ds,
                          desc_column = desc_column, 
                          desc_show_tot_perc = desc_show_tot_perc,
                          desc_numb_first = desc_numb_first,
                          desc_continuous_fn = desc_continuous_fn, 
                          desc_prop_fn = desc_prop_fn,
                          desc_factor_fn = desc_factor_fn, 
                          desc_show_missing = desc_show_missing,
                          desc_digits = desc_digits,
                          desc_colnames = desc_colnames,
                          use_labels = use_labels)
      if (length(groups) > 0){
        if (length(groups) == length(attr(reordered_groups, "rgroup"))){
          attr(reordered_groups, "rgroup") <- groups
        }else{
          warning("You have wanted to use groups but the number of rgroups identified ",
              " by the automatic add_reference (", length(attr(reordered_groups, "rgroup")), " rgroups)",
              " is not equal the number of groups provided by you (", length(groups), ").",
              "\n You have provided the groups: ", paste(groups, collapse=", "), 
              "\n and the rgroups are: ", paste(attr(reordered_groups, "rgroup"), collapse=", "))
        }
      }
    }else if (length(add_references) == length(greps)){
      if (desc_column)
        warning("The descriptive column works so far only when used with automated references")
      for(i in 1:length(var_order)){
        # Add reference if it's not empty
        if (length(add_references) > 1 &&
          is.na(add_references[i]) == FALSE){
          within_pos <- ifelse(add_references[i] %in% add_references_pos, 
            add_references_pos[add_references[i]], 0)
          reordered_groups <- insertRowAndKeepAttr(reordered_groups, 
              head(var_order[[i]]$location, 1) + within_pos, 
              rep(c(reference_zero_effect, "ref"), times=2),  
              rName=add_references[i])

          var_order[[i]]$no_rows <- 
            var_order[[i]]$no_rows + 1
          
          for (ii in i:length(var_order)){
            start_pos <- 0
            if (ii > 1){
              start_pos <- tail(var_order[[ii-1]]$location, 1)
            }
            var_order[[ii]]$location <- start_pos + 1:var_order[[ii]]$no_rows
          }
        }
      }
    }
  }else{
    reordered_groups <- x
    if (length(add_references) == 1 && 
      add_references == TRUE){
      reordered_groups <- 
        prCaAddRefAndStat(model = model,
                          var_order = var_order, 
                          add_references = add_references,
                          add_references_pos = add_references_pos,
                          reference_zero_effect = reference_zero_effect, 
                          values = reordered_groups, 
                          ds = ds,
                          desc_column = desc_column, 
                          desc_show_tot_perc = desc_show_tot_perc,
                          desc_numb_first = desc_numb_first,
                          desc_continuous_fn = desc_continuous_fn, 
                          desc_prop_fn = desc_prop_fn,
                          desc_factor_fn = desc_factor_fn, 
                          desc_show_missing = desc_show_missing,
                          desc_digits = desc_digits,
                          desc_colnames = desc_colnames,
                          use_labels = use_labels)
    }
    greps <- NULL
  }
  
  # The prCaAddRefAndStat adds references and updates the 
  # var_order accordingly, therefore we need to change
  # the var_order according to the reordered_groups if it exists
  if (!is.null(attr(reordered_groups, "var_order"))){
    var_order <- attr(reordered_groups, "var_order")
    attr(reordered_groups, "var_order") <- NULL
  }
  
  if (is.function(rowname.fn)){
    rn <- list()
    for (name in rownames(reordered_groups)){
      new_name <- rowname.fn(name)
      if (new_name == name)
        new_name <- prCaGetRowname(vn = name, use_labels = use_labels, dataset = ds)
      rn <- append(rn, new_name)
    }
  }else{
    rn <- rownames(reordered_groups)
    for (name in names(var_order)){
      # Only change the names of variables if they are not factors
      # Factors already have variable names assigned to them
      if (is.null(var_order[[name]]$lvls) &&
            var_order[[name]]$no_rows == 1){
        rn[var_order[[name]]$location] <- 
          prCaGetRowname(vn = name, 
                         use_labels = use_labels, 
                         dataset = ds)
      }
    }
  }
  rownames(reordered_groups) <- unlist(rn)
  
  coef_name <- ifelse("coxph" %in% class(model), 
    "HR", 
    ifelse("lrm" %in% class(model) |
          ("glm" %in% class(model) &&
          model$family$family == "binomial"),
      "OR",
      "Coef"))
  if(desc_column){
    extra_cols <- ncol(reordered_groups)-4
    attr(reordered_groups, "align") <- c(rep("r", times=extra_cols), rep(c("r", "c"), times=2))
    attr(reordered_groups, "n.cgroup") <- c(extra_cols, 2, 2)
    attr(reordered_groups, "cgroup") = c("", "Crude", "Adjusted")
  }else{
    attr(reordered_groups, "align") <- rep(c("r", "c"), times=2)
    attr(reordered_groups, "n.cgroup") <- c(2, 2)
    attr(reordered_groups, "cgroup") <- c("Crude", "Adjusted")
  }
  
  if (!missing(impute_args)){
    impute_cols <- prCaGetImputationCols(impute_args = impute_args,
                                         output_mtrx = reordered_groups,
                                         model = model,
                                         data = ds)
    
    if (is.matrix(impute_cols)){
      # Merge with original
      tmp <- cbind(reordered_groups, impute_cols)
      reordered_groups <- copyAllNewAttributes(reordered_groups, tmp)
      attr(reordered_groups, "align") <- c(attr(reordered_groups, "align"),
                                           rep("r", times=ncol(impute_cols)))
      attr(reordered_groups, "n.cgroup") <- c(attr(reordered_groups, "n.cgroup"),
                                              ncol(impute_cols))
      attr(reordered_groups, "cgroup") <- c(attr(reordered_groups, "cgroup"),
                                            "Imputation effect")
    }
  }
  
  # Create rgroup and n.rgroup stuff if any variable is a factor
  if (any(sapply(var_order, function(var) !is.null(var$lvls)))){
    rgroup <- n.rgroup <- c()
    for (vn in names(var_order)){
      if (var_order[[vn]]$no_rows == 1){
        if (length(rgroup) == 0 ||
              tail(rgroup, 1) != ""){
          rgroup <- c(rgroup,
                      "")
          n.rgroup <- c(n.rgroup,
                        1)
        }else{
          n.rgroup[length(rgroup)] <- 
            n.rgroup[length(rgroup)] + 1
        }
      }else{
        rgroup <- c(rgroup,
                    prCaGetRowname(vn = vn, use_labels = use_labels, dataset = ds))
        n.rgroup <- c(n.rgroup,
                      var_order[[vn]]$no_rows)
      }
    }
    attr(reordered_groups, "rgroup") <- rgroup
    attr(reordered_groups, "n.rgroup") <- n.rgroup
  }

  class(reordered_groups) <- c("printCrudeAndAdjusted", class(reordered_groups))

  attr(reordered_groups, "headings") <- sub("(Crude|Adjusted)", coef_name, colnames(reordered_groups))
  attr(reordered_groups, "rowlabel.just") <-  "l" 
  attr(reordered_groups, "rowlabel") <-  "Variable"
  attr(reordered_groups, "other") <- list(...)
  return(reordered_groups)
}

setClass("printCrudeAndAdjusted", contains = "matrix")

#' @param x The output object from the printCrudeAndAdjustedModel function 
#' @param rgroupCSSstyle Css style for the rgorup, if different styles are wanted for each of the
#'  rgroups you can just specify a vector with the number of elements. Passed on to \code{\link{htmlTable}}.
#' @rdname printCrudeAndAdjustedModel
#' @method print printCrudeAndAdjusted
#' @S3method print printCrudeAndAdjusted
print.printCrudeAndAdjusted <- function(x,
  rgroupCSSstyle        = "", ...){
  
  call_list <- list(x = x, 
    headings      = attr(x, "headings"), 
    rowlabel.just = attr(x, "rowlabel.just"), 
    rowlabel      = attr(x, "rowlabel"),
    n.cgroup      = attr(x, "n.cgroup"), 
    cgroup        = attr(x, "cgroup"), 
    align         = attr(x, "align"),
    rgroupCSSstyle= rgroupCSSstyle)
  

  if (!is.null(attr(x, "rgroup"))){
    call_list[["rgroup"]] <- attr(x, "rgroup")
    call_list[["n.rgroup"]] <- attr(x, "n.rgroup")
  }
  
  if (length(attr(x, "other")) > 0){
    other <- attr(x, "other")
    for (option in names(other))
      if (nchar(option) > 0) call_list[option] <- other[[option]]
  }
  
  dots <- list(...)
  if (length(dots) > 0){
    for (option in names(dots))
      if (nchar(option) > 0) call_list[option] <- dots[[option]]
  }

  htmlTable_str <- do.call(htmlTable, call_list)
  
  # Output the string since this is the print function
  print(htmlTable_str)
}

#' @param object The output object from the printCrudeAndAdjustedModel function 
#' @seealso \code{\link[Hmisc]{latex}} for details regarding the \code{latex()} function.
#' @rdname printCrudeAndAdjustedModel
#' @method latex printCrudeAndAdjusted
#' @S3method latex printCrudeAndAdjusted
#' @importFrom Hmisc latex
latex.printCrudeAndAdjusted <- function(object, ...){
  call_list <- 
    list(colheads      = attr(reordered_groups, "headings"), 
         rowlabel.just = attr(reordered_groups, "rowlabel.just"), 
         rowlabel      = attr(reordered_groups, "rowlabel"),
         rowname       = latexTranslate(rownames(object)),
         cgroup        = attr(object, "cgroup"), 
         n.cgroup      = attr(object, "n.cgroup"), 
         align         = attr(object, "align"))
  
  if (!is.null(attr(object, "rgroup"))){
    call_list[["rgroup"]] <- attr(object, "rgroup")
    call_list[["n.rgroup"]] <- attr(object, "n.rgroup")
  }
  
  dots <- list(...)
  if (length(dots) > 0){
    for (option in names(dots))
      if (nchar(option) > 0) 
        call_list[option] <- dots[[option]]
  }
  
  return(do.call(latex, call_list))
}

#' Function for retrieving the imputation arguments
#' 
#' @param impute_args The imputation arguments from \code{\link{printCrudeAndAdjusted}}
#'  function call.
#' @param output_mtrx The reordered groups matrix (a nx4 matrix)
#'  that have been prepared in for the \code{\link{printCrudeAndAdjusted}} 
#'  function. It is important that the references
#'  if any have been added.
#' @param model The imputation model. Currently only \code{\link[Hmisc]{fit.mult.impute}}
#'  is supported by the function.
#' @param data The data that has been used for generating the model.
#' @return \code{matrix} Returns a matrix with the requested columns
prCaGetImputationCols <- function(impute_args,
                                  output_mtrx,
                                  model,
                                  data){
  # Check if the reqquested imputation information has been implemented
  # if not return NULL
  if (!any(names(impute_args) %in% c("coef_change", 
                                    "variance.inflation"))){
    return(NULL)
  }
  
  impute_cols <- NULL
  custom_sprintf <- function(sp_str, variable){
    digits_search <- regexpr("%.[0-9]+f", sp_str)
    if(digits_search == -1)
      stop("Your output string for sprintf does not seem to contain a %.[0-9]+f",
           " regex compatible string, your output string is: ", sp_str)
    
    round_2 <- as.numeric(substr(sp_str, digits_search + 2, 
                                 digits_search + 2 + attr(digits_search, "match.length") - 4))
    n <- names(variable)
    # It's a little tricky to actually remove the -0.0 section...
    rounded_variable <- as.numeric(as.character(round(variable, digits = round_2)))
    out <- sprintf(sp_str, rounded_variable)
    names(out) <- n
    return(out)
  }
  
  # Compare the coefficients from the imputation with the 
  # original coefficients. This can give an idea to the 
  # direction of the imputed results, i.e. if the effect
  # size increases or decreases when adding the observations 
  # with missing data
  if (!is.null(impute_args$coef_change)){
    
    # Call the fitter without the imputed data in order
    # to get the original coefficients
    raw_call_lst <- list(formula=formula(model),
                         data=data)
    for (param_name in names(model$call)){
      # A few parameters are only used by the fit.mult.impute
      # and should not be forwarderd to the fitter
      if (!param_name %in% c("", "xtrans", "fitter", "n.impute",
                             "data", "formula", "fit.reps", 
                             "dtrans", "derived", "vcovOpts",
                             "pr")){
        raw_call_lst[[param_name]] <- model$call[[param_name]]
      }
    }
    non_imputed_fit <- 
      do.call(as.character(model$call$fitter),
              raw_call_lst)
    
    diff <- coef(model) - coef(non_imputed_fit)
    name <- "Coefficient change"
    

    # Do exp() if the variabels should be presented in that format
    # for that specific function type
    antilog <- FALSE
    if (inherits(model, "coxph") ||
          (!is.null(model$family) &&
             !is.null(model$family$link) &&
             grepl("^log", model$family$link))){
      antilog <- TRUE
    }
    
    if (!is.list(impute_args$coef_change)){
      change <- diff
      out_str <- "%.1f"
    }else{
      if (is.logical(impute_args$coef_change$antilog)){
        antilog <- impute_args$coef_change$antilog
      }
      
      out_str <- impute_args$coef_change$out_str
      
      if (tolower(impute_args$coef_change$type) %in% c("%", 
                                                       "percent", 
                                                       "percentages")){
        if (is.null(out_str)){
          if (!is.numeric(impute_args$coef_change$digits)){
            out_str <- "%.0f%%"
          }else{
            out_str <- paste0("%.",
                              impute_args$coef_change$digits,
                              "f%%")
          }
        }
        change <- diff/abs(coef(non_imputed_fit))*100

        if (is.character(impute_args$coef_change$name)){
          name <- impute_args$coef_change$name
        }
      }else if(tolower(impute_args$coef_change$type) %in% c("ratio",
                                                          "/")){
        if (is.null(out_str)){
          if (!is.numeric(impute_args$coef_change$digits)){
            out_str <- "%.2f"
          }else{
            out_str <- paste0("%.",
                              impute_args$coef_change$digits,
                              "f")
          }
        }
        change <- diff/abs(coef(non_imputed_fit))
        
        if (is.character(impute_args$coef_change$name)){
          name <- impute_args$coef_change$name
        }
        
      }else if(!is.character(impute_args$coef_change$type) ||
                 tolower(impute_args$coef_change$type) %in% c("abs",
                                                          "absolute",
                                                          "diff",
                                                          "difference",
                                                          "-")){
        if (is.null(out_str)){
          if (!is.numeric(impute_args$coef_change$digits)){
            out_str <- "%.2f"
          }else{
            out_str <- paste0("%.",
                              impute_args$coef_change$digits,
                              "f")
          }
        }
        change <- diff
        
        if (is.character(impute_args$coef_change$name)){
          name <- impute_args$coef_change$name
        }
        
      }else{
        stop("The requested type '", impute_args$coef_change$type ,"' for coef_reps",
             " is not yet implemented. Currently only percent or ratio is available.")
      }
    }
    
    if (antilog){
      change <- exp(change)
    }
    change <- custom_sprintf(out_str, change)
    impute_cols <- cbind(impute_cols,
                         change)
    
    colnames(impute_cols)[ncol(impute_cols)] <- name
  }
  
  if (!is.null(impute_args$variance.inflation)){
    
    name <- "Variance change"

    if (!is.list(impute_args$variance.inflation)){
      inflation <- custom_sprintf("%.2f", model$variance.inflation.impute)
    }else{
      if (is.character(impute_args$variance.inflation$name)) 
        name <- impute_args$variance.inflation$name
        
      if (is.character(impute_args$variance.inflation$type)){
        type <- tolower(impute_args$variance.inflation$type)
      }else{
        type <- "raw"
      }
      
      if (is.character(impute_args$variance.inflation$name)){
        name <- impute_args$variance.inflation$name
      }
      
      out_str <- impute_args$variance.inflation$out_str
      
      if (type %in% c("%", "percent", "percentages")){
        if (is.null(out_str)){
          if (!is.numeric(impute_args$variance.inflation$digits)){
            out_str <- "%.0f%%"
          }else{
            out_str <- paste0("%.",
                              impute_args$variance.inflation$digits,
                              "f%%")
          }
        }
        
        inflation <- custom_sprintf(out_str, model$variance.inflation.impute*100)
      }else if (type %in% c("raw", "ratio")){
        if (is.null(out_str)){
          if (!is.numeric(impute_args$variance.inflation.impute$digits)){
            out_str <- "%.2f"
          }else{
            out_str <- paste0("%.",
                              impute_args$variance.inflation.impute$digits,
                              "f")
          }
        }
                
        inflation <- custom_sprintf(out_str, model$variance.inflation.impute)
      }else{
        stop("The requested type '", impute_args$coef_change$type ,"'",
             " for variance.inflation.type",
             " is not yet implemented. Currently only percent or raw is available.")
      }
    }
    
    impute_cols <- cbind(impute_cols,
                         inflation)
    colnames(impute_cols)[ncol(impute_cols)] <- name
  }
  
  # To add additional imputation information we need to know
  # what rows contain the reference information
  reference_rows <- which(output_mtrx[,4] == "ref")
  
  if (length(reference_rows) > 0){
    for (row_no in reference_rows){
      impute_cols <-
        insertRowAndKeepAttr(impute_cols, r = row_no, 
                             v = rep("", times=ncol(impute_cols)))
      
    }
  }
  
  # The rms version does not provide the intercept and therefore the
  # intercept should be removed
  if (inherits(model, "rms") &&
        !grepl("intercept", x = rownames(output_mtrx)[1],  ignore.case = TRUE) &&
        grepl("intercept", x = rownames(impute_cols)[1],  ignore.case = TRUE)){
    impute_cols <- impute_cols[-1,,drop=FALSE]
  }

  return(impute_cols)
}

#' Add reference according to the model
#' 
#' This is of course for factored variables and not in general.
#'  
#' @param model The regression model 
#' @param order The order
#' @param add_references True if it should use the dataset to look for references, otherwise
#'   supply the function with a vector with names. Sometimes you want to indicate 
#'   the reference row for each group. This needs to be just as many as the 
#'   groups as the order identified. Use NA if you don't want to have a 
#'   reference for that particular group.
#' @param add_references_pos The position where a reference should be added. Sometimes
#'   you don't want the reference to be at the top, for instance if you have age groups
#'   then you may have < 25, 25-39, 40-55, > 55 and you have the reference to be 25-39 then
#'   you should set the reference list for \code{age_groups} as \code{add_references_pos = list(age_groups = 2)}
#'   so that you have the second group as the position for the reference.
#' @param reference_zero_effect The zero effect that the reference uses
#' @param values The values that are to be outputted
#' @param ds The dataset
#' @param desc_column Add descriptive column to the crude and adjusted table
#' @param desc_show_tot_perc Show percentages for the total column
#' @param desc_numb_first Whether to show the number before the percentages
#' @param desc_continuous_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeMean}}
#' @param desc_prop_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeProp}}
#' @param desc_factor_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeFactors}}
#' @param desc_show_missing Show missing variables in the descriptive columns
#' @param desc_digits Number of digits to use in the descriptive columns. Defaults
#'  to the general digits if not specified.
#' @param desc_colnames The names of the two descriptive columns. By default
#'  Total and Event.
#' @param use_labels If labels should be used for rownames
#' @return list 
#' 
#' @importFrom Gmisc copyAllNewAttributes
#' 
#' @author max
prCaAddRefAndStat <- function(model, 
  var_order, 
  add_references,
  add_references_pos,
  reference_zero_effect, 
  values, 
  ds,
  desc_column, 
  desc_show_tot_perc,
  desc_numb_first,
  desc_continuous_fn, 
  desc_prop_fn,
  desc_factor_fn, 
  desc_show_missing,
  desc_digits,
  desc_colnames,
  use_labels
  ){

  outcome <- NULL
  if (desc_column){
    stats <- list()
    
    # Get the original data
    outcome <- prExtractOutcomeFromModel(model)
    if (is.matrix(outcome) && 
          "coxph" %in% class(model)){
      # Get the left part of the formula
      outcome <- outcome[,"status"]
    }
  }
  
  stats <- list()
  
  for(vn in names(var_order))
  {
    if (desc_column && !is.null(outcome)){
      stats[[vn]] <- prCaGetVnStats(model = model,
        vn = vn, 
        outcome = outcome,
        ds = ds,
        add_references = add_references,
        add_references_pos = add_references_pos,
        desc_digits= desc_digits, 
        desc_continuous_fn=desc_continuous_fn, 
        desc_prop_fn=desc_prop_fn,
        desc_factor_fn=desc_factor_fn, 
        desc_show_missing=desc_show_missing,
        desc_show_tot_perc=desc_show_tot_perc,
        desc_colnames=desc_colnames)
    }

    # Add the refrence to the values matrix if it is a factor variable with a ref.
    if (!is.null(var_order[[vn]]$lvls)){
      values <- prCaAddReference(vn = vn, 
                                 var_order = var_order, 
                                 values = values,
                                 add_references_pos = add_references_pos,
                                 reference_zero_effect = reference_zero_effect,
                                 ds = ds,
                                 use_labels = use_labels)
      # Add reference to no_rows, i.e. no_rows == lenght(lvls)
      var_order[[vn]]$no_rows <- var_order[[vn]]$no_lvls
      
      # Update locations after the added references or the 
      # next reference locator will look for the reference among
      # the wrong rows
      for(i in which(vn == names(var_order)):(length(var_order))){
        start_pos <- 0
        if (i > 1){
          start_pos <- tail(var_order[[i-1]]$location, 1)
        }
        var_order[[i]]$location <- start_pos + 1:var_order[[i]]$no_rows
      }
    }
  }
  
  
  if (desc_column){
    desc_mtrx <- matrix("-", 
                        ncol=NCOL(stats[[1]]), 
                        nrow=NROW(values))
    rownames(desc_mtrx) <- rownames(values)
    
    # Should probably make sure we're always dealing
    # with a matrix but this is a quick fix for now
    # TODO: fix consistent matrix handling
    getRows <- function(x){
      ifelse(is.matrix(x), nrow(x), length(x))
    }
    getRownames <- function(x){
      if(is.matrix(x))
        rownames(x)
      else
        names(x)
    }
    getValue <- function(x, rn){
      if(is.matrix(x))
        x[rn,,drop=FALSE] 
      else
        x[rn]
    }
    for(vn in names(var_order)){
      if (!is.null(var_order[[vn]]$lvls)){
        existing_labels <- rownames(values)[var_order[[vn]]$location]
        if (any(!existing_labels %in% rownames(stats[[vn]])))
          stop(paste("A few labels from factor", vn, "weren't found in the stats:",
              paste(existing_labels[!existing_labels %in% rownames(stats[[vn]])], 
                collapse=", ")))
        
        # Add the stats to the desc
        # This slightly complicated structure is to make sure that
        # the descriptive value corresponds to the regression row
        for(rn in existing_labels){
          # Find that row within the group
          group_rownames <- rownames(desc_mtrx[var_order[[vn]]$location, ,drop=FALSE])
          row_within_group <- which(rn == group_rownames)
          if (length(row_within_group) > 1)
            stop("There are more than one occurrence within group ", vn,
              "that have the value: '", rn, "'\n",
              "The rownames in that group are: '", paste(group_rownames, "', '"), "'")
          else if (length(row_within_group) == 0)
            stop("There was no match within group ", vn,
              "for the value: '", rn, "'\n",
              "The rownames in that group are: '", paste(group_rownames, "', '"), "'")
          
          # Set the value of that row
          desc_mtrx[head(var_order[[vn]]$location, 1) + 
                      row_within_group - 1, ] <- getValue(stats[[vn]], rn)
        }
        
        # There are more values in the stats than in the 
        # regression, this is probably due to missing values,
        # these will be added to the current group last
        if (getRows(stats[[vn]]) > var_order[[vn]]$no_rows){
          rows_2_add <- getRownames(stats[[vn]])[!getRownames(stats[[vn]]) %in% 
                                                   existing_labels]
          for (i in 1:length(rows_2_add)){
            rn <- rows_2_add[i]
            values <- insertRowAndKeepAttr(values, 
                                           r = tail(var_order[[vn]]$location, 1) + 1, 
                                           v = rep("-", length.out=cols), 
                                           rName = rn)
            desc_mtrx <- insertRowAndKeepAttr(desc_mtrx, 
                                              r = tail(var_order[[vn]]$location, 1) + 1,
                                              v = getValue(stats[[vn]], rn), 
                                              rName = rn)
          }
          
          var_no <- which(vn == names(var_order))
          var_order[[var_no]]$no_rows <- var_order[[var_no]]$no_rows + length(rows_2_add)
          
          # Update the value_order
          for(ii in var_no:length(var_order)){
            start_pos <- 0
            if (ii > 1){
              start_pos <- tail(var_order[[ii - 1]]$location, 1) - 1
            }
            var_order[[ii]]$location <- start_pos + 1:var_order[[ii]]$no_rows
          }
        }
      }else{
        # This occurrs if the element is logical and you have
        # a TRUE/FALSE situation
        
        if (var_order[[vn]]$no_rows != NROW(stats[[vn]])){
          if (var_order[[vn]]$no_rows == 1 &&
                NROW(stats[[vn]]) == 2 &&
                is.logical(ds[,vn])){
            stats[[vn]] <- stats[[vn]]["TRUE",]
          }else{
            stop("The description statistics did not work for '", vn, "'",
                 " it returned ", NROW(stats[[vn]]), " rows",
                 " while expecting ", var_order[[vn]]$no_rows, " row(s).",
                 " The rowlabels returned are:",
                 " '", paste(rownames(stats[[vn]]), collapse="', '"), "'")
          }
        }
        desc_mtrx[var_order[[vn]]$location, ] <- stats[[vn]]
      }
    }
    
    values <- copyAllNewAttributes(from = values, 
                                   to = cbind(desc_mtrx, values)) 
    if (NCOL(desc_mtrx) == 1 && colnames(values)[1] == "")
      colnames(values)[1] <- desc_colnames[1]
    else if(all(colnames(values)[1:2] == ""))
      colnames(values)[1:2] <- desc_colnames
  }
  
  attr(values, "var_order") <- var_order
  return(values)
}

#' Adds a reference to value matrix
#' 
#' @param vn Variable name 
#' @param var_order The output from the \code{\link{prMapVariable2Name}}
#' @param values The value matrix
#' @param add_references_pos The position within the factors if it exists
#' @param reference_zero_effect The reference zero effect 
#' @param ds The data set
#' @param use_labels If labels should be used for row names
#' @return \code{matrix} A matrix with rgroup and n.rgroup attributes 
#' 
#' @author max
prCaAddReference <- function(vn, 
                             var_order, 
                             values, 
                             add_references_pos, 
                             reference_zero_effect, 
                             ds, use_labels){
  ref_value <- rep(c(reference_zero_effect, "ref"), times=2)
  
  reference <- NULL
  rms_format <- FALSE
  # The rms package generates rownames with factor name:reference factor
  # and it is therefore a good idea to find the refreence by checking
  # which one is at the end
  for (f_name in var_order[[vn]]$lvls){
    # The substr is just to avoid having to check for regular expression
    # characters within f_name and escaping them
    beginning <- substr(rownames(values)[var_order[[vn]]$location], 
                        1,
                        nchar(rownames(values)[var_order[[vn]]$location])-
                          nchar(f_name))
    end <- substring(rownames(values)[var_order[[vn]]$location], 
                     nchar(rownames(values)[var_order[[vn]]$location])-
                       nchar(f_name) + 1)
    if (all(grepl(":$", beginning)) && 
          all(f_name %in% end)){
      reference <- f_name
      rms_format <- TRUE
      break
    }
  }
  
  if (is.null(reference)){
    # TODO: Could probably be extended to other regression models but needs testing
    used_factors <- gsub("^[ ]{0,1}[=-]{0,1}{0,1}", "", 
                         substring(rownames(values)[var_order[[vn]]$location], nchar(vn) + 1))
    
    # Fetch the reference level, would probably work just as well with a levels()[1]
    reference <- var_order[[vn]]$lvls[!var_order[[vn]]$lvls %in% used_factors]
    if (length(reference) != 1)
      stop("Error occurred in looking for reference,",
           " found ", length(reference), " reference categories",
           " while expecting 1, out of these factors:",
           "\n '", paste(var_order[[vn]]$lvls, collapse="'', '"), "'",
           ifelse(length(reference) > 1, 
                  sprintf(" \n The refrences found: '%s'",
                          paste(reference, collapse="', '")),
                  sprintf(" \n The rownames that have searched: '%s'", 
                          paste(rownames(values)[var_order[[vn]]$location], collapse="', '"))))
  }else{
    used_factors <- var_order[[vn]]$lvls[reference != var_order[[vn]]$lvls]
  }
  
  clean_rn = rownames(values)
  for (uf_name in used_factors){
    if (rms_format){
      clean_rn <- gsub("^.* - (.*):.*$", "\\1", rownames(values))
      r_no <- which(clean_rn == uf_name)
    }else{
      r_no <- grep(uf_name, clean_rn, fixed=TRUE)
    }
    
    if (!any(r_no %in% var_order[[vn]]$location))
      stop("Could not find rowname with factor ", uf_name, 
           " among any of the row names: ", paste(clean_rn, collapse=", "))
    
    r_no <- r_no[r_no %in% var_order[[vn]]$location]
    if (length(r_no) > 1)
      stop("Multiple rows matched the factor ", uf_name,
        " from the available: ", paste(clean_rn[var_order[[vn]]$location], collapse=", "))
    
    # Remove the main label as that goes into the attr(values, "rgroup")
    rownames(values)[r_no] <- uf_name
  }
  
  offset <- ifelse(vn %in% names(add_references_pos),
                   add_references_pos[[vn]] - 1,
                   0)
  if (offset > length(var_order[[vn]]$lvls) - 1 ||
        offset < 0){
    warning("You have a reference position '", add_references_pos[[vn]], "'",
            " that is outside the number of levels, '", offset + 1, "'",
            " is not among, 1 to ", length(var_order[[vn]]$lvls),
            ". This will therefore be ignored")
    offset <- 0
  }
  
  values <- insertRowAndKeepAttr(values, 
    var_order[[vn]]$location[1] + offset, 
    ref_value,
    rName=reference)
  
  return(values)
}

#' Gets the variable stats
#' 
#' @param model The model
#' @param vn The variable name
#' @param outcome The outcome vector
#' @param ds The dataset
#' @param add_references True if it should use the dataset to look for references, otherwise
#'   supply the function with a vector with names. Sometimes you want to indicate 
#'   the reference row for each group. This needs to be just as many as the 
#'   groups as the order identified. Use NA if you don't want to have a 
#'   reference for that particular group.
#' @param add_references_pos The position where a reference should be added. Sometimes
#'   you don't want the reference to be at the top, for instance if you have age groups
#'   then you may have < 25, 25-39, 40-55, > 55 and you have the reference to be 25-39 then
#'   you should set the reference list for \code{age_groups} as \code{add_references_pos = list(age_groups = 2)}
#'   so that you have the second group as the position for the reference.
#' @param desc_show_tot_perc Show percentages for the total column
#' @param desc_continuous_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeMean}}
#' @param desc_prop_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeProp}}
#' @param desc_factor_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeFactors}}
#' @param desc_show_missing Show missing variables in the descriptive columns
#' @param desc_digits Number of digits to use in the descriptive columns. Defaults
#'  to the general digits if not specified.
#' @param desc_colnames The names of the two descriptive columns. By default
#'  Total and Event.
#' @return \code{matrix} A matrix from \code{\link[Gmisc]{getDescriptionStatsBy}} or
#'  \code{\link{prGetStatistics}}
#' 
#' @importFrom Gmisc getDescriptionStatsBy
#' @author max
prCaGetVnStats <- function(model,
  vn, 
  outcome, 
  ds,
  add_references, 
  add_references_pos,
  desc_digits, 
  desc_continuous_fn, 
  desc_prop_fn,
  desc_factor_fn, 
  desc_show_missing,
  desc_show_tot_perc,
  desc_colnames){
  # TODO: add some option of handling missing from the model, a second/third column
  # TODO: add handling for logical values
  
  # If there is a binomial outcome variable then 
  # it makes sense to have two columns, the overall
  # and the event data.
  if (any(class(model) %in% c("lrm", "coxph")) ||
    ("glm" %in% class(model) &&
      model$family$family == "binomial")){
    if (grepl("intercept", vn, ignore.case = TRUE)){
      desc_mtrx <- matrix("-", 
                          ncol = length(unique(outcome[is.na(outcome) == FALSE])),
                          nrow = 1)
    }else{
      desc_mtrx <- 
        getDescriptionStatsBy(x=ds[is.na(outcome) == FALSE,vn], 
                              by=outcome[is.na(outcome) == FALSE],
                              hrzl_prop = TRUE,
                              digits = desc_digits,
                              continuous_fn = desc_continuous_fn,
                              prop_fn = desc_prop_fn,
                              factor_fn = desc_factor_fn,
                              show_all_values = add_references,
                              show_missing = desc_show_missing,
                              add_total_col = TRUE,
                              total_col_show_perc = desc_show_tot_perc, 
                              html = TRUE)
    }
    
    # Don't select the no-event alternative as this is usually
    # not interesting since we have the total column
    desc_mtrx <- desc_mtrx[,c(1,3),drop=FALSE]
    colnames(desc_mtrx) <- desc_colnames
  }else{
    if (grepl("intercept", vn, ignore.case = TRUE)){
      desc_mtrx <- matrix("-", 
                          ncol = 1,
                          nrow = 1)
    }else{
      desc_mtrx <- 
        prGetStatistics(x=ds[is.na(outcome) == FALSE,vn],  
                        show_perc = desc_show_tot_perc, 
                        html = TRUE,
                        digits = desc_digits,
                        continuous_fn = desc_continuous_fn,
                        prop_fn = desc_prop_fn,
                        factor_fn = desc_factor_fn,
                        show_missing = desc_show_missing)
    }
  }
  
  if (!is.matrix(desc_mtrx)){
    rn <- names(desc_mtrx)
    desc_mtrx <- matrix(desc_mtrx, ncol=1)
    if (!is.null(rn))
      rownames(desc_mtrx) <- rn
  }
  
  # As the first element in a factor is always the
  # reference then we need to move it to the wanted
  # position
  if (!grepl("intercept", vn, ignore.case = TRUE) &&
        is.factor(ds[,vn]) && 
        vn %in% names(add_references_pos) &&
        add_references_pos[[vn]] != 1){
    
    if (nrow(desc_mtrx) == 2){
      if (add_references_pos[[vn]] == 2)
        desc_mtrx <- desc_mtrx[c(2,1), ]
    }else if (nrow(desc_mtrx) > 2){
      if (add_references_pos[[vn]] == nrow(desc_mtrx)){
        desc_mtrx <- desc_mtrx[c(2:nrow(desc_mtrx), 1), ] 
      }else{
        desc_mtrx <- desc_mtrx[c(2:add_references_pos[[vn]], 
                                 1,
                                 (add_references_pos[[vn]]+1):nrow(desc_mtrx)), ]
      }
    }
  }
  
  return(desc_mtrx)
}

#' Gets the labelled rowname if it exists
#' 
#' Looks for matches inside factors if rowname
#' contains the name of the column. 
#' 
#' @param vn The variable name 
#' @param use_labels If labels should be used
#' @param dataset The dataset
#' @return \code{string} The rowname 
#' 
#' @importFrom Hmisc label
#' @author max
prCaGetRowname <- function(vn, use_labels, dataset){
  vn <- as.character(vn)
  if(vn %in% colnames(dataset) &&
    use_labels && 
    label(dataset[,vn]) != ""){
    return(label(dataset[,vn]))
  }else if (any(vn == colnames(dataset))){
    # An exact match means that there is no factor information
    # for this row and we should be able to return this row
    return(vn)
  }else if (grepl("intercept", vn, ignore.case = TRUE)){
    return(vn)
  }
    
  # Check if this is actually a factor and return that factors name
  colno_containing_name <-
    unlist(lapply(colnames(dataset), 
                  function(x) grepl(x, vn, fixed=TRUE)))
  if (sum(colno_containing_name) == 1){
    # Remove the column name from the beginning of the vn 
    # as this may otherwise cause a search conflict if
    # the name consists the searched labels
    cn <- colnames(dataset)[colno_containing_name]
    if (cn == substr(vn, 1, nchar(cn))){
      vn <- substring(vn, nchar(cn) + 1)
    }
    
    lvls <- levels(dataset[,colno_containing_name])
    matching_lvl <- unlist(lapply(lvls, function(x) grepl(x, vn, fixed=TRUE)))
    if (sum(matching_lvl) == 1)
      return(lvls[matching_lvl])
    
    # The rms-package returns the levels so that the reference appears after
    # each level and thus may give the appearance that the levels appears
    # several times. We therefore need to remove that information.
    if (grepl(sprintf("%s$", lvls[1]), vn)){
      vn <- gsub(sprintf("%s$", lvls[1]), "", vn)
      matching_lvl <- unlist(lapply(lvls, function(x) grepl(x, vn, fixed=TRUE)))
      if (sum(matching_lvl) == 1)
        return(lvls[matching_lvl])
    }
    
    
    warning("Could not identify the rowname '", vn, "'",
      " from the factor variable '", colnames(dataset)[colno_containing_name], "'",
      " that has the factors: '", paste(lvls, collapse="', '"), "'",
      " The rowname with label therefore defaults to ", vn)
  }
  
  return(vn)
}

#' Re-order variables
#' 
#' @param names The names of the variables 
#' @param order The order regular expression
#' @param ok2skip If you have the intercept then
#'  it should be ok for the function to skip that
#'  variable if it isn't found among the variable list
#' @return \code{vector} A vector containing the greps 
#' 
#' @author max
prCaGetOrderVariables <- function(names, order, ok2skip = FALSE){
  greps <- c()
  for (r_expr in order) {
    # Find the names that matches
    matches <- grep(r_expr, names)
    if (length(matches) == 0 & !ok2skip){
      stop("You have a strange selection order,",
           "this could be due to that you try to select a factor level",
           " and not the full variable.",
           " Re-arranging factors should be done in the factor() function and not here.",
           " Anyway the expression '", r_expr, "' was not found",
           " in these variable names: '",
           paste(names, collapse="', '"), "'")
    }else if (length(matches) > 0){
      # Avoid reselecting
      new_vars <- setdiff(matches, unlist(greps))
      if (length(new_vars) > 0){
        greps <- append(greps, list(new_vars))
      }
    }
  }
  return(unlist(greps))
}


#' Prettify the text
#' 
#' Sets the number of digits, formats the confidence interval and 
#' changes the number of cols into 4 where the upper and lower CI 
#' meet in one string column
#' 
#' @param x The value matrix from getCrudeAndAdjusted 
#' @param ci_max The max confidence interval
#' @param ci_min The min confidence interval
#' @param digits The number of decimal digits to use
#' @param sprintf_ci_str The \code{\link{sprintf}} code for the confidence interval
#' @return \code{matrix} A string matrix with the values formated
#' 
#' @author max
prCaPrepareCrudeAndAdjusted <- function(x, ci_max, ci_min, digits, sprintf_ci_str){
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  
  # Just to make sure that it gives 1.0 and
  # not 1 if digits = 1, in cases where a
  # adding another decimal that is used
  # since everyone is so hyped about p-val < 0.05
  format_number <- function(x){
    if (length(grep("[0-9]", as.character(x))) == 0)
      return(x)
    
    # Matrix forces all values to be either char or
    # numeric and therefore we need to convert them
    # to numeric
    x <- as.numeric(x)
    
    if (is.wholenumber(x) && x > 100)
      return(as.character(x))
    
    return(sprintf(sprintf("%%0.%df", digits), x))
  }
  
  # A way to set the min/max of the confidence interval
  # according to the parameters,
  # round to appropriate number of digits and 
  # format into a string as specified in the sprintf string
  format_ci <- function(ci){
    # If there is an NA then we don't know
    # much about the data
    if (any(is.na(ci))){
      return("-")
    }
    
    upper <- max(ci)
    if (upper > ci_max)
      upper <- sprintf("&gt; %s", format_number(ci_max))
    else
      upper <- format_number(upper)
    
    lower <- min(ci)
    if (lower < ci_min)
      lower <- sprintf("&gt; %s", format_number(ci_min))
    else
      lower <- format_number(lower)
    
    return(sprintf(sprintf_ci_str, lower, upper))
  }
  
  values <- cbind(
    tapply(x[,1, drop=FALSE], 1:NROW(x), FUN = format_number),
    apply(x[,2:3, drop=FALSE], MARGIN=1, FUN=format_ci),
    tapply(x[,4, drop=FALSE], 1:NROW(x), FUN = format_number),
    apply(x[,5:6, drop=FALSE], MARGIN=1, FUN=format_ci))
  
  colnames(values) <- c(
    colnames(x)[1], 
    sprintf("%s to %s", colnames(x)[2], colnames(x)[3]),
    colnames(x)[4], 
    sprintf("%s to %s", colnames(x)[5], colnames(x)[6]))
  
  rownames(values) <- rownames(x)
  
  return(values)
}