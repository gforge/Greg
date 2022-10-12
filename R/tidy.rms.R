#' Tidy a(n) rms model object
#' 
#' Tidy summarizes information about the components of a model. A model component 
#' might be a single term in a regressions. Exactly what tidy considers to be a model 
#' component varies across models but is usually self-evident. If a model has several 
#' distinct types of components, you will need to specify which components to return.
#' 
#' This is a quick fix for addressing the lack of `rms`-compatibility with the `broom`
#' package, see [broom issue 30](https://github.com/tidymodels/broom/issues/30).
#'
#' @param x An rms model, e.g. [`rms::cph()`], [`rms::lrm()`]
#' @param .add_print_p_and_stat_values For estimating print values there is a workaround that
#'  relies on capturing output from the `print(x)` and is not considered safe.
#' @return A tibble::tibble() with columns:
#' - `term` The name of the regression term.
#' - `factor` The factor if the term is a character/factor term.
#' - `column_term` The full name as in the original input data
#' - `estimate`	The estimated value of the regression term.
#' - `conf.high`	Upper bound on the confidence interval for the estimate.c
#' - `conf.low`	Lower bound on the confidence interval for the estimate.
#' - `p.value` The two-sided p-value associated with the observed statistic.
#' - `statistic` The value of a statistic to use in a hypothesis that the regression term is non-zero.
#' - `std.error` The standard error of the regression term.
#'
#' @inheritParams broom::tidy.lm
#' @importFrom stringr str_replace
#' @importFrom broom tidy
#' @importFrom dplyr matches one_of
#' @export
#' @example inst/examples/tidy_rms_example.R
tidy.rms <- function(x,
                     conf.int = FALSE,
                     conf.level = 0.95,
                     exponentiate = FALSE,
                     ...,
                     .add_print_p_and_stat_values = getOption("Greg.tidy_add_p_and_stat_values", default = FALSE)) {
  base_data <- tibble(term = names(coef(x)),
                      estimate = coef(x),
                      std.error = sqrt(diag(vcov(x))))
  if (conf.int) {
    ci <- confint(x, level = conf.level)
    base_data$conf.low <- ci[,1]
    base_data$conf.high <- ci[,2]
  }
  
  ret <- base_data |> 
    convert_value_to_term_and_factor(model_fit = x)
  
  if (.add_print_p_and_stat_values) {
    print_tibble <- retrieve_rms_print_output_tibble(x) |> 
      dplyr::rename(p.value = starts_with("Pr("))
    
    ret <- dplyr::left_join(ret,
                            print_tibble |> dplyr::select(column_term, factor, statistic, `p.value`),
                            by = c("column_term", "factor"))
    
    if (nrow(ret) != nrow(base_data)) {
      stop("Failed to interpret rms-regression print output")
    }
  } else {
    ret <- mutate(ret,
                  p.value = NA_real_,
                  statistic = NA_real_)
  }
  
  
  if (exponentiate) {
    ret <- mutate(ret,
                  across(matches("^(estimate|conf\\.)"), exp))
  }
  
  if (conf.int) {
    return(ret |> 
             dplyr::select(term,
                           column_term,
                           factor,
                           estimate,
                           conf.low,
                           conf.high,
                           std.error,
                           `p.value`))
  }
  
  ret |> 
    dplyr::select(term,
                  column_term,
                  factor,
                  estimate,
                  std.error,
                  statistic,
                  `p.value`)
}

wrap_unsafe_function <- function(ns, name) 
{
  f = getExportedValue(ns, name)
  wrapper = function(...) eval.parent(`[[<-`(match.call(), 
                                             1L, f))
  formals(wrapper) = formals(f)
  wrapper
}

utils::globalVariables(c("term", 
                         "column_term",
                         "factor",
                         "estimate",
                         "conf.low",
                         "conf.high",
                         "std.error",
                         "p.value",
                         "value",
                         "statistic",
                         # tidyselect fix
                         "where"))

convert_value_to_term_and_factor <- function(data, model_fit) {
  mf <- model.frame(model_fit)
  mutate(data,
         column_term = if_else(str_detect(term, " * "),
                               term,
                               find_best_start_match(needle = term, haystack = colnames(mf) |> 
                                                       str_replace("[a-zA-Z]+\\(([^, ]+).+\\)", "\\1"))),
         column_label = purrr::map_chr(column_term, \(t) if_else(is.null(Hmisc::label(mf[[t]])), t, Hmisc::label(mf[[t]]))),
         # Add the intercept if any
         column_label = if_else(row_number() == 1 & is.na(column_term), term, column_term),
         factor = mapply(column_term = column_term,
                         needle = term,
                         FUN = function(needle, column_term) {
                           haystack <- levels(mf[[column_term]])
                           if (is.null(haystack) && is.character(mf[[column_term]])) {
                             haystack <- unique(mf[[column_term]])
                           }
                           find_best_end_match(needle = needle,
                                               haystack = haystack)
                         }),
         # Catch splines and add them as a factor option
         factor = case_when(!is.na(factor) ~ factor,
                            !is.na(column_term) & column_term != term ~ substring(term, nchar(column_term) + 1),
                            TRUE ~ NA_character_),
         across(where(is.character), str_trim))
}

build_column_label_from_column_term <- function(data, model_fit) {
  mf <- model.frame(model_fit)
  get_label <- function(varname) {
    label <- Hmisc::label(mf[[varname]])
    
    if (is.null(label) || label == "") {
      return(varname)
    }
    return(label)
  }

  mutate(data,
         column_label = purrr::map_chr(column_term, get_label),
         across(where(is.character), str_trim))
}

find_best_start_match <- function(needle, haystack) {
  if (length(needle) > 1) {
    return(sapply(needle, find_best_start_match, haystack = haystack))
  }
  
  matches <- sapply(haystack, function(element) { 
    if (element == substr(needle, 1, nchar(element))) {
      return(nchar(element))
    }
    
    return(-1)
  })
  
  if (!length(matches[matches > 0])) {
    return(NA_character_)
  }
  
  which.max(matches) |> names()
}

find_best_end_match <- function(needle, haystack) {
  if (is.null(haystack)) {
    return(NA_character_)
  }
  
  if (length(needle) > 1) {
    return(sapply(needle, find_best_end_match, haystack = haystack))
  }
  
  matches <- sapply(haystack, function(element) { 
    if (nchar(needle) < nchar(element)) {
      return(-1)
    }
    
    if (element == substring(needle, nchar(needle) - nchar(element) + 1)) {
      return(nchar(element))
    }
    
    return(-1)
  })
  
  if (!length(matches[matches > 0])) {
    return(NA_character_)
  }
  
  which.max(matches) |> names()
}

# Not used - parts borrowed from https://stackoverflow.com/questions/47724189/extract-all-model-statistics-from-rms-fits
#' @importFrom dplyr everything
#' @importFrom tidyr separate
retrieve_rms_print_output_tibble <- function(x, precision = getOption("Greg.rms_print_value_precision", default = 6e1)) {
  # remember old number formatting function
  # (which would round and transforms p-values to formats like "<0.01")
  old_format_np = rms::formatNP
  
  rcheckFix <- wrap_unsafe_function(ns = "utils", name = "assignInNamespace")
  # substitute it with a function which will print out as many digits as we want
  rcheckFix("formatNP", function(x, ...) formatC(x, format = "f", digits = precision), "rms")
  # remember old width setting
  old_width = options('width')$width
  # substitute it with a setting making sure the table will not wrap
  options(width = old_width + 4 * precision)
  
  on.exit({
    # restore original settings
    options(width = old_width)
    rcheckFix("formatNP", old_format_np, "rms")
  })
  
  # actually print the data and capture it
  print_out = capture.output(print(x)) |> 
    str_trim() |> 
    purrr::discard(\(x) x == "")
  
  
  
  header <- grep("^Coef\\s+S\\.E\\.", print_out)
  header_elements <- c("estimates", "std.error", "statistic", "p.value")
  
  name_and_num_pattern <- glue("(.+)\\s+({num}\\s+{num}\\s+{num}\\s+{num})$",
                               num = "[-]{0,1}\\d[0-9.]{2,}")
  print_out[(header + 1):length(print_out)] |> 
    dplyr::as_tibble() |> 
    dplyr::mutate(term = str_replace(value, name_and_num_pattern, "\\1"),
                  value = str_replace(value, name_and_num_pattern, "\\2"),
                  across(c(term, value), str_trim)) |> 
    convert_value_to_term_and_factor(model_fit = x) |> 
    dplyr::select(everything() & !value, value) |> 
    tidyr::separate(col = value,
                    sep = "[ ]+",
                    into = header_elements) |> 
    dplyr::mutate(across(one_of(header_elements), as.numeric))
}