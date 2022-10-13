#' Get model data
#'
#' A helper function for \code{\link{forestplotCombineRegrObj}()}. Extracts
#' the data from the regression model fits and returns a \code{list}
#' with model data gathered by the function [broom::tidy()]
#'
#' @inheritParams forestplotCombineRegrObj
#' @example inst/examples/forestplotCombineRegrObj_example.R
#' @keywords internal
#' @importFrom broom tidy
getModelData4Forestplot <- function(regr.obj,
                                    exp = TRUE,
                                    variablesOfInterest.regexp = NULL,
                                    add_first_as_ref = FALSE) {
  if (missing(variablesOfInterest.regexp)) {
    variablesOfInterest.regexp <- NULL
  }
  
  if (is.null(names(regr.obj))) {
    names(regr.obj) <- paste("Model", 1:length(regr.obj))
  }
  
  regr.obj |> 
    purrr::map(\(model) broom::tidy(model,
                                    conf.int = TRUE,
                                    conf.level = 0.95,
                                    exponentiate = exp) |> 
                 structure(source_model = model)) |> 
    purrr::map(function(data) {
      if (!is.null(data$column_term)) {
        return(data)
      }

      convert_value_to_term_and_factor(data, model_fit = attr(data, "source_model")) |> 
        structure(source_model = attr(data, "source_model"))
    }) |> 
    purrr::map(\(data) build_column_label_from_column_term(data, model_fit = attr(data, "source_model")) |> 
             structure(source_model = attr(data, "source_model"))) |> 
    purrr::map(function(data) {
      if (is.null(variablesOfInterest.regexp)) {
        return(data)
      }
      
      data[grep(variablesOfInterest.regexp, data$column_term), , drop = FALSE] |> 
        structure(source_model = attr(data, "source_model"))
    }) |> 
    purrr::map(function(data) {
      if (!add_first_as_ref) {
        return(data)
      }
      
      mf <- model.frame(attr(data, "source_model"))
      for (term in unique(data$column_term)) {
        data <- extend_with_reference_value(model_tidy_data = data,
                                            column_data = mf[[term]],
                                            term = term,
                                            exp = exp)
      }
      return(data)
    }) |> 
    purrr::map(function(data) {
      bind_rows(tibble(column_label = NA_character_) |> 
                  mutate(is.summary = TRUE),
                data |> 
                  mutate(is.summary = FALSE))
    }) |> 
    dplyr::bind_rows(.id = "model_id") |> 
    mutate(column_label = if_else(is.na(column_label), model_id, column_label))
}

extend_with_reference_value <- function(model_tidy_data, 
                                        term,
                                        column_data, 
                                        exp) {
  if (!is.character(column_data) && !is.factor(column_data)) {
    return(model_tidy_data)
  }
  
  index <- which(model_tidy_data$column_term == term)
  if (length(index) == 0) {
    return(model_tidy_data)
  }

  reference_value <- get_reference_value(column_data = column_data,
                                         model_tidy_data = model_tidy_data,
                                         model_term = term)
  tibble::add_row(model_tidy_data,
                  term = term,
                  column_term = term,
                  factor = reference_value,
                  estimate = if_else(exp, 1, 0),
                  conf.low = if_else(exp, 1, 0),
                  conf.high = if_else(exp, 1, 0),
                  .before = index[1])
}

get_reference_value <- function(column_data, model_tidy_data, model_term) {
  if (is.factor(column_data)) {
    return(levels(column_data)[1])
  }
  
  active_factors <- model_tidy_data |> 
    dplyr::filter(column_term == model_term) |> 
    purrr::pluck("factor") |> 
    unique()
  available_levels <- unique(column_data)
  reference_value <- available_levels[!(available_levels %in% active_factors)]
  stopifnot(length(reference_value) == 1)
  
  return(reference_value)
}