utils::globalVariables(c("txt_estimate", "txt_conf.low", "txt_conf.high", "estimate", "term"))

#' @importFrom dplyr bind_rows mutate filter group_by case_when row_number if_else across
#' @importFrom tidyselect starts_with
#' @importFrom broom tidy
#' @importFrom purrr pluck
#' @importFrom tibble tibble
#' @importFrom stringr str_detect
#' @importFrom htmlTable txtRound
#' @importFrom glue glue
#' @importFrom stats model.frame
prForestPlotPrep <- function(regressions,
                             skip.variables,
                             add.empty_row,
                             boxsize,
                             postprocess_estimates.fn,
                             xlab,
                             exp,
                             rowname,
                             estimate.txt = xlab,
                             ci.txt,
                             ci.glue,
                             get_box_size = fpBoxSize,
                             digits,
                             ...) {
  if (is.null(estimate.txt)) {
    estimate.txt = xlab
  }
  header <- tibble(rowname = rowname, is_summary = TRUE)
  fit_data <- sapply(regressions,
                     FUN = function(regression, ...) 
                     {
                       estimates <- tidy(regression, ...)
                       
                       labels <- sapply(model.frame(regression), label)
                       estimates <- mutate(
                         estimates,
                         rowname = labels[term],
                         rowname = case_when(is.na(rowname) ~ term,
                                             rowname == '' ~ term,
                                             TRUE ~ rowname),
                         is_summary = FALSE,
                         across(.cols = c(estimate, starts_with("conf.")), 
                                txtRound, 
                                digits = digits, .names = "txt_{.col}"),
                         est_txt = if_else(row_number() == 1,
                                           estimate.txt,
                                           txt_estimate),
                         ci = if_else(row_number() == 1,
                                      ci.txt,
                                      glue(ci.glue,
                                           lower = txt_conf.low,
                                           higher = txt_conf.high) |> 
                                        as.character()))
                       bind_rows(header,
                                 estimates |> 
                                   filter(!str_detect(term, "Intercept")) |> 
                                   postprocess_estimates.fn())
                     },
                     simplify = FALSE,
                     conf.int = TRUE,
                     conf.level = 0.95,
                     exponentiate = exp) |> 
    bind_rows(.id = "Model") |> 
    mutate(model_no = as.numeric(factor(Model)))
  
  # Make the box smaller if there are many
  # models that share the same space
  if (missing(boxsize)) {
    boxsize <- .4 / length(regressions)
  }
  
  # if (length(regressions) == 1) {
  #   # To complex for most
  #   # col2 <- list(expression(plain(e)^beta));
  #   col2 <- list(estimate.txt)
  #   
  #   # TODO: should probably use options("digits") but
  #   # it defaults to 7 why this might be more annoying
  #   # than helpful
  #   # The first element is NA and should not be included
  #   # as it's HR instead
  #   for (coef in models_fit_fp_data[[1]][, "beta"][-1]) {
  #     col2 <- append(col2, ifelse(is.na(coef), "", sprintf(" %.2f ", coef)))
  #   }
  #   
  #   # The first is the names and the second the hazard rate
  #   rn <- list(col1, col2)
  # }
  
  args <- list(...)
  args$boxsize <- boxsize
  args$xlab <- xlab
  
  if (max(fit_data$model_no) > 1) {
    ret <- fit_data |> 
      group_by(Model)
    class_id <- "forestplotRegrObj.grouped"
  } else {
    ret <- fit_data
    class_id <- "forestplotRegrObj.single"
  }
  
  ret |> 
    structure(args = args,
              class = c(class_id, class(ret))) |> 
    forestplot()
}

utils::globalVariables(c("Model", "est_txt"))

#' @export
forestplot.forestplotRegrObj.grouped <- function(x, ...) {
  # Drop both forestplotRegrObj.grouped and grouped_df class
  class(x) <- class(x)[3:length(class(x))]
  args <- list(x = x,
               ...,
               align = c("l", "c", "c")) |> 
    append(attr(x, "args"))
  args <- args[!duplicated(names(args))]
  
  args$labeltext <- as_name(quote(rowname))
  args$mean <- as_name(quote(estimate))
  args$lower <- as_name(quote(conf.low))
  args$upper <- as_name(quote(conf.high))
  args$is.summary <- as_name(quote(is_summary))
  args$legend <- NULL
  
  do.call(forestplot, args)
}

#' @export
forestplot.forestplotRegrObj.single <- function(x, ...) {
  class(x) <- class(x)[2:length(class(x))]
  args <- list(x = tibble::tibble(x),
               ...,
               align = c("l", "c", "c")) |> 
    append(attr(x, "args"))
  args <- args[!duplicated(names(args))]
  
  args$labeltext <- c(quote(rowname), quote(est_txt), quote(ci)) |> sapply(as_name)
  args$mean <- as_name(quote(estimate))
  args$lower <- as_name(quote(conf.low))
  args$upper <- as_name(quote(conf.high))
  args$is.summary <- as_name(quote(is_summary))
  
  do.call(forestplot, args)
}

#' @export
#' @importFrom rlang as_name
print.forestplotRegrObj.grouped <- function(x, ...) {
  forestplot(x, ...) |> 
    print()
}

#' @export
#' @importFrom rlang as_name
print.forestplotRegrObj.single <- function(x, ...) {
  forestplot(x, ...) |> 
    print()
}
