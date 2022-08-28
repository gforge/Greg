
#' @importFrom dplyr bind_rows mutate filter
#' @importFrom broom tidy
#' @importFrom purrr pluck
prForestPlotPrep <- function(regressions,
                             skip.variables,
                             add.empty_row,
                             box.default.size,
                             postprocess_estimates.fn = \(x) x,
                             xlab,
                             exp,
                             estimate.txt = xlab,
                             get_box_size = fpBoxSize,
                             ...) {
  fit_data <- sapply(regressions,
                     FUN = function(regression, ...) {
                       estimates <- tidy(regression, ...)
                       labels <- sapply(model.frame(regression), label)
                       bind_rows(tibble(rowname = estimate.txt),
                                 estimates |> 
                                   mutate(rowname = labels[term],
                                          rowname = case_when(is.na(rowname) ~ term,
                                                              rowname == '' ~ term,
                                                              TRUE ~ rowname)) |> 
                                   filter(!str_detect(term, "Intercept")) |> 
                                   postprocess_estimates.fn())
                     },
                     simplify = FALSE,
                     conf.int = TRUE,
                     conf.level = 0.95,
                     exponentiate = exp) |> 
    bind_rows(.id = "Model") |> 
    mutate(model_no = as.numeric(factor(Model))) |> 
    group_by(Model)

  # The top is the header and should be bold
  is.summary <- c(TRUE, rep(FALSE, fit_data |> filter(model_no == 1) |> nrow() - 1))
  
  # Make the box smaller if there are many
  # models that share the same space
  if (missing(box.default.size)) {
    box.default.size <- .75 / length(regressions)
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
  
  fit_data |> 
    forestplot(labeltext = rowname,
               mean = estimate,
               lower = conf.low,
               upper = conf.high,
               boxsize = box.default.size,
               xlab = xlab,
               is.summary = is.summary,
               ...)
}