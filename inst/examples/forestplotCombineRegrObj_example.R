org.par <- par("ask" = TRUE)

# simulated data to test
library(tidyverse)
set.seed(10)
cov <- tibble(ftime = rexp(200),
              fstatus = sample(0:1, 200, replace = TRUE),
              x1 = runif(200),
              x2 = runif(200),
              x3 = runif(200)) |> 
  # Add some column labels
  Gmisc::set_column_labels(x1 = "First variable",
                           x2 = "Second variable")

library(rms)
ddist <- datadist(cov)
options(datadist = "ddist")

fit1 <- cph(Surv(ftime, fstatus) ~ x1 + x2, data = cov)
fit2 <- cph(Surv(ftime, fstatus) ~ x1 + x3, data = cov)

list(`First model` = fit1, 
     `Second model` = fit2) |> 
  forestplotCombineRegrObj(variablesOfInterest.regexp = "(x2|x3)")

# How to add expressions to the plot label
list(fit1, fit2) |> 
  forestplotCombineRegrObj(variablesOfInterest.regexp = "(x2|x3)",
                           reference.names = c("First model", "Second model"),
                           post_process_data = \(data) {
                             data$column_label[4] <- c(rlang::expr(expression(Fever >= 38.5)))
                             return(data)
                           })

par(org.par)