org.par <- par("ask" = TRUE)

library(tidyverse)
# simulated data to test
set.seed(102)
cov <- tibble(ftime = rexp(200)) |> 
  mutate(x1 = runif(n()),
         x2 = runif(n()),
         x3 = runif(n()),
         fstatus1 = if_else(x1 * 1 + 
                              x2 * 0.2 + 
                              x3 * 0.5 + 
                              runif(n()) * 0.5 > 1, 
                            1, 0),
         fstatus2 = if_else(x1 * 0.2 + 
                              x2 * 0.5 + 
                              x3 * 0.1 + 
                              runif(n()) * 2 > 1, 
                            1, 0)) |> 
  # Add some column labels
  Gmisc::set_column_labels(x1 = "First variable",
                           x2 = "Second variable")

library(rms)
dd <- datadist(cov)
options(datadist = "dd")

fit1 <- cph(Surv(ftime, fstatus1 == 1) ~ x1 + x2 + x3, data = cov)
fit2 <- cph(Surv(ftime, fstatus2 == 1) ~ x1 + x2 + x3, data = cov)

forestplotRegrObj(regr.obj = fit1)

list("Frist model" = fit1, "Second model"  = fit2) |> 
  forestplotRegrObj(legend_args = fpLegend(title = "Type of regression"),
                    variablesOfInterest.regexp = "(x2|x3)",
                    col = fpColors(box = c("darkblue", "darkred")))

forestplotRegrObj(
  regr.obj = list(fit1, fit2),
  legend = c("First model", "Second model"),
  legend_args = fpLegend(title = "Models"),
  rowname.fn = modifyNameFunction, new_page = TRUE
)

par(org.par)