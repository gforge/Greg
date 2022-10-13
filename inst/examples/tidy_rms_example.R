library(rms)
library(broom)
library(tidyverse)

set.seed(10)
cov <- tibble(x1 = runif(200)) |> 
  mutate(x_bool_fact = if_else(x1 > 0.5,
                               "Yes",
                               sample(c("Yes", "No"), size = n(), replace = TRUE)),
         x_multi_fact = sample(c("Strange", "Factor", "Names"), size = n(), replace = TRUE),
         ftime = rexp(n()),
         fstatus = sample(0:1, size = n(), replace = TRUE),
         x_good_predictor = fstatus * runif(n()))

ddist <- datadist(cov)
options(datadist = "ddist")

cph_fit <- cph(Surv(ftime, fstatus) ~ x1 + x_bool_fact + 
                 x_multi_fact + x_good_predictor, data = cov)
tidy(cph_fit)