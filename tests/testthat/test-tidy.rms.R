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

test_that("Cox model is equal to pure coxph", {
  cph_fit <- cph(Surv(ftime, fstatus) ~ x1 + x_bool_fact + x_multi_fact + x_good_predictor, data = cov)
  coxph_fit <- formula(cph_fit) |> coxph(data = cov)
  expect_equal(tidy(cph_fit) |> select(estimate, std.error) |> as.matrix(),
               tidy(coxph_fit) |> select(estimate, std.error) |> as.matrix(),
               tolerance = 1e-4)
})

test_that("Logistic model is equal to pure glm", {
  lrm_fit <- lrm(fstatus ~ x1 + x_bool_fact + x_multi_fact, data = cov)
  glm_fit <- formula(lrm_fit) |> glm(data = cov, family = binomial())
  expect_equal(tidy(lrm_fit) |> select(estimate, std.error) |> as.matrix(),
               tidy(glm_fit) |> select(estimate, std.error) |> as.matrix(),
               tolerance = 1e-4)
})

test_that("Logistic model is equal to pure glm", {
  Glm_fit <- Glm(x1 ~ x_bool_fact + x_multi_fact, data = cov)
  glm_fit <- formula(Glm_fit) |> glm(data = cov)
  expect_equal(tidy(Glm_fit) |> select(estimate, std.error) |> as.matrix(),
               tidy(glm_fit) |> select(estimate, std.error) |> as.matrix(),
               tolerance = 1e-4)
})

