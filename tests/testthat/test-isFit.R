library("testthat")
context("isFit")

library("survival")
library("rms")

test_that("Check isFitCoxPH works", {
  set.seed(10)
  n <- 500
  ds <<- data.frame(
    ftime = rexp(n),
    fstatus = sample(0:1, size = n, replace = TRUE),
    y = rnorm(n = n),
    x1 = factor(sample(LETTERS[1:4], size = n, replace = TRUE)),
    x2 = rnorm(n, mean = 3, 2),
    x3 = factor(sample(letters[1:3], size = n, replace = TRUE))
  )

  dd <<- datadist(ds)
  options(datadist = "dd")

  fit1 <- coxph(Surv(ds$ftime, ds$fstatus == 1) ~ x1 + x2 + x3, data = ds)

  fit_cox <- coxph(Surv(ds$ftime, ds$fstatus == 1) ~ x1 + x2 + x3, data = ds)
  fit_cph <- cph(Surv(ds$ftime, ds$fstatus == 1) ~ x1 + x2 + x3, data = ds)

  fit_logistic <- glm(fstatus ~ x1 + x2 + x3, data = ds, family = binomial)
  fit_lrm <- lrm(fstatus ~ x1 + x2 + x3, data = ds)

  expect_true(isFitCoxPH(fit_cox))

  expect_true(isFitCoxPH(fit_cph))

  expect_false(isFitCoxPH(fit_logistic))

  expect_false(isFitCoxPH(fit_lrm))
})


test_that("Check isFitLogit works", {
  set.seed(10)
  n <- 500
  ds <- data.frame(
    ftime = rexp(n),
    fstatus = sample(0:1, size = n, replace = TRUE),
    y = rnorm(n = n),
    x1 = factor(sample(LETTERS[1:4], size = n, replace = TRUE)),
    x2 = rnorm(n, mean = 3, 2),
    x3 = factor(sample(letters[1:3], size = n, replace = TRUE))
  )

  dd <<- datadist(ds)
  options(datadist = "dd")

  fit1 <- coxph(Surv(ftime, fstatus == 1) ~ x1 + x2 + x3, data = ds)

  fit_cox <- coxph(Surv(ftime, fstatus == 1) ~ x1 + x2 + x3, data = ds)
  fit_cph <- cph(Surv(ftime, fstatus == 1) ~ x1 + x2 + x3, data = ds)

  fit_logistic <- glm(fstatus ~ x1 + x2 + x3, data = ds, family = binomial)
  fit_lrm <- lrm(fstatus ~ x1 + x2 + x3, data = ds)

  expect_false(isFitLogit(fit_cox))

  expect_false(isFitLogit(fit_cph))

  expect_true(isFitLogit(fit_logistic))

  expect_true(isFitLogit(fit_lrm))
})
