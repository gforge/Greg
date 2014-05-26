library("testthat")
context("isFit")

library("survival")

s <- Surv(ds$ftime, ds$fstatus == 1)
fit1 <- coxph(s ~ x1 + x2 + x3, data=ds)

s <- Surv(ds$ftime, ds$fstatus == 1)
fit_cox <- coxph(s ~ x1 + x2 + x3, data=ds)
fit_cph <- cph(s ~ x1 + x2 + x3, data=ds)

fit_logistic <- glm(fstatus ~ x1 + x2 + x3, data=ds, family=binomial)
fit_lrm <- lrm(fstatus ~ x1 + x2 + x3, data=ds)

test_that("Cox regression isFit works", {
  expect_true(isFitCoxPH(fit_cox))
  
  expect_true(isFitCoxPH(fit_cph))
  
  expect_false(isFitCoxPH(fit_logistic))

  expect_false(isFitCoxPH(fit_lrm))
})


test_that("Logit regression isFit works", {
  expect_false(isFitLogit(fit_cox))
  
  expect_false(isFitLogit(fit_cph))
  
  expect_true(isFitLogit(fit_logistic))
  
  expect_true(isFitLogit(fit_lrm))
})
