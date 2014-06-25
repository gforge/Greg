library("testthat")
library("survival")

set.seed(10)
n <- 500
ds <- data.frame(
  ftime = rexp(n),
  fstatus = sample(0:1, size = n, replace = TRUE),
  x1 = factor(sample(LETTERS[1:4], size = n, replace = TRUE)),
  x2 = rnorm(n, mean = 3, 2),
  x3 = factor(sample(letters[1:3], size = n, replace = TRUE)),
  boolean = sample(0L:1L, size = n, replace = TRUE),
  subsetting = factor(sample(c(TRUE, FALSE), size = n, replace = TRUE)))

context("getCrudeAndAdjustedModelData - coxph")
test_that("Correct number of rows and columns", {
  fit1 <- coxph(Surv(ds$ftime, ds$fstatus == 1) ~ x1 + x2 + x3, data=ds)
  
  # Check that it doesn't include the rcs() spline since this doesn't
  # make sense 
  fit2 <- coxph(Surv(ds$ftime, ds$fstatus == 1) ~ x1 + ns(x2, 4) + strata(x3), data=ds)
  
  data_matrix <- getCrudeAndAdjustedModelData(fit1)
  expect_that(NROW(data_matrix), equals(3 + 1 + 2))
  expect_that(NCOL(data_matrix), equals(6))
  
  data_matrix <- getCrudeAndAdjustedModelData(fit2)
  expect_that(NROW(data_matrix), equals(3 + 0 + 0))
  expect_that(NCOL(data_matrix), equals(6))
})

test_that("Same order of rows and matching results", {
  fit1 <- coxph(Surv(ds$ftime, ds$fstatus == 1) ~ x1 + x2 + x3, data=ds)
  data_matrix <- getCrudeAndAdjustedModelData(fit1)
  expect_equal(rownames(data_matrix), names(coef(fit1)))
  expect_equal(data_matrix[,"Adjusted"], exp(coef(fit1)))
  expect_equal(data_matrix[,tail(1:ncol(data_matrix), 2)], exp(confint(fit1)))
  
  unadjusted_fit <- coxph(Surv(ds$ftime, ds$fstatus == 1) ~ x2, data=ds)
  expect_true(data_matrix["x2","Crude"] == exp(coef(unadjusted_fit)))
  expect_true(all(data_matrix["x2",2:3] == exp(confint(unadjusted_fit))))
})

test_that("Check subsetting", {
  fit1 <- coxph(Surv(ds$ftime, ds$fstatus == 1) ~ x1 + x2 + x3, data=ds, 
                subset=subsetting == TRUE)
  data_matrix <- getCrudeAndAdjustedModelData(fit1)
  expect_equal(rownames(data_matrix), names(coef(fit1)))
  expect_equal(data_matrix[,"Adjusted"], exp(coef(fit1)))
  expect_equal(data_matrix[,tail(1:ncol(data_matrix), 2)], exp(confint(fit1)))
  
  unadjusted_fit <- update(fit1, .~x2)
  expect_true(data_matrix["x2","Crude"] == exp(coef(unadjusted_fit)))
  expect_true(all(data_matrix["x2",2:3] == exp(confint(unadjusted_fit))))

  fit1 <- with(ds, coxph(Surv(ftime, fstatus == 1) ~ x1 + x2 + x3,  
                         subset=subsetting == TRUE))
  data_matrix <- getCrudeAndAdjustedModelData(fit1)
  expect_equal(rownames(data_matrix), names(coef(fit1)))
  expect_equal(data_matrix[,"Adjusted"], exp(coef(fit1)))
  expect_equal(data_matrix[,tail(1:ncol(data_matrix), 2)], exp(confint(fit1)))
  
  unadjusted_fit <- update(fit1, .~x2)
  expect_true(data_matrix["x2","Crude"] == exp(coef(unadjusted_fit)))
  expect_true(all(data_matrix["x2",2:3] == exp(confint(unadjusted_fit))))

  unadjusted_fit <- update(fit1, .~x2, subset =  subsetting == FALSE)
  expect_false(data_matrix["x2","Crude"] == exp(coef(unadjusted_fit)))
})

context("getCrudeAndAdjustedModelData - cph")
test_that("Same order of rows", {
  dd <- with(ds, datadist(ds))
  options(datadist="dd")
  fit1 <- cph(Surv(ds$ftime, ds$fstatus == 1) ~ x1 + x2 + x3, data=ds)
  data_matrix <- getCrudeAndAdjustedModelData(fit1)
  expect_match(rownames(data_matrix)[1:(length(levels(ds$x1))-1)], "^x1")
  expect_match(rownames(data_matrix)[length(levels(ds$x1))], "^x2")
  expect_match(rownames(data_matrix)[(length(levels(ds$x1)) +1):nrow(data_matrix)], "^x3 - [bc]")
  
  unadjusted_fit <- cph(Surv(ds$ftime, ds$fstatus == 1) ~ x2, data=ds)
  expect_true(data_matrix["x2","Crude"] - exp(coef(unadjusted_fit)) < .Machine$double.eps)
  expect_true(all(data_matrix["x2",2:3] - exp(confint(unadjusted_fit)) < .Machine$double.eps))
  
})

test_that("A few bug tests",{
  dd <- with(ds, datadist(ds))
  options(datadist="dd")
  
  # Produced an  error with integer as input due to sort:
  #   Error in value.chk(at, i, factors[[jf]], 0, Limval) : 
  #     character value not allowed for variable boolean
  fit1 <- cph(Surv(ds$ftime, ds$fstatus == 1) ~ x1 + x2 + x3 + boolean, data=ds)
  expect_is(getCrudeAndAdjustedModelData(fit1), "matrix")
  
})
context("getCrudeAndAdjustedModelData - lm")
test_that("Same order of rows", {
  set.seed(200)
  ds$y <- rnorm(length(ds$x1))
  fit_lm <- lm(y ~ x1 + x2 + x3, data=ds) 
  
  data_matrix <- getCrudeAndAdjustedModelData(fit_lm)
  expect_match(rownames(data_matrix)[1], "^Intercept")
  expect_match(rownames(data_matrix)[2], "^x1")
  expect_match(rownames(data_matrix)[3], "^x2")
  expect_match(rownames(data_matrix)[4:5], "^x3.*[BC]")

  dd <- with(ds, datadist(x1, x2, x3, y))
  options(datadist="dd")
  
})

test_that("Correct values for rows - lm", {
  set.seed(200)
  ds$y <- rnorm(length(ds$x1))
  fit_lm <- lm(y ~ x1 + x2 + x3, data=ds) 
  
  data_matrix <- getCrudeAndAdjustedModelData(fit_lm)
  expect_true(all(data_matrix[,"Adjusted"] ==coef(fit_lm)))
  expect_true(all(data_matrix[,tail(1:ncol(data_matrix), 2)] ==confint(fit_lm)))
  
  fit_lm_ua <- lm(y ~ x3, data=ds)
  expect_true(all(data_matrix[grep("^x3", rownames(data_matrix)),
                              "Crude"] ==
                    coef(fit_lm_ua)[2:3]))
  expect_true(all(data_matrix[grep("^x3", rownames(data_matrix)),
                              2:3] ==
                    confint(fit_lm_ua)[2:3,]))
  
  fit_lm_ua <- lm(y ~ x1, data=ds)
  expect_true(all(data_matrix[grep("^x1", rownames(data_matrix)),
                              "Crude"] ==
                    coef(fit_lm_ua)[2]))
  expect_true(all(data_matrix[grep("^x1", rownames(data_matrix)),
                              2:3] ==
                    confint(fit_lm_ua)[2,]))
})


test_that("Correct values for rows - ols", {
  set.seed(200)
  ds$y <- rnorm(length(ds$x1))
  dd <- with(ds, datadist(x1, x2, x3, y))
  options(datadist="dd")
  
  fit_ols <- ols(y ~ x1 + x2 + x3, data=ds) 
  
  # The rms-package does not report the intercept in summary
  # and we therefore skip
  data_matrix <- getCrudeAndAdjustedModelData(fit_ols)
  expect_true(all(data_matrix[,"Adjusted"] == coef(fit_ols)[-1]))
  expect_true(all(data_matrix[,tail(1:ncol(data_matrix), 2)] ==confint(fit_ols)[-1,]))
  
  fit_ols_ua <- ols(y ~ x3, data=ds)
  expect_true(all(data_matrix[grep("^x3", rownames(data_matrix)),
                              "Crude"] ==
                    coef(fit_ols_ua)[2:3]))
  expect_true(all(data_matrix[grep("^x3", rownames(data_matrix)),
                              2:3] ==
                    confint(fit_ols_ua)[2:3,]))
  
  fit_ols_ua <- ols(y ~ x1, data=ds)
  expect_true(all(data_matrix[grep("^x1", rownames(data_matrix)),
                              "Crude"] ==
                    coef(fit_ols_ua)[2]))
  expect_true(all(data_matrix[grep("^x1", rownames(data_matrix)),
                              2:3] ==
                    confint(fit_ols_ua)[2,]))
})



test_that("Correct values for rows - glm", {
  set.seed(200)
  ds$y <- rnorm(length(ds$x1))
  fit_glm <- glm(y ~ x1 + x2 + x3, data=ds) 
  
  data_matrix <- getCrudeAndAdjustedModelData(fit_glm)
  expect_true(all(abs(data_matrix[,"Adjusted"] - 
                        coef(fit_glm)) < .Machine$double.eps))
  expect_true(all(abs(data_matrix[,tail(1:ncol(data_matrix), 2)] -
                        confint(fit_glm)) < .Machine$double.eps))
  
  fit_glm_ua <- glm(y ~ x3, data=ds)
  expect_true(all(abs(data_matrix[grep("^x3", rownames(data_matrix)), "Crude"] -
                        coef(fit_glm_ua)[2:3]) < .Machine$double.eps))
  expect_true(all(abs(data_matrix[grep("^x3", rownames(data_matrix)), 2:3] -
                        confint(fit_glm_ua)[2:3,]) < .Machine$double.eps ))
  
  fit_glm_ua <- glm(y ~ x1, data=ds)
  expect_true(all(abs(data_matrix[grep("^x1", rownames(data_matrix)), "Crude"] -
                        coef(fit_glm_ua)[2]) < .Machine$double.eps))
  expect_true(all(abs(data_matrix[grep("^x1", rownames(data_matrix)), 2:3] -
                    confint(fit_glm_ua)[2,]) < .Machine$double.eps))
})


