library("testthat")

library("survival")

context("getCrudeAndAdjustedModelData - coxph")
s <- Surv(ds$ftime, ds$fstatus == 1)
fit1 <- coxph(s ~ x1 + x2 + x3, data=ds)

# Check that it doesn't include the rcs() spline since this doesn't
# make sense 
fit2 <- coxph(s ~ x1 + ns(x2, 4) + strata(x3), data=ds)

test_that("Correct number of rows and columns", {
    data_matrix <- getCrudeAndAdjustedModelData(fit1)
    expect_that(NROW(data_matrix), equals(4))
    expect_that(NCOL(data_matrix), equals(6))
    
    data_matrix <- getCrudeAndAdjustedModelData(fit2)
    expect_that(NROW(data_matrix), equals(1))
    expect_that(NCOL(data_matrix), equals(6))
})


test_that("Same order of rows", {
  fit1 <- coxph(s ~ x1 + x2 + x3, data=ds)
  data_matrix <- getCrudeAndAdjustedModelData(fit1)
  expect_equal(rownames(data_matrix), names(coef(fit1)))
  expect_equal(data_matrix[,"Adjusted"], exp(coef(fit1)))
  expect_equal(data_matrix[,tail(1:ncol(data_matrix), 2)], exp(confint(fit1)))
  
  unadjusted_fit <- coxph(s ~ x2, data=ds)
  expect_true(data_matrix["x2","Crude"] == exp(coef(unadjusted_fit)))
  expect_true(all(data_matrix["x2",2:3] == exp(confint(unadjusted_fit))))
})

context("getCrudeAndAdjustedModelData - cph")
test_that("Same order of rows", {
  dd <- with(ds, datadist(x1, x2, x3, ftime, fstatus))
  options(datadist="dd")
  fit1 <- cph(s ~ x1 + x2 + x3, data=ds)
  data_matrix <- getCrudeAndAdjustedModelData(fit1)
  expect_match(rownames(data_matrix)[1], "^x1")
  expect_match(rownames(data_matrix)[2], "^x2")
  expect_match(rownames(data_matrix)[3:4], "^x3 - [BC]")
  
  unadjusted_fit <- cph(s ~ x2, data=ds)
  expect_true(data_matrix["x2","Crude"] == exp(coef(unadjusted_fit)))
  expect_true(all(data_matrix["x2",2:3] == exp(confint(unadjusted_fit))))
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
