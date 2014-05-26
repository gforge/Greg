library("testthat")
context("getCrudeAndAdjustedModelData")

library("survival")
# Just a simple simulation
set.seed(10)
ds <- data.frame(
  ftime = rexp(200),
  fstatus = sample(0:1,200,replace=TRUE),
  x1 = runif(200),
  x2 = runif(200),
  x3 = factor(sample(LETTERS[1:3], size=200, replace=TRUE)))

s <- Surv(ds$ftime, ds$fstatus == 1)
s_fit1 <- coxph(s ~ x1 + x2 + x3, data=ds)

# Check that it doesn't include the rcs() spline since this doesn't
# make sense 
s_fit2 <- coxph(s ~ x1 + ns(x2, 4) + strata(x3), data=ds)

test_that("Correct number of rows and columns", {
    data_matrix <- getCrudeAndAdjustedModelData(s_fit1)
    expect_that(NROW(data_matrix), equals(4))
    expect_that(NCOL(data_matrix), equals(6))

    data_matrix <- getCrudeAndAdjustedModelData(s_fit2)
    expect_that(NROW(data_matrix), equals(1))
    expect_that(NCOL(data_matrix), equals(6))
})


test_that("Correct row info", {
  d1 <- getCrudeAndAdjustedModelData(s_fit1)
  expect_equivalent(d1[,5:6],
                    exp(confint(s_fit1)))

  expect_equivalent(exp(confint(update(s_fit1, .~x1))),
                    d1[1,2:3, drop=FALSE])
  
  expect_equivalent(exp(confint(update(s_fit1, .~x2))),
                    d1[2,2:3, drop=FALSE])
  
  expect_equivalent(exp(confint(update(s_fit1, .~x3))),
                    d1[3:4,2:3, drop=FALSE])
  
})


dd <- datadist(ds)
options(datadist="dd")
lbin_fit1 <- Glm(fstatus ~ x1 + x2 + x3, data=ds, family=binomial)
lbin_fit2 <- glm(fstatus ~ x1 + x2 + x3, data=ds, family=binomial)

exp(confint(lbin_fit2))
test_that("Check log reg. model",{
  # Should not return the intercept for the Glm
  d1 <- getCrudeAndAdjustedModelData(lbin_fit1)
  expect_equal(nrow(d1), 4)
  
  expect_equivalent(d1[,3],
                    exp(summary(lbin_fit1)[, c("Effect")]))
})