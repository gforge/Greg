library("testthat")
library("survival")

set.seed(10)
n <- 500
ds <- data.frame(
  ftime = rexp(n),
  fstatus = sample(0:1, size = n, replace = TRUE),
  y = rnorm(n = n),
  x1 = factor(sample(LETTERS[1:4], size = n, replace = TRUE)),
  x2 = rnorm(n, mean = 3, 2),
  x3 = factor(sample(letters[1:3], size = n, replace = TRUE)),
  boolean = sample(0L:1L, size = n, replace = TRUE),
  subsetting = factor(sample(c(TRUE, FALSE), size = n, replace = TRUE)))

library(rms)
dd <- with(ds, datadist(ds))
options(datadist="dd")

context("prExtractOutcomeFromModel")
test_that("Handling cox regression survival object", {
  fit <- coxph(Surv(ds$ftime, ds$fstatus == 1) ~ x1 + x2 + x3, data=ds)

  expect_equivalent(prExtractOutcomeFromModel(fit), 
                    ds$fstatus == 1)
  
}

test_that("Handling simple linear regression outcomes", {
  fit <- lm(ftime ~ x1 + x2 + x3, data=ds)
  
  expect_equivalent(prExtractOutcomeFromModel(fit), 
                    ds$ftime)
  
  fit <- lm(ds$ftime ~ x1 + x2 + x3, data=ds)
  
  expect_equivalent(prExtractOutcomeFromModel(fit), 
                    ds$ftime)

}
