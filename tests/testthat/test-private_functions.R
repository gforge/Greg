library(testthat)

context("prGetModelVariables")
test_that("Check how well variables are identified and stratification, etc are removed", {
  set.seed(10)
  n <- 500
  ds <- data.frame(
    ftime = rexp(n),
    fstatus = sample(0:1, size = n, replace = TRUE),
    a = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)),
    aa = rnorm(n, mean = 3, 2),
    aaa = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)))

  fit <- lm(ftime ~ ., data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    colnames(ds)[-1])
  
  fit <- lm(ftime ~ a, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    "a")
  
  fit <- lm(ftime ~ a + aa, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a", "aa"))
  
  fit <- lm(ftime ~ a * aa, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a", "aa"))
  
  fit <- lm(ftime ~ a * aa, data=ds)
  expect_equivalent(length(prGetModelVariables(fit, 
                                               remove_interaction_vars = TRUE)),
                    0)

  fit <- lm(ftime ~ a * aa + aaa, data=ds)
  expect_equivalent(length(prGetModelVariables(fit, 
                                               remove_interaction_vars = TRUE)),
                    1)

  fit <- lm(ftime ~ a : aa, data=ds)
  expect_equivalent(length(prGetModelVariables(fit)),
                    0)
  
  fit <- lm(ftime ~ a : aa + a, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a"))
  
  fit <- lm(ftime ~ offset(aa) + a, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a"))
  
  fit <- glm(ftime ~ I(aa*2) + a, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a"))
  
  fit <- glm(ftime ~ I(aa*2) + a + aa:a, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a"))
  
  fit <- Glm(fstatus ~ I(aa*2) + a + aa:a, data=ds, family=binomial)
  expect_equivalent(prGetModelVariables(fit),
                    c("a"))

  library(nlme)
  fit <- lme( distance ~ age , data=Orthodont)
  expect_equivalent(prGetModelVariables(fit),
                    c("age"))
  fit <- lme( distance ~ age +Sex, data=Orthodont, random=~1)
  expect_equivalent(prGetModelVariables(fit),
                    c("age", "Sex"))
  
  fit <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
              data = Loblolly,
              fixed = Asym + R0 + lrc ~ 1,
              random = Asym ~ 1,
              start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
  expect_equivalent(prGetModelVariables(fit),
                    c("Asym","R0","lrc"))
  
  library(rms)
  ddist <- datadist(ds)
  options(datadist = "ddist")
  # Bug:
#   fit <- ols(ftime ~ ., data=ds)
#   expect_equivalent(prGetModelVariables(fit),
#                     colnames(ds)[-1])

  
  fit <- ols(ftime ~ a, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    "a")
  
  fit <- ols(ftime ~ a + aa, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a", "aa"))
  
  fit <- ols(ftime ~ a * aa, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a", "aa"))
  
  fit <- ols(ftime ~ a * aa + aaa, data=ds)
  expect_equivalent(length(prGetModelVariables(fit, 
                                               remove_interaction_vars = TRUE)),
                    1)

#Bug:
#   fit <- ols(ftime ~ a : aa, data=ds)
#   expect_equivalent(length(prGetModelVariables(fit)),
#                     0)
  
#   fit <- ols(ftime ~ a : aa + a, data=ds)
#   expect_equivalent(prGetModelVariables(fit),
#                     c("a"))
  
  fit <- ols(ftime ~ offset(aa) + a, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a"))
  
  fit <- Glm(ftime ~ I(aa*2) + a, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a"))

#Bug:  
#   fit <- Glm(ftime ~ I(aa*2) + a + aa:a, data=ds)
#   expect_equivalent(prGetModelVariables(fit),
#                     c("a"))
  

  fit <- cph(Surv(ftime, fstatus) ~ a + aa + aaa, data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a", "aa", "aaa"))

  fit <- cph(Surv(ftime, fstatus) ~ a + aa + strat(aaa), data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a", "aa"))

  fit <- cph(Surv(ftime, fstatus) ~ a + I(aa^2) + strat(aaa), data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a"))
  
  fit <- coxph(Surv(ftime, fstatus) ~ a + I(aa^2) + strata(aaa), data=ds)
  expect_equivalent(prGetModelVariables(fit),
                    c("a"))
})

context("prMapVariable2Name")
test_that("Check how the mapping of rows work", {
  set.seed(10)
  n <- 500
  ds <- data.frame(
    y = rnorm(n),
    a = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)),
    aa = rnorm(n, mean = 3, 2),
    aaa = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)))
  
  fit <- lm(y~., data=ds)
  expect_error(prMapVariable2Name(prGetModelVariables(fit),
                                  names(coef(fit)),
                                  data=ds),
               info="Impossible to resolve the true relationship")
  
  ds <- data.frame(
    y = rnorm(n),
    a = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)),
    aa = rnorm(n, mean = 3, 2),
    baa = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)))
  fit <- lm(y~., data=ds)
  out <- prMapVariable2Name(prGetModelVariables(fit),
                            names(coef(fit)),
                            data=ds)
  
  expect_equivalent(out$a$location, 2:3)
  expect_equivalent(out$aa$location, 4)
  expect_equivalent(out$baa$location, 5:6)
  
  ds <- data.frame(
    y = rnorm(n),
    a = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)),
    aaa = rnorm(n, mean = 3, 2),
    baa = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)))
  fit <- lm(y~., data=ds)

  expect_error(prMapVariable2Name(prGetModelVariables(fit),
                                  names(coef(fit)),
                                  data=ds))

  ds <- data.frame(
    y = rnorm(n),
    a = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)),
    ada = rnorm(n, mean = 3, 2),
    aal = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)))
  fit <- lm(y~a*ada+aal, data=ds)
  prMapVariable2Name(prGetModelVariables(fit, remove_interaction_vars = TRUE),
                     names(coef(fit)),
                     data=ds)
  
  fit <- lm(y~a+ada+aal, data=ds)
  out <- prMapVariable2Name(prGetModelVariables(fit, 
                                                add_intercept = TRUE,
                                                remove_interaction_vars = TRUE),
                     names(coef(fit)),
                     data=ds)
  expect_equal(out$`(Intercept)`$location, 1)
  expect_equal(out$`a`$location, 2:3)
  expect_equal(out$`ada`$location, 4)
  expect_equal(out$`aal`$location, 5:6)
  
  fit <- ols(y~a+ada+aal, data=ds)
  out <- prMapVariable2Name(prGetModelVariables(fit, 
                                                add_intercept = TRUE,
                                                remove_interaction_vars = TRUE),
                            names(coef(fit)),
                            data=ds)
  expect_equal(out$`Intercept`$location, 1)
  expect_equal(out$`a`$location, 2:3)
  expect_equal(out$`ada`$location, 4)
  expect_equal(out$`aal`$location, 5:6)
  
  ds <- data.frame(
    ftime = rexp(n),
    fstatus = sample(0:1, size = n, replace = TRUE),
    a = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)),
    aa = rnorm(n, mean = 3, 2),
    aaa = factor(sample(c("a", "aa", "aaa"), size = n, replace = TRUE)))
  
  fit <- cph(Surv(ftime, fstatus)~a+aa+aaa, data=ds)
  out  <- prMapVariable2Name(prGetModelVariables(fit, 
                                                 add_intercept = TRUE,
                                                 remove_interaction_vars = TRUE),
                             names(coef(fit)),
                             data=ds)
  expect_equivalent(length(out), 3)
  expect_equal(out$`a`$location, 1:2)
  expect_equal(out$`aa`$location, 3)
  expect_equal(out$`aaa`$location, 4:5)
  
})