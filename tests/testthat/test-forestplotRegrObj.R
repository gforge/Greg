library("testthat")

# simulated data to test
set.seed(1000)
n <- 1000
cov <- data.frame(
  ftime = rexp(n),
  fstatus = sample(0:2, n, replace = TRUE),
  x1 = runif(n),
  x2 = runif(n),
  x3 = runif(n)
)

library(rms)
dd <<- datadist(cov)
options(datadist = "dd")

test_that("Simple linear regression", {
  fit <- mtcars |> 
    set_column_labels(cyl = "Number of cylinders",
                      hp = "Gross horsepower") |> 
    (\(x) lm(mpg ~ cyl + disp + hp, data = x))()

  expect_s3_class(forestplotRegrObj(regr.obj = fit),
                  "gforge_forestplot")
})

test_that("Simple linear regression", {
  fit <- mtcars |> 
    set_column_labels(cyl = "Number of cylinders",
                      hp = "Gross horsepower") |> 
    (\(x) glm(mpg ~ cyl + disp + hp, data = x, family = gaussian()))()
  
  expect_s3_class(forestplotRegrObj(regr.obj = fit),
                  "gforge_forestplot")
})

test_that("Simple linear regression", {
  data <- mtcars |> 
    set_column_labels(cyl = "Number of cylinders",
                      hp = "Gross horsepower")
  fit1 <- lm(mpg ~ cyl + disp + hp, data = data)
  fit2 <- lm(mpg ~ cyl + disp + gear, data = data)
  
  expect_s3_class(forestplotRegrObj(regr.obj = list(fit1, fit2)),
                  "gforge_forestplot")
})

test_that("Basic test for coverage for forestplotRegrObj", {
  fit1 <- cph(Surv(ftime, fstatus == 1) ~ x1 + x2 + x3, data = cov)
  fit2 <- cph(Surv(ftime, fstatus == 2) ~ x1 + x2 + x3, data = cov)
  
  ret <- forestplotRegrObj(
    regr.obj = list(fit1, fit2),
    col = fpColors(box = c("darkblue", "darkred")),
    postprocess_estimates.fn = \(x) filter(x, str_detect(column_term, "(x2|x3)")),
    legend = c("First model", "Second model"),
    legend_args = fpLegend(title = "Models"))
  expect_equal(ret$is.summary, rep(c(TRUE, FALSE, FALSE), 2))
})
