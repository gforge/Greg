context("printCrudeAndAdjustedModel")
test_that("Check position of reference", {
  set.seed(10)
  n <- 500
  ds <- data.frame(
    ftime = rexp(n),
    fstatus = sample(0:1, size = n, replace = TRUE),
    x = factor(sample(LETTERS[1:4], size = n, replace = TRUE)),
    same_label = factor(sample(c("Yes", "No"), size = n, replace = TRUE)),
    same_labell = factor(sample(c("Yes", "No"), size = n, replace = TRUE)),
    same_labelll = factor(sample(c("Yes", "No"), size = n, replace = TRUE)),
    boolean = sample(c(TRUE, FALSE), size = n, replace = TRUE),
    subsetting = factor(sample(c(TRUE, FALSE), size = n, replace = TRUE)))
  
  library(rms)
  dd <<- datadist(ds)
  options(datadist="dd")

  fit <- cph(Surv(ftime, fstatus == 1) ~ x + boolean, data=ds)
  
  a <- printCrudeAndAdjustedModel(fit, add_references=TRUE)
  expect_match(a[1,2], "ref")
  
  expect_equivalent(attr(a, "rgroup"),
                    c("x", ""), info="Rgroup test")
  
  tmp <- getCrudeAndAdjustedModelData(fit)
  b <- printCrudeAndAdjustedModel(tmp, add_references=TRUE)
  
  expect_equivalent(a, b)
  # Getting the name wrong should not change the reference
  a <- printCrudeAndAdjustedModel(fit, add_references=TRUE, 
                                  add_references_pos=list(a=3))
  expect_match(a[1,2], "ref")
  
  # This should move the reference
  a <- printCrudeAndAdjustedModel(fit, add_references=TRUE, add_references_pos=list(x=2))
  expect_match(a[2,2], "ref")
  
  # Should end up at first position if referenced outside
  expect_warning(a <- printCrudeAndAdjustedModel(fit, add_references=TRUE, add_references_pos=list(x=5)))
  expect_match(a[1,2], "ref")
  
  # Bug with the same label occurring miultiple times
  complx_fit <- update(fit, .~.+
                         same_label +
                         same_labell +
                         same_labelll)
  a <- printCrudeAndAdjustedModel(complx_fit, 
                                  add_references=TRUE, desc_column=TRUE)
  expect_equal(nrow(a), 11)
})

test_that("Variable select",{
  set.seed(10)
  n <- 500
  ds <- data.frame(
    y = sample(0:1, size = n, replace = TRUE),
    x1 = factor(sample(LETTERS[1:4], size = n, replace = TRUE)),
    x2 = factor(sample(c("Yes", "No"), size = n, replace = TRUE)),
    x3 = factor(sample(c("Yes", "No"), size = n, replace = TRUE)),
    subsetting = factor(sample(c(TRUE, FALSE), size = n, replace = TRUE)))
  
  library(rms)
  dd <<- datadist(ds)
  options(datadist="dd")
  
  fit <- Glm(y ~ x1 + x2 + x3, data=ds, family=binomial)
  
  a <- printCrudeAndAdjustedModel(fit, order = c("x[12]"), add_references=TRUE)
  expect_equivalent(attr(a, "rgroup"), c("x1", "x2"))
  
  a <- printCrudeAndAdjustedModel(fit, order = c("x2", "x1"), add_references=TRUE)
  expect_equivalent(attr(a, "rgroup"), c("x2", "x1"))
  
  fit <- glm(y ~ x1 + x2 + x3, data=ds, family=binomial)
  
  a <- printCrudeAndAdjustedModel(fit, order = c("x[12]"), add_references=TRUE)
  expect_equivalent(attr(a, "rgroup"), c("x1", "x2"))
  
  a <- printCrudeAndAdjustedModel(fit, order = c("x2", "x1"), add_references=TRUE)
  expect_equivalent(attr(a, "rgroup"), c("x2", "x1"))
})