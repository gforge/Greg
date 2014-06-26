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
context("printCrudeAndAdjustedModel")
test_that("Check position of reference", {
  dd <- datadist(ds)
  options(datadist="dd")
  
  s <- Surv(ds$ftime, ds$fstatus == 1)
  fit <- cph(s ~ x + boolean, data=ds)
  
  a <- printCrudeAndAdjustedModel(fit, add_references=TRUE)
  expect_match(a[1,2], "ref")
  
  # Bug with the same label occurring miultiple times
  a <- printCrudeAndAdjustedModel(update(fit, .~.+
                                           same_label +
                                           same_labell +
                                           same_labelll), add_references=TRUE, desc_column=TRUE)
  expect_equal(nrow(a), 11)
  
  tmp <- getCrudeAndAdjustedModelDataAndAdjustedModelData(fit)
  b <- printCrudeAndAdjustedModel(tmp, add_references=TRUE)
  
  expect_equal(a, b)
  # Getting the name wrong should not change the reference
  a <- printCrudeAndAdjustedModel(fit, add_references=TRUE, 
                                  add_references_pos=list(a=3))
  expect_match(a[1,2], "ref")
  
  # This should move the reference
  a <- printCrudeAndAdjustedModel(fit, add_references=TRUE, add_references_pos=list(x=2))
  expect_match(a[2,2], "ref")
  
  # Should end up at first position if referenced outside
  a <- printCrudeAndAdjustedModel(fit, add_references=TRUE, add_references_pos=list(x=5))
  expect_match(a[1,2], "ref")
  
  # Should end up at first position if referenced outside
  a <- printCrudeAndAdjustedModel(fit, add_references=TRUE, add_references_pos=list(x=-5))
  expect_match(a[1,2], "ref")
})



