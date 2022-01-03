org_par <- par(xaxs = "i", ask = TRUE)
library(survival)
library(rms)
library(dplyr)
library(Gmisc)

# Get data for example
n <- 1000
set.seed(731)

ds <- tibble(age = round(50 + 12 * rnorm(n), 1),
             smoking = sample(c("Yes", "No"), n, rep = TRUE, prob = c(.2, .75)),
             sex = sample(c("Male", "Female"), n, rep = TRUE, prob = c(.6, .4))) %>% 
  # Build outcome
  mutate(h = .02 * exp(.02 * (age - 50) + .1 * 
                         ((age - 50) / 10)^3 + .8 * 
                         (sex == "Female") + 2 * 
                         (smoking == "Yes")),
         cens = 15 * runif(n),
         dt = -log(runif(n)) / h,
         e = if_else(dt <= cens, 1, 0),
         dt = pmin(dt, cens),
         # Add missing data to smoking
         smoking = case_when(runif(n) < 0.05 ~ NA_character_,
                             TRUE ~ smoking)) %>% 
  set_column_labels(age = "Age",
                    dt = "Follow-up time") %>% 
  set_column_units(dt = "Year")


library(splines)
fit.coxph <- coxph(Surv(dt, e) ~ bs(age, 3) + sex + smoking, data = ds)

plotHR(fit.coxph, term = "age", plot.bty = "o", xlim = c(30, 70), xlab = "Age")

dd <- datadist(ds)
options(datadist = "dd")
fit.cph <- cph(Surv(dt, e) ~ rcs(age, 4) + sex + smoking, data = ds, x = TRUE, y = TRUE)

plotHR(fit.cph,
       term = 1,
       plot.bty = "L",
       xlim = c(30, 70),
       ylim = 2^c(-3,3),
       xlab = "Age")

plotHR(fit.cph,
       term = "age", 
       plot.bty = "l", 
       xlim = c(30, 70),
       ylog = FALSE,
       rug = "ticks",
       xlab = "Age")

unadjusted_fit <- cph(Surv(dt, e) ~ rcs(age, 4), data = ds, x = TRUE, y = TRUE)
plotHR(list(fit.cph, unadjusted_fit),
       term = "age", 
       xlab = "Age",
       polygon_ci = c(TRUE, FALSE),
       col.term = c("#08519C", "#77777799"),
       col.se = c("#DEEBF7BB", grey(0.6)),
       lty.term = c(1, 2),
       plot.bty = "l", xlim = c(30, 70))
par(org_par)
