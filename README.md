Greg - the G-forge regression package
=====================================

This package helps with building and conveying regression models. It has also a few functions for handling [robust confidence](https://en.wikipedia.org/wiki/Heteroscedasticity-consistent_standard_errors) intervals for the ols() regression in the rms-package. It is closely interconnected with the the Gmisc, htmlTable, and the forestplot packages.

## Conveying regression models

Communicating statistical results is in my opinion just as important as performing the statistics. Often effect sizes may seem technical to clinicians but putting them into context often helps and makes you to get your point across.

### Crude and adjusted estimates

The method that I most frequently use in this package is the `printCrudeAndAdjustedModel`. It generates a table that has the full model coefficients together with confidence intervals alongside a crude version using only that single variable. This allows the user to quickly gain insight into how strong each variable is and how it interacts with the full model, e.g. a variable that shows a large change when adding the other variables suggests that there is significant confounding. See the vignette for more details, `vignette("Print_crude_and_adjusted_models")`.

### Forest plots for regression models

I also like to use forest plots for conveying regression models. A common alternative to tables is to use a forest plot with estimates and confidence intervals displayed in a graphical manner. The actual numbers of the model may be better suited for text while the graphs quickly tell how different estimates relate. 

Sometimes we also have situations where one needs to choose between two models, e.g. a [Poisson regression](https://en.wikipedia.org/wiki/Poisson_regression) and a [Cox regression](https://en.wikipedia.org/wiki/Proportional_hazards_model). This package provides a `forestplotCombineRegrObj` function that allows you to simultaneously show two models and how they behave in different settings. This is also useful when performing sensitivity analyses and comparing different selection criteria, e.g. only selecting the patients with high-quality data and see how that compares.

### Plotting non-linear hazard ratios

The `plotHR` function was my first attempt at doing something more advanced version based upon Reinhard Seifert's original adaptation of the `stats::termplot` function. It has some neat functionality although I must admit that I now more commonly use [ggplot2](http://ggplot2.org/) for many of my plots as I like to have a consistent look throughout the plots. The function has though a neat way of displaying the density of the variable at the bottom.

## Modeling helpers

Often much of our modeling ends up a little repetitive and this package contains a set of functions that I've found useful. The approach that I have for modeling regressions is heavily influenced by [Frank Harrell](http://biostat.mc.vanderbilt.edu/wiki/Main/FrankHarrell)'s [regression modeling strategies](http://www.springer.com/us/book/9783319194240#aboutBook). The core idea consist of:

- Choose the variables that should be in the model (for this I often use DAG diagrams drawn with [dagitty.net](http://dagitty.net))
- I build the basic model and then test the [continuous variables](http://biostat.mc.vanderbilt.edu/wiki/Main/CatContinuous) for non-linearity using the `addNonLinearity` function. The function tests using [ANOVA](https://en.wikipedia.org/wiki/Analysis_of_variance) for non-linearity and if such is found it maps a set of knots, e.g. 2-7 knots, of a spline function and then checks for the model with the lowest AIC/BIC value. If it is the main variable I do this by hand to avoid choosing a too complex model when the AIC/BIC values are very similar but for confounders I've found this a handy approach.
- I then check for interactions that I believe to exist using the ANOVA approach
- Finally I see if there are any violations to the model assumptions. I often use linear regression for Health Related Quality of Life ([HRQoL](https://en.wikipedia.org/wiki/Quality_of_life_%28healthcare%29)) scores and these are often plagued by problems in homoscedasticity and using robust variance-covariance matrices takes care of this issue with the `robcov_alt` method. In survival analyses the non-proportional hazards assumption can sometimes be violated where the `timeSplitter` function helps you to set-up a dataset that allows you to build time-interaction models (see `vignette("timeSplitter")` for details).

