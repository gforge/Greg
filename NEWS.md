NEWS for the **Greg** package

Changes for 2.0.2
-----------------
* Fixed minor documentation issue with hidden functions

Changes for 2.0.1
-----------------
* Improved forestplot integration so that fp_... functions are available (see example for `forestplotRegrObj`)

Changes for 2.0.0
-----------------
* BREAKING: Refactored the forestplogRegrObj function to S3-format
* Added broom::tidy for rms-models
* Updated to new forestplot API together with some general improvements to the forestplot functions
* Dropped magrittr in favor of R native-pipe
* Converted examples to `tidyverse` style

Changes for 1.4.1
-----------------
* Fix for updated survival package with a more consistent naming (3.4-0)
* Fix for plotHR axes & bty and improved example with dplyr syntax
* Fix for bad offset() handling, was ignored in getCrudeAndAdjusted unless presented as an `offset=` parameter.

Changes for 1.4.0
-----------------
* The plotHR has now a more elegant `plot` S3-function separating the preparing of the plot and the actual output (*Note*: if called within a loop you now need to call `print`/`plot` on the returned object)

Changes for 1.3.4
-----------------
* Fix for `htmlTable` 2.0 which means that the css.group option has been dropped in favor of the new theming & `addHtmlTableStyle` function. See example and vignette for instructions on how-to.

Changes for 1.3.3
-----------------
* Fix for new handling of data.frames in next R version

Changes for 1.3
-----------------
* Fix vignette bug
* Fixed CRAN package names
* Fix bug due to change in `survival::coxph`

Changes for 1.2
-----------------
* Fixed spelling errors, travis package check, added tests for coverage and other things related to CRAN submission
* Added a `timeSplitter` for making time splits through the `Epi::Lexis` object simpler
* Added a check for null as input parameter to `printCrudeAndAdjusted`
* Added a subset function for `get/printCrudeAndAdjusted`
* Updated the test cases
* The reference now has the same number of digits as the rest of the coefficients
* Added `htmlTable.printCrudeAndAdjusted` for more flexible handling

Changes for 1.1
-----------------
* The `printCrudeAndAdjusted` calls now the correct print version
* Added vignette for basic use cases
* Fixed non-linearity
* Added options-alternative to tailor `printCrudeAndAdjusted` output
* Fixed issue #5
* Fixed `tspanner` handling for `printCrudeAndAdjustedModel`
* Bug when with descriptive stats due to missing misspelled (useNa instead of useNA)
* Added a `rbind` for `printCrudeAndAdjusted`
* Added handling of missing variable for the outcome estimator
* Fixed `simpleRmsAnova`
* Fixed `getModelData` with subsetting arguments not being constants
* All show_missing are now `useNA` to comply with main package standards
* A few bug fixes

Changes for 1.0.0
-----------------
* Changed the desc_ arguments into a list using the `caDescribeOpts`
* Refactored major parts of the `printCrudeAndAdjusted`
* Changed to use `model.frame()` under the hood - hopefully stabilizing the functions
  as it uses a more standard R-approach
* The update() now actively runs under the model environment
* The `plotHR` is updated and can now plot contrasts if provided a rms-package regression
  allowing for efficient comparison of multiple models. Internal functions have also
  been externalized.
* All current unit tests now pass and several new ones have been added
* Implemented DRY roxygen2 code
* The `getCrudeAndAdjusted` now retains cluster and stratas - there are options for
  leaving these out to retain old behavior
* The `getCrudeAndAdjusted` now allows for selecting variables. The `printCrudeAndAdjusted`
  now passes on the order option when variable selection is of interest.
* Cleaned variables for the `forestplot2` functions
* Added the `addNonlinearity` that adds non-linearity for a variable if it finds
  support for it in the data.

Changes for 0.7.1
-----------------
* Internalized some of the private function documentation
* Improved the outcome extractor function and added test cases

Changes for 0.7.0
-----------------
* Major remake of the `print-/getCrudeAndAdjusted` so that they depend on prMapVariable2Name
  and everything is now centered around the variables 
* The `printCrudeAndAdjustedModel` no longer capitalizes the first letter in order 
  to allow all lower case var names
* Added imputation compatibility with the `Hmisc::fit.mult.impute` function
* Added multiple test cases for stability

Changes for 0.6.1
-----------------
* Changed to semantic version numbering
* Fixed bug for `printCrudeAndAdjusted` when using matrix from the getC&A
* Fixed bug when boolean desc_column was generated
* Added `stop()` when using descriptive column without add_refrerences

Changes for 0.6.0.1
-------------------
* The `getCrudeAndAdjusted` now handles the intercept term better for naming the 
  rows (Thanks to Victor)
* The `getCrudeAndAdjusted`/`printCrudeAndAdjusted` now use 'model' instead 'fit' for 
  the model regression object name
* The `printCrudeAndAdjusted` now calls print on the `htmlTable_str` object so that it 
  appears as expected
* Added unit tests

Changes for 0.6.0.0
-------------------
* The split from the Gmisc package
