library('testthat')
library('Greg')

test_check('Greg')
## To run the tests manually
if (FALSE){
  my_test_dir <- gsub("Greg.*", "Greg/tests/testthat", getwd())
  test_dir(my_test_dir)
}
