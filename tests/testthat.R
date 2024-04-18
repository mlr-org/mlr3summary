# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files
library(testthat)
library(checkmate)
library(mlr3learners)
library(mlr3summary)
library(data.table)

test_check("mlr3summary")
