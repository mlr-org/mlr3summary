# helper files
library(data.table)
library(mlr3)
library(mlr3learners)

set.seed(3100)

x1 = runif(n = 100)
x2 = runif(n = 100)
x3 = runif(n = 100)
x4 = rnorm(n = 100)
x5 = rbinom(n = 100, size = 1, prob = 0.75)
x6 = as.factor(sample(1:5, size = 100, replace = TRUE, prob = c(0.5, 0.2, 0.15, 0.1, 0.05)))
X = data.table(x1, x2, x3, x4, x5, x6)

# first 3 features main effects, interaction effect between x3 and x5
ytrue = 4*x1 + 4*x2 + 4*x5*x3^2
epsilon = rnorm(n = 100, mean = 0, sd = ytrue*0.1)
target = ytrue + epsilon

## regression
get_regression_task = function() {
  y = target
  dt = data.table(X, y)
  TaskRegr$new(id = "regression", backend = dt, target = "y")
}

## binary classification
get_binary_task = function() {
  y = as.factor(ifelse(target < 5, 1, 0))
  dt = data.table(X, y)
  TaskClassif$new(id = "binary", backend = dt, target = "y")
}

## multiclass classification
get_multiclass_task = function() {
  y = as.factor(ifelse(target < 2.5, 1, ifelse(target < 6, 2, 3)))
  dt = data.table(X, y)
  TaskClassif$new(id = "multiclass", backend = dt, target = "y")
}

