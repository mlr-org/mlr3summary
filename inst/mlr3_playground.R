############ mlr3 playground ##############
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3filters)
library(mlr3pipelines)
set.seed(1812L)

# REGRESSION ------

tsk_cars = tsk("mtcars")
lrn_rpart = lrn("regr.rpart")
mod_rpart = lrn_rpart$train(task = tsk_cars) # final model!


# Different resampling strategies
# 3-fold CV
cv3 = rsmp("cv", folds = 3)
rrcv3 = resample(tsk_cars, lrn_rpart, cv3, store_model = TRUE)
load_all()
sm = summary(mod_rpart, rrcv3)
sm


# With preprocessing
graph = po("filter", filter = mlr3filters::flt("variance"), filter.frac = 0.5) %>>%
  po("learner", mlr3::lrn("regr.rpart"))
mod_graph = as_learner(graph)
mod_graph$train(tsk_cars)
rrcv3g = resample(tsk_cars, mod_graph, cv3, store_model = TRUE)
load_all()
sm = summary(mod_graph, rrcv3g)
sm

# Complex graph
graph_complex = po("scale", center = TRUE, scale = FALSE) %>>%
  gunion(list(
    po("missind"),
    po("imputemedian")
  )) %>>%
  po("featureunion") %>>%
  po("learner", mlr3::lrn("regr.rpart"))
print(graph_complex)
mod_graphc = as_learner(graph_complex)
mod_graphc$train(tsk_cars)
rrcv3gc = resample(tsk_cars, mod_graphc, cv3, store_model = TRUE)
load_all()
sm = summary(mod_graphc, rrcv3gc)
sm

# CLASSIFICATION ----

tsk_penguins = tsk("penguins")
lrn_rpart = lrn("classif.rpart", predict_type = "response")
mod_rpart = lrn_rpart$train(task = tsk_penguins) # final model!

# Different resampling strategies
# 3-fold CV
cv3 = rsmp("cv", folds = 3)
rrcv3 = resample(tsk_penguins, lrn_rpart, cv3, store_model = TRUE)
load_all()
sm = summary(mod_rpart, rrcv3)
sm


tsk_penguins = tsk("penguins")
lrn_rpart = lrn("classif.rpart", predict_type = "prob")
mod_rpart = lrn_rpart$train(task = tsk_penguins) # final model!

# Different resampling strategies
# 3-fold CV
cv3 = rsmp("cv", folds = 3)
rrcv3 = resample(tsk_penguins, lrn_rpart, cv3, store_model = TRUE)
load_all()
sm = summary(mod_rpart, rrcv3)
sm

# # Complex graph
# graph_complex = po("scale", center = TRUE, scale = FALSE) %>>%
#   gunion(list(
#     po("missind"),
#     po("imputemedian")
#   )) %>>%
#   po("featureunion") %>>%
#   po("learner", mlr3::lrn("classif.rpart"))
# print(graph_complex)











# Subsampling with 3 repeats and 9/10 ratio
ss390 = rsmp("subsampling", repeats = 3, ratio = 0.9)
# 2-repeats 5-fold CV
rcv25 = rsmp("repeated_cv", repeats = 2, folds = 5)

rrcv3 = resample(tsk_penguins, lrn_rpart, cv3, store_model = TRUE)
rrcv3g = resample(tsk_penguins, graph, cv3, store_model = TRUE)
rrcv3_store = resample(tsk_penguins, lrn_rpart, cv3, store_model = TRUE)

# Inspect ResampleResult
## general learner
rrcv3$learner
## trained models --> store_model necessary
rrcv3$learners[[1]]$model # NULL
str(rrcv3_store$learners[[1]]$model) # store_model needed
## task & data
rrcv3$task$data()
## instance ids for each fold
rrcv3$resampling$instance
# predictions (test set)
rrcv3$prediction()
rrcv3$predictions()

# Other resampling strategies
rrss390 = resample(tsk_penguins, lrn_rpart, ss390)
rrss390$resampling$instance
rrcv25 = resample(tsk_penguins, lrn_rpart, rcv25)
rrcv25$resampling$instance

# Performance
rrcv3$score(measures = msr("classif.ce"))

# Ideas
summary.ResampleResult = function(object, final_model, digits = max(3L, getOption("digits") - 3L), ...) {
  browser()
}
summary(rrcv3, final_model = mod_rpart)

