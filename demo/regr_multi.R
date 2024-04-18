# ---- Regression & Multiclass Examples ----

# ---- setup ----
library("mlr3summary")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3fairness")


############### REGRESSION & XGBOOST ################

# ---- create task ----
task_cars = tsk("mtcars")

# ---- summary ranger ----
xg = lrn("regr.xgboost")
set.seed(12005L)
xg$train(task_cars)

cv3 = rsmp("cv", folds = 3L)
rr_c = resample(task = task_cars, learner = xg, resampling = cv3, store_models = TRUE)
summary(object = xg, resample_result = rr_c)



############### MULTICLASS & TREE ################

# ---- create task ----
task_wine = tsk("wine")

# ---- summary ranger ----
tree = lrn("classif.rpart")
set.seed(12005L)
tree$train(task_wine)

bs3 =  rsmp("bootstrap", repeats = 3L)
rr_w = resample(task = task_wine, learner = tree, resampling = bs3, store_models = TRUE)
summary(object = tree, resample_result = rr_w, summary_control(n_important = 4L))


