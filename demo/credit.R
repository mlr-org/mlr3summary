# ---- Credit Example ----

# ---- setup ----
library("mlr3summary")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3fairness")

data("credit", package = "mlr3summary")

# ---- glm ----
logreg = glm(risk ~., data = credit, family = binomial(link = "logit"))
summary(logreg)

# ---- create task ----
task = TaskClassif$new(id = "credit", backend = credit, target = "risk", positive = "good")

# ---- summary ranger ----
rf = lrn("classif.ranger", predict_type = "prob")
set.seed(12005L)
rf$train(task)

cv3 = rsmp("cv", folds = 3L)
rr = resample(task = task, learner = rf, resampling = cv3, store_models = TRUE)
rr$aggregate(msrs(list("classif.acc", "classif.auc")))

summary(object = rf, resample_result = rr)

# ---- fairness assessment ----
summary(object = rf, resample_result = rr,
  control = summary_control(protected_attribute = "sex"))

# ---- adapt control ----
summary(object = rf, resample_result = rr,
  control = summary_control(measures = msrs(list("classif.acc"))))

summary(object = rf, resample_result = rr,
  control = summary_control(importance_measures = c("pfi.f1", "shap")))

# ---- omit certain parts ----
# summary(object = rf, resample_result = rr,
#   control = summary_control(measures = msrs(list("classif.acc"))),
#   hide = c("performance", "residuals", "complexity"))

# ---- pipelines ----
library(mlr3pipelines)

graphlrn = as_learner(
  po("scale") %>>%
    po("encode") %>>%
    lrn("classif.ranger", predict_type = "prob"))
graphlrn$train(task)
summary(graphlrn)

set.seed(1234L)
graph_complex = as_learner(
  po("scale", center = TRUE, scale = FALSE) %>>%
  gunion(list(
    po("missind"),
    po("imputemedian")
  )) %>>%
  po("featureunion") %>>%
  po("learner", mlr3::lrn("classif.rpart"))
)
graph_complex = as_learner(graph_complex)
graph_complex$train(task)
summary(graph_complex)

# ---- getting help ---
?summary.Learner
?summary_control
as.data.table(mlr_measures_fairness)

# AutoTuner

