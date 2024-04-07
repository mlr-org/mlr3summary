# ----- Credit Example ----

#--- setup ----
library("mlr3")
library("mlr3learners")
library("iml")
library("mlr3pipelines")
library("devtools")
load_all()

###---- get data ----
credit = read.csv("inst/german_credit_data.csv", row.names = 1, stringsAsFactors = TRUE)
names(credit)
# omit rows with NA entries
credit = na.omit(credit)
# join groups with small frequencies
levels(credit$Purpose) = c("others", "car", "others", "others",
  "furniture", "radio/TV", "others", "others")
levels(credit$Saving.accounts) = c("little", "moderate", "rich", "rich")
credit$Job = factor(credit$Job, levels = c(0, 1, 2, 3),
  labels = c("unskilled", "unskilled", "skilled", "highly skilled"))

# colnames to lower
names(credit) = tolower(names(credit))
# Drop levels
credit = droplevels.data.frame(credit)
credit$saving.accounts = as.ordered(credit$saving.accounts)
credit$checking.account = as.ordered(credit$checking.account)

cols = c("age", "sex", "saving.accounts", "duration", "credit.amount", "risk")
credit = credit[, cols]

# ---- glm ----
logreg = glm(risk ~., family = binomial(link = "logit"), data = credit)
summary(logreg)

# ---- svm ----
library(e1071)
svmmod = svm(risk ~., data = credit)
summary(svmmod)

# # ---- rpart ---
# library(rpart)
# rpart = rpart(risk~., data = credit)
# summary(rpart)

#----- create task -----
task = TaskClassif$new(id = "credit", backend = credit, target = "risk")

#----- summary ranger ----
mod = lrn("classif.ranger", predict_type = "prob")
set.seed(12005L)
mod$train(task)

pred = mod$predict(task)
pred$score(msrs(list("classif.acc")))

cv3 = rsmp("cv", folds = 3L)
rsmp = resample(task = task, learner = mod, resampling = cv3, store_models = TRUE)
rsmp$aggregate(msrs(list("classif.acc", "classif.auc")))

summary(object = mod, resample_result = rsmp)


summary(object = mod, resample_result = rsmp,
  control = summary_control(measures = msrs(list("classif.acc"))))

summary(object = mod, resample_result = rsmp,
  control = summary_control(importance_measures = c("pfi.ce", "shap", "pdp")))


# pipeline:
library(mlr3pipelines)

graphlrn = as_learner(po("scale") %>>%
    po("encode") %>>%
    lrn("classif.ranger", predict_type = "prob"))
graphlrn$train(task)
summary(graphlrn)

# ---- summary glm -----
mod = lrn("classif.log_reg", predict_type = "prob")
set.seed(12005L)
mod$train(task)

pred = mod$predict(task)
pred$score(msrs(list("classif.acc", "classif.auc")))

cv3 = rsmp("cv", folds = 3L)
rsmp = resample(task = task, learner = mod, resampling = cv3, store_models = TRUE)
rsmp$aggregate(msrs(list("classif.acc", "classif.auc")))

summary(object = mod, resample_result = rsmp)


### Pipeline
graphmodel = as_learner(po("scale") %>>%
    po("learner", mlr3::lrn("classif.ranger", predict_type = "prob")))
graphmodel$train(task)
rsmp_gm = resample(task, graphmodel, cv3, store_model = TRUE)
summary(graphmodel)


### summary hard-label
mod = lrn("classif.ranger")
set.seed(12005L)
mod$train(task)
pred = mod$predict(task)
pred$score(msrs(list("classif.acc", "classif.auc")))
cv3 = rsmp("cv", folds = 3L)
rsmp = resample(task = task, learner = mod, resampling = cv3, store_models = TRUE)
summary(object = mod, resample_result = rsmp)


### fairness
summary(object = mod, resample_result = rsmp, control = summary_control(protected_attribute = "sex",
  importance_measures = NULL, effect_measures = NULL, complexity_measures = NULL))

