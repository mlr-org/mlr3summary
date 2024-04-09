# ----- Credit Example ----

#--- setup ----
library("mlr3")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3fairness")
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
logreg = glm(risk ~., data = credit, family = binomial(link = "logit"))
summary(logreg)

#----- create task -----
task = TaskClassif$new(id = "credit", backend = credit, target = "risk", positive = "good")

#----- summary ranger ----
mod = lrn("classif.ranger", predict_type = "prob")
set.seed(12005L)
mod$train(task)

cv3 = rsmp("cv", folds = 3L)
rr = resample(task = task, learner = mod, resampling = cv3, store_models = TRUE)
rr$aggregate(msrs(list("classif.acc", "classif.auc")))

summary(object = mod, resample_result = rr)

#--- EXTRAS ----
# pipeline:
library(mlr3pipelines)

graphlrn = as_learner(po("scale") %>>%
    po("encode") %>>%
    lrn("classif.ranger", predict_type = "prob"))
graphlrn$train(task)
summary(graphlrn)

# performance
summary(object = mod, resample_result = rr,
  control = summary_control(measures = msrs(list("classif.acc"))))

# importances
summary(object = mod, resample_result = rr,
  control = summary_control(importance_measures = c("pfi.f1", "shap")))

# fairness
library(mlr3fairness)
summary(object = mod, resample_result = rr, control = summary_control(protected_attribute = "sex"))

