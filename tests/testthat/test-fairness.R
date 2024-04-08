require("rpart")

test_that("Works for regression", {
  set.seed(1003)
  tsk = get_regression_task()
  lrn_rr = lrn("regr.rpart", maxdepth = 2L)
  lrn_rr$train(tsk)
  cv3 = rsmp("cv", folds = 3L)
  rr_reg = resample(tsk, lrn_rr, cv3, store_models = TRUE)
  rr_reg$task
  sm = summary(lrn_rr, rr_reg, summary_control(protected_attribute = "x5",
    complexity_measures = NULL, effect_measures = NULL, importance_measures = "pdp"))
  expect_true(!is.null(sm$fairness))
})

test_that("Works for binary classif", {
  set.seed(1003)
  tsk = get_binary_task()
  lrn_rr = lrn("classif.rpart", maxdepth = 2L, predict_type = "prob")
  lrn_rr$train(tsk)
  cv3 = rsmp("cv", folds = 3L)
  rr_classif = resample(tsk, lrn_rr, cv3, store_models = TRUE)
  rr_classif$task
  sm = summary(lrn_rr, rr_classif, summary_control(protected_attribute = "x5",
    complexity_measures = NULL, effect_measures = NULL, importance_measures = "pdp"))
  expect_true(!is.null(sm$fairness))
})

test_that("Works for multiclass", {
  tsk_peng = tsk("penguins")
  tsk_peng$set_col_roles("sex", add_to = "pta")
  lrn_rpart =  lrn("classif.rpart", predict_type = "prob")
  lrn_rpart$train(task = tsk_peng)
  rsmp_cv2 = rsmp("cv", folds = 2L)
  rr_9 = resample(tsk_peng, lrn_rpart, rsmp_cv2, store_model = TRUE)
  sm = summary(lrn_rpart, rr_9, summary_control(complexity_measures = NULL, effect_measures = NULL))
  expect_true(!is.null(sm$fairness))
})
