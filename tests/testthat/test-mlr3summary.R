
test_that("Correct output for regression", {
  set.seed(1003)
  tsk = get_regression_task()
  lrn_rr = lrn("regr.rpart", maxdepth = 2L)
  lrn_rr$train(tsk)
  sm_simple = summary(lrn_rr)
  expect_equal(sm_simple$task_type, "regr")
  expect_equal(sm_simple$feature_names, tsk$feature_names)

  cv3 = rsmp("cv", folds = 3L)
  rr_reg = resample(tsk, lrn_rr, cv3, store_models = TRUE)

  sm_complex = summary(lrn_rr, rr_reg, summary_control(importance_measures = c("pfi.mse", "pdp")))
  expect_true(all(sm_complex$importances$pdp$mean[1:3] > 0.05 &
      sm_complex$importances$pfi.mse$mean[4:6] < 6e-8))
  expect_true(all(sm_complex$importances$pfi.mse$mean[1:3] > 0.05 &
      sm_complex$importances$pfi.mse$mean[4:6] == 0))

  expect_true(all(sm_complex$effects$ale$grid %in% sm_complex$effects$pdp$grid))
  cnt_pdp = sm_complex$effects$pdp[feature %in% c("x1", "x2", "x3", "x4", "x5", "x6"), .(count = .N), by = feature]$count
  expect_true(all(cnt_pdp == 5))
  cnt_ale = sm_complex$effects$ale[feature %in% c("x1", "x2", "x3", "x4", "x6"), .(count = .N), by = feature]$count
  expect_true(all(cnt_ale == 5))
})

test_that("Correct output for binary classification", {
  set.seed(1003)
  tsk = get_binary_task()
  lrn_rr = lrn("classif.ranger", num.trees = 10L, predict_type = "prob")
  lrn_rr$train(tsk)
  sm_simple = summary(lrn_rr)
  expect_equal(sm_simple$task_type, "classif")
  expect_equal(sm_simple$feature_names, tsk$feature_names)

  cv3 = rsmp("cv", folds = 3L)
  rr_clas = resample(tsk, lrn_rr, cv3, store_models = TRUE)

  sm_complex = summary(lrn_rr, rr_clas)
  expect_true(all(order(sm_complex$importances$pdp$mean, decreasing = TRUE)[1:3] %in% 1:3))

  expect_true(all(sm_complex$effects$ale$grid %in% sm_complex$effects$pdp$grid))
  cnt_pdp = sm_complex$effects$pdp[feature %in% c("x1", "x2", "x3", "x4", "x5", "x6"), .(count = .N), by = feature]$count
  expect_true(all(cnt_pdp == 5))
  cnt_ale = sm_complex$effects$ale[feature %in% c("x1", "x2", "x3", "x4", "x6"), .(count = .N), by = feature]$count
  expect_true(all(cnt_ale == 5))
})


test_that("Correct output for binary classification", {
  set.seed(1003)
  tsk = get_multiclass_task()
  lrn_rr = lrn("classif.ranger", num.trees = 10L, predict_type = "prob")
  lrn_rr$train(tsk)
  sm_simple = summary(lrn_rr)
  expect_equal(sm_simple$task_type, "classif")
  expect_equal(sm_simple$feature_names, tsk$feature_names)

  cv3 = rsmp("cv", folds = 3L)
  rr_clas = resample(tsk, lrn_rr, cv3, store_models = TRUE)

  sm_complex = summary(lrn_rr, rr_clas)
  expect_true(all(order(sm_complex$importances$pdp$mean, decreasing = TRUE)[1:2] %in% 1:2))

  expect_true(all(sm_complex$effects$ale$grid %in% sm_complex$effects$pdp$grid))
  cnt_pdp = sm_complex$effects$pdp[feature %in% c("x1", "x2", "x3", "x4", "x5", "x6"), .(count = .N), by = feature]$count
  expect_true(all(cnt_pdp == 5*3L))
  cnt_ale = sm_complex$effects$ale[feature %in% c("x1", "x2", "x3", "x4", "x6"), .(count = .N), by = feature]$count
  expect_true(all(cnt_ale == 5*3L))

  expect_true(all(sm_complex$effects$ale$class %in% 1:3))
  expect_true(all(sm_complex$effects$pdp$class %in% 1:3))
})
