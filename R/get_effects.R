## inspired by mlr3:::score_measures and mlr3:::score_single_measure
get_effects = function(obj, effect_measures) {

  browser()
  tab = get_private(obj)$.data$as_data_table(view = NULL,
    reassemble_learners = TRUE, convert_predictions = FALSE)
  tmp = unique(tab, by = c("task_hash", "learner_hash"))[,
    c("task", "learner"), with = FALSE]
  # correction factor acc. Molnar et al. (2023),
  # https://doi.org/10.1007/978-3-031-44064-9_24  p. 468,
  # based on Nadeau, C., Bengio, Y.: Inference for the generalization error. Mach. Learn. 52(3),
  # 239–281 (2003)
  correction_factor = 1/obj$iters + mean(sapply(1:obj$iters, FUN = function(i) {
    length(obj$resampling$test_set(i = i)) / length(obj$resampling$train_set(i = i))
  }))

  effs_list = sapply(effect_measures, function(x) NULL)

  # step through effect measures
  for (eff_msr in effect_measures) {

    # step through resample folds
    effs = pmap_dtc(tab[, c("task", "learner", "resampling",
      "iteration", "prediction"), with = FALSE], function(task,
        learner, resampling, iteration, prediction) {
        get_single_effect(eff_msr, task, learner, train_set = resampling$train_set(iteration),
          prediction)
      })

    # aggregate results (mean, variance, corrected variance acc. to Molnar et al. (2023), see above)
    effs_list[[eff_msr]] = aggregate_effect(effs, correction_factor)
  }

  effs_list
}

get_single_effect = function(effect_measure, task, learner, train_set, prediction) {

  test_ids = prediction$test$row_ids
  test_tsk = task$clone()$filter(test_ids)
  learner$state$train_task = task

  if (grepl("pfi", effect_measure)) {
    loss = gsub("pfi.", "", effect_measure)
    effect_measure = "pfi"
  }

  switch(effect_measure,
    "pdp" = get_pdp_or_ale_effect(learner, test_tsk, method = "pdp"),
    "ale" = get_pdp_or_ale_effect(learner, test_tsk, method = "ale"))
}


get_pdp_or_ale_effect = function(learner, test_tsk, method) {
  if (!requireNamespace("iml", quietly = TRUE)) {
    stop("Package 'iml' needed for this function to work. Please install it.", call. = FALSE)
  }
  pred = iml::Predictor$new(model = learner, data = test_tsk$data(),
    y = test_tsk$target_names)

  # <FIXME:> what about categorical features
  eff = lapply(test_tsk$feature_names, function(feature) {
    grid = seq(from = min(test_tsk$data()[[feature]]),
      to   = max(test_tsk$data()[[feature]]),
      length.out = 30)
    iml::FeatureEffect$new(predictor = pred, feature = feature, method = method, grid.points = grid)$results
  })
  names(eff) = test_tsk$feature_names

  eff
}

aggregate_effects = function(dt, correction_factor) {
  browser()
  # based on Molnar et al. (2023), p. 468-469
  ### TODO
  # mm = dt[, mean(importance), by = feature]
  # setnames(mm, "V1", "mean")
  # vardt = dt[, stats::var(importance), by = feature]
  # vardt$corrvar = correction_factor * vardt$V1
  # setnames(vardt, "V1", "var")
  # merge(mm, vardt, by = "feature")
}


