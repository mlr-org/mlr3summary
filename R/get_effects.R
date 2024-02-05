## inspired by mlr3:::score_measures and mlr3:::score_single_measure
get_effects = function(obj, effect_measures) {

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

  # get min/max for grid
  min_val = obj$task$data()[, lapply(.SD, min, na.rm = TRUE), .SDcols = is.numeric]
  max_val = obj$task$data()[, lapply(.SD, max, na.rm = TRUE), .SDcols = is.numeric]

  effs_list = sapply(effect_measures, function(x) NULL)

  # step through effect measures
  for (eff_msr in effect_measures) {

    # step through resample folds
    effs = pmap(tab[, c("task", "learner", "resampling",
      "iteration", "prediction"), with = FALSE], function(task,
        learner, resampling, iteration, prediction) {
        get_single_effect(eff_msr, task, learner, train_set = resampling$train_set(iteration),
          prediction, min_val = min_val, max_val = max_val)
      })
    effs = do.call(rbind, effs)
    effs_list[[eff_msr]] = effs[, mean(value, na.rm = TRUE), by = list(feature, grid)]
  }
  effs_list
}

get_single_effect = function(effect_measure, task, learner, train_set, prediction, min_val, max_val) {

  test_ids = prediction$test$row_ids
  test_tsk = task$clone()$filter(test_ids)
  learner$state$train_task = task

  if (grepl("pfi", effect_measure)) {
    loss = gsub("pfi.", "", effect_measure)
    effect_measure = "pfi"
  }

  em = switch(effect_measure,
    "pdp" = get_pdp_or_ale_effect(learner, test_tsk, method = "pdp", min_val, max_val),
    "ale" = get_pdp_or_ale_effect(learner, test_tsk, method = "ale", min_val, max_val))
}


get_pdp_or_ale_effect = function(learner, test_tsk, method, min_val, max_val) {
  if (!requireNamespace("iml", quietly = TRUE)) {
    stop("Package 'iml' needed for this function to work. Please install it.", call. = FALSE)
  }
  pred = iml::Predictor$new(model = learner, data = test_tsk$data(),
    y = test_tsk$target_names)

  # <FIXME:> what about categorical features
  gridsize = 5L
  eff = lapply(test_tsk$feature_names, function(feature) {
    grid = seq(from = min_val[[feature]],
      to   = max_val[[feature]], length.out = gridsize)
    ef = iml::FeatureEffect$new(predictor = pred, feature = feature,
      method = method, grid.points = grid)$results
    ef$feature = feature
    data.table(feature = ef$feature, grid = 1:gridsize, value = ef$.value)
  })
  do.call(rbind, eff)
}


# aggregate_effect = function(ll, min_val, max_val) {
#   feats = unique(names(ll))
#   res = pmap_dfr(feats, function(feat) {
#     range = seq(from = min_val[[feat]], max_val[[feat]], length.out = 5L)
#     vals = sapply(ll[which(names(ll) == feat)], function(pdp) {
#       pdp$predict(range, extrapolate = TRUE)
#     })
#     rowMeans(vals)
#   })
#   names(res) = feats
#   res
# }
#
#
# aggregate_effect = function(ll, correction_factor) {
#   # add feature name to data tables in list
#
#   dtl = lapply(ll, function(llsingle) {
#     featnam = setdiff(names(llsingle), c(".type", ".value"))
#     llsingle$feat = featnam
#     names(llsingle)[which(names(llsingle) == featnam)] = "grid"
#     llsingle
#   })
#   # rbind list elements to data.table
#   dt = data.table(do.call(rbind, dtl))
#
#   mean = dt[, mean(.value, na.rm = TRUE), by = list(feat, grid)]
#   var = dt[, var(.value, na.rm = TRUE), by = list(feat, grid)]
#
#   res = merge(mean, var, by = c("feat", "grid"))
#   setnames(res, c("V1.x", "V1.y"), c("mean", "var"))
#
#   res
# }


