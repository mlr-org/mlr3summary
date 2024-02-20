## inspired by mlr3:::score_measures and mlr3:::score_single_measure
get_effects = function(obj, effect_measures) {

  tab = get_private(obj)$.data$as_data_table(view = NULL,
    reassemble_learners = TRUE, convert_predictions = FALSE)
  tmp = unique(tab, by = c("task_hash", "learner_hash"))[,
    c("task", "learner"), with = FALSE]

  # get min/max for grid
  min_val = obj$task$data()[, map(.SD, min, na.rm = TRUE), .SDcols = is.numeric]
  max_val = obj$task$data()[, map(.SD, max, na.rm = TRUE), .SDcols = is.numeric]

  # step through effect measures
  effs_list = map(effect_measures, function(eff_msr) {

    # step through resample folds
    effs = pmap_dtr(tab, function(task,
      learner, resampling, iteration, prediction, ...) {
      get_single_effect(eff_msr, task, learner, train_set = resampling$train_set(iteration),
        prediction, min_val = min_val, max_val = max_val)
    })
    if (!is.null(effs$class)) {
      groupvars = c("feature", "grid", "class")
    } else {
      groupvars = c("feature", "grid")
    }
    effs[, mean(value, na.rm = TRUE), by = groupvars]
  })
  names(effs_list) = effect_measures
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

  gridsize = 5L
  eff = map(test_tsk$feature_names, function(feature) {
    col_info = test_tsk$col_info[id == feature,]
    if (!col_info$type %in% c("numeric", "integer")) {
      if (col_info$type == "ordered" & length(col_info$levels[[1]]) > 5) {
        levs = col_info$levels[[1]]
        grid = levs[round(seq(1, length(levs), length.out = gridsize))]
      } else {
        grid = NULL
      }
    } else {
      grid = seq(from = min_val[[feature]],
        to   = max_val[[feature]], length.out = gridsize)
    }
    ef = iml::FeatureEffect$new(predictor = pred, feature = feature,
      method = method, grid.points = grid)$results
    ef = data.table(ef[ef[[feature]] %in% grid,])
    ef$featurenam = feature

    if (!is.na(test_tsk$positive)) {
      ef = ef[.class == test_tsk$positive]
      ef[, .class := NULL]
    }
    data.table(feature = ef$featurenam, grid = ef[[feature]], value = ef$.value, class = ef$.class)
  })

  do.call(rbind, eff)
}


