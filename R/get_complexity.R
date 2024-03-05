## inspired by mlr3:::score_measures and mlr3:::score_single_measure
get_complexity = function(obj, complexity_measures) {

  tab = get_private(obj)$.data$as_data_table(view = NULL,
    reassemble_learners = TRUE, convert_predictions = FALSE)
  tmp = unique(tab, by = c("task_hash", "learner_hash"))[,
    c("task", "learner"), with = FALSE]

  # step through complexity measures
  comps_list = map(complexity_measures, function(comp_msr) {
    # step through resample folds
    comps = pmap_dbl(tab, function(task,
      learner, resampling, iteration, prediction, ...) {
      get_single_complexity(comp_msr, task, learner, train_set = resampling$train_set(iteration),
        prediction)
    })
    comps
  })
  names(comps_list) = complexity_measures
  comps_list
}

get_single_complexity = function(complexity_measure, task, learner, train_set, prediction) {
  test_ids = prediction$test$row_ids
  test_tsk = task$clone()$filter(test_ids)
  learner$state$train_task = task
  em = switch(complexity_measure,
    "sparsity" = get_sparsity_or_interaction_strength(learner, test_tsk, method = "sparsity"),
    "interaction_strength" = get_sparsity_or_interaction_strength(learner, test_tsk, method = "interaction_strength"))
}

get_sparsity_or_interaction_strength = function(learner, test_tsk, method) {
  if (!requireNamespace("iml", quietly = TRUE)) {
    stop("Package 'iml' needed for this function to work. Please install it.", call. = FALSE)
  }
  pred = iml::Predictor$new(model = learner, data = test_tsk$data(),
    y = test_tsk$target_names)

  grid.size = switch(method, "sparsity" = 20L, "interaction_strength" = 100L)
  ales = iml::FeatureEffects$new(pred, method = "ale", grid.size = grid.size)

  if (method == "sparsity") {
    get_sparsity(ales)
  } else if (method == "interaction_strength") {
    compute_interaction_strength(pred, ales)
  }
}

#' Get number of used features based on Molnar (2019)
#' A feature is used if its feature effect is not constant.
#' Feature effects are measured by ALE or PDP.
#' @param effects (iml::FeatureEffects) \cr
#' FeatureEffects Object
get_sparsity = function(effects){
  id_used = mlr3misc:::map_lgl(effects$results, .f = function(ef) {
    if (var(ef$.value) != 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  sum(id_used)
}

#' Compute interaction strength based on Molnar (2019)
#' @param predictor (iml::Predictor) \cr
#' Predictor object.
#' @param effects (iml::FeatureEffects) \cr
#' FeatureEffects Object
compute_interaction_strength = function(predictor, effects) {

  # compute ALE
  X = predictor$data$get.x()
  pred = predictor$predict(X)
  if (ncol(pred) > 1) {
    stop("Complexity measure 'interaction_strength' does not work for multiClass")
  } else {
    pred = pred[[1]]
  }
  mean_pred = mean(pred)

  res = data.frame(lapply(effects$effects, function(eff) {
    eff$predict(data.frame(X))
  }))
  ale_predictions = rowSums(res) + mean_pred
  ssq_bb = ssq(pred - mean_pred)

  if(ssq_bb == 0) {
    r2 = 1
  } else {
    ssq_1st_order_e = ssq(ale_predictions - pred)
    r2 = 1 - ssq_1st_order_e/ssq_bb
  }

}

ssq = function(x) {
  assert_numeric(x, any.missing = FALSE, min.len = 1)
  sum(x^2)
}
