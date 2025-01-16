## inspired by mlr3:::score_measures and mlr3:::score_single_measure
get_importances = function(obj, importance_measures) {
  tab = get_private(obj)$.data$as_data_table(
    view = NULL, reassemble_learners = TRUE, convert_predictions = FALSE
  )
  tmp = unique(tab, by = c("task_hash", "learner_hash"))[,
    c("task", "learner"), with = FALSE]

  # step through importance measures
  imps_list = map(importance_measures, function(imp_msr) {

    # step through resample folds
    imps = rbindlist(
      future_mapply(function(task, learner, resampling, iteration, prediction, ...) {
        get_single_importance(
          imp_msr, task, learner, train_set = resampling$train_set(iteration), prediction
        )
      }, tab$task, tab$learner, tab$resampling, tab$iteration, tab$prediction,
      future.seed = NULL, SIMPLIFY = FALSE)
    )

    # aggregate results (mean, sd)
    mm = imps[, list(mean = mean(importance)), by = feature]
    varimps = imps[, list(sd = stats::sd(importance)), by = feature]
    merge(mm, varimps, by = "feature")
  })

  set_names(imps_list, importance_measures)
}

get_single_importance = function(importance_measure, task, learner, train_set, prediction) {
  test_ids = prediction$test$row_ids
  test_tsk = task$clone()$filter(test_ids)
  learner$state$train_task = task

  if (grepl("pfi", importance_measure, fixed = TRUE)) {
    loss = gsub("pfi.", "", importance_measure)
    importance_measure = "pfi"
  }

  switch(importance_measure,
    pdp = get_pdp_importance(learner, test_tsk),
    pfi = get_pfi_importance(learner, test_tsk, loss),
    shap = get_shap_importance(learner, test_tsk)
  )
}

get_pdp_importance = function(learner, test_tsk) {
  # based on Greenwell et al. (2018)
  if (!requireNamespace("iml", quietly = TRUE)) {
    stopf("Package 'iml' needed for this function to work. Please install it.", call. = FALSE)
  }
  pred = iml::Predictor$new(model = learner, data = test_tsk$data(), y = test_tsk$target_names)
  if (learner$task_type == "classif") {
    if ("multiclass" %in% learner$state$train_task$properties) {
      # get class frequencies for weighting
      tgtname = test_tsk$target_names
      weights = test_tsk$data(cols = learner$state$train_task$target_names)[, .N / test_tsk$nrow, by = tgtname]
      setnames(weights, "V1", "weights")
    } else {
      pred$class = test_tsk$positive
    }
  }
  pdp = iml::FeatureEffects$new(predictor = pred, method = "pdp")
  # <FIXME:> multiclass probabilities --> how to approach this?
  # currently just sum over variances weighted by class frequencies
  imp = map(pdp$results, function(dt) {
    dt = as.data.table(dt)
    if (learner$task_type == "classif" && "multiclass" %in% learner$state$train_task$properties) {
      sdvals = dt[, stats::sd(.value), by = .class]
      sdvals = merge(sdvals, weights, by.x = ".class", by.y = tgtname)
      sum(sdvals$V1 * sdvals$weights)
      # }
    } else {
      dt[, stats::sd(.value)]
    }
  })
  data.table(feature = names(pdp$results), importance = as.numeric(unlist(imp)))
}

get_pfi_importance = function(learner, test_tsk, loss) {
  # based on Breiman (2001) and Fisher et al. (2019)
  if (!requireNamespace("iml", quietly = TRUE)) {
    stopf("Package 'iml' needed for this function to work. Please install it.")
  }
  pred = iml::Predictor$new(
    model = learner, data = test_tsk$data(), y = test_tsk$target_names
  )
  imp = iml::FeatureImp$new(predictor = pred, loss = loss, compare = "difference")$results[, c("feature", "importance")]
}

get_shap_importance = function(learner, test_tsk, loss) {
  # based on Lundberg and Lee (2017)
  if (!requireNamespace("fastshap", quietly = TRUE)) {
    stopf("Package 'fastshap' needed for this measuring importance. Please install it.")
  }
  if (learner$task_type == "regr") {
    outcome_classes = "response"
  } else if (learner$task_type == "classif") {
    outcome_classes = learner$state$train_task$class_names
  }
  temp = vapply(outcome_classes, FUN.VALUE = numeric(length(test_tsk$feature_names)), function(reference) {
    pfun = function(object, newdata) {
      if (object$task_type == "regr") {
        object$predict_newdata(newdata)[[reference]]
      } else if (object$task_type == "classif") {
        if (object$predict_type == "response") {
          stopf("Importance measure 'shap' requires a learner with `predict_type = 'prob'.")
        } else {
          object$predict_newdata(newdata)$prob[, reference]
        }
      }
    }
    shap = fastshap::explain(learner, X = test_tsk$data(),
      feature_names = test_tsk$feature_names,
      pred_wrapper = pfun, newdata = test_tsk$data(),
      nsim = 10
    )
    colMeans(abs(shap))
  })
  # Sum aggregated shap values over outcome classes
  # --> inspired by shap python module output for `shap.summary_plot(plot_type = "bar")`
  # --> See: https://towardsdatascience.com/explainable-ai-xai-with-shap-multi-class-classification-problem-64dd30f97cea
  imp = rowSums(temp)
  data.table(feature = names(imp), importance = as.numeric(unlist(imp)))
}
