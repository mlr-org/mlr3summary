## inspired by mlr3:::score_measures and mlr3:::score_single_measure
get_importances = function(obj, importance_measures) {

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

  imps_list = sapply(importance_measures, function(x) NULL)

  # step through importance measures
  for (imp_msr in importance_measures) {

    # step through resample folds
    imps = pmap_dtr(tab[, c("task", "learner", "resampling",
      "iteration", "prediction"), with = FALSE], function(task,
        learner, resampling, iteration, prediction) {
        get_single_importance(imp_msr, task, learner, train_set = resampling$train_set(iteration),
          prediction)
      })

    # aggregate results (mean, variance, corrected variance acc. to Molnar et al. (2023), see above)
    imps_list[[imp_msr]] = aggregate_importance(imps, correction_factor)
  }

  imps_list
}

get_single_importance = function(importance_measure, task, learner, train_set, prediction) {

  test_ids = prediction$test$row_ids
  test_tsk = task$clone()$filter(test_ids)
  learner$state$train_task = task

  if (grepl("pfi", importance_measure)) {
    loss = gsub("pfi.", "", importance_measure)
    importance_measure = "pfi"
  }

  switch(importance_measure,
    "pdp" = get_pdp_importance(learner, test_tsk),
    "pfi" = get_pfi_importance(learner, test_tsk, loss),
    "shap" = get_shap_importance(learner, test_tsk))
}


get_pdp_importance = function(learner, test_tsk) {
  # based on Greenwell et al. (2018)
  if (!requireNamespace("iml", quietly = TRUE)) {
    stop("Package 'iml' needed for this function to work. Please install it.", call. = FALSE)
  }
  pred = iml::Predictor$new(model = learner, data = test_tsk$data(),
    y = test_tsk$target_names)
  pdp = iml::FeatureEffects$new(predictor = pred, method = "pdp")
  # <FIXME:> multiclass probabilities --> how to approach this?
  # currently just variance over all values simultaneously
  imp = lapply(pdp$results, FUN = function(dt) {
    dt = as.data.table(dt)
    if (learner$task_type == "regr") {
      dt[, stats::sd(.value)]
    } else if (learner$task_type == "classif") {
      sum(dt[, stats::sd(.value), by = .class]$V1)
    }
  })
  data.table(feature = names(pdp$results),
    importance = as.vector(unlist(imp), "numeric"))
}


get_pfi_importance = function(learner, test_tsk, loss) {
  # based on Breiman (2001) and Fisher et al. (2019)
  if (!requireNamespace("iml", quietly = TRUE)) {
    stop("Package 'iml' needed for this function to work. Please install it.", call. = FALSE)
  }
  pred = iml::Predictor$new(model = learner, data = test_tsk$data(),
    y = test_tsk$target_names)
  imp = iml::FeatureImp$new(predictor = pred, loss = loss)$results[, c("feature", "importance")]
}

get_shap_importance = function(learner, test_tsk, loss) {
  # based on Lundberg and Lee (2017)
  if (!requireNamespace("fastshap", quietly = TRUE)) {
    stop("Package 'fastshap' needed for this measuring importance. Please install it.", call. = FALSE)
  }
  if (learner$task_type == "regr") {
    pfun = function(object, newdata) {
      object$predict_newdata(newdata)[["response"]]
    }
    shap = fastshap::explain(learner, X = test_tsk$data(),
      pred_wrapper = pfun, newdata = test_tsk$data(),
      nsim = 10)
    imp = colMeans(abs(shap))
  } else if (learner$task_type == "classif") {
    temp = sapply(learner$state$train_task$class_names, function(reference) {
      pfun = function(object, newdata) {
        if (object$predict_type == "response") {
          stop("Importance measure 'shap' requires a learner with `predict_type = 'prob'")
        } else{
          object$predict_newdata(newdata)$prob[, reference]
        }
      }
      shap = fastshap::explain(learner, X = test_tsk$data(),
        pred_wrapper = pfun, newdata = test_tsk$data(),
        nsim = 10)
      colMeans(abs(shap))
    })
    # Sum aggregated shap values over outcome classes
    # --> inspired by shap python module output for `shap.summary_plot(plot_type = "bar")`
    # --> See: https://towardsdatascience.com/explainable-ai-xai-with-shap-multi-class-classification-problem-64dd30f97cea
    imp = rowSums(temp)
  }
  data.table(feature = names(imp),
    importance = as.vector(unlist(imp), "numeric"))
}

aggregate_importance = function(dt, correction_factor) {
  # based on Molnar et al. (2023), p. 468-469
  mm = dt[, mean(importance), by = feature]
  setnames(mm, "V1", "mean")
  vardt = dt[, stats::var(importance), by = feature]
  vardt$corrvar = correction_factor * vardt$V1
  setnames(vardt, "V1", "var")
  merge(mm, vardt, by = "feature")
}
