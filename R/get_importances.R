get_importances = function(resample_result, importance_measures) {
  imps = c()
  models = resample_result$learners
  num_models = length(models)
  dt_row_ids = resample_result$task$row_ids
  for (i in 1:num_models) {
    test_ids = setdiff(dt_row_ids, resample_result$resampling$train_set(i = i))
    assert_true(all(test_ids, resample_result$predictions()[[i]]$data$row_ids))
    test_dt = resample_result$task$clone()$filter(test_ids)$data()
    models[[i]]$state$train_task = resample_result$task
    if (any(c("pfi", "pdp") %in% importance_measures)) {
      if (!requireNamespace("iml", quietly = TRUE)) {
        stop("Package 'iml' needed for this function to work. Please install it.", call. = FALSE)
      }
      pred = iml::Predictor$new(model = models[[i]], data = test_dt,
        y = test_dt[, resample_result$task$target_names])
      pfiind = grepl("pfi", importance_measures)
      if (any(pfiind)) {
        pfis = importance_measures[pfiind]
        lses = gsub("pfi.", "", pfis)
        for (l in lses) {
          imp = iml::FeatureImp$new(predictor = pred, loss = l)$results
          imps[[paste0("pfi.", l)]] = data.table(rbind(imps[[paste0("pfi.", l)]], imp))
        }
      }
      if ("pdp" %in% importance_measures) {

        pdp = iml::FeatureEffects$new(predictor = pred, method = "pdp")
        imp = lapply(pdp$results, FUN = function(dt) stats::var(dt$.value))
        imp = data.table(feature = names(pdp$results),
          importance = as.vector(unlist(imp), "numeric"))
        imps[["pdp"]] = data.table(rbind(imps[["pdp"]], imp))
      }
    }
    models[[i]]$state$train_task = NULL
  }

  compute_importance_measures = function(dt) {
    mm = dt[, mean(importance), by = feature]
    setnames(mm, "V1", "mean")
    vardt = dt[, stats::var(importance), by = feature]
    corr = 1/num_models + length(test_ids)/(length(dt_row_ids) - length(test_ids))
    vardt$corrvar = corr * vardt$V1
    setnames(vardt, "V1", "var")
    merge(mm, vardt, by = "feature")
  }
  lapply(imps, compute_importance_measures)
}
