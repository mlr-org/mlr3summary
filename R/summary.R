#' @export
summary.Learner = function(object, resample_result = NULL, control = summary_control(), ...) {
  # FIXME: assertions

  # assignment to shorter names
  tt = object$task_type
  fn = object$state$train_task$feature_names

  ans = list(
    task_type = tt,
    feature_names = fn
  )

  ### performance only if hold-out data available!
  ## <FIXME:> also allow extra data???
  if (!is.null(resample_result)) {
    ## residuals
    res = resample_result$prediction()
    if (tt == "regr") {
      rs = res[["truth"]] - res[["response"]]
    } else if (tt == "classif") {
      if (object$predict_type == "response") {
        rs = NULL
      } else {
        truth = as.character(res$truth)
        res = res$data$prob
        rs = vector(length = nrow(res))
        for (i in 1:nrow(res)) {
          rs[i] <- 1 - res[i, truth[i]]
        }
      }
    }

    ## performance
    pf = resample_result$aggregate(measures = control$measures)
    sc = resample_result$score(measures = control$measures)
    nam_multimeas = names(sc)[grep(tt, names(sc))]
    sc = sc[, nam_multimeas, with = FALSE]
    stdt = apply(sc, MARGIN = 2L, stats::sd)

    ## importance
    imps = c()
    models = resample_result$learners
    num_models = length(models)
    dt_row_ids = resample_result$task$row_ids
    for (i in 1:num_models) {
      test_ids = setdiff(dt_row_ids, resample_result$resampling$train_set(i = i))
      assert_true(all(test_ids, resample_result$predictions()[[i]]$data$row_ids))
      test_dt = resample_result$task$clone()$filter(test_ids)$data()
      models[[i]]$state$train_task = resample_result$task
      if (any(c("pfi", "pdp") %in% control$importance_measures)) {
        if (!requireNamespace("iml", quietly = TRUE)) {
          stop("Package 'iml' needed for this function to work. Please install it.", call. = FALSE)
        }
        pred = iml::Predictor$new(model = models[[i]], data = test_dt,
          y = test_dt[, resample_result$task$target_names])
        ## <FIXME:> allow more losses: currently available in IML,
        ## "ce", "f1", "logLoss", "mae", "mse", "rmse", "mape", "mdae", "msle",
        ## "percent_bias", "rae", "rmse", "rmsle", "rse", "rrse" and "smape"
        if ("pfi" %in% control$importance_measures) {
          imp = iml::FeatureImp$new(predictor = pred, loss = "mse")$results
          imps[["pfi"]] = data.table(rbind(imps[["pfi"]], imp))
        }
        if ("pdp" %in% control$importance_measures) {

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

    imps_res = lapply(imps, compute_importance_measures)

    ans = c(ans, list(
      residuals = rs,
      performance = pf,
      performance_sd = stdt,
      importance = imps_res,
      control = control)
    )
  }

  # convert list to summary.Learner such that right printer is called
  class(ans) <- "summary.Learner"

  ans
}

#' @export
summary.GraphLearner = function(object, resample_result = NULL, control = summary_control(), ...) {

  # input checks
  ## <FIXME:> to add
  ## <FIXME:> store_model must be set to true in resample_result!!

  # get all info as Learner
  ans = NextMethod()

  # pipeline
  arr = "  --->  "
  if (inherits(object, "GraphLearner")) {
    if(all(!duplicated(object$graph$edges[["src_id"]]))) {
      ppunit = paste0(object$graph$ids(), collapse = arr)
    } else {
      ppunit = "<complex>"
    }
  }
  pp = paste0(c("<INPUT>", ppunit, "<OUTPUT>"), collapse = arr)
  ans$pipeline = pp

  return(ans)
}


#' @export
summary.Graph = function(object, resample_result = NULL, control = summary_control(), ...) {

  # input checks
  ## <FIXME:> to add

  # convert to GraphLearner and run summary
  summary(mlr3::as_learner(object), resample_result = resample_result, control = control, ...)
}


#' @title Control for model summaries
#'
#' @description Various parameters that control aspect of the model summaries.
#'
#' @param measures ([mlr3::Measure] | list of [mlr3::Measure])\cr
#'   Measure(s) to calculate performance on.
#' @param importance_measures (character())\cr
#'   To Do.
#' @param n_important (numeric(1))\cr
#'   To Do.
#' @return [list]
#'
#' @export
summary_control = function(measures = NULL, importance_measures = "pdp",
  n_important = NULL) {

  # input checks
  if (!is.null(measures)) {
    measures = as_measures(measures)
  }
  mlr3::assert_measures(measures)
  importance_measures = match.arg(importance_measures, several.ok = TRUE)
  for (imp_measure in importance_measures) {
    checkmate::assert_choice(imp_measure, c("pdp", "pfi", "loco"), null.ok = TRUE)
  }
  checkmate::assert_int(n_important, lower = 1L, null.ok = TRUE)

  # create list
  list(measures = measures, importance_measures = importance_measures,
    n_important = n_important)

}

#' @export
print.summary.Learner = function(x, digits = max(3L, getOption("digits") - 3L), ...) {

  catn("Task type:", x$task_type)
  cat("\nFeature names:", paste(x$feature_names, collapse = ", "))

  if (!is.null(x$model_type)) {
    cat("\nModel type:", x$model_type)
  }

  if (!is.null(x$pipeline)) {
    cat("\nPipeline:\n", x$pipeline)
  }

  if (!is.null(x$residuals)) {
    cat("\n")
    cat("\nResiduals:\n")
    resid = x$residuals
    nam = c("Min", "1Q", "Median", "3Q", "Max")
    zz = zapsmall(stats::quantile(resid), digits + 1L)
    rq = structure(zz, names = nam)
    print(rq, digits = digits, ...)
    cat("Residual Standard Error:", round(stats::sd(x$residuals), digits))
  }

  if (!is.null(x$performance)) {
    cat("\n")
    namp = names(x$performance)
    # namp = sub(".*\\.", "", names(x$performance))
    # namp = paste(toupper(substr(namp, 1, 1)), substr(namp, 2, nchar(namp)), sep="")
    cat("\nPerformance [sd]:\n")
    cat(paste0(namp, ": ",
      round(x$performance, digits),
      " [",
      round(x$performance_sd, digits),
      "]", collapse = "\n"))
  }

  if (!is.null(x$importance)) {
    cat("\n")
    cat("\nImportances:\n")

    tquant = 1.96 #t(1-alpha)
    ## create imp [l, u]

    # featnams = as.data.frame(lapply(x$importance, function(dt) dt$feature))
    # assert_true(all(apply(featnams, MARGIN = 1, FUN = function(row) length(unique(row)) == 1)))

    featorder = x$importance[[1]][order(mean, decreasing = TRUE), feature]

    compute_imp_summary = function(imp) {
      imp[, mean := round(mean, digits)]
      imp[, lower := round(mean - tquant * sqrt(corrvar), digits)]
      imp[, upper := round(mean + tquant * sqrt(corrvar), digits)]
      imp[, res:=paste0(mean, " [", lower, ",", upper, "]")]
      imp[, c("feature", "res")]
    }

    rr = lapply(x$importance, compute_imp_summary)
    rr = Reduce(merge,rr)
    rr = rr[order(match(feature, featorder))]
    names(rr) = c("feature", names(x$importance))
    rownames(rr) = rr$feature
    rr[,feature:=NULL]
    col = names(x$importance)[[1]]

    if (!is.null(x$control$n_important) && nrow(rr) > x$control$n_important) {
      rr = rr[1:x$control$n_important,]
      featorder = featorder[1:x$control$n_important]
    }
    rr = as.matrix(rr, rownames = featorder)
    print.default(rr, quote = FALSE, right = TRUE, ...)

  }
  invisible(x)
}
