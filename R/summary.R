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

  if (!"GraphLearner" %in% class(object)) {
      ans[["model_type"]] = object$id
  }

  ### performance only if hold-out data available!
  ## <FIXME:> also allow extra data???
  if (!is.null(resample_result)) {

    ## ResampleResult info
    ans[["resample_info"]] = paste(resample_result$resampling$id, "with",
      as_short_string(resample_result$resampling$param_set$values, 1000L))
    ## <FIXME:> input checks
    ## - resample_result model must match object/model type!!
    ## - also trained on same task!
    ## residuals
    res = resample_result$prediction()
    if (tt == "regr") {
      ans[["residuals"]] = res[["truth"]] - res[["response"]]
    } else if (tt == "classif") {
      if (object$predict_type == "response") {
        ans[["confusion_matrix"]] = resample_result$prediction()$confusion
      } else {
        truth = as.character(res$truth)
        res = res$data$prob
        rs = vector(length = nrow(res))
        for (i in 1:nrow(res)) {
          rs[i] = 1 - res[i, truth[i]]
        }
        ans[["residuals"]] = rs
      }
    }


    ## performance
    pf = resample_result$aggregate(measures = control$measures)
    sc = resample_result$score(measures = control$measures)
    nam_multimeas = names(sc)[grep(tt, names(sc))]
    sc = sc[, nam_multimeas, with = FALSE]
    stdt = apply(sc, MARGIN = 2L, stats::sd)

    ans = c(ans, list(
      performance = pf,
      performance_sd = stdt))
  }

  # convert list to summary.Learner such that right printer is called
  class(ans) = "summary.Learner"
  ans
}

#' @export
summary.GraphLearner = function(object, resample_result = NULL, control = summary_control(), ...) {

  # input checks
  ## <FIXME:> to add

  # get all info as Learner
  ans = NextMethod()

  # pipeline
  arr = "  ->  "
  if (inherits(object, "GraphLearner")) {
    if(all(!duplicated(object$graph$edges[["src_id"]]))) {
      ppunit = paste0(object$graph$ids(), collapse = arr)
    } else {
      ppunit = "<SUPPRESSED>"
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
summary_control = function(
  measures = NULL,
  importance_measures = "pdp",
  n_important = 50L
  ) {

  # input checks
  if (!is.null(measures)) {
    measures = as_measures(measures)
  }
  mlr3::assert_measures(measures)
  importance_measures = match.arg(importance_measures)
  checkmate::assert_choice(importance_measures, c("pdp", "pfi", "loco"), null.ok = TRUE)
  checkmate::assert_int(n_important, lower = 1L, null.ok = TRUE)

  # create list
  list(measures = measures, importance_measures = importance_measures,
    n_important = n_important)

}

#' @export
print.summary.Learner = function(x, digits = max(3L, getOption("digits") - 3L), ...) {

  catn("Task type: ", x$task_type)
  catn("Feature names: ", paste(x$feature_names, collapse = ", "))

  if (!is.null(x$model_type)) {
    catn("Model type: ", x$model_type)
  }

  if (!is.null(x$pipeline)) {
    catn("Pipeline:\n", x$pipeline)
  }

  if (!is.null(x$resample_info)) {
    catn("Resampling: ", x$resample_info)
  }

  if (!is.null(x$residuals)) {
    cat("\n")
    catn("Residuals:")
    resid = x$residuals
    nam = c("Min", "1Q", "Median", "3Q", "Max")
    zz = zapsmall(stats::quantile(resid), digits + 1L)
    rq = structure(zz, names = nam)
    print(rq, digits = digits, ...)
  }

  if (!is.null(x$confusion_matrix)) {
    cat("\n")
    catn("Confusion matrix:")
    max_cols = 8L
    browser()
    if (ncol(x$confusion_matrix) >= max_cols) {
      conf = x$confusion_matrix[1:(max_cols + 1), 1:(max_cols + 1)]
      conf[max_cols+1,] = "..."
      conf[,max_cols+1] = "..."
      rownames(conf)[max_cols + 1] = "..."
      colnames(conf)[max_cols + 1] = "..."
    } else {
     print(x$confusion_matrix)
    }
  }

  if (!is.null(x$performance)) {
    cat("\n")
    namp = names(x$performance)
    # namp = sub(".*\\.", "", names(x$performance))
    # namp = paste(toupper(substr(namp, 1, 1)), substr(namp, 2, nchar(namp)), sep="")
    catn("Performance [sd]:")
    cat(paste0(namp, ": ",
      round(x$performance, digits),
      " [",
      round(x$performance_sd, digits),
      "]", collapse = "\n"))
  }
  invisible(x)
}
