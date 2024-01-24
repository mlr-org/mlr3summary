#' @export
summary.Learner = function(object, resample_result = NULL, control = summary_control(), ...) {

  # input checks
  assert_learner(object)
  if (is.null(object$state$train_task)) {
    stopf("Learner '%s' has not been trained yet", object$id)
  }
  if (!is.null(resample_result)) {
    assert_resample_result(resample_result)
    # assert that store_model = TRUE
    if (is.null(resample_result$learners[[1]]$model)) {
      stop("resample_result does not contain trained models, ensure resample() was run with 'store_models = TRUE'")
    }
    # ensure underlying algo and task of object and resample_result match
    if (!(object$base_learner()$hash == resample_result$learner$base_learner()$hash)) {
      stop("Learning algorithm of object does not match algorithm used for resampling. Ensure equality.")

    }
    if (!all.equal(object$state$train_task$hash, resample_result$task$hash)) {
      stop("object and resample_result seem to be trained on differing tasks. Ensure equality.")
    }
  }
  assert_class(control, classes = "summary_control", null.ok = FALSE)

  # assignment to shorter names
  tt = object$task_type
  fn = object$state$train_task$feature_names

  ans = list(
    task_type = tt,
    feature_names = fn
  )

  if (!inherits(object, "GraphLearner")) {
    ans[["model_type"]] = paste(object$id, "with",
      as_short_string(object$param_set$values, 1000L))
  }

  ### performance only if hold-out data available!
  ## <FIXME:> also allow extra data???
  if (!is.null(resample_result)) {

    ## ResampleResult info
    ans[["resample_info"]] = paste(resample_result$resampling$id, "with",
      as_short_string(resample_result$resampling$param_set$values, 1000L))
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

    # Set default measures if no measures specified
    if (is.null(control$measures)) {
      control$measures = get_default_measures(task_type = object$task_type,
        properties = object$state$train_task$properties,
        predict_type = object$predict_type)
    }

    pf = resample_result$aggregate(measures = control$measures)
    sc = resample_result$score(measures = control$measures)
    nam_multimeas = names(sc)[grep(tt, names(sc))]
    sc = sc[, nam_multimeas, with = FALSE]
    stdt = apply(sc, MARGIN = 2L, stats::sd)

    ans = c(ans, list(
      performance = pf,
      performance_sd = stdt))
  }

  ans$control = control
  # convert list to summary.Learner such that right printer is called
  class(ans) = "summary.Learner"
  ans
}

#' @export
summary.GraphLearner = function(object, resample_result = NULL, control = summary_control(), ...) {

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

  stop("object of type 'Graph' cannot be processed, convert 'Graph' to 'GraphLearner' via mlr3::as_learner() and retrain.")
  # # convert to GraphLearner and run summary
  # summary(as_learner(object), resample_result = resample_result, control = control, ...)
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
#' @param digits (numeric(1))\cr
#'   To Do.
#' @return [list]
#'
#' @export
summary_control = function(measures = NULL, importance_measures = "pdp", n_important = 15L, digits = max(3L, getOption("digits") - 3L)) {

  # input checks
  if (!is.null(measures)) {
    measures = as_measures(measures)
  }
  assert_measures(measures)
  importance_measures = match.arg(importance_measures)
  assert_choice(importance_measures, c("pdp", "pfi", "loco"), null.ok = TRUE)
  assert_int(n_important, lower = 1L, null.ok = TRUE)
  assert_int(digits, lower = 0L, null.ok = FALSE)

  # create list
  ctrlist = list(measures = measures, importance_measures = importance_measures,
    n_important = n_important, digits = digits)

  class(ctrlist) = "summary_control"
  ctrlist
}

#' @export
print.summary.Learner = function(x, digits = NULL, n_important = NULL, ...) {

  # input checks
  assert_int(digits, lower = 0L, null.ok = TRUE)
  assert_int(n_important, lower = 1L, null.ok = TRUE)

  if (!is.null(digits)) {
    x$control$digits = digits
    x$control$n_important = n_important
  }

  catn("Task type: ", x$task_type)

  catn(str_indent("Feature names: ", str_collapse(x$feature_names), exdent = 4L))

  if (!is.null(x$model_type)) {
    catn("Model type: ", x$model_type)
  }

  if (!is.null(x$pipeline)) {
    catn("Pipeline:\n", x$pipeline)
  }

  if (!is.null(x$resample_info)) {
    catn(str_indent("Resampling: ", x$resample_info, exdent = 4L))
  }

  if (!is.null(x$residuals)) {
    cat("\n")
    catn("Residuals:")
    resid = x$residuals
    zz = zapsmall(summary(resid), x$control$digits + 1L)
    nam = c("Min", "1Q", "Median", "Mean", "3Q", "Max")
    rq = structure(zz, names = nam)
    print(rq, digits = x$control$digits, ...)
  }

  if (!is.null(x$confusion_matrix)) {
    cat("\n")
    catn("Confusion matrix:")
    max_cols = 8L
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
    catn("Performance [sd]:")
    cat(paste0(namp, ": ",
      round(x$performance, x$control$digits),
      " [",
      round(x$performance_sd, x$control$digits),
      "]", collapse = "\n"))
  }
  invisible(x)
}
