#' Learner and model summaries
#'
#' @param object (`Learner`)
#'  To Do.
#' @param resample_result (`Resampling`)
#'  To Do.
#' @param control (`summary_control`)
#'  To Do.
#'
#' @param ... (any)
#'  To Do.
#'
#' @return summary.Learner list
#'
#' @references
#' `r format_bib("greenwell_simple_2018")`
#'
#' `r format_bib("fisher_all_2019")`
#'
#' `r format_bib("molnar_relating_2023")`
#'
#' `r format_bib("breiman_leo_random_2001")`
#'
#' @importFrom stats sd
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
  tn = object$state$train_task$target_names
  fn = object$state$train_task$feature_names

  ans = list(
    task_type = tt,
    target_name = tn,
    feature_names = fn
  )

  if (tt == "classif") {
    ans$classes = object$state$train_task$col_info[id == tn,]$levels[[1]]
  }

  if (!inherits(object, "GraphLearner")) {
    params = object$param_set$values
    if (length(params) > 0) {
      ans[["model_type"]] = paste(object$id, "with",
        as_short_string(object$param_set$values, 1000L))
    } else {
      ans[["model_type"]] = object$id
    }
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

    control$measures = map(control$measures, function(pmsr) {
      pmsr = pmsr$clone()
      if (is.na(pmsr$minimize)) {
        arrow = ""
      } else if (pmsr$minimize) {
        arrow = cli::symbol[["arrow_down"]]
      } else {
        arrow = cli::symbol[["arrow_up"]]
      }
      pmsr$id = sprintf("%s%s (%s)", arrow, pmsr$id, pmsr$average)
      pmsr
    })
    pf = resample_result$aggregate(measures = control$measures)
    sc = resample_result$score(measures = control$measures)
    nam_multimeas = names(sc)[grep(tt, names(sc))]
    sc = sc[, nam_multimeas, with = FALSE]
    stdt = map_dbl(sc, stats::sd)

    ## importance
    ## <FIXME:> This should be rather exported into own R6 classes??
    if (!is.null(control$importance_measures)) {
      imps_res = get_importances(resample_result, control$importance_measures)
      ans$importances = imps_res
    }

    ## effects
    if (!is.null(control$effect_measures)) {
      effs_res = get_effects(resample_result, control$effect_measures)
      ans$effects = effs_res
    }

    if (!is.null(control$complexity_measures)) {
      comp_res = get_complexity(resample_result, control$complexity_measures)

      ans$complexity = comp_res
    }

    ans = c(ans, list(
      performance = pf,
      performance_sd = stdt,
      n_iters = resample_result$iters,
      control = control)
    )
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
#' @param effect_measures (character)\cr
#'   To Do.
#' @param digits (numeric(1))\cr
#'   To Do.
#' @return [list]
#'
#' @export
summary_control = function(measures = NULL, importance_measures = "pdp", n_important = 15L, effect_measures = c("pdp", "ale"), complexity_measures = c("sparsity", "interaction_strength"), digits = max(3L, getOption("digits") - 3L)) {

  # input checks
  if (!is.null(measures)) {
    measures = as_measures(measures)
  }
  assert_measures(measures)

  iml_pfi_losses = c("ce", "f1", "logLoss", "mae", "mse", "rmse", "mape", "mdae",
    "msle", "percent_bias", "rae", "rmse", "rmsle", "rse", "rrse", "smape")
  for (imp_measure in importance_measures) {
    assert_choice(imp_measure, c("pdp", "shap", paste("pfi", iml_pfi_losses, sep = ".")), null.ok = TRUE)
  }
  assert_int(n_important, lower = 1L, null.ok = TRUE)
  for (eff_measure in effect_measures) {
    assert_choice(eff_measure, c("pdp", "ale"))
  }
  assert_int(digits, lower = 0L, null.ok = FALSE)

  # create list
  ctrlist = list(measures = measures, importance_measures = importance_measures,
    n_important = n_important, effect_measures = effect_measures,
    complexity_measures = complexity_measures, digits = digits)

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
  }

  if (!is.null(n_important)) {
    x$control$n_important = n_important
  }

  cli_div(theme = list(.val = list(digits = x$control$digits)))
  cli_h1("General")
  cli_text("Task type: {x$task_type}")
  if (!is.null(x$classes)) {
    tn = cli_vec(x$classes, list("vec-trunc" = 15))
    cli_text("Target name: {x$target_name} ({tn})")
  } else {
    cli_text("Target name: {x$target_name}")
  }
  fn = cli_vec(x$feature_names, list("vec-trunc" = 15))
  cli_text("Feature names: {fn}")

  if (!is.null(x$model_type)) {
    cli_text("Model type: {x$model_type}")
  }

  if (!is.null(x$pipeline)) {
    cli_text("Pipeline: {x$pipeline}")
  }

  if (!is.null(x$resample_info)) {
    cli_text("Resampling: {x$resample_info}")
  }

  if (!is.null(x$residuals)) {
    cli_h1("Residuals")
    zz = zapsmall(summary(x$residuals), x$control$digits + 1L)
    nam = c("Min", "1Q", "Median", "Mean", "3Q", "Max")
    rq = cli_vec(structure(zz, names = nam))
    print(rq)
  }

  if (!is.null(x$confusion_matrix)) {
    cli_h1("Confusion matrix")
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
    cli_h1("Performance [sd]")
    namp = structure(paste0(round(x$performance, x$control$digits),
      " [", round(x$performance_sd, x$control$digits), "]"),
      names = names(x$performance))
    names(namp) = paste0(names(namp), ":")
    perf = as.matrix(namp)
    colnames(perf) = ""
    print.default(perf, quote = FALSE, right = FALSE, ...)

  }

  if (!is.null(x$complexity)) {
    cli_h1("Complexity [sd]")
    aggregate_complexity = function(com) {
      paste0(round(mean(com), x$control$digits),
        " [", round(sd(com), x$control$digits), "]")
    }
    rr = map(x$complexity, aggregate_complexity)
    res =  Reduce(merge, rr)
    com = as.matrix(t(res))
    rownames(com) = paste0(names(rr), ":")
    colnames(com) = ""
    print.default(com, quote = FALSE, right = TRUE, ...)
  }


  if (!is.null(x$importance)) {
    cli_h1("Importance [sd]")

    featorder = x$importances[[1]][order(mean, decreasing = TRUE), feature]

    compute_imp_summary = function(imp) {
      imp[, "res" := paste0(round(mean, x$control$digits), " [",
        round(sd, x$control$digits), "]")]
      imp[, c("feature", "res")]
    }

    rr = map(x$importance, compute_imp_summary)
    rr = Reduce(merge,rr)
    rr = rr[order(match(feature, featorder))]
    names(rr) = c("feature", names(x$importances))
    rownames(rr) = rr$feature
    rr[,feature:=NULL]
    col = names(x$importances)[[1]]

    if (!is.null(x$control$n_important) && nrow(rr) > x$control$n_important) {
      rr = rr[1:x$control$n_important,]
      featorder = featorder[1:x$control$n_important]
    }
    rr = as.matrix(rr, rownames = featorder)
    print.default(rr, quote = FALSE, right = FALSE, ...)

  }


  if (!is.null(x$effects)) {
    # Size of effect plots are derived based on ALE/PDP curves
    scale_values = function(x, range){(x - range[1])/(range[2] - range[1])*(7) + 1}
    get_effect_plot = function(x, range) {
      symb = map_chr(paste("lower_block", round(scale_values(x, range)), sep = "_"),
        function(s) symbol[[s]])
      return(paste0(symb, collapse = ""))
    }

    if (!is.null(x$importances)) {
      featorder = rownames(rr)
    } else {
      featorder = x$feature_names
    }
    effs = imap_dtc(x$effects, function(effs, nams) {
      range = range(effs$V1)
      effs = effs[feature %in% featorder]
      if (!is.null(effs$class)) {
        groupvars = c("class", "feature")
      } else {
        groupvars = "feature"
      }
      res = effs[, get_effect_plot(round(V1, digits = x$control$digits), range), by = groupvars]
      if (!is.null(effs$class)) {
        res = res[order(class, match(feature, featorder))]
      } else {
        res = res[order(match(feature, featorder))]
      }
      res
    })

    effs = effs[ , which( !duplicated(t(effs))) , with = FALSE]

    cli_h1("Effects")
    names(effs)[grepl("class", colnames(effs))] = "class"
    names(effs)[grepl("feature", colnames(effs))] = "feature"
    names(effs) = gsub("\\.V1", "", names(effs))

    # if multi-class, effects for each class are displayed one below the other
    if (!is.null(effs$class)) {
      effs = split(effs, effs$class)
      for (i in 1:length(effs)) {
        cat("\n")
        cli_h2(names(effs[i]))
        ef = copy(effs[[i]])
        ef = ef[order(match(feature, featorder))]
        ef = ef[, !c("class", "feature")]
        ef = as.matrix(ef, rownames = featorder)
        print(unclass(ef), quote = FALSE, right = FALSE, ...)
      }

    } else {
      effs$feature = NULL
      ef = as.matrix(effs, rownames = featorder)
      print(unclass(ef), quote = FALSE, right = FALSE, ...)
    }
  }

  invisible(x)
}
