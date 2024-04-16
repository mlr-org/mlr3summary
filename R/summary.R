#' @rdname summary.Learner
#' @name summary.Learner
#' @aliases summary.Learner
#' @aliases summary.GraphLearner
#' @aliases print.summary.Learner
#' @title Summarizing mlr3 Learners
#'
#' @description
#' summary method for class `Learner`.
#' The output could be tailored via the `control` argument, see [summary_control].
#'
#' @details
#'
#' This function can be parallelized with the \CRANpkg{future} package.
#' One job is one resampling iteration, and all jobs are send to an apply function
#' from \CRANpkg{future.apply} in a single batch.
#' To select a parallel backend, use [future::plan()].
#'
#'
#' @param object ([mlr3::Learner])\cr
#'  trained model of class `Learner`.
#' @param x (`summary.Learner`)\cr
#'  an object of class "summary.Learner", usually a result of a call to `summary.Learner`.
#' @param resample_result ([mlr3::ResampleResult])\cr
#'  outcome of `resample`. If  NULL (default), no residuals, performances, etc.
#'  are derived.
#' @param control (`summary_control`)\cr
#'  a list with control parameters, see `summary_control`.
#' @param digits (numeric(1))\cr
#'   the number of digits to use when printing.
#' @param n_important (numeric(1))\cr
#'   number of important variables to be displayed.
#'   If NULL, `x$control$n_important` is used.
#' @param ... (any)\cr
#'  further arguments passed to or from other methods.
#'
#' @return summary.Learner returns an object of class "summary.Learner", a [list] with the following entries.
#' \itemize{
#' \item{task_type: }{The type of task, either `classif` (classification) or `regr` (regression).}
#' \item{target_name: }{The name of the target variable.}
#' \item{feature_names: }{The names of the features.}
#' \item{classes: }{The classes of the target variable. NULL if regression task.}
#' \item{resample_info: }{Information on the resample objects, strategy type and hyperparameters.}
#' \item{residuals: }{Vector of hold-out residuals over the resampling iterations of `resample_result`.
#' For regression models, residuals are difference between true and predicted outcome.
#' For classifiers with probabilities, the residuals are the difference
#' between predicted probabilities and a one-hot-encoding of the true class.
#' For hard-label classifier `confusion_matrix` is given instead of `residuals`.}
#' \item{confusion_matrix: }{Confusion matrix of predicted vs. true classes.
#'      Alternative to `residuals`, in case of hard-label classification.}
#' \item{performance: }{Vector of aggregated performance measures over the iterations of `resample_result`.
#'      The arrows display whether lower or higher values are better.
#'      (micro/macro) displays whether it is a micro or macro measure.
#'      For macro aggregation measures are computed
#'      for each iteration separately before averaging.
#'      For micro, measures are computed across all iterations.
#'      See Bischl et al. (2024), for details.}
#' \item{performance_sd: }{Vector of standard deviations of performance measures
#'       over the iterations of `resample_result`.
#'       The arrows display whether lower or higher values are better.
#'      (micro/macro) displays whether it is a micro or macro measure.}
#' \item{fairness: }{Vector of aggregated fairness measures over the iterations of `resample_result`.
#'      The arrows display whether lower or higher values are better.
#'      (micro/macro) displays whether it is a micro or macro measure.}
#' \item{fairness_sd: }{Vector of standard deviations of fairness measures
#'       over the iterations of `resample_result`.
#'       The arrows display whether lower or higher values are better.
#'      (micro/macro) displays whether it is a micro or macro measure (see details above).}
#' \item{importances: }{List of `data.table` that display the feature importances
#'      per importance measure. Given are the means and standard deviations (sd)
#'      over the resampling iterations of `resample_result`.}
#' \item{effects: }{List of `data.table` that display the feature effects
#'      per effect method. Given are the mean effects
#'      over the resampling iterations of `resample_result` for a maximum of
#'      5 grid points. For binary classifiers, effects are only displayed for
#'      the positively-labeled class.
#'      For multi-class, effect plots are displayed separately for each class.
#'      For categorical features, the factor levels of the feature determine
#'      the ordering of the bars.}
#' \item{complexity: }{List of vectors that display the complexity values
#'      per complexity measure for each resampling iteration.}
#' \item{control: }{[summary_control] used as an input for `summary.Learner`.}
#' }
#'
#' For details on the performance measures, complexity measures, feature
#' importance and feature effect methods, see [summary_control].
#'
#' @references
#' `r format_bib("mlr3book")`
#'
#' @examples
#' if (require("mlr3")) {
#'   tsk_iris = tsk("iris")
#'   lrn_rpart =  lrn("classif.rpart", predict_type = "prob")
#'   lrn_rpart$train(task = tsk_iris)
#'   rsmp_cv3 = rsmp("cv", folds = 3L)
#'   rr = resample(tsk_iris, lrn_rpart, rsmp_cv3, store_model = TRUE)
#'   summary(lrn_rpart, rr)
#' }
#' @importFrom stats sd
#' @importFrom stats setNames
#' @importFrom stats var
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
    if (!inherits(object, "AutoTuner")) {
      if (!(object$base_learner()$hash == resample_result$learner$base_learner()$hash)) {
        stop("Learning algorithm of object does not match algorithm used for resampling. Ensure equality.")
      }
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
    if (length(params)) {
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

    ans$performance = pf
    ans$performance_sd = stdt

    # <FIXME:> currently only binary classification metrics available:
      if (!is.null(control$protected_attribute) || length(object$state$train_task$col_roles$pta)) {
        if (is.null(control$fairness_measures)) {
          control$fairness_measures = get_default_fairness_measures(task_type = object$task_type,
            properties = object$state$train_task$properties,
            predict_type = object$predict_type)
        }
        # deep clone required, otherwise hash differs of task in object and task in resample_result
        if (!is.null(control$protected_attribute)) {
          resample_result$task$set_col_roles(control$protected_attribute, add_to = "pta")
        }

        control$fairness_measures = map(control$fairness_measures, function(pmsr) {
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
        fair = resample_result$aggregate(measures = control$fairness_measures)
        fairscores = resample_result$score(measures = control$fairness_measures)
        nam_multimeas = names(fairscores)[grep("fairness", names(fairscores))]
        fairscores = fairscores[, nam_multimeas, with = FALSE]
        stdfair = map_dbl(fairscores, stats::sd)

        ans$fairness = fair
        ans$fairness_sd = stdfair

        if (!is.null(control$protected_attribute)) {
          resample_result$task$set_col_roles(control$protected_attribute, remove_from = "pta")
        }
        if (length(resample_result$task$col_roles$pta)) {
          control$protected_attribute = resample_result$task$col_roles$pta
        }
      }

    ## importance
    ## <FIXME:> This should be rather exported into own R6 classes??

    if (is.null(control$importance_measures)) {
      control$importance_measures = get_default_importances(
        task_type = object$task_type, ...)
    }

    if (!is.null(control$importance_measures)) {
      imps_res = get_importances(resample_result, control$importance_measures)
      ans$importances = imps_res
    }

    ## effects
    if (!is.null(control$effect_measures)) {
      effs_res = get_effects(resample_result, control$effect_measures)
      ans$effects = effs_res
    }


      # <FIXME:> remove interaction_strength if multi_class
      if ("interaction_strength" %in% control$complexity_measures) {
        multi_class = object$state$train_task$task_type == "classif" &&
          object$state$train_task$properties == "multiclass"
        if (multi_class) {
          control$complexity_measures = setdiff(control$complexity_measures, "interaction_strength")
              messagef("complexity measure 'interaction_strenght' is ignored because it does not work for multiClass")
        }
      }
    if (!is.null(control$complexity_measures) & length(control$complexity_measures)) {
      comp_res = get_complexity(resample_result, control$complexity_measures)
      ans$complexity = comp_res
    }

    ans = c(ans, list(
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
#' @rdname summary.Learner
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


#' @title Control for Learner summaries
#'
#' @description Various parameters that control aspects of `summary.Learner`.
#'
#' @details
#'
#' The following provides some details on the different paragraphs in the summary output.
#'
#' \strong{Performance}
#' The default performance measures depend on the type of task. Therefore, NULL is displayed and
#' the measures will be initialized in `summary.Learner` with the help of `mlr3::msr`.
#' The following provides an overview:
#' \itemize{
#'  \item{Regression: }{\link[mlr3::mlr_measures_regr.rmse]{"regr.rmse"},
#'  \link[mlr3::mlr_measures_regr.rsq]{"regr.rsq"},
#'  \link[mlr3::mlr_measures_regr.mae]{"regr.mae"},
#'  \link[mlr3::mlr_measures_regr.medae]{"regr.medae"}}
#'  \item{Binary classification with probabilities: }{
#'  \link[mlr3::mlr_measures_classif.auc]{"classif.auc"},
#'  \link[mlr3::mlr_measures_classif.fbeta]{"classif.fbeta"},
#'  \link[mlr3::mlr_measures_classif.bbrier]{"classif.bbrier"},
#'  \link[mlr3::mlr_measures_classif.mcc]{"classif.mcc"}}
#'  \item{Binary classification with hard labels: }{
#'  \link[mlr3::mlr_measures_classif.acc]{"classif.acc"},
#'  \link[mlr3::mlr_measures_classif.bacc]{"classif.bacc"},
#'  \link[mlr3::mlr_measures_classif.fbeta]{"classif.fbeta"},
#'  \link[mlr3::mlr_measures_classif.mcc]{"classif.mcc"}}

#'  \item{Multi-class classification with probabilities: }{
#'  \link[mlr3::mlr_measures_classif.mauc_aunp]{"classif.mauc_aunp"},
#'  \link[mlr3::mlr_measures_classif.mbrier]{"classif.mbrier"}}
#'
#' }
#'
#'
#' @param measures ([mlr3::Measure] | list of [mlr3::Measure] | NULL)\cr
#'   measure(s) to calculate performance on. If NULL (default), a set of
#'   selected measures are calculated (choice depends on Learner type (classif vs. regr)).
#' @param complexity_measures (character)\cr
#'   vector of complexity measures. Possible choices are "sparsity" (the number
#'   of used features) and "interaction_strength" (see Molnar et al. (2020)).
#'   Both are the default.
#' @param importance_measures (character()|NULL)\cr
#'   vector of importance measure names. Possible choices are "pfi.<loss>"
#'   ([iml::FeatureImp]), "pdp" ([iml::FeatureEffects], see ) and
#'   "shap" ([fastshap::explain]). Default of NULL results in "pfi.<loss> and
#'   "pdp", where the <loss> depends on the Learner type (classif vs. regr).
#' @param n_important (numeric(1))\cr
#'   number of important variables to be displayed. Default is 15L.
#' @param effect_measures (character | NULL)\cr
#'   vector of effect method names. Possible choices are "pfi" and "ale"
#'   (see [iml::FeatureEffects]). Both are the default.
#' @param fairness_measures ([mlr3fairness::MeasureFairness] |
#' list of [mlr3fairness::MeasureFairness] | NULL)\cr
#'  measure(s) to assess fairness. If NULL (default), a set of
#'  selected measures are calculated (choice depends on Learner type (classif vs. regr)).
#' @param protected_attribute (character(1))\cr
#'  name of the binary feature that is used as a protected attribute.
#'  If no `protected_attribute` is specified (and also no `pta` feature is
#'  available in the `mlr3::Task` for training the `mlr3::Learner`),
#'  no fairness metrics are computed.
#' @param digits (numeric(1))\cr
#'   the number of digits to use when printing.
#' @return [list] of class `summary_control`
#' @references
#' `r format_bib("molnar_complexity_2020")`
#'
#' `r format_bib("greenwell_simple_2018")`
#' @export

summary_control = function(measures = NULL,
  complexity_measures = c("sparsity", "interaction_strength"),
  importance_measures = NULL, n_important = 15L,
  effect_measures = c("pdp", "ale"),
  fairness_measures = NULL, protected_attribute = NULL,
  digits = max(3L, getOption("digits") - 3L)) {

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
  for (comp_measure in complexity_measures) {
    assert_choice(comp_measure, c("sparsity", "interaction_strength"))
  }
  if (!is.null(fairness_measures)) {
    fairness_measures = as_measures(fairness_measures)
  }
  assert_measures(fairness_measures)
  assert_character(protected_attribute, null.ok = TRUE, len = 1L)
  assert_int(digits, lower = 0L, null.ok = FALSE)

  # create list
  ctrlist = list(measures = measures, complexity_measures = complexity_measures,
    importance_measures = importance_measures, n_important = n_important,
    effect_measures = effect_measures, fairness_measures = fairness_measures,
    protected_attribute = protected_attribute, digits = digits)

  class(ctrlist) = "summary_control"
  ctrlist
}

#' @export
#' @rdname summary.Learner
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

  if (!is.null(x$fairness)) {
    cli_h1("Fairness [sd]")
    cli_text("Protected attribute: {x$control$protected_attribute}")
    nampf = setNames(paste0(round(x$fairness, x$control$digits),
      " [", round(x$fairness_sd, x$control$digits), "]"),
      paste0(names(x$fairness), ":"))
    fair = as.matrix(nampf)
    colnames(fair) = ""
    print.default(fair, quote = FALSE, right = FALSE, ...)
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
