#' @export
summary.Learner = function(object, resample_result = NULL, control = summary_control(), ...) {

  # input checks
  ## <FIXME:> to add

  ans = list()

  # create information list

  ## task type
  tt = object$task_type
  ans[["task_type"]] = tt

  ## feature names
  fn = object$state$train_task$feature_names
  ans[["feature_names"]] = fn

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
    sc = data.table::data.table(sc[, nam_multimeas, with = FALSE])
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
      if (any(c("pfi", "pdp") %in% control$importance_measures)) {
        require("iml")
        pred = iml::Predictor$new(model = models[[i]]$model, data = test_dt,
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
          imp = lapply(pdp$results, FUN = function(dt) var(dt$.value))
          imp = data.table(feature = names(pdp$results),
            importance = as.vector(unlist(imp), "numeric"))
          imps[["pdp"]] = data.table(rbind(imps[["pdp"]], imp))
        }
      }
    }
    browser()
    compute_importance_measures = function(dt) {
      mm = dt[, mean(importance), by = feature]
      setnames(mm, "V1", "mean")
      var = dt[, var(importance), by = feature]
      corr = 1/num_models + length(test_ids)/(length(dt_row_ids) - length(test_ids))
      var$corrvar = corr * var$V1
      setnames(var, "V1", "var")
      merge(mm, var, by = "feature")
    }

    imps_res = lapply(imps, compute_importance_measures)

    ans = c(ans, list(
      residuals = rs,
      performance = pf,
      performance_sd = stdt,
      importance = imps_res)
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

  # get all info as Learner
  ans = summary.Learner(object = object, resample_result = resample_result, control = control, ...)

  # pipeline
  arr = "  --->  "
  if ("GraphLearner" %in% class(object)) {
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


importance_choices = c("pdp", "pfi", "loco")

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
summary_control = function(measures = NULL, importance_measures = importance_choices,
  n_important = 50L) {

  # input checks
  if (!is.null(measures)) {
    measures = mlr3::as_measures(measures)
  }
  mlr3::assert_measures(measures)
  importance_measures = match.arg(importance_measures, several.ok = TRUE)
  for (imp_measure in importance_measures) {
    checkmate::assert_choice(imp_measure, importance_choices, null.ok = TRUE)
  }
  checkmate::assert_int(n_important, lower = 1L, null.ok = TRUE)

  # create list
  list(measures = measures, importance_measures = importance_measures,
    n_important = n_important)

}

#' @export
print.summary.Learner = function(x, digits = max(3L, getOption("digits") - 3L), ...) {

  cat("\nTask type:", x$task_type)
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
    resid <- x$residuals
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
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

  browser()

  if (!is.null(x$importance)) {
    cat("\nImportances:\n")

    tquant = 1.96 #t(1-alpha)
    ## create imp [l, u]

    compute_imp_summary = function(imp) {
      ## <FIXME:> order by first imp measure
      imp[, mean := round(mean, digits)]
      imp[, lower := round(mean - tquant * sqrt(corrvar), digits)]
      imp[, upper := round(mean + tquant * sqrt(corrvar), digits)]
      imp[, res:=paste0(mean, " [", lower, ",", upper, "]")]
      imp[, c("feature", "res")]
    }

    # rr = mapply(function(imp, nam) {
    #   res = compute_imp_summary(imp)
    #   setnames(res, "res", nam)
    # }, x$importance, names(x$importance))
    #

    rr = lapply(x$importance, compute_imp_summary)
    rr = Reduce(merge,rr)
    names(rr) = c("feature", names(x$importance))
    col = names(x$importance)[[1]]

    ## <FIXME:> nice print out


    # cbind(Estimate = est, `Std. Error` = se,
    #   `t value` = tval, `Pr(>|t|)` = 2 * pt(abs(tval), rdf,
    #     lower.tail = FALSE))

    # Browse[2]> coefs
    # Estimate Std. Error  t value     Pr(>|t|)
    # (Intercept)    5.032  0.2202177 22.85012 9.547128e-15
    # groupTrt      -0.371  0.3114349 -1.19126 2.490232e-01
    #
    # printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
    #   na.print = "NA", ...)
  }

  ### Copied from summary.lm()
  # else {
  #   cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
  #   cat("\n")
  # }

  #   else cat("\nCoefficients:\n")
  #   coefs <- x$coefficients
  #   if (any(aliased <- x$aliased)) {
  #     cn <- names(aliased)
  #     coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn,
  #       colnames(coefs)))
  #     coefs[!aliased, ] <- x$coefficients
  #   }
  #   printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
  #     na.print = "NA", ...)
  # }
  # cat("\nResidual standard error:", format(signif(x$sigma,
  #   digits)), "on", rdf, "degrees of freedom")
  # cat("\n")
  # if (nzchar(mess <- naprint(x$na.action)))
  #   cat("  (", mess, ")\n", sep = "")
  # if (!is.null(x$fstatistic)) {
  #   cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
  #   cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared,
  #     digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L],
  #       digits = digits), "on", x$fstatistic[2L], "and",
  #     x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L],
  #       x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE),
  #       digits = digits))
  #   cat("\n")
  # }
  # correl <- x$correlation
  # if (!is.null(correl)) {
  #   p <- NCOL(correl)
  #   if (p > 1L) {
  #     cat("\nCorrelation of Coefficients:\n")
  #     if (is.logical(symbolic.cor) && symbolic.cor) {
  #       print(symnum(correl, abbr.colnames = NULL))
  #     }
  #     else {
  #       correl <- format(round(correl, 2), nsmall = 2,
  #         digits = digits)
  #       correl[!lower.tri(correl)] <- ""
  #       print(correl[-1, -p, drop = FALSE], quote = FALSE)
  #     }
  #   }
  # }
  # cat("\n")
  invisible(x)
}
