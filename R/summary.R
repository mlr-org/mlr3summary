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

    ans = c(ans, list(
      residuals = rs,
      performance = pf,
      performance_sd = stdt))
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

  catn("Task type:", x$task_type)
  cat("\nFeature names:", paste(x$feature_names, collapse = ", "))

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


  ### Copied from summary.lm()
  # else {
  #   cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
  #   cat("\n")
  # }
  # if (length(x$aliased) == 0L) {
  #   cat("\nNo Coefficients\n")
  # }
  # else {
  #   if (nsingular <- df[3L] - df[1L])
  #     cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n",
  #       sep = "")
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
