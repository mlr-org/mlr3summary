#' @export
summary.Learner = function(model, resample_result, control = summary_control(), ...) {
  # input checks

  # create information list

  ## task type
  tt = model$task_type

  ## feature names
  fn = model$state$train_task$feature_names

  ## pipeline
  arr = "  --->  "
  if ("GraphLearner" %in% class(model)) {
    if(all(!duplicated(model$graph$edges[["src_id"]]))) {
      ppunit = paste0(model$graph$ids(), collapse = arr)
    } else {
      ppunit = "<complex>"
    }
  } else {
    ppunit = model$base_learner()$id
  }
  pp = paste0(c("<INPUT>", ppunit, "<OUTPUT>"), collapse = arr)

  ## residuals
  # <FIXME:> add for class & regression
  res = resample_result$prediction()
  if (tt == "regr") {
    rs = res[["truth"]] - res[["response"]]
  } else if (tt == "classif") {
    if (model$predict_type == "response") {
      rs = as.numeric(res$truth == res$response)
    } else {
      # <FIXME:> to add
      truth = as.character(res$truth)
      res = res$data$prob
      rs = vector(length = nrow(res))
      for (i in 1:nrow(res)) {
          rs[i] <- 1 - res[i, truth[i]]
        }
    }
  } else {
    stop()
  }

  ## performance
  pf = resample_result$aggregate(measures = control$measures)
  sc = resample_result$score()
  ## <FIXME:> by name!
  sd = sd(sc[,ncol(sc)])

  ans = list(
    task_type = tt,
    feature_names = fn,
    pipeline = pp,
    residuals = rs,
    performance = pf,
    performance_sd = sd
  )

  # convert list to summary.Learner such that right printer is called
  class(ans) <- "summary.Learner"
  ans

}

importance_choices = c("pdp", "pfi", "loco")

#' @export
summary_control = function(measures = NULL, importance_measures = importance_choices,
  n_important = 50L) {


  # input checks
  checkmate::assert_class(measures, "Measure",null.ok = TRUE)
  importance_measures = match.arg(importance_measures)
  checkmate::assert_choice(importance_measures, importance_choices, null.ok = TRUE)
  checkmate::assert_int(n_important, lower = 1L, null.ok = TRUE)

  # create list
  list(measures = measures, importance_measures = importance_measures,
    n_important = n_important)

}

#' @export
print.summary.Learner = function(x, digits = max(3L, getOption("digits") - 3L), ...)
{

  cat("\nTask type:", x$task_type)
  cat("\nFeature names:", paste(x$feature_names, collapse = ", "))
  cat("\nPipeline:\n", x$pipeline)

  cat("\n")
  cat("\nResiduals:\n")
  resid <- x$residuals
  nam <- c("Min", "1Q", "Median", "3Q", "Max")
  zz = zapsmall(quantile(resid), digits + 1L)
  rq = structure(zz, names = nam)
  print(rq, digits = digits, ...)
  cat("Residual Standard Error:", round(sd(x$resid), digits))

  ## <FIXME:> allow multiple performance measures!
  cat("\n")
  namp = sub(".*\\.", "", names(x$performance))
  namp = paste(toupper(substr(namp, 1, 1)), substr(namp, 2, nchar(namp)), sep="")
  cat(paste0("\nPerformance [sd]\n", namp, ": ",
    paste0(
      round(x$performance, digits),
      " [",
      round(x$performance_sd, digits),
      "]")))


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
  cat("\n")
  invisible(x)
}
