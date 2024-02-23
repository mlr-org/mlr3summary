### BASED ON https://github.com/mlr-org/mlr3fairness/blob/main/R/reports.R

#' Create a Model/Learner Report
#'
#' Creates a \CRANpkg{rmarkdown} template with a skeleton of reported metrics and visualizations.
#' Uses the awesome markdown template created by Chris Garbin
#' \href{https://github.com/fau-masters-collected-works-cgarbin/model-card-template}{from Github}.
#' @param filename (`character(1)`)\cr
#'   File path or name for new file that should be created.
#' @param object (`Learner`)
#'  To Do.
#' @param resample_result (`Resampling`)
#'  To Do.
#' @param control (`report_control`)
#'  To Do.
#' @param edit (`logical(1)`)\cr
#'   `TRUE` to edit the template immediately.
#' @param build (`logical(1)`)\cr
#'   Should the report be built after creation? Initialized to `FALSE`.
#' @param ... (any)
#'  To Do.
#'
#' @return Invisibly returns the path to the newly created file(s).
#' @export

report = function(filename = "report.Rmd", object, resample_result = NULL, control = report_control(), edit = FALSE, build = TRUE, ...) {
  assert_path_for_output(filename)
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
  assert_class(control, classes = "report_control", null.ok = FALSE)
  assert_flag(edit)
  assert_flag(build)

  filepath = rmarkdown::draft(filename, template = "report", package = "mlr3summary", create_dir = TRUE, edit = edit)
  write_files(objects, dirname(filepath))
  if (build) rmarkdown::render(filepath)
  invisible(filepath)
}



#' @title Control for model reports
#'
#' @description Various parameters that control aspect of the report.
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
report_control = function(measures = NULL, importance_measures = "pdp", n_important = 15L, digits = max(3L, getOption("digits") - 3L)) {

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
  assert_int(digits, lower = 0L, null.ok = FALSE)

  # create list
  ctrlist = list(measures = measures, importance_measures = importance_measures,
    n_important = n_important, digits = digits)

  class(ctrlist) = "report_control"
  ctrlist
}


#' Write objects as .RDS files into path.
#'
#' @param objects [list] list of objects
#' @param path [character] path to save to
#'
#' @return NULL
#' @noRd
write_files = function(objects, path) {
  reads = pmap_chr(list(objects, names(objects)), function(x, nm) {
    fn = sprintf("%s.rds", nm)
    saveRDS(x, file = file.path(path, fn))
    paste0(nm, " = readRDS('", fn, "')")
  })

  writeLines(con = paste0(path, "/read_data.Rmd"), c(
    "```{r read-data, include = FALSE}",
    reads,
    "```"))
}
