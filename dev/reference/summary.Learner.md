# Summarizing mlr3 Learners

summary method for
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html). The
output can be tailored via the `control` argument, see
[summary_control](https://mlr3summary.mlr-org.com/dev/reference/summary_control.md).

## Usage

``` r
# S3 method for class 'Learner'
summary(object, resample_result = NULL, control = summary_control(), ...)

# S3 method for class 'GraphLearner'
summary(object, resample_result = NULL, control = summary_control(), ...)

# S3 method for class 'summary.Learner'
print(x, digits = NULL, n_important = NULL, hide = NULL, ...)
```

## Arguments

- object:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  trained model of class `Learner`.

- resample_result:

  ([mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html))  
  outcome of `resample`. If NULL (default), no residuals, performances,
  etc. are derived.

- control:

  (`summary_control`)  
  a list with control parameters, see `summary_control`.

- ...:

  (any)  
  further arguments passed to or from other methods.

- x:

  (`summary.Learner`)  
  an object of class "summary.Learner", usually a result of a call to
  `summary.Learner`.

- digits:

  (numeric(1))  
  the number of digits to use when printing.

- n_important:

  (numeric(1))  
  number of important variables to be displayed. If NULL,
  `x$control$n_important` is used.

- hide:

  (character)  
  Names of paragraphs which should not be part of the summary. Possible
  values are "general", "residuals", "performance", "complexity",
  "fairness", "importance", "effect". If NULL, no paragraph is hided.

## Value

summary.Learner returns an object of class "summary.Learner", a
[list](https://rdrr.io/r/base/list.html) with the following entries.

- task_type: The type of task, either `classif` (classification) or
  `regr` (regression).

- target_name: The name of the target variable.

- feature_names: The names of the features.

- classes: The classes of the target variable. NULL if regression task.

- resample_info: Information on the resample objects, strategy type and
  hyperparameters.

- residuals: Vector of hold-out residuals over the resampling iterations
  of `resample_result`. For regression models, residuals are the
  difference between true and predicted outcome. For classifiers with
  probabilities, the residuals are the difference between predicted
  probabilities and a one-hot-encoding of the true class. For hard-label
  classifier, a `confusion_matrix` is shown instead of `residuals`.

- confusion_matrix: Confusion matrix of predicted vs. true classes.
  Alternative to `residuals`, in case of hard-label classification.

- performance: Vector of aggregated performance measures over the
  iterations of `resample_result`. The arrows display whether lower or
  higher values are better. (micro/macro) displays whether it is a micro
  or macro measure. For macro aggregation, measures are computed for
  each iteration separately before averaging. For micro aggregation,
  measures are computed across all iterations. See Bischl et al. (2024),
  for details.

- performance_sd: Vector of standard deviations of performance measures
  over the iterations of `resample_result`. The arrows display whether
  lower or higher values are better. (micro/macro) displays whether it
  is a micro or macro measure.

- fairness: Vector of aggregated fairness measures over the iterations
  of `resample_result`. The arrows display whether lower or higher
  values are better. (micro/macro) displays whether it is a micro or
  macro measure.

- fairness_sd: Vector of standard deviations of fairness measures over
  the iterations of `resample_result`. The arrows display whether lower
  or higher values are better. (micro/macro) displays whether it is a
  micro or macro measure (see details above).

- importances: List of `data.table` that display the feature importances
  per importance measure. Given are the means and standard deviations
  over the resampling iterations of `resample_result`. Higher average
  values display higher importance of a feature.

- effects: List of `data.table`s that display the feature effects per
  effect method. Given are the mean effects over the resampling
  iterations of `resample_result` for a maximum of 5 grid points. For
  binary classifiers, effects are only displayed for the
  positively-labeled class. For multi-class, effect plots are displayed
  separately for each class. For categorical features, the factor levels
  of the feature determine the ordering of the bars.

- complexity: List of vectors that display the complexity values per
  complexity measure for each resampling iteration.

- control:
  [summary_control](https://mlr3summary.mlr-org.com/dev/reference/summary_control.md)
  used as an input for `summary.Learner`.

For details on the performance measures, complexity measures, feature
importance and feature effect methods, see
[summary_control](https://mlr3summary.mlr-org.com/dev/reference/summary_control.md).

## Details

This function can be parallelized with the
[future](https://CRAN.R-project.org/package=future) package. One job is
one resampling iteration, and all jobs are sent to an apply function
from [future.apply](https://CRAN.R-project.org/package=future.apply) in
a single batch. To select a parallel backend, use
[`future::plan()`](https://future.futureverse.org/reference/plan.html).

## References

Bischl, Bernd, Sonabend, Raphael, Kotthoff, Lars, Lang, Michel (2024).
*Applied machine learning using mlr3 in R*. Chapman and Hall/CRC. ISBN
9781003402848, <https://mlr3book.mlr-org.com/>.

## Examples

``` r
if (require("mlr3")) {
  data.table::setDTthreads(1L)
  tsk_iris = tsk("iris")
  lrn_rpart = lrn("classif.rpart", predict_type = "prob")
  lrn_rpart$train(task = tsk_iris)
  rsmp_cv2 = rsmp("cv", folds = 2L)
  rr = resample(tsk_iris, lrn_rpart, rsmp_cv2, store_model = TRUE)
  summary(lrn_rpart, rr)
}
#> Loading required package: mlr3
#> complexity measure 'interaction_strenght' is ignored because it does not work for multiClass
#> 
#> ── General ─────────────────────────────────────────────────────────────────────
#> Task type: classif
#> Target name: Species (setosa, versicolor, and virginica)
#> Feature names: Petal.Length, Petal.Width, Sepal.Length, and Sepal.Width
#> Model type: classif.rpart with xval=0
#> Resampling: cv with folds=2
#> 
#> ── Residuals ───────────────────────────────────────────────────────────────────
#>     Min      1Q  Median    Mean      3Q     Max 
#> 0.00000 0.00000 0.03704 0.08129 0.04545 1.00000 
#> 
#> ── Performance [sd] ────────────────────────────────────────────────────────────
#>                                            
#> ↑classif.mauc_aunp (macro): 0.972 [0.0103] 
#> ↓classif.mbrier (macro):    0.1025 [0.0338]
#> 
#> ── Complexity [sd] ─────────────────────────────────────────────────────────────
#>                       
#> sparsity: 1.5 [0.7071]
#> 
#> ── Importance [sd] ─────────────────────────────────────────────────────────────
#>              pdp             pfi.ce       
#> Petal.Length 0.3683 [0.0993] 0.52 [0.1697]
#> Petal.Width  0.1006 [0.1422] 0.14 [0.198] 
#> Sepal.Width  0 [0]           0 [0]        
#> Sepal.Length 0 [0]           0 [0]        
#> 
#> ── Effects ─────────────────────────────────────────────────────────────────────
#> 
#> 
#> ── setosa ──
#> 
#>              pdp   ale  
#> Petal.Length █▄▁▁▁ █▅▂▂▂
#> Petal.Width  ▃▃▃▃▃ ▄▄▄▄▄
#> Sepal.Width  ▃▃▃▃▃ ▄▄▄▄▄
#> Sepal.Length ▃▃▃▃▃ ▄▄▄▄▄
#> 
#> ── versicolor ──
#> 
#>              pdp   ale  
#> Petal.Length ▁▃▇▄▄ ▁▄▆▄▄
#> Petal.Width  ▅▅▅▂▂ ▅▅▅▂▂
#> Sepal.Width  ▄▄▄▄▄ ▄▄▄▄▄
#> Sepal.Length ▄▄▄▄▄ ▄▄▄▄▄
#> 
#> ── virginica ──
#> 
#>              pdp   ale  
#> Petal.Length ▁▂▂▅▅ ▃▃▃▅▅
#> Petal.Width  ▂▂▂▄▄ ▃▃▃▆▆
#> Sepal.Width  ▃▃▃▃▃ ▄▄▄▄▄
#> Sepal.Length ▃▃▃▃▃ ▄▄▄▄▄
```
