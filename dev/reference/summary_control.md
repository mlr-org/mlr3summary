# Control for Learner summaries

Various parameters that control aspects of `summary.Learner`.

## Usage

``` r
summary_control(
  measures = NULL,
  complexity_measures = c("sparsity", "interaction_strength"),
  importance_measures = NULL,
  n_important = 15L,
  effect_measures = c("pdp", "ale"),
  fairness_measures = NULL,
  protected_attribute = NULL,
  hide = NULL,
  digits = max(3L, getOption("digits") - 3L)
)
```

## Arguments

- measures:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html) \|
  list of
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html) \|
  NULL)  
  measure(s) to calculate performance on. If NULL (default), a set of
  selected measures are calculated (choice depends on Learner type
  (classif vs. regr)). See details below.

- complexity_measures:

  (character)  
  vector of complexity measures. Possible choices are "sparsity" (the
  number of used features) and "interaction_strength" (see Molnar et al.
  (2020)). Both are the default. See details below.

- importance_measures:

  (character()\|NULL)  
  vector of importance measure names. Possible choices are
  "pfi.`<`loss`>`"
  ([iml::FeatureImp](https://giuseppec.github.io/iml/reference/FeatureImp.html)),
  "pdp"
  ([iml::FeatureEffects](https://giuseppec.github.io/iml/reference/FeatureEffects.html),
  see ) and "shap"
  ([fastshap::explain](https://bgreenwell.github.io/fastshap/reference/explain.html)).
  Default of NULL results in "pfi.`<`loss`>`" and "pdp", where the
  `<`loss`>` depends on the Learner type (classif vs. regr). See details
  below.

- n_important:

  (numeric(1))  
  number of important variables to be displayed. Default is 15L.

- effect_measures:

  (character \| NULL)  
  vector of effect method names. Possible choices are "pfi" and "ale"
  (see
  [iml::FeatureEffects](https://giuseppec.github.io/iml/reference/FeatureEffects.html)).
  Both are the default. See details below.

- fairness_measures:

  ([mlr3fairness::MeasureFairness](https://mlr3fairness.mlr-org.com/reference/MeasureFairness.html)
  \| list of
  [mlr3fairness::MeasureFairness](https://mlr3fairness.mlr-org.com/reference/MeasureFairness.html)
  \| NULL)  
  measure(s) to assess fairness. If NULL (default), a set of selected
  measures are calculated (choice depends on Learner type (classif vs.
  regr)). See details below.

- protected_attribute:

  (character(1))  
  name of the binary feature that is used as a protected attribute. If
  no `protected_attribute` is specified (and also no `pta` feature is
  available in the
  [`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html) for
  training the
  [`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html)),
  no fairness metrics are computed.

- hide:

  (character)  
  names of paragraphs which should not be part of the summary. Possible
  values are "general", "residuals", "performance", "complexity",
  "fairness", "importance", "effect". If NULL, no paragraph is hided.

- digits:

  (numeric(1))  
  number of digits to use when printing.

## Value

[list](https://rdrr.io/r/base/list.html) of class `summary_control`

## Details

The following provides some details on the different choices of
measures.

**Performance** The default `measures` depend on the type of task.
Therefore, NULL is displayed as default and the measures will be
initialized in `summary.Learner` with the help of
[`mlr3::msr`](https://mlr3.mlr-org.com/reference/mlr_sugar.html). The
following provides an overview of these defaults:

- Regression:
  [regr.rmse](https://mlr3.mlr-org.com/reference/mlr_measures_regr.rmse.html),
  [regr.rsq](https://mlr3.mlr-org.com/reference/mlr_measures_regr.rsq.html),
  [regr.mae](https://mlr3.mlr-org.com/reference/mlr_measures_regr.mae.html),
  [regr.medae](https://mlr3.mlr-org.com/reference/mlr_measures_regr.medae.html)

- Binary classification with probabilities:
  [classif.auc](https://mlr3.mlr-org.com/reference/mlr_measures_classif.auc.html),
  [classif.fbeta](https://mlr3.mlr-org.com/reference/mlr_measures_classif.fbeta.html),
  [classif.bbrier](https://mlr3.mlr-org.com/reference/mlr_measures_classif.bbrier.html),
  [classif.mcc](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mcc.html)

- Binary classification with hard labels:
  [classif.acc](https://mlr3.mlr-org.com/reference/mlr_measures_classif.acc.html),
  [classif.bacc](https://mlr3.mlr-org.com/reference/mlr_measures_classif.bacc.html),
  [classif.fbeta](https://mlr3.mlr-org.com/reference/mlr_measures_classif.fbeta.html),
  [classif.mcc](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mcc.html)

- Multi-class classification with probabilities:
  [classif.mauc_aunp](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_aunp.html),
  [classif.mbrier](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mbrier.html)

**Complexity** Currently only two `complexity_measures` are available,
which are based on Molnar et al. (2020):

- `sparsity`: The number of used features, that have a non-zero effect
  on the prediction (evaluated by accumulated local effects (ale, Apley
  and Zhu (2020)). The measure can have values between 0 and the number
  of features.

- `interaction_strength`: The scaled approximation error between a main
  effect model (based on ale) and the prediction function. It can have
  values between 0 and 1, where 0 means no interaction and 1 only
  interaction, and no main effects. Interaction strength can only be
  measured for binary classification and regression models.

**Importance** The `importance_measures` are based on the `iml` and
`fastshap` packages. Multiple measures are available:

- pdp: This corrensponds to importances based on the standard deviations
  in partial dependence plots (Friedmann (2001)), as proposed by
  Greenwell et al. (2018).

- pfi.`<`loss`>`: This corresponds to the permutation feature importance
  as implemented in
  [iml::FeatureImp](https://giuseppec.github.io/iml/reference/FeatureImp.html).
  Different loss functions are possible and rely on the task at hand.

- shap: This importance corresponds to the mean absolute Shapley values
  computed with
  [fastshap::explain](https://bgreenwell.github.io/fastshap/reference/explain.html).
  Higher values display higher importance.

NULL is the default, corresponding to importance calculations based on
pdp and pfi. Because the loss function for pfi relies on the task at
hand, the importance measures are initialized in `summary`."pdp" and
"pfi.ce" are the defaults for classification, "pdp" and "pfi.mse" for
regression.

**Effects** The `effect_measures` are based on
[iml::FeatureEffects](https://giuseppec.github.io/iml/reference/FeatureEffects.html).
Currently partial dependence plots (pdp) and accumulated local effects
are available (ale). Ale has the advantage over pdp that it takes
feature correlations into account but has a less natural interpretation
than pdp. Therefore, both "pdp" and "ale" are the defaults.

**Fairness** The default `fairness_measures` depend on the type of task.
Therefore, NULL is displayed as default and the measures will be
initialized in `summary.Learner` based on
[mlr3fairness::mlr_measures_fairness](https://mlr3fairness.mlr-org.com/reference/mlr_measures_fairness.html).
There is currently a mismatch between the naming convention of measures
in `mlr3fairness` and the underlying measurements displayed. To avoid
confusion, the id of the fairness measures were adapted. The following
provides an overview of these defaults and adapted names:

- Binary classification: "fairness.dp" (demographic parity) based on
  "fairness.cv", "fairness.cuae" (conditional use accuracy equality)
  based on "fairness.pp", "fairness.eod" (equalized odds) based on
  "fairness.eod". Smaller values are better.

- Multi-class classification: "fairness.acc", the smallest absolute
  difference in accuracy between groups of the `protected_attribute`.
  Smaller values are better.

- Regression: "fairness.rmse" and "fairness.mae", the smallest absolute
  difference (see
  [mlr3fairness::groupdiff_absdiff](https://mlr3fairness.mlr-org.com/reference/groupdiff_tau.html))
  in the either the root mean-squared error (rmse) or the mean absolute
  error (mae) between groups of the `protected_attribute`. Smaller
  values are better.

## References

Molnar, Christoph, Casalicchio, Giuseppe, Bischl, Bernd (2020).
“Quantifying Model Complexity via Functional Decomposition for Better
Post-hoc Interpretability.” In *Communications in Computer and
Information Science*, chapter 1, 193–204. Springer International
Publishing.

Greenwell, M. B, Boehmke, C. B, McCarthy, J. A (2018). “A Simple and
Effective Model-Based Variable Importance Measure.” arXiv preprint.
arXiv:1805.04755, <http://arxiv.org/abs/1805.04755>.

Apley, W. D, Zhu, Jingyu (2020). “Visualizing the Effects of Predictor
Variables in Black Box Supervised Learning Models.” *Journal of the
Royal Statistical Society Series B: Statistical Methodology*, **82**(4),
1059-1086.

Friedman, H. J (2001). “Greedy Function Approximation: A Gradient
Boosting Machine.” *The Annals of Statistics*, **29**(5).
