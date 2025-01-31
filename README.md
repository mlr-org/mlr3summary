
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlr3summary

Package website: [release](https://mlr3summary.mlr-org.com/) \|
[dev](https://mlr3summary.mlr-org.com/dev/)

<!-- badges: start -->

[![r-cmd-check](https://github.com/mlr-org/mlr3summary/actions/workflows/rcmdcheck.yaml/badge.svg)](https://github.com/mlr-org/mlr3summary/actions/workflows/rcmdcheck.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/mlr3summary)](https://CRAN.R-project.org/package=mlr3summary)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

Concise, informative summaries of machine learning models. Based on
[mlr3](https://github.com/mlr-org/mlr3). Inspired by the summary output
of (generalized) linear models.

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3summary")
```

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("mlr-org/mlr3summary")
```

## Example

### Load data and create a task

``` r
library(mlr3summary)
data("credit", package = "mlr3summary")
task = as_task_classif(credit, target = "risk", positive = "good")
```

### Fit a model and resampling strategy

``` r
set.seed(12005L)
rf = lrn("classif.ranger", predict_type = "prob")
rf$train(task)

cv3 = rsmp("cv", folds = 3L)
rr = resample(task = task, learner = rf, resampling = cv3, store_models = TRUE)
rr$aggregate(msrs(list("classif.acc", "classif.auc")))
```

### Apply the summary function

``` r
summary(object = rf, resample_result = rr)
```

<figure>
<img
src="https://github.com/slds-lmu/mlr3summary/assets/25373845/84b6cf8f-72d6-42ae-8218-5df1623008a3"
alt="summary_output" />
<figcaption aria-hidden="true">summary_output</figcaption>
</figure>

More examples can be found in
[demo/](https://github.com/mlr-org/mlr3summary/tree/master/demo).

## Citation

If you use `mlr3summary`, please cite:

    Dandl S, Becker M, Bischl B, Casalicchio G, Bothmann L (2024).
    mlr3summary: Model and learner summaries for 'mlr3'.
    R package version 0.1.0.

A BibTeX entry for LaTeX users is

``` bibtex
@Manual{
  title = {mlr3summary: Model and learner summaries for 'mlr3'},
  author = {Susanne Dandl and Marc Becker and Bernd Bischl and Giuseppe Casalicchio and Ludwig Bothmann},
  year = {2024},
  note = {R package version 0.1.0}
}
```
