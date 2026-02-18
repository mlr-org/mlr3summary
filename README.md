
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlr3summary

Package website: [release](http://mlr3summary.mlr-org.com/) \|
[dev](http://mlr3summary.mlr-org.com/dev/)

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
library(mlr3)
library(mlr3summary)

data("credit", package = "mlr3summary")
task = as_task_classif(credit, target = "risk", positive = "good")
```

### Fit a model and resampling strategy

``` r
library(mlr3learners)

rf = lrn("classif.ranger", predict_type = "prob")$train(task)
cv3 = rsmp("cv", folds = 3L)
rr = resample(task = task, learner = rf, resampling = cv3, store_models = TRUE)
rr$aggregate(msrs(list("classif.acc", "classif.auc")))
#> classif.acc classif.auc 
#>   0.6398467   0.6811851
```

### Apply the summary function

``` r
summary(object = rf, resample_result = rr)
#> 
#> ── General ─────────────────────────────────────────────────────────────────────
#> Task type: classif
#> Target name: risk (good and bad)
#> Feature names: age, credit.amount, duration, saving.accounts, and sex
#> Model type: classif.ranger with num.threads=1
#> Resampling: cv with folds=3
#> 
#> ── Residuals ───────────────────────────────────────────────────────────────────
#>     Min      1Q  Median    Mean      3Q     Max 
#> 0.06633 0.28596 0.41013 0.43297 0.56701 0.94773
#> 
#> ── Performance [sd] ────────────────────────────────────────────────────────────
#>                                         
#> ↑classif.auc (macro):    0.6812 [0.0489]
#> ↑classif.fbeta (macro):  0.6917 [0.0578]
#> ↓classif.bbrier (macro): 0.2262 [0.0239]
#> ↑classif.mcc (macro):    0.2622 [0.079]
#> 
#> ── Complexity [sd] ─────────────────────────────────────────────────────────────
#>                                      
#> sparsity:                       5 [0]
#> interaction_strength: 0.5935 [0.1207]
#> 
#> ── Importance [sd] ─────────────────────────────────────────────────────────────
#>                 pdp             pfi.ce          
#> duration        0.1647 [0.0251] 0.0977 [0.0414] 
#> credit.amount   0.1274 [0.014]  0.0517 [0.0263] 
#> saving.accounts 0.0878 [0.0514] 0.0115 [0.0057] 
#> age             0.0488 [0.0132] -0.0077 [0.0176]
#> sex             0.0301 [0.0232] 0 [0.023]
#> 
#> ── Effects ─────────────────────────────────────────────────────────────────────
#>                 pdp   ale  
#> duration        █▅▄▁▁ █▅▄▁▁
#> credit.amount   ▆▆▃▂▂ ▆▆▄▄▄
#> saving.accounts ▅▆█   ▅▆▇  
#> age             ▅▅▆▆▇ ▅▅▆▆▆
#> sex             ▅▆    ▅▆
```

More examples can be found in [inst/demo](./inst/demo).

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
