
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
#>   0.6455939   0.6787890
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
#> 0.05832 0.28068 0.40930 0.43374 0.56692 0.94926
#> 
#> ── Performance [sd] ────────────────────────────────────────────────────────────
#>                                         
#> ↑classif.auc (macro):    0.6788 [0.0468]
#> ↑classif.fbeta (macro):  0.6973 [0.0536]
#> ↓classif.bbrier (macro): 0.2272 [0.0229]
#> ↑classif.mcc (macro):    0.2747 [0.0712]
#> 
#> ── Complexity [sd] ─────────────────────────────────────────────────────────────
#>                                      
#> sparsity:                       5 [0]
#> interaction_strength: 0.6036 [0.0987]
#> 
#> ── Importance [sd] ─────────────────────────────────────────────────────────────
#>                 pdp             pfi.ce          
#> duration        0.1631 [0.0252] 0.1054 [0.0465] 
#> credit.amount   0.1337 [0.0228] 0.0575 [0.0152] 
#> saving.accounts 0.0909 [0.058]  0.0153 [0.0145] 
#> age             0.0497 [0.0146] -0.0038 [0.0346]
#> sex             0.0319 [0.0245] 0.0038 [0.0133]
#> 
#> ── Effects ─────────────────────────────────────────────────────────────────────
#>                 pdp   ale  
#> duration        █▅▄▁▁ █▅▄▁▁
#> credit.amount   ▆▆▃▂▂ ▆▆▄▃▄
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
