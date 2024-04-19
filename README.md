
# mlr3summary

<!-- badges: start -->
<!-- badges: end -->

Concise, informative summaries of machine learning models.
Based on [mlr3](https://github.com/mlr-org/mlr3).
Inspired by the summary output of (generalized) linear models.


## Installation

You can install the development version of mlr3summary: 

```{r eval = FALSE}
remotes::install_github("mlr-org/mlr3summary")
```

If you want to get started with `mlr3` (the basis of `mlr3summary`), we recommend installing the [mlr3verse](https://mlr3verse.mlr-org.com/) meta-package which installs `mlr3` and some of the most important extension packages:
```{r eval = FALSE}
install.packages("mlr3verse")
library(mlr3verse)
```

## Example

### Load data and create a task

```{r}
library(mlr3summary)
data("credit", package = "mlr3summary")
task = TaskClassif$new(id = "credit", backend = credit, target = "risk", positive = "good")
```

### Fit a model and resampling strategy

```{r}
set.seed(12005L)
rf = lrn("classif.ranger", predict_type = "prob")
rf$train(task)

cv3 = rsmp("cv", folds = 3L)
rr = resample(task = task, learner = rf, resampling = cv3, store_models = TRUE)
rr$aggregate(msrs(list("classif.acc", "classif.auc")))

```

### Apply the summary function

```{r}
summary(object = rf, resample_result = rr)
```
![summary_output](https://github.com/slds-lmu/mlr3summary/assets/25373845/84b6cf8f-72d6-42ae-8218-5df1623008a3)

## Citation

If you use mlr3summary, please cite: 

```
Dandl S, Becker M, Bischl B, Casalicchio G, Bothmann L (2024).
mlr3summary: Model and learner summaries for 'mlr3'.
R package version 0.1.0.
```
A BibTeX entry for LaTeX users is

```
  @Manual{,
    title = {mlr3summary: Model and learner summaries for 'mlr3'},
    author = {Susanne Dandl and Marc Becker and Bernd Bischl and Giuseppe Casalicchio and Ludwig Bothmann},
    year = {2024},
    note = {R package version 0.1.0},
  }
```
