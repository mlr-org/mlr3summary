
# mlr3summary

<!-- badges: start -->
<!-- badges: end -->

Concise, informative summaries of machine learning models.
Based on the [mlr3](https://github.com/mlr-org).
Inspired by the summary output of (generalized) linear models.


## Installation

You can install the development version of mlr3summary: 

```{r eval = FALSE}
remotes::install_github("mlr-org/mlr3summary")
```

If you want to get started with `mlr3` (the basis of `mlr3summary`), we recommend installing the [mlr3verse](https://mlr3verse.mlr-org.com/) meta-package which installs `mlr3` and some of the most important extension packages:
```{r eval = FALSE}
install.packages("mlr3verse")
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


## Extension Packages

If you use mlr3summary, please cite: 
```{r echo = FALSE, comment = ""}
toBibtex(citation("mlr3summary"))
```

