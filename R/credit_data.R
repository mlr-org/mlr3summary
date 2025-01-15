#' @name credit
#' @title German Credit Dataset (Preprocessed)
#'
#' @description
#' Preprocessed version of the German Credit Risk dataset available on kaggle,
#' based on the Statlog (German credit dataset) of Hofmann (1994) available on
#' UCI.
#' @details
#' The dataset was further adapted: rows with missing values were removed,
#' low-cardinal classes were binned, classes of the job feature were renamed,
#' the features on the savings and checking account were defined as ordinal variables,
#' and all feature names were transposed to lower. Only a subset of features was
#' selected: "age", "sex", "saving.accounts", "duration", "credit.amount", "risk".
#'
#' @format A data frame with 522 and 6 variables:
#' \describe{
#'   \item{age}{age of the customer \[19-75\]}
#'   \item{sex}{sex of the customer (female, male)}
#'   \item{saving.accounts}{saving account balance of the customer (little, moderate, rich)}
#'   \item{duration}{payback duration of credit (in month) \[6-72\]}
#'   \item{credit.amount}{credit amount \[276-18424\]}
#'   \item{risk}{whether the credit is of low/good or high/bad risk (bad, good)}
#'   }
#' @references
#' `r format_bib("creditdata", "kaggle16")`
NULL
