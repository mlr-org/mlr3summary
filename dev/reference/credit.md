# German Credit Dataset (Preprocessed)

Preprocessed version of the German Credit Risk dataset available on
kaggle, based on the Statlog (German credit dataset) of Hofmann (1994)
available on UCI.

## Format

A data frame with 522 and 6 variables:

- age:

  age of the customer \[19-75\]

- sex:

  sex of the customer (female, male)

- saving.accounts:

  saving account balance of the customer (little, moderate, rich)

- duration:

  payback duration of credit (in month) \[6-72\]

- credit.amount:

  credit amount \[276-18424\]

- risk:

  whether the credit is of low/good or high/bad risk (bad, good)

## Details

The dataset was further adapted: rows with missing values were removed,
low-cardinal classes were binned, classes of the job feature were
renamed, the features on the savings and checking account were defined
as ordinal variables, and all feature names were transposed to lower.
Only a subset of features was selected: "age", "sex", "saving.accounts",
"duration", "credit.amount", "risk".

## References

Hofmann, Hans (1994). “Statlog (German Credit Data).” *UCI Machine
Learning Repository*.

Ferreira L (2018). “German credit risk.” Last accessed 10.04.2024.
