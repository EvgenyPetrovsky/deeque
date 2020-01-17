[![Build Status](https://travis-ci.org/EvgenyPetrovsky/deeque.svg?branch=master)](https://travis-ci.org/EvgenyPetrovsky/deeque)
[![codecov](https://codecov.io/gh/EvgenyPetrovsky/deeque/branch/master/graphs/badge.svg)](https://codecov.io/gh/EvgenyPetrovsky/deeque)

# dataque

Data Quality control framework for dataframes in R.

With some creativity checks can be performed on anything that fits dataframe. For example: directory can be checked for presence of all required files.

## purpose

The purspoe of this package is to provide easy for use functionality to apply data-quality control to dataframes (as a main data representation format). This implementation is influenced and inspired by [awslabs/deequ](https://github.com/awslabs/deequ) and related paper [Automating large-scale data quality verification](http://www.vldb.org/pvldb/vol11/p1781-schelter.pdf).

## quick start

Install package from github using `devtools` package

```R
devtools::install_github(repo = "EvgenyPetrovsky/deeque")
```

define test data-set and check it

```R
library(magrittr)
library(deeque)

# define dataset
test_df <- as.data.frame(datasets::Titanic, stringsAsFactors = T)

# define checks
checks <-
  new_group() %>%
  add_check(new_check(
    description = "Dataset must have column 'Class'",
    severity = "ERROR", function_name = "tab_hasColumn", column = "Class"
  )) %>%
  add_check(new_check(
    "Minimum value of 'Freq' must be positive number",
    "INFO", col_hasMin, column = "Freq", udf = function(x) {x >= 0}
  )) %>%
  add_check(new_check(
    "Column 'Sex' must value have values from list [Male, Female]",
    "WARNING", col_isInLOV, column = "Sex", lov = factor(c("Male", "Female"))
  )) %>%
  add_check(new_check(
    "Combination of values in columns 'Class', 'Sex', 'Age', 'Survived' must be unique",
    "WARNING", tab_hasUniqueKey, columns = c("Class", "Sex", "Age", "Survived")
  ))

# show checks
checks %>% convert_checks_to_df()

# verify dataset using checks defined
chk_res <- test_df %>% run_checks(checks)

# view dataset in Rstudio
View(chk_res %>% convert_run_results_to_df())

# another option verify; stop execution if condition is not satisfied
test_df %T>%
  run_checks(
    checks,
    condition = severity_under_threshold(severity$WARNING)
  ) %>%
  head(5)
```

or check folder content for all required files

```R
library(magrittr)
library(deeque)

# dir() function should be called here with proper parameters
# dir_content <- dir(recursive = TRUE)
dir_content <- c(
  "REAMDE.txt",
  "config.yaml",
  "input_data/internal/employees.csv",
  "input_data/internal/goals.csv",
  "input_data/internal/incidents.csv",
  "input_data/external/market_rates.csv"
)

# put dir content into data frame
test_df <- data.frame(
  file_name = dir_content,
  stringsAsFactors = FALSE
)

# define checks
checks <-
  new_group() %>%
  add_check(new_check(
    "Data about employees and goals musrt present in internal input folder",
    "ERROR", col_hasAllValues, column = "file_name",
    lov = c(
      "input_data/internal/employees.csv",
      "input_data/internal/goals.csv")
  ))

# verify dataset using checks defined
chk_res <- test_df %>% run_checks(checks)

# view dataset in Rstudio
View(chk_res %>% convert_run_results_to_df())
```

## structure

This data quality framework defines following building blocks:

* `validation-functions-*` validation functions library - functions that analyze data for data-quality issues;
* `checks` set of operations to manage checks; checks are validation functions applied to specific context (with specified column names, severity, user-defined functions that support decision taking about check result;
* `adapters` set of operations for serialization / deserialization of check results and checks themselves;
* `runner` set of operations to execute data quality verification and help to take a decision in control flow (like stop if ERROR-severity issues are found).

## standard user flow

User has a dataset and needs to ensure that its shape and content meets requirements. For this reason:

1. User specified dq checks by describing them in a declarative way one by one. Checks themselves are instructions that are applied later to data checks are combined in groups.
2. User might decide to export check groups and load them later for future use or for sharing with others.
3. User runs verification by saying what group of checks needs to be applied to dataset.

Results of execution may be:

* report that describes checks and fraction of valid records
* termination of workflow execution with information written into standard output
* continuation of process with warning messages / no messages and all if findings have severity lower that needs to be reported

## important information about functions

Some functions operate with statistics (like min, max, uniqueness ratio) and can return only one logical value, this can be TRUE / FALSE. Others operate on lower lever and return value for every element. They return logical vector of values. Both of these cases may be properly treated by basic data.frame functionality and data manipulation packages such as dlpyr. It is up to user to decide what result to use.

However, when implementing functions, one should think what is proper result and either return vector for every row that was checked or return 1 value. There is no reason to replicate one value to number of rows.
