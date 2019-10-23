# dataque
Data Quality control framework for dataframes in R

## purpose

The purspoe of this package is to provide easy for use functionality to apply data-quality control to dataframes (as a main data representation format). This implementation is influenced and inspired by [awslabs/deequ](https://github.com/awslabs/deequ) and related paper [Automating large-scale data quality verification](http://www.vldb.org/pvldb/vol11/p1781-schelter.pdf).

## quick start

Install package from github using `devtools` package

```{r}
devtools::install_github(repo = "EvgenyPetrovsky/dataque")
```

define test data-set
```{r}
library(magrittr)
library(deeque)

# define dataset
test_df <- data.frame(...)

# define checks
checks <- new_group() %>%
  add_check(new_check("ERROR", function_name = "frame_hasColumn", column_name = "Price")) %>%
  add_check(new_check("INFO", function_name = "hasMin", column_name = "Price", udf = function(x) {x > 0})) %>%
  ...

# verify dataset using checks defined
test_df %>% verify(checks)

# another option verify; stop execution if stop condition is achieved
test_df %T>% verify_and_stop(checks, stop_udf = ...) %>% write.table(...)
```

## structure

This data quality framework defines following building blocks:

* `validation-functions` validation functions library - functions that analyze data for data-quality issues;
* `checks` set of operations to manage checks; checks are validation functions applied to specific context (with specified column names, severity, user-defined functions that support decision taking about check result;
* `adapters` set of operations for serialization / deserialization of check results and checks themselves;
* `verification` set of operations to execute data quality verification and help to take a decision in control flow (like stop if ERROR-severity issues are found).

## standard user flow

User has a dataset and needs to ensure that its shape and content meets requirements. For this reason:
1. User specified dq checks by describing them in a declarative way one by one. Checks themselves are instructions that are applied later to data checks are combined in groups.
2. User might decide to export check groups and load them later for future use or for sharing with others.
3. User runs verification by saying what group of checks needs to be applied to dataset.

Results of execution may be: 

* report that describes checks and fraction of valid records
* termination of workflow execution with information written into standard output
* continuation of process with warning messages / no messages and all if findings have severity lower that needs to be reported
