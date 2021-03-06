% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation-functions-column.R
\name{col_hasUniqueness}
\alias{col_hasUniqueness}
\title{Assess uniqueness of column values with user-defined function}
\usage{
col_hasUniqueness(data, column, udf)
}
\arguments{
\item{data}{dataframe}

\item{column}{column name}

\item{udf}{user-defined function that takes ratio and returns logical value}
}
\description{
Uniqueness is defined by formula |{v belongs to V | cv = 1}| / |V| where V is total set of values that column takes, v - exact value, cv - values count in column
In other words - number of values that occur only 1 time divided by total number of possible values. For example uniqueness of [1, 2, 2, 3] is 2/3
}
