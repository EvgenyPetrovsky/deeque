% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runner.R
\name{severity_at_threshold}
\alias{severity_at_threshold}
\title{Function that creates severity check function.}
\usage{
severity_at_threshold(severity)
}
\arguments{
\item{severity}{- severity text label (INFO, WARNING, ERROR)}
}
\description{
Function takes only severity level parameter and returns function that
accepts data check result and checks whether defined severity level was
reached. If severity level was reached function returns TRUE.
}
