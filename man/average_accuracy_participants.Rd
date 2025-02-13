% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/average_var_over_repetition.R
\name{average_accuracy_participants}
\alias{average_accuracy_participants}
\title{Plot average accuracy of all participants over repetitions}
\usage{
average_accuracy_participants(data, xlim = NULL, ylim = NULL, filepath = NULL)
}
\arguments{
\item{data}{A data frame. NA values will be removed before plotting.}

\item{xlim}{A vector of 2 (for example: c(0, 10)), indicating the range of
the x-axis.If NULL the default value is used.}

\item{ylim}{A vector of 2 (for example: c(0, 1000)), indicating the range of
the y-axis.If NULL the default value is used: c(0, 1).}

\item{filepath}{A relative or explicit path where plots will be saved}
}
\value{
Plot of accuracy over repetition, with every line a session and a
pdf file in filepath
}
\description{
Plots the average accuracy of the first session of every participant in a
lesson over the repetitions of the facts.
}
\details{
Assumes that the data set has a repetition column with fact repetition in
integers
}
\seealso{
Other average functions: 
\code{\link{average_ROF_facts}()},
\code{\link{average_ROF_participants}()},
\code{\link{average_RT_participants}()}
}
\concept{average functions}
