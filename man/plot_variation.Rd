% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_variation.r
\name{plot_variation}
\alias{plot_variation}
\title{Make a plot based on a data frame}
\usage{
plot_variation(
  input_data = NULL,
  xlab = "Area",
  ylab = "Rate",
  type = "histogram",
  num_groups = 5,
  decimal_mark = ",",
  big_mark = " "
)
}
\arguments{
\item{input_data}{Data to be plotted}

\item{xlab}{Label on x-axis}

\item{ylab}{Label on y-axis}

\item{type}{What kind of plot. Default and only choice, at the moment:
"histogram"}

\item{num_groups}{Number of natural break groups}

\item{decimal_mark}{sign to use for decimal}

\item{big_mark}{sign to use for numbers over a thousand}
}
\description{
Make a plot based on a data frame
}
