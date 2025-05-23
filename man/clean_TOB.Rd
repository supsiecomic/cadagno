% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cadagno.r
\name{clean_TOB}
\alias{clean_TOB}
\title{clean_TOB}
\usage{
clean_TOB(x, selection_method = FALSE, outfile = FALSE)
}
\arguments{
\item{x}{dataframe obtained from calling read_TOB()}

\item{selection_method}{method to treat the multiple
measurements per depth value. Either by "random" sample or
by taking the "mean"}

\item{outfile}{name of the output file. If false,
then a default filename is used

OUTPUT}
}
\value{
a data frame
}
\description{
DESCRIPTION
this function clean previously read .TOB data file.
The cleaning consists of three steps:
0)  removal of measurements when the probe is ascending
i)  reduction from multiple measurements per depth to single
measurement per depth value
ii) reording the dataset based on depth
}
\details{
PARAMETERS
}
