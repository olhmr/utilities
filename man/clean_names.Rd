% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_data.R
\name{clean_names}
\alias{clean_names}
\title{Clean Data Frame Names}
\usage{
clean_names(x)
}
\arguments{
\item{x}{Input. Either a data frame or a character vector.}
}
\value{
Data frame or character vector, corresponding to input.
}
\description{
Applies some general formatting and replacements to enforce a naming
convention that is clear and easy to read, while also ensuring it's valid in
the context of data frame column names.
}
\details{
Essentially functions as a wrapper for make.names, making some useful
substitutions before passing to make.names for final validation. Inputs are
given as either a data frame or a character vector, and the return type will
correspond to the input.
}
