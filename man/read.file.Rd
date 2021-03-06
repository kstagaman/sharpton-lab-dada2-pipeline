% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_file.R
\name{read.file}
\alias{read.file}
\title{Read File}
\usage{
read.file(file, has.header = TRUE, ...)
}
\arguments{
\item{file}{character; a filename in a common format: csv, tsv/txt, xls/xlsx, or rds}

\item{has.header}{logical; for csv or tsv/txt files, whether the first row is column names. Default is TRUE.}

\item{...}{additional arguments passed to read.table, read_xls, or read_xlsx as needed.}
}
\description{
An auxilliary function to read in metadata (or other) files in common format types
}
\seealso{
\code{\link{read.table}}, \code{\link{readRDS}}, \code{\link{read_xls}}, \code{\link{read_xlsx}}
}
