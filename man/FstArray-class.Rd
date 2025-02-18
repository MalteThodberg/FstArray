% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FstArray.R
\docType{class}
\name{FstArray-class}
\alias{FstArray-class}
\alias{FstArray}
\alias{FstMatrix-class}
\title{FstArray: A DelayedArray backend for fst (multiple columns)}
\usage{
FstArray(resource, columns = NULL, type = NULL)
}
\arguments{
\item{resource}{character: Path to fst file.}

\item{columns}{character or NULL: Columns to use from the fst file. If NULL, defaults to all double & integer columns}

\item{type}{character or NULL: resulting type of matrix. If NULL, default to logical < integer < double < complex < character.}
}
\value{
A FstMatrix object (A 2D FstArray)
}
\description{
FstArray allows you to use the fst package as a backend for
DelayedArrays, similarly to the default HDF5Array package. Only reading from fst files is supported. Optionally, only a subset of columns from the fst file can be used.
}
\examples{
# Simulate large data.frame
large_df <- DataFrame(A=rnorm(1e6),
                      B=runif(1e6),
                      C=rpois(1e6, 1),
                      D=sample(letters, 1e6, replace=TRUE),
                      E=factor(sample(LETTERS, 1e6, replace=TRUE)))
object.size(large_df)

# Export
fst_fname <- tempfile(fileext=".fst")
export(large_df, fst_fname)

# FstArray doesn't read the data from disk until needed
fa <- FstArray(fst_fname)
object.size(fa)

# The type of the resulting array can be directly controlledcontrolled
fa_int <- FstArray(fst_fname, type="integer")

# The default is to use all numeric and integer columns, but this can be manually specified
fa_char <- FstArray(fst_fname, columns=c("D", "E"))

# All DelayedArray operations are supported for FstArray
fa + 1
rowSums(fa)
# See DelayedArray for mor
}
