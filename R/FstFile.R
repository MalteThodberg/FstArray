#### FstFile-class ####

#' FstFile-class: Import, export and introspection of fst-files
#'
#' @param resource character: path to fst-file
#' @param object FstFile-object
#' @param x FstFile-object
#' @param asDataFrame logical: Whether to return as a DataFrame (TRUE) or data.frame (FALSE)
#' @inheritParams BiocIO::import
#' @inheritParams BiocIO::export
#' @inheritParams fst::read_fst
#' @inheritParams fst::write_fst
#'
#' @return FstFile-object
#' @export
#' @examples
#' # Save mtcars as an .fst file with default settings
#' fst_fname <- tempfile(fileext=".fst")
#' export(mtcars, fst_fname)
#'
#' # Change compression
#' export(mtcars, fst_fname, compress=25)
#'
#' # Import everything
#' import(fst_fname)
#'
#' # Import subset of rows and columns
#' import(fst_fname,
#'        columns=c("hp", "vs", "carb"),
#'        from=5, to=10)
#'
#' # Get file info without loading the .fst-file
#' ff <- FstFile(fst_fname)
#'
#' # Dimension
#' nrow(ff)
#' ncol(ff)
#' dim(ff)
#'
#' # Column names type
#' colnames(ff)
#' type(ff)
#'
#' # Access to the raw summary from the fst package
#' summary(ff)
.FstFile <- setClass("FstFile", contains = "BiocFile")

#' @rdname FstFile-class
#' @export
FstFile <- function(resource){
    .FstFile(resource = resource)
    }

#### Getters ####

# Expose S3 fst-summary
#' @rdname FstFile-class
#' @export
setMethod(summary, "FstFile",
          function(object){fst::metadata_fst(resource(object))})

# Dimensions and dimnames
#' @rdname FstFile-class
#' @export
setMethod("nrow", "FstFile",
          function(x) as.integer(summary(x)$nrOfRows))

#' @rdname FstFile-class
#' @export
setMethod("colnames", "FstFile",
          function(x) summary(x)$columnNames)

#' @rdname FstFile-class
#' @export
setMethod("ncol", "FstFile", function(x) length(colnames(x)))

#' @rdname FstFile-class
#' @export
setMethod("dim", "FstFile", function(x) c(nrow(x), ncol(x)))

# Column types
#' @rdname FstFile-class
#' @export
setMethod("type", "FstFile", function(x){
    # Types used by fst
    types <- c(
        "unknown", "character", "factor", "ordered factor", "integer", "POSIXct", "difftime",
        "IDate", "ITime", "double", "Date", "POSIXct", "difftime", "ITime", "logical", "integer64",
        "nanotime", "raw")

    # Return named vector
    s <- summary(x)
    o <- types[s$columnTypes]
    names(o) <- s$columnNames

    # Return
    o
})

#### IO ####

#' @rdname FstFile-class
#' @export
setMethod("import", "FstFile", function(con, format, text,
                                        columns = NULL,
                                        from = 1,
                                        to = NULL,
                                        asDataFrame = TRUE){
    checkmate::assertFlag(asDataFrame)

    # Read (rest of arguments checked here)
    o <- fst::read_fst(path = resource(con),
                       columns = columns,
                       from = from,
                       to = to)

    # Convert to DataFrame by default
    if(isTRUE(asDataFrame)){
        o <- as(o, "DataFrame")
    }

    # Return
    o
})

#' @rdname FstFile-class
#' @export
setMethod("export", c("ANY", "FstFile"),
          function(object, con, format, compress = 50) {
              o <- as(object, "data.frame")
              fst::write_fst(x = o,
                             path = resource(con),
                             compress = compress)
          }
)
