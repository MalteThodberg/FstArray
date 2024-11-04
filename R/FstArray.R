#### FstArraySeed ####

# CLass definition
.FstArraySeed <- setClass("FstArraySeed",
                          contains = "Array",
                          slots=c(resource="character",
                                  columns="character",
                                  dim="integer",
                                  type="character"))

# Determine class of output
.multi_col_type <- function(x){
    y <- c("complex", "double", "integer", "logical")

    if(!all(x %in% y)){
        return("character")
    }else if("complex" %in% x){
        return("complex")
    }else if("double" %in% x){
        return("double")
    }else if("integer" %in% x){
        return("integer")
    }else if("logical" %in% x){
        return("logical")
    }else{
        stop("Something went wrong when determining type")
    }
}

# Constructor
FstArraySeed <- function(resource, columns = NULL, type = NULL){
    checkmate::assertFile(resource, access = "r", extension = "fst")
    checkmate::assertCharacter(columns, null.ok = TRUE)
    checkmate::assertString(type, null.ok = TRUE)

    # Fails if not an fst file
    resource <- tools::file_path_as_absolute(resource)
    fm <- FstFile(resource)
    coltypes <- type(fm)

    # Figure out columns
    if(is.null(columns)){
        columns <- names(coltypes[coltypes %in% c("double", "integer")])
        stopifnot(length(columns) >= 1)
    }else{
        checkmate::assertSubset(columns,
                                choices = names(coltypes),
                                empty.ok = FALSE)
    }

    # Figure out output type
    if(is.null(type)){
        type <- .multi_col_type(coltypes[columns])
    }

    # Build object
    .FstArraySeed(resource=resource,
                  columns=columns,
                  dim=c(nrow(fm), length(columns)),
                  type=type)
}

#' @rdname FstArray-package
setMethod("dimnames", "FstArraySeed", function(x){list(NULL, x@columns)})

# Extracting as matrix
.extract_fst_array <- function(x, index){
    # Extract feather
    fo <- fst::fst(x@resource)

    # Extract indices
    i <- index[[1]]
    j <- index[[2]]

    # Make sure it handles NULL index
    if(!is.null(i) && !is.null(j)){
        o <- fo[i, x@columns[j]]
    }else if(is.null(i) && !is.null(j)){
        o <- fo[, x@columns[j]]
    }else if(!is.null(i) && is.null(j)){
        o <- fo[i, x@columns]
    }else if(is.null(i) && is.null(j)){
        o <- fo[,x@columns]
    }else{
        stop("This should not happen!")
    }

    # Convert to matrix
    o <- as.matrix(o)
    if(type(o) != x@type){
        type(o) <- x@type
    }

    # Return
    o
}

setMethod("extract_array", "FstArraySeed", .extract_fst_array)

#### FstArray-class ####

#' FstArray: A DelayedArray backend for fst (multiple columns)
#'
#' FstArray allows you to use the fst package as a backend for
#' DelayedArrays, similarly to the default HDF5Array package. Only reading from fst files is supported. Optionally, only a subset of columns from the fst file can be used.
#'
#' @param resource character: Path to fst file.
#' @param columns character or NULL: Columns to use from the fst file. If NULL, defaults to all double & integer columns
#' @param type character or NULL: resulting type of matrix. If NULL, default to logical < integer < double < complex < character.
#' @return A FstMatrix object (A 2D FstArray)
#'
#' @export
#' @examples
#' # Simulate large data.frame
#' large_df <- DataFrame(A=rnorm(1e6),
#'                       B=runif(1e6),
#'                       C=rpois(1e6, 1),
#'                       D=sample(letters, 1e6, replace=TRUE),
#'                       E=factor(sample(LETTERS, 1e6, replace=TRUE)))
#' object.size(large_df)
#'
#' # Export
#' fst_fname <- tempfile(fileext=".fst")
#' export(large_df, fst_fname)
#'
#' # FstArray doesn't read the data from disk until needed
#' fa <- FstArray(fst_fname)
#' object.size(fa)
#'
#' # The type of the resulting array can be directly controlledcontrolled
#' fa_int <- FstArray(fst_fname, type="integer")
#'
#' # The default is to use all numeric and integer columns, but this can be manually specified
#' fa_char <- FstArray(fst_fname, columns=c("D", "E"))
#'
#' # All DelayedArray operations are supported for FstArray
#' fa + 1
#' rowSums(fa)
#' # See DelayedArray for mor
setClass("FstArray",
         contains = "DelayedArray",
         slots = c(seed = "FstArraySeed"))

# DelayedArray infrastructure
setMethod("DelayedArray", "FstArraySeed",
          function(seed) new_DelayedArray(seed, Class = "FstArray")
)

#' @rdname FstArray-class
#' @export
FstArray <- function(resource, columns=NULL, type=NULL){
    # All checks done in FstArraySeed

    # Accept pre-made seed
    if (is(resource, "FstArraySeed")){
        seed <- resource
    }else {
        seed <- FstArraySeed(resource=resource, columns=columns, type=type)
    }

    # Output
    DelayedArray(seed)
}

#' @rdname FstArray-class
#' @export
setClass("FstMatrix", contains=c("FstArray", "DelayedMatrix"))

# DelayedArray infrastructure
setMethod("matrixClass", "FstArray", function(x) "FstMatrix")
setAs("FstArray", "FstMatrix", function(from) new("FstMatrix", from))
setAs("FstMatrix", "FstArray", function(from) from)
