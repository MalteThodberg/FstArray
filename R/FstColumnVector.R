#### FstColumnSeed-class ####

# Class definitino
.FstColumnSeed <- setClass("FstColumnSeed",
                           contains = "Array",
                           slots=c(resource="character",
                                   column="character",
                                   dim="integer",
                                   type="character"))

# Function for single column coercion type
.single_col_type <- function(x){
    y <- c("complex", "double", "integer", "logical")

    if(x %in% y){
        return(x)
    }else{
        return("character")
    }
}

# User-facing Constructor
FstColumnSeed <- function(resource, column = NULL, type = NULL){
    checkmate::assertFile(resource, access = "r", extension = "fst")
    checkmate::assertString(column, null.ok = TRUE)
    checkmate::assertString(type, null.ok = TRUE)

    # Fails if not an fst-file
    resource <- tools::file_path_as_absolute(resource)
    fm <- FstFile(resource)
    coltypes <- type(fm)

    # Check column
    checkmate::assertSubset(column,
                            choices = names(coltypes),
                            empty.ok = FALSE)

    # Get type
    if(is.null(type)){
        type <- .single_col_type(coltypes[column])
    }

    # Build object
    .FstColumnSeed(resource=resource,
                   column=column,
                   dim=nrow(fm),
                   type=type)
}

# Extract as 1D matrix as given type
.extract_fst_column <- function(x, index){
    # Prepare
    fst_obj <- fst::fst(x@resource)
    i <- index[[1]]

    # Slice out with NULL
    if(is.null(i)){
        o <- fst_obj[, x@column]
    }else{
        o <- fst_obj[i, x@column]
    }

    # Ensure output has the correct type
    if (!is(o, x@type)) {
        o <- as(o, x@type)
    }

    # Convert to 1D array
    o <- array(o)

    # Ensure output has the correct type
    # if(type(o) != x@type){
    #     type(o) <- x@type
    # }

    # Return
    o
}

setMethod("extract_array", "FstColumnSeed", .extract_fst_column)

#### FstColumnVector ####

#' FstColumnVector: A DelayedArray backend for fst (single column)
#'
#' FstColumnVector allows you to use the fst package as a backend for
#' DelayedArrays, similarly to the default SQLDataFrame package. Only reading from fst files is supported.
#'
#' @param resource character: Path to fst file.
#' @param column character: Column to use from the fst file.
#' @param type character or NULL: resulting type of matrix. If NULL, default to logical < integer < double < complex < character.
#' @return A FstColumnVector-object (A 1D FstArray) or a DataFrame of FstColumnVector-objects.
#'
#' @export
#' @examples
#' # Write the iris dataset to an fst
#' fst_fname <- tempfile(fileext=".fst")
#' export(iris, fst_fname)
#'
#' # A column can be accessed as a single vector:
#' fcv <- FstColumnVector(fst_fname, column = "Petal.Length")
#'
#' # The common use cases is making a DFrame of FstColumnVector-objects.
#' # This works much like a regular DFrame, except all calculations
#' # are handled via DelayedArray.
#' fcvs <- FstColumnVectors(fst_fname)
#'
#' # Adding a new column will be another DelayedArray 1D array
#' fcvs$LengthProduct <- fcvs$Sepal.Length * fcvs$Petal.Length
#'
#' # Subsetting causes all columns to become DelayedArray
#' fcvs <- subset(fcvs, Species == "virginica")
#'
#' # Realizing the delayed calculations with as.vector()
#' fcvs$LengthProductRealized <- as.vector(fcvs$LengthProduct)
setClass("FstColumnVector",
         contains="DelayedArray",
         slots=c(seed="FstColumnSeed"))

setMethod("DelayedArray", "FstColumnSeed",
          function(seed) new_DelayedArray(seed, Class="FstColumnVector"))

#' @rdname FstColumnVector-class
#' @export
FstColumnVector <- function(resource, column, type=NULL) {
    if (is(resource, "FstColumnSeed")){
        seed <- resource
    }else {
        seed <- FstColumnSeed(resource=resource, column=column, type=type)
    }

    DelayedArray(seed)
}

#### FstColumnVectors ####

#' @rdname FstColumnVector-class
#' @export
FstColumnVectors <- function(resource){
    # Empty data frame
    fm <- FstFile(resource)
    o <- S4Vectors::make_zero_col_DFrame(nrow = nrow(fm))

    # Add columns
    for (i in colnames(fm)){
        o[[i]] <- FstColumnVector(resource = resource, column = i)
    }

    colnames(o) <- colnames(fm)

    # Return
    o
}
