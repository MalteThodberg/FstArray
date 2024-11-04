
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FstArray

<!-- badges: start -->
<!-- badges: end -->

The FstArray R-package provides a DelayedArray array backend for the
.fst file format. This includes FstArray for matrix-like data (multiple
columns of the same type) and FstColumnVector for data.frame-like data
(columns of different types).

FstArray only supports reading from .fst-files. The FstFile-class
provides functionality for reading, writing and quering .fst-file
metadata.

See the `DelayedArray`, `HDF5Array` and `SQLDataFrame` packages for more
information.

## Getting the package

Install from github using:

``` r
devtools::install_github("MalteThodberg/FstArray")
```

Load the package:

``` r
library(FstArray)
```

## Quick usage

IO and introspection with the `FstFile`-class:

``` r
# Simulate large data.frame
large_df <- DataFrame(A=rnorm(1e6),
                      B=runif(1e6),
                      C=rpois(1e6, 1),
                      D=sample(letters, 1e6, replace=TRUE),
                      E=factor(sample(LETTERS, 1e6, replace=TRUE)))
object.size(large_df)
#> 32005840 bytes

# Export
fst_fname <- tempfile(fileext=".fst")
export(large_df, fst_fname)

# Import everything
import(fst_fname)
#> DataFrame with 1000000 rows and 5 columns
#>                 A         B         C           D        E
#>         <numeric> <numeric> <integer> <character> <factor>
#> 1        0.980640 0.7197927         1           i        N
#> 2        0.914788 0.0119196         1           e        W
#> 3       -0.816683 0.5239682         1           g        Z
#> 4       -1.423527 0.9505083         1           o        B
#> 5       -0.552832 0.0801431         0           x        M
#> ...           ...       ...       ...         ...      ...
#> 999996  -0.204663 0.6810639         1           n        O
#> 999997   0.407041 0.3052591         0           a        M
#> 999998  -1.042158 0.0618068         0           r        E
#> 999999   0.986843 0.8920404         0           i        G
#> 1000000 -0.701197 0.7590176         3           n        R

# Import subset of rows and columns
import(fst_fname, columns=c("A", "E"), from=100, to=150)
#> DataFrame with 51 rows and 2 columns
#>             A        E
#>     <numeric> <factor>
#> 1    0.616826        T
#> 2    0.707906        N
#> 3   -1.440993        X
#> 4    0.045180        K
#> 5   -0.618507        G
#> ...       ...      ...
#> 47   2.120486        I
#> 48  -1.498224        K
#> 49  -1.021795        F
#> 50  -0.616418        I
#> 51  -1.050277        R

# Introspection
ff <- FstFile(fst_fname)
resource(ff)
#> [1] "/var/folders/gw/tt90wf010d76wtzgtq2532k00000gp/T//Rtmp3noyzJ/file1e2a3b10013d.fst"
dim(ff)
#> [1] 1000000       5
colnames(ff)
#> [1] "A" "B" "C" "D" "E"
type(ff)
#>           A           B           C           D           E 
#>    "double"    "double"   "integer" "character"    "factor"
```

Use .fst files to store array data (all columns of the same type) with
the `FstArray`-class:

``` r
# FstArray doesn't read the data from disk until needed
fa <- FstArray(fst_fname)

# Resulting object is small, as data is only read from disk when needed.
object.size(fa)
#> 2248 bytes

# The type of the resulting array can be directly controlled
fa_int <- FstArray(fst_fname, type="integer")

# The default is to use all numeric and integer columns, but this can be manually specified
fa_char <- FstArray(fst_fname, columns=c("D", "E"))

# All DelayedArray operations are supported for FstArray
fa + 1
#> <1000000 x 3> DelayedMatrix object of type "double":
#>                      A           B           C
#>       [1,]   1.9806399   1.7197927   2.0000000
#>       [2,]   1.9147876   1.0119196   2.0000000
#>       [3,]   0.1833170   1.5239682   2.0000000
#>       [4,]  -0.4235269   1.9505083   2.0000000
#>       [5,]   0.4471679   1.0801431   1.0000000
#>        ...           .           .           .
#>  [999996,]  0.79533707  1.68106390  2.00000000
#>  [999997,]  1.40704116  1.30525906  1.00000000
#>  [999998,] -0.04215793  1.06180683  1.00000000
#>  [999999,]  1.98684340  1.89204044  1.00000000
#> [1000000,]  0.29880298  1.75901759  4.00000000
rowSums(fa[100:110,])
#>  [1]  0.9893867  3.5548669 -0.2053424  0.1370226 -0.3434429  4.1886792
#>  [7]  1.3799390  1.9047192  1.3660852  0.5376370  0.6721188
# See DelayedArray for more
```

Use .fst files to store data.frame data (columns of different types)
using the `FstColumnVector`-class:

``` r
# FstColumnVectors
fcvs <- FstColumnVectors(fst_fname)

# Delayed operations are supported for each column, causing it to be demoted to a 1D DelayedArray tracking the delayed operations.
fcvs |> 
    subset(D == "a") |> 
    transform(F = A+B) |>
  transform(G = as.vector(F)) # Force delayed operations using as.vector
#> DataFrame with 38382 rows and 7 columns
#>                          A                 B              C              D
#>             <DelayedArray>    <DelayedArray> <DelayedArray> <DelayedArray>
#> 1        0.113745241060956 0.855473468778655              0              a
#> 2       0.0195232734151196  0.92870041471906              1              a
#> 3        0.595042302364098 0.784896731376648              0              a
#> 4        0.623334380566332 0.744932850822806              1              a
#> 5         0.61330045101422 0.905134091852233              2              a
#> ...                    ...               ...            ...            ...
#> 38378   -0.886783848997674 0.755716235144064              2              a
#> 38379 -0.00753870624535636 0.356654018163681              1              a
#> 38380    0.796909674211175 0.257475262042135              1              a
#> 38381    -1.29794885175482 0.569168557645753              1              a
#> 38382    0.407041157796897 0.305259058484808              0              a
#>                    E                  F         G
#>       <DelayedArray>     <DelayedArray> <numeric>
#> 1                  E  0.969218709839611  0.969219
#> 2                  T   0.94822368813418  0.948224
#> 3                  H   1.37993903374075  1.379939
#> 4                  U   1.36826723138914  1.368267
#> 5                  E   1.51843454286645  1.518435
#> ...              ...                ...       ...
#> 38378              B -0.131067613853611 -0.131068
#> 38379              J  0.349115311918325  0.349115
#> 38380              U   1.05438493625331  1.054385
#> 38381              U -0.728780294109064 -0.728780
#> 38382              M  0.712300216281704  0.712300
```
