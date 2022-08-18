#' Given a URL or ncdf object test if the data matches the 
#' specified data source pattern
#' 
#' @export
#' @param x character or NCDF4 object
#' @param pattern charcater, fixed or regex pattern to match
#' @param fixed logical, is the \code{pattern} argument
#'   a fixed pattern or regular expression
#' @return logical where TRUE finds a match
is_source <- function(x, pattern = "hycom", fixed = TRUE){
  ok <- FALSE
  if (inherits(x, 'ncdf4')){
    ok <- grepl(pattern[1], x$filename, fixed = fixed[1])
  } else if (inherits(x, 'character')){
    ok <- grepl(pattern[1], x[1], fixed = fixed[1])
  }
  ok
}

#' extract generic
#'
#' @export
#' @param x \code{sf} object
#' @param y \code{ncdf4} object
#' @param ... Arguments passed to or from other methods
#' @return data frame of covariates (point, or raster)
extract <- function(x, ...) {
  UseMethod("extract")
}

#' @export
#' @param x \code{sf} object
#' @param y \code{ncdf4} object
#' @describeIn extract Extract data from a NCDF4 object
extract.default <- function(x, y = NULL, ...){
  stop("class not known:", paste(class(x), collapse = ", "))
}

#' @export
#' @param x \code{ncdf4} object
#' @param y \code{sf} object
#' @param verbose logical, output helpful messages?
#' @param ... arguments to be passed to source data \code{extract} method 
#' @describeIn extract Extract from a stars object using any sf object
extract.stars <- function(x, y = NULL, 
                       verbose = FALSE, ...){
  requireNamespace("stars", quietly = TRUE)
  typ <- xyzt::get_geometry_type(x)
  if (verbose[1]) {
    cat("extract.stars typ =", typ, "\n" )
  }
  r <- switch(typ,
         "POINT" = stars::st_extract(x, at = y, ...),
         "POLYGON" = r <- x[y, ...])
  r
}


#' @export
#' @param x \code{ncdf4} object
#' @param y \code{sf} object
#' @param verbose logical, output helpful messages?
#' @param ... arguments to be passed to source data \code{extract} method 
#' @describeIn extract Extract from a NCDF4 object using any sf object
extract.ncdf4 <- function(x, y = NULL, 
                          verbose = FALSE, ...){
  typ <- xyzt::get_geometry_type(y)
  if (verbose[1]) {
    cat("extract.ncdf4 typ =", typ, "\n" )
  }
  
  if (is_source(x, 'oceandata')){
    requireNamespace("obpg", quietly = TRUE)
    r <- obpg::extract(y, x, ...)
  } else if(is_source(x, "ghrsst")){
    requireNamespace("ghrsst", quietly = TRUE )
    r <- ghrsst::extract(y, x, ...)
  } else if(is_source(x, "ersst")){
    requireNamespace("ersst", quietly = TRUE)
    r <- ersst::extract(y, x, ...)
  } else if(is_source(x, "hycom")) {
    requireNamespace("hycom", quietly = TRUE)
    r <- hycom::extract(y, x, ...)
  } else {
    stop("source data not known")
  }
  r
}
