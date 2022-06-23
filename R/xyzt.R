#' Convert a data frame (or tibble) to an sf POINT object
#' 
#' @export
#' @param x data frame or tibble
#' @param crs the CRS of the location coordinates
#' @param dims character, one of 'xy', 'xyz' 'xyt' or 'xyzt'
#' @param ... other arguments passed to \code{\link[sf]{st_as_sf}}
#' @return sf POINT object
as_POINT <- function(x = read_gom(),
                     crs = 4326,
                     dims = 'xy',
                     ...){
  if (!inherits(x, "tbl_df")) x <- dplyr::as_tibble(x)
  
  lut <- var_lut()
  dimset <- strsplit(dims, "", fixed = TRUE)[[1]]
  xnames <- colnames(x)
  ix <- which(xnames %in% lut$x)[1]
  if (length(ix) == 0){
    stop("input must have one column named one of:", paste(lut$x, collapse = ", "))
  } 
  iy <- which(xnames %in% lut$y)[1]
  if (length(iy) == 0){
    stop("input must have one column named one of:", paste(lut$y, collapse = ", "))
  }
  if ("z" %in% dimset){
    hasZ <- TRUE
    iz <- which(xnames %in% lut$z)[1]
    if (length(iz) == 0){
      stop("input must have one column named one of:", paste(lut$z, collapse = ", "))
    } 
  } else {
    hasZ <- FALSE
  }
  if ("t" %in% dimset){
    hasT <- TRUE
    it <- which(xnames %in% lut$t)[1]
    if (length(it) == 0){
      stop("input must have one column named one of:", paste(lut$t, collapse = ", "))
    } 
  } else {
    hasT <- FALSE
  }
  
  switch(dims,
    "xy" = sf::st_as_sf(x, coords = c(ix, iy), dim = "XY", crs = crs, ...),
    "xyz" = sf::st_as_sf(x, coords = c(ix, iy, iz), dim = "XYZ", crs = crs, ...),
    "xyt" = sf::st_as_sf(x, coords = c(ix, iy, it), dim = "XYM", crs = crs, ...),
    "xyzt" = sf::st_as_sf(x, coords = c(ix, iy, iz, it), dim = "XYZM", crs = crs, ...))
}


#' Retrieve a series of allowed variable name associations
#' 
#' @export
#' @return list of variable name associations
var_lut <- function(){
  list(
    x = c("x", "lon", "longitude", "long", "lng"),
    y = c("y", "lat", "latitude"),
    z = c("z", "depth", "altitude"),
    t = c("t", "dt", "time", "date", "datetime", "timestamp", "datestamp")
  )
}

#' Assert the the input has a valid time (\code{t}) variable
#' @param x xyzt tibble
#' @return the input tibble
assert_time <- function(x){
  assertthat::assert_that("t" %in% colnames(x))
  assertthat::assert_that(inherits(x$t, "Date") || inherits(x$t, "POSIXt") )
  x
}

#' Assert the input has a valid \code{x} (lon) variable
#' @param x xyzt tibble
#' @return the input tibble
assert_x <- function(x){
  assertthat::assert_that("x" %in% colnames(x))
  assertthat::assert_that(inherits(x$x, "numeric"))
  assertthat::assert_that(x$x >= -180 && x$x <= 180)
  x
}

#' Assert the input has a valid \code{y} (lat) variable
#' @param x xyzt tibble
#' @return the input tibble
assert_y <- function(x){
  assertthat::assert_that("y" %in% colnames(x))
  assertthat::assert_that(inherits(x$y, "numeric"))
  assertthat::assert_that(x$y >= -90 && x$y <= 90)
  x
}

#' Assert the input has a valid \code{z} (depth/altitude) variable
#' @param x xyzt tibble
#' @return the input tibble
assert_z <- function(x){
  assertthat::assert_that("z" %in% colnames(x))
  assertthat::assert_that(inherits(x$y, "numeric"))
  x
}