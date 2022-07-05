#' Get geometry dimension code
#' 
#' @export
#' @param x sf or sfc object
#' @return character vector such as "XY" or "XYZ"
get_geometry_dimension <- function(x){
  x <- sf::st_geometry(x)
  sort(unique(sapply(x, function(x) class(x)[1])))
}

#' Get geometry type code
#' 
#' @export
#' @param x sf or sfc object
#' @return character vector such as "POINT" or "POLYGON"
get_geometry_type <- function(x){
  klass <- sf::st_geometry(x) |>
    class()
  sub("sfc_", "", klass[1])
}

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
  ix <- which(tolower(xnames) %in% lut$x)[1]
  if (length(ix) == 0){
    stop("input must have one column named one of:", paste(lut$x, collapse = ", "))
  } 
  iy <- which(tolower(xnames) %in% lut$y)[1]
  if (length(iy) == 0){
    stop("input must have one column named one of:", paste(lut$y, collapse = ", "))
  }
  if ("z" %in% dimset){
    hasZ <- TRUE
    iz <- which(tolower(xnames) %in% lut$z)[1]
    if (length(iz) == 0){
      stop("input must have one column named one of:", paste(lut$z, collapse = ", "))
    } 
  } else {
    hasZ <- FALSE
  }
  if ("t" %in% dimset){
    hasT <- TRUE
    it <- which(tolower(xnames) %in% lut$t)[1]
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

#' Convert a POINTS object to a POLYGON.  Assumes that the points are ordered
#' correctly and not closed (we'll close it for you.) Note that attributes (variables)
#' are dropped.
#' 
#' @export
#' @param x POINTS object
#' @return sf object with one POLYGON record
#' @seealso \href{https://stackoverflow.com/questions/48383990/convert-sequence-of-longitude-and-latitude-to-polygon-via-sf-in-r}{Stackoverflow discussion}
as_POLYGON <- function(x){
  if(!inherits(x, "sf")) x <- as_POINT(x)
  g <- sf::st_geometry(x)
  xy <- sf::st_coordinates(g)
  xy <- sf::st_sfc(sf::st_polygon(list(rbind(xy, xy[1,]))))
  sf::st_sf(dplyr::tibble(xy), 
                crs = sf::st_crs(g))
}

#' Convert any object to a bounding box POLYGON.  
#' Note that attributes (variables) are dropped.
#' 
#' @export
#' @param x sf object
#' @return sf object with one POLYGON record
as_BBOX <- function(x){
  if(!inherits(x, "sfc")) x <- as_POINT(x)
  bb <- sf::st_bbox(x)
  xy <- cbind(bb[c(1,3,3,1,1)], bb[c(2,2,4,4,2)])
  xy <- sf::st_sfc(sf::st_polygon(list(xy)))
  sf::st_sf(xy, 
            crs = sf::st_crs(x))
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