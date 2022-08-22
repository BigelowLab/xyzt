#' Retrieve the POSIX epoch
#' 
#' @export
#' @param x character date and time of epoch
#' @param tz character, time zone
#' @param ... other arguments for \code{\link[base]{as.POSIXct}}
#' @return POSIXct time
POSIX_epoch <- function(x = '1970-01-01 00:00:00', tz = 'UTC', ...){
  as.POSIXct(x, tz = tz, ...)
}


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
  dimset <- tolower(strsplit(dims, "", fixed = TRUE)[[1]])
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
#' correctly and not closed (we'll close it for you.) 
#' 
#' Note that attributes (variables) are sliced to just the first record attributes.
#' 
#' @export
#' @param x POINTS object
#' @param ... arguments for \code{\link{as_POINT}}
#' @return sf object with one POLYGON record
#' @seealso \href{https://stackoverflow.com/questions/48383990/convert-sequence-of-longitude-and-latitude-to-polygon-via-sf-in-r}{Stackoverflow discussion}
as_POLYGON <- function(x, ...){
  if(!inherits(x, "sf")) x <- as_POINT(x, ...)
  g <- sf::st_geometry(x)
  xy <- sf::st_coordinates(g)
  d <- get_geometry_dimension(x)
  nd <- nchar(d)
  if (nd == 3){
    gc <- sf::st_coordinates(x) 
    xy <- cbind(xy, rep(gc[1,3], nrow(xy)))
  } else if (nd == 4){
    gc <- sf::st_coordinates(x)
    xy <- cbind(xy, rep(gc[1,3], nrow(xy)), rep(gc[1,4], nrow(xy)))
  }
  p <- sf::st_polygon(list(rbind(xy, xy[1,])))
  xy <- sf::st_sfc(p, crs = sf::st_crs(g))
  dplyr::slice(x, 1) |>             # retain first row of attributes
    sf::st_set_geometry(xy) |>      # first set the geometry column
    sf::st_set_geometry("geometry") # then rename it
}

#' Convert any object to a 2d bounding box \code{bbox} or \code{POLYGON}.  
#' 
#' @export
#' @param x sf object
#' @param form character, one of "POLYGON" or "bbox" to determine
#'   the class of the data returned.  See \code{\link[sf]{st_bbox}}.
#' @param buffer one or two element positive numeric, used to buffer the bounding
#'   box by this much in the [x,y] directions.  A very simplistic approach
#'   see if \code{\link[sf]{st_buffer}} might be better.  By default there is
#'   no buffer.
#' @param ... arguments for \code{\link{as_POINT}}
#' @return sf object with one POLYGON record OR a \code{st_bbox} object
as_BBOX <- function(x, 
                    form = c("POLYGON", "bbox")[1], 
                    buffer = c(0,0),
                    ...){
  
  if(!inherits(x, "sf")) x <- as_POINT(x, ...)
  bb <- sf::st_bbox(x) 
  if (length(buffer) == 1) buffer <- c(buffer, buffer)
  bb <- bb + c(-buffer, buffer)
  if (tolower(form[1]) == "polygon"){
    bb <- sf::st_as_sfc(bb, crs = sf::st_crs(x))
  }
  bb
  #xy <- cbind(bb[c(1,3,3,1,1)], bb[c(2,2,4,4,2)])
  #d <- get_geometry_dimension(x)
  #nd <- nchar(d)
  #if (nd == 3){
  #  gc <- sf::st_coordinates(x) 
  #  xy <- cbind(xy, rep(gc[1,3], nrow(xy)))
  #} else if (nd == 4){
  #  gc <- sf::st_coordinates(x)
  #  xy <- cbind(xy, rep(gc[1,3], nrow(xy)), rep(gc[1,4], nrow(xy)))
  #}
  #p <- sf::st_polygon(list(xy))
  #xy <- sf::st_sfc(p, crs = sf::st_crs(x))
  #dplyr::slice(x, 1) |>            # retain first row of attributes
  # sf::st_set_geometry(xy) |>      # first set the geometry column
  # sf::st_set_geometry("geometry") # then rename it
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