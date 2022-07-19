#' Extract a bb vector of [xmin, ymin, xmax, ymax] from numeric vector or sf object
#' 
#' @export
#' @param x object accepted by st_bbox()
#' @return bb vector of [xmin, ymin, xmax, ymax]
as_bb <- function(x) {
  if (!inherits(x, "sf") || !inherits(x, "sfc")) x <- sf::st_bbox(x)
  x <- as.numeric(x)
  names(x) <- c("xmin", "ymin", "xmax", "ymax")
  class(x) <- "bb"
  x
}


#' Construct a bbox from a 'bb' object or [xmin, ymin, xmax, ymax] numeric vector
#' 
#' @export
#' @param x object accepted by \code{\link{as_bb}}
#' @param crs coordinate reference system, such as '4326'
#' @return \code{\link[sf]{st_bbox}}
as_st_bbox <- function(x, crs = 4326) {
  if (!inherits(x, "bb")) x <- as_bb(x)
  sf::st_bbox(unclass(x), crs = crs)
}


#' Convert bounding box [0,360] longitudes to [-180, 180]
#'
#' Bounding boxes are 4 element vectors of [xmin, ymin, xmax, ymax]
#'
#' @export
#' @param x numeric bounding box vector, no check is done for being withing 0,360 range
#' @return numeric bounding box vector
bb_to_180 <- function(x) {
  if (!inherits(x, "bb")) x <- as_bb(x)
  ix <- c(1,3)
	x[ix] <- to_180(x[ix])
	if (identical(x[ix[1]], 180)) x[ix[1]] <- -180
	if (identical(x[ix[2]], -180)) x[ix[2]] <- 180
	x
}

#' Convert [-180,180] bounding box longitudes to [0,360]
#'
#' Bounding boxes are 4 element vectors of [xmin, ymin, xmax, ymax]
#'
#' @export
#' @param x numeric bounding box vector, no check is done for being withing 0,360 range
#' @return numeric bounding box vector
bb_to_360 <- function(x) {
  if (!inherits(x, "bb")) x <- as_bb(x)
  ix <- c(1,3)
	x[ix] <- to_360(x[ix])
	if (identical(x[ix[1]], 360)) x[ix[1]] <- 0   # western edge
	if (identical(x[ix[2]], 0)) x[ix[2]] <- 360   # eastern edge
	x
}

#' Convert [0,360] longitudes to [-180, 180]
#'
#' @seealso \url{https://gis.stackexchange.com/questions/201789/verifying-formula-that-will-convert-longitude-0-360-to-180-to-180/201793}
#' @export
#' @param x numeric vector, no check is done for being withing [0, 360] range
#' @return numeric vector
to_180 <- function(x) { ((x + 180) %% 360) - 180 }

#' Convert [-180,180] longitudes to [0, 360]
#'
#' @seealso \url{https://gis.stackexchange.com/questions/201789/verifying-formula-that-will-convert-longitude-0-360-to-180-to-180/201793}
#' @export
#' @param x numeric vector, no check is done for being within [0,3 60] range
#' @return numeric vector
to_360 <- function(x) {x %% 360}


#' Convert a 4-element bbox vector to a sf bbox object
#'
#' @export
#' @param bb a 4-element numeric vector of [xmin, ymin, xmax, ymax] coordinates
#' @param crs character, the coordinate reference system
#' @return sf bbox object
bb_to_bbox <- function(bb = c(-72, 39, -63, 46),
                       crs = 4326){
  if (!inherits(bb, "bb")) bb <- as_bb(bb)
  sf::st_bbox(c(xmin = bb[1], xmax = bb[3], ymin = bb[2], ymax = bb[4]),
              crs = crs)
}

#' Convert a 4-element bounding box vector to a polygon simple feature object.
#'
#' Alternatively, input a list of bounding boxes, each is transformed into one
#' feature in a multi-feature object.
#'
#' @export
#' @param bb a 4-element numeric vector of [xmin, ymin, xmax, ymax]
#'        coordinates or a list of the same.
#' @param crs character, the coordinate reference system
#' @return a simple feature with a POLYGON or multiple feature object
bb_to_polygon <- function(bb = c(-72, 39, -63, 46),
                          crs = 4326){
  if (!inherits(bb, "bb")) bb <- as_bb(bb)
  if (is.list(bb)){
    pp <- lapply(seq_len(length(bb)),
                 function(i){
                   x <- bb[[i]]
                   sf::st_polygon(x = list(cbind(x[c(1,3,3,1,1)],
                                                 x[c(2,2,4,4,2)]))) |>
                     sf::st_sfc(crs = crs) |>
                     sf::st_sf() |>
                     dplyr::mutate(ID = i)
                 })
    p <- do.call(rbind, pp)
  } else {
    p <- sf::st_polygon(x = list(cbind(bb[c(1,3,3,1,1)], bb[c(2,2,4,4,2)]))) |>
      sf::st_sfc(crs= crs) |>
      sf::st_sf()
  }
  p
}

#' Split a bounding box into two at \code{at}
#'
#' @export
#' @param bb numeric, 4 element bounding box of [xmin, ymin, xmax, ymax] coordinates
#' @param at numeric, longitude to split around
#' @return list of one or two bounding box vectors
bb_split <- function(bb = c(-170, -60, 50, 60),
                     at = 0){
  if (!inherits(bb, "bb")) bb <- as_bb(bb)
  if (bb_straddles(bb, at = at)){
    x <- list(
      bb1 = c(bb[1], at, bb[c(2,4)]),
      bb2 = c(at, bb[c(3,2,4)])
    )
  } else {
    x <- list(bb1 = bb)
  }
  x
}

#' Test if a bounding box straddles a longitude
#'
#' @export
#' @param bb numeric, 4 element bounding box of [xmin, ymin, xmax, ymax] coordinates
#'   or an object accepted by \code{\link{as_bb}}
#' @param at numeric, longitude to test straddle-ness
#' @return logical
bb_straddles <- function(bb = c(-170,-60, 50,60),
                         at = 0){
  
  if (!inherits(bb, "bb")) bb <- as_bb(bb)
  bb[1] < at && bb[3] > at
}
