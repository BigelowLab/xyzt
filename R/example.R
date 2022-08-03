#' Read an example point dataset for Gulf of Maine
#' 
#' @export
#' @param filename character, the file to read
#' @return tibble
read_gom <- function(filename = system.file(file.path("extdata", "gom.csv"), 
                                            package = 'xyzt')){
  readr::read_csv(filename, 
                  col_types = readr::cols(
                    id = readr::col_character(),
                    name = readr::col_character(),
                    lon = readr::col_double(),
                    lat = readr::col_double(),
                    depth = readr::col_double(),
                    time = readr::col_datetime(format = "") ))
}


#' Read an example point dataset for South Atlantic Bight
#' 
#' @export
#' @param filename character, the file to read
#' @return tibble
read_sab <- function(filename = system.file(file.path("extdata", "sab.csv"), 
                                            package = 'xyzt')){
  readr::read_csv(filename, 
                  col_types = readr::cols(
                    id = readr::col_character(),
                    name = readr::col_character(),
                    lon = readr::col_double(),
                    lat = readr::col_double(),
                    depth = readr::col_double(),
                    time = readr::col_datetime(format = "") ))
}


#' Read an example point dataset for the CalCOFI Sampling Stations (since 1984)
#' 
#' @export
#' @param url character string url to data
#' @return a tibble of CalCOFI points
read_calcofi <- function(url = "https://calcofi.org/downloads/maps/CalCOFIStationOrder.csv") {
  
  readr::read_csv(url,
                  col_types = readr::cols(
                    `Order Occ` = readr::col_double(),
                    Line = readr::col_double(),
                    Sta = readr::col_double(),
                    `Lat (dec)` = readr::col_double(),
                    `Lat (deg)` = readr::col_double(),
                    `Lat (min)` = readr::col_double(),
                    `Lat (deg min)` = readr::col_character(),
                    `Lon (dec)` = readr::col_double(),
                    `Lon (deg)` = readr::col_double(),
                    `Lon (min)` = readr::col_double(),
                    `Lon (deg min)` = readr::col_character(),
                    `Est Depth` = readr::col_double(),
                    `Sta Type` = readr::col_character()
                  )) |>
    dplyr::select(.data$`Order Occ`,
                  .data$`Lat (dec)`, 
                  .data$`Lon (dec)`, 
                  .data$`Est Depth`) |>
    dplyr::rename(station = .data$`Order Occ`,
                  lat = .data$`Lat (dec)`,
                  lon = .data$`Lon (dec)`,
                  depth = .data$`Est Depth`)
}