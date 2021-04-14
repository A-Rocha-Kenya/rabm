#' Find the GPS coordinate of pentad code
#'
#' @param pentad_codes A list of pentad codes
#'
#' @return list of pentad code string
#'
#' @export
#'
#' @examples
#'
#' # get pentad for a list of coordinate
#' pentad_codes <- data.frame(lon = -122.335167, lat = 47.608013)
#' pentad <- find_pentad_coordinates(coordinates)
#'
#' pentad_codes <- data.frame(lon = c(-10,10,10,-10),lat = c(10,-10,10,-10))
#' pentad <- find_pentad_coordinates(coordinates)
#'
#'
find_pentad_coordinates <- function(pentad_codes, plotting=FALSE) {

  Check <- ArgumentCheck::newArgCheck()
  #if (typeof(pentad_codes) != 'list') {
  #  ArgumentCheck::addError(msg = "`pentad` needs to be a list",
  #                          argcheck = Check)
  #}
  ArgumentCheck::finishArgCheck(Check)

  res <- 5/60

  coordinates <- data.frame(pentad_codes = pentad_codes) %>%
    dplyr::mutate(letter = substr(pentad_codes, 5, 5)) %>%
    dplyr::mutate(
      lat = as.numeric(substr(pentad_codes, 1, 2)) + as.numeric(substr(pentad_codes, 3, 4))/60,
      lon = as.numeric(substr(pentad_codes, 6, 7)) + as.numeric(substr(pentad_codes, 8, 9))/60
    ) %>%
    dplyr::mutate(
      lat = ifelse((letter == '_' | letter == 'a'), -lat, lat),
      lon = ifelse((letter == 'a' | letter == 'b'), -lon, lon)
      )

  if (plotting){
    pentads_poly<-list()
    for (i in 1:nrow(coordinates)){
      pentads_poly[[i]] <- sp::Polygons(list(
        sp::Polygon( cbind(
          c(coordinates[i,'lon'], coordinates[i,'lon'], coordinates[i,'lon']+res, coordinates[i,'lon']+res, coordinates[i,'lon']),
          c(coordinates[i,'lat'], coordinates[i,'lat']+res, coordinates[i,'lat']+res, coordinates[i,'lat'], coordinates[i,'lat'])
        ))
      ), i)
    }
    pentads_poly <- sp::SpatialPolygons(pentads_poly)
    pid <- sapply(slot(pentads_poly, "polygons"), function(x) slot(x, "ID"))
    p.df <- data.frame( ID=1:length(pentads_poly), row.names = pid)
    p <- sp::SpatialPolygonsDataFrame(pentads_poly, p.df)
    p@data$pentad_code <- pentad_codes

    p %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(label = ~paste0(pentad_code))
  }

  coordinates %>%
    dplyr::select(lat, lon)
}
