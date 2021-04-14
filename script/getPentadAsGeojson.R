
library(dplyr)
library(leaflet)
library(rgdal)



list_json1 <- jsonlite::fromJSON("http://api.adu.org.za/sabap2/v2/coverage/country/kenya")
coordinates<- list_json1[3]$data$coverage_pentad_monthly
coordinates$pentad_codes = coordinates$pentad

# find_pentad_coordinates(coordinates$pentad, plotting = TRUE)
# pentad_codes <- coordinates$pentad
res <- 5/60
pentad

coordinates <- coordinates %>%
  mutate(letter = substr(pentad_codes, 5, 5)) %>%
  mutate(
    lat = as.numeric(substr(pentad_codes, 1, 2)) + as.numeric(substr(pentad_codes, 3, 4))/60,
    lon = as.numeric(substr(pentad_codes, 6, 7)) + as.numeric(substr(pentad_codes, 8, 9))/60
  ) %>%
  mutate(
    lat = ifelse((letter == '_' | letter == 'a'), -lat, lat),
    lon = ifelse((letter == 'a' | letter == 'b'), -lon, lon)
  )

pentads_poly<-list()
for (i in 1:nrow(coordinates)){
  pentads_poly[[i]] <- sp::Polygons(list(
    sp::Polygon( cbind(
      c(coordinates[i,'lon'], coordinates[i,'lon'], coordinates[i,'lon']+res, coordinates[i,'lon']+res, coordinates[i,'lon']),
      c(coordinates[i,'lat'], coordinates[i,'lat']-res, coordinates[i,'lat']-res, coordinates[i,'lat'], coordinates[i,'lat'])
    ))
  ), i)
}

pentads_poly <- sp::SpatialPolygons(pentads_poly)
pid <- sapply(slot(pentads_poly, "polygons"), function(x) slot(x, "ID"))
p.df <- data.frame( ID=1:length(pentads_poly), row.names = pid)
p <- sp::SpatialPolygonsDataFrame(pentads_poly, p.df)
p@data$pentad_code <- pentad_codes
p@data$card <- as.numeric(coordinates$Total)

p %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons(label = ~paste0(pentad_code))



geojson::geo_write(geojson::as.geojson(p),'pentads.geojson')

#proj4string(p) <- CRS("+init=epsg:28992")
#p_ll <- spTransform(p, CRS("+proj=longlat +datum=WGS84"))
#writeOGR(p_ll, "pentad.kml", layer="test", driver="KML")
