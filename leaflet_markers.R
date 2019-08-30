library(leaflet)

long_lat_data<-read_excel("data/CDNA_SistemOrganisasi.xlsx")
long_lat_data$`_respgeopoint_latitude` <- as.numeric(long_lat_data$`_respgeopoint_latitude`)
long_lat_data$`_respgeopoint_longitude` <- as.numeric(long_lat_data$`_respgeopoint_longitude`)
kobo_data <- subset(long_lat_data, select=c(`_respgeopoint_latitude`, `_respgeopoint_longitude`, provinsi_001))
colnames(kobo_data) = c("lat", "long", "prov")
leaflet(data = kobo_data) %>% addTiles() %>%
  addMarkers(~`_respgeopoint_longitude`, ~`_respgeopoint_latitude`, popup = ~as.character(provinsi_001), label = ~as.character(provinsi_001)) 

leaflet(data = kobo_data) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)

data(quakes)

# Show first 20 rows from the `quakes` dataset
leaflet(data = quakes[1:20,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))