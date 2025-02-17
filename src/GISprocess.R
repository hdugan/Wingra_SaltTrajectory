# Read in and process GIS data
library(sf)

daneroads = st_read('GISdata/DANE_ROADS_ROW_2024.gdb/', layer = 'DANE_ROADS_2024')
yaharaLakes = st_read('GISdata/YaharaLakes/YaharaLakes_DaneCty.shp')

wingraWS = st_read('GISdata/YaharaBasins/Wingra_Basin.shp')
mendotaWS = st_read('GISdata/YaharaBasins/Mendota_Basin.shp')
mononaWS = st_read('GISdata/YaharaBasins/Monona_Basin.shp')
subbasinsWS = st_read('GISdata/YaharaBasins/Yahara_subBasins.shp')
mononaWS2 = subbasinsWS |> filter(BasinName %in% c('Starkweather Creek', 'Lake Monona')) |> 
  st_union()

# Ensure both shapefiles are in the same CRS (Coordinate Reference System)
wingraWS <- st_transform(wingraWS, st_crs(yaharaLakes))
mendotaWS <- st_transform(mendotaWS, st_crs(yaharaLakes))
mononaWS <- st_transform(mononaWS, st_crs(yaharaLakes))
mononaWS2 <- st_transform(mononaWS2, st_crs(yaharaLakes))

# Perform the difference operation: subtract shapefile1 from shapefile2
wingraCat <- st_difference(wingraWS, yaharaLakes |> filter(NAME == 'Lake Wingra'))
mendotaCat <- st_difference(mendotaWS, yaharaLakes |> filter(NAME == 'Lake Mendota'))
mononaCat <- st_difference(mononaWS, yaharaLakes |> filter(NAME == 'Lake Monona'))
mononaCat2 <- st_difference(mononaWS2, yaharaLakes |> filter(NAME == 'Lake Monona'))

# Optionally, plot the resulting catchments
plot(st_geometry(wingraCat))
plot(st_geometry(mendotaCat))
plot(st_geometry(mononaCat))
plot(st_geometry(mononaCat2), col = 'red')

# Perform intersection on road datasets
wingraRoads = st_intersection(daneroads, wingraCat)
plot(st_geometry(wingraRoads))
mendotaRoads = st_intersection(daneroads, mendotaCat)
mononaRoads = st_intersection(daneroads, mononaCat)
mononaRoads2 = st_intersection(daneroads, mononaCat2)

table(wingraRoads$RdCode)
ggplot(wingraRoads) +
  geom_sf()+
  facet_wrap(~RdCode)

# Length of roads in the catchments 
sum(as.numeric(st_length(wingraRoads))) * 0.3048 # convert survey foot to meters
sum(as.numeric(st_length(mendotaRoads))) * 0.3048 # convert survey foot to meters
sum(as.numeric(st_length(mononaRoads2))) * 0.3048 # convert survey foot to meters
