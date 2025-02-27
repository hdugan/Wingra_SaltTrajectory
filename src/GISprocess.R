# Read in and process GIS data
library(sf)

daneroads = st_read('GISdata/DANE_ROADS_ROW_2024.gdb/', layer = 'DANE_ROADS_2024')
table(daneroads$RdCode)

# st_write(daneroads, 'GISdata/daneroads.shp')
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

# Get city/county boundaries
library(tigris)
library(sf)

# Get the county boundaries for Wisconsin
wi_counties <- counties(state = "WI", cb = TRUE, class = "sf")
# Filter for Dane County
dane_county <- wi_counties[wi_counties$NAME == "Dane", ]
# Plot to verify
plot(st_geometry(dane_county), main = "Dane County, WI")

# load madison boundary
madison = st_read('GISdata/Madison/City_Limit.shp')
dane = st_transform(dane_county, st_crs(madison))

################ LAKE MENDOTA ################ 
# Area of Madison within Lake Mendota catchment
# Find the intersection (overlapping area)
intersection <- st_intersection(madison, mendotaCat)
# Calculate the area of Shapefile 1
area_shp1 <- st_area(madison)
# Calculate the area of the intersection
area_intersection <- st_area(intersection)
# Compute the percentage of Shapefile 1 that falls within Shapefile 2
percent_inside_madison <- (sum(area_intersection) / sum(area_shp1)) * 100

# Area of Dane County within Lake Mendota catchment
intersection <- st_intersection(dane, mendotaCat)
# Calculate the area of Shapefile 1
area_shp1 <- st_area(dane)
# Calculate the area of the intersection
area_intersection <- st_area(intersection)
# Compute the percentage of Shapefile 1 that falls within Shapefile 2
percent_inside_dane <- (sum(area_intersection) / sum(area_shp1)) * 100

# Area of Dane County within Lake Mendota catchment
daneHighways = daneroads |> filter(RdCode %in% c('County Highway','Interstate Highway','State Highway','US Highway'))
intersection <- st_intersection(daneHighways, mendotaCat)
intersectionRoads = sum(st_length(intersection))/sum(st_length(daneHighways))

# For Dane county: Road intersection says 16.5% and area intersection says 15%, so very close via both methods 
ggplot(dane) +
  geom_sf() +
  geom_sf(data = madison) +
  geom_sf(data = mendotaCat, fill = NA, color = 'blue')

################ LAKE WINGRA ################ 
# Area of Madison within Lake Wingra catchment
# Find the intersection (overlapping area)
intersection <- st_intersection(madison, wingraCat)
# Calculate the area of Shapefile 1
area_shp1 <- st_area(madison)
# Calculate the area of the intersection
area_intersection <- st_area(intersection)
# Compute the percentage of Shapefile 1 that falls within Shapefile 2 = #8.66
percent_inside_madison <- (sum(area_intersection) / sum(area_shp1)) * 100

# Area of Dane County within Lake Wingra catchment
intersection <- st_intersection(dane, wingraCat)
# Calculate the area of Shapefile 1
area_shp1 <- st_area(dane)
# Calculate the area of the intersection
area_intersection <- st_area(intersection)
# Compute the percentage of Shapefile 1 that falls within Shapefile 2 = #0.61
percent_inside_dane <- (sum(area_intersection) / sum(area_shp1)) * 100

# Area of Dane County within Lake Mendota catchment
daneHighways = daneroads |> filter(RdCode %in% c('County Highway','Interstate Highway','State Highway','US Highway'))
intersection <- st_intersection(daneHighways, mendotaCat)
intersectionRoads = sum(st_length(intersection))/sum(st_length(daneHighways))

# For Dane county: Road intersection says 16.5% and area intersection says 15%, so very close via both methods 
ggplot(dane) +
  geom_sf() +
  geom_sf(data = madison) +
  geom_sf(data = mendotaCat, fill = NA, color = 'blue')
