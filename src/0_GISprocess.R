# Read in and process GIS data
library(sf)
library(tigris)

# Load watersheds
wingraWS = st_read('data_GIS/YaharaBasins/Wingra_Basin.shp')
yaharaLakes = st_read('data_GIS/YaharaLakes/YaharaLakes_DaneCty.shp')

# Ensure both shapefiles are in the same CRS (Coordinate Reference System)
wingraWS <- st_transform(wingraWS, st_crs(yaharaLakes))

# Load big road database
# daneroads = st_read('data_GIS/DANE_ROADS_ROW_2024.gdb/', layer = 'DANE_ROADS_2024')
# write dane county shapefile that intersections yahara basins 
# daneRdIntersect = st_intersection(st_transform(daneroads, st_crs(yaharaLakes)), subbasinsWS)
# st_write(daneRdIntersect |> select(RdCode, MuniName_L), 'data_GIS/DaneCounty_Roads/daneroads.shp', delete_dsn = TRUE)
daneroads = st_read('data_GIS/DaneCounty_Roads/daneroads.shp')

# Perform the difference operation: subtract shapefile1 from shapefile2
wingraCat <- st_difference(wingraWS, yaharaLakes |> filter(NAME == 'Lake Wingra'))

# Optionally, plot the resulting catchments
plot(st_geometry(wingraCat))

# Perform intersection on road datasets
wingraRoads = st_intersection(daneroads, wingraCat)
plot(st_geometry(wingraRoads))

table(wingraRoads$RdCode)
ggplot(wingraRoads) +
  geom_sf()+
  facet_wrap(~RdCode)

# Length of roads in the catchments 
sum(as.numeric(st_length(wingraRoads))) * 0.3048 # convert survey foot to meters

# Get city/county boundaries
# Get the county boundaries for Wisconsin
wi_counties <- counties(state = "WI", cb = TRUE, class = "sf")
# Filter for Dane County
dane_county <- wi_counties[wi_counties$NAME == "Dane", ]
# Plot to verify
plot(st_geometry(dane_county), main = "Dane County, WI")

# load madison boundary
madison = st_read('data_GIS/Madison/City_Limit.shp')
dane = st_transform(dane_county, st_crs(madison))


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

### Impervious Surface Madison Data ###
# is = st_read('data_GIS/Stormwater_Modeling_Impervious_Land_Cover_Type/Stormwater_Modeling_Impervious_Land_Cover_Type.shp')
# table(is$source_a_2)
# 
# is_wingra = st_intersection(is, wingraCat)
# st_write(is_wingra, 'data_GIS/Stormwater_Modeling_Impervious_Land_Cover_Type/WINGRA_Stormwater_Modeling_Impervious_Land_Cover_Type.shp')
# Read in smaller file
is_wingra = st_read('data_GIS/Stormwater_Modeling_Impervious_Land_Cover_Type/WINGRA_Stormwater_Modeling_Impervious_Land_Cover_Type.shp')

is_wingra.paved = is_wingra %>% filter(source_a_2 %in% 
             c('Driveways', 'Parking', 'Sidewalks')) %>% 
  st_transform("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
is_wingra.roads = is_wingra %>% filter(source_a_2 %in% 'Streets') %>% 
  st_transform("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
is_wingra.parkinglots = is_wingra %>% filter(source_a_2 %in% 'Parking') %>% 
  st_transform("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# ggplot(is_wingra.paved) +
#   geom_sf()
# ggplot(is_wingra.roads) +
#   geom_sf()


