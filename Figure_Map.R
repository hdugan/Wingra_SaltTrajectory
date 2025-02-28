library(sf)       # For handling spatial data
library(terra)    # For raster data
library(tmap)     # For map visualization
library(FedData)
library(ggspatial)
library(tidyverse)

# Load watershed
wingraWS = st_read('GISdata/YaharaBasins/Wingra_Basin.shp')
lakewingra = st_read('GISdata/YaharaLakes/YaharaLakes_DaneCty.shp') %>% 
  filter(NAME == 'Lake Wingra')

# Load NCLD data
nlcd <- get_nlcd(template = wingraWS, 
                 label = "Wingra", year = 2019, dataset = "landcover")
# Plot with terra::plot
terra::plot(nlcd)

nlcd.2023 = terra::rast('GISdata/NCLD/Annual_NLCD_LndCov_2023_CU_C1V0_5yBUfLryZkDCeOYocVzN.tiff')
terra::plot(nlcd.2023)

# Transform CRS
wingraWS = st_transform(wingraWS, st_crs(nlcd.2023))
lakewingra = st_transform(lakewingra, st_crs(nlcd.2023))

# Crop raster and mask to watershed 
cropped_landuse <- crop(nlcd.2023, wingraWS)
masked_landuse <- mask(cropped_landuse, wingraWS)

# Convert SpatRaster to Dataframe
landuse_df <- as.data.frame(masked_landuse, xy = TRUE)  # Include x, y coordinates
colnames(landuse_df) <- c("x", "y", "landuse")  # Rename columns for clarity

landuse_background <- as.data.frame(cropped_landuse, xy = TRUE)  # Include x, y coordinates
colnames(landuse_background) <- c("x", "y", "landuse")  # Rename columns for clarity

# extract colors
landuse_classes <- cats(nlcd)[[1]]  # Get category names
landuse_colors <- terra::coltab(nlcd)  # Get associated colors

# Merge some classes in the palette
landuse_classes = landuse_classes %>% 
  mutate(Class_merge = recode(Class, 
                              "Pasture/Hay" = "Pasture/Crops", 
                              "Cultivated Crops" = "Pasture/Crops",
                              "Shrub/Scrub" = "Shrub/Grassland", 
                              "Grassland/Herbaceous" = "Shrub/Grassland",
                              "Woody Wetlands" = "Wetlands", 
                              "Barren Land (Rock/Sand/Clay)" = "Barren Land",
                              "Emergent Herbaceous Wetlands" = "Wetlands",
                              "Evergreen Forest" = "Forest", 
                              "Deciduous Forest" = "Forest",
                              "Mixed Forest" = "Forest"))
# Convert to a named vector for ggplot2
palette_OG <- setNames(landuse_classes$Color, landuse_classes$Class) 
palette_merge <- setNames(landuse_classes$Color, landuse_classes$Class_merge) 
palette_merge["Barren Land"] = '#524a30'
palette_merge["Shrub/Grassland"] = '#a6c7a5'

# Merge landuse
landuse_merge = landuse_df %>% 
  left_join(landuse_classes %>% select(-Description), by = c('landuse' = 'ID')) %>% 
  mutate(landuse = Class_merge)
  # mutate(landuse = recode(landuse, 
  #        "Pasture/Hay" = "Pasture/Crops", 
  #        "Cultivated Crops" = "Pasture/Crops",
  #        "Shrub/Scrub" = "Shrub/Grassland", 
  #        "Grassland/Herbaceous" = "Shrub/Grassland",
  #        "Barren Land (Rock/Sand/Clay)" = "Barren Land",
  #        "Woody Wetlands" = "Wetlands", 
  #        "Emergent Herbaceous Wetlands" = "Wetlands",
  #        "Evergreen Forest" = "Forest", 
  #        "Deciduous Forest" = "Forest",
  #        "Mixed Forest" = "Forest")) %>% 
  # group_by(landuse) 

landuse_background = landuse_background %>% 
  left_join(landuse_classes %>% select(-Description), by = c('landuse' = 'ID')) %>% 
  mutate(landuse = Class_merge)

ggplot() +
  geom_raster(data = landuse_df, aes(x = x, y = y, fill = factor(landuse))) +
  geom_sf(data = wingraWS, fill = NA, color = "black", size = 1) +
  scale_fill_manual(values = palette_OG, name = "Land Use") +
  coord_sf() +
  theme_bw(base_size = 10) +
  theme(legend.text = element_text(size = 8))

ggplot() +
  geom_raster(data = landuse_background, 
              aes(x = x, y = y, fill = factor(landuse)), alpha = 0.2) +
  geom_raster(data = landuse_merge, 
              aes(x = x, y = y, fill = factor(landuse))) +
  geom_sf(data = wingraWS, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = lakewingra, fill = '#5475A8') +
  annotate('text',x = 532800, y = 2247150, label = "Lake Wingra", 
           size = 2.5, angle = 25, color = 'white', fontface = 2) +
  scale_fill_manual(values = palette_merge, name = "Land Use") +
  coord_sf(expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.3) +  # Adds scale bar
  theme_bw(base_size = 10) +
  theme(legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.key.height = unit(0.4,'cm'),
        # legend.position = 'bottom',
        axis.title = element_blank())

ggsave('Figures_new/Map_2023.png', width = 6, height = 2.5, dpi = 500)

# Land use stats 
landuse_merge %>% ungroup() %>% 
  mutate(group = str_detect(landuse, "Developed")) %>% 
  mutate(tot = n()) %>% 
  group_by(group) %>% 
  summarise(n = n()/first(tot))

# 74.1% Developed land use vs other
# 54.7% "low-high intensity developed" 

landuse_merge %>% ungroup() %>% 
  mutate(group = str_detect(landuse, "Open Space")) %>% 
  mutate(tot = n()) %>% 
  group_by(group) %>% 
  summarise(n = n()/first(tot))
