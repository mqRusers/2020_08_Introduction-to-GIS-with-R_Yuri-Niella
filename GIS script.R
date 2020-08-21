#===========================================#
#	 Introduction to GIS with R	    #
#				            #
# Yuri Niella (yuri.niella@hdr.mq.edu.au)   #
# Department of Biological Sciences         #
# Macquarie University 		  	    #
#===========================================#

# Installing packages:
install.packages("tidyverse")
install.packages("maptools")
install.packages("cmocean")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("ggmap")
install.packages("ggrepel")
install.packages("cowplot")
install.packages("sp")
install.packages("sf")
install.packages("ozmaps")
install.packages("raster")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ncdf4")
install.packages("reshape2")
install.packages("ggsn")

# Loading packages:
library(tidyverse)
library(maptools)
library(cmocean)
library(patchwork)
library(ggplot2)
library(ggmap)
library(ggrepel)
library(cowplot)
library(sp)
library(sf)
library(ozmaps)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(ncdf4)
library(reshape2)
library(ggsn)


#============================#
# 1. Working with point data #
#============================#

# Loading point data (population coastal cities NSW)
locs <- read.csv("NSW_cities.csv") # Location
locs
pop <- read.csv("NSW_population.csv") # Population
pop
geo_data <- left_join(pop, locs, by = "City") # Join population to coords object
geo_data 

# Convert to geogragical information
points_sf <- st_as_sf(geo_data, coords = c("Longitude", "Latitude"), crs = 4326) # Create GIS object
points_sf

# Load world shapefiles
coast_sf <- ne_coastline(scale = "medium", returnclass = "sf")   # Coastline data 
plot(coast_sf, max.plot = 1)
country_sf <- ne_countries(scale = "medium", returnclass = "sf") # Land data
plot(country_sf, max.plot = 1)


# Plotting data with ggplot2
  
  # Using coastline
  ggplot() + theme_minimal() + 
  	geom_sf(data = coast_sf, color = "darkblue") + 
    	geom_sf(data = points_sf, color = "darkorange", aes(size = Population), 
    		show.legend = "point", alpha = 0.7) +
    	coord_sf(xlim = c(146, 156), ylim = c(-39, -28)) +
    	geom_text_repel(data = locs, 
    		aes(x = Longitude, y = Latitude, 
    			label = City), size = 3) +
      scalebar(x.min = 152, x.max = 155, y.min = -38, y.max = -37, transform = TRUE, 
        dist_unit = "km", dist = 150, st.dist = 0.4, st.size = 3)

  # Using landmass
  ggplot() + theme_minimal() + 
  	geom_sf(data = country_sf, color = "darkgray", fill = "lightgray", alpha = 0.5) + 
    	geom_sf(data = points_sf, color = "darkorange", aes(size = Population), 
    		show.legend = "point", alpha = 0.7) +
    	coord_sf(xlim = c(146, 156), ylim = c(-39, -28)) +
    	geom_text_repel(data = locs, 
    		aes(x = Longitude, y = Latitude, 
    			label = City), size = 3) +
      scalebar(x.min = 152, x.max = 155, y.min = -38, y.max = -37, transform = TRUE, 
        dist_unit = "km", dist = 150, st.dist = 0.4, st.size = 3, height = 0.2, border.size = 0.5)


# Add Australian inset: ozmaps
# Load fine-scale Australian shapefiles
oz_sf_states <- ozmap_data("states")   # State boundaries
plot(oz_sf_states) 
oz_sf_country <- ozmap_data("country") # Country contour
plot(oz_sf_country)

# Plot population data with state boundaries
plot.data <- ggplot() + theme_minimal() + 
	geom_sf(data = oz_sf_states, color = "darkgray", fill = "lightgray", alpha = 0.5) + 
  	geom_sf(data = points_sf, color = "darkorange", aes(size = Population), 
  		show.legend = "point", alpha = 0.7) +
  	coord_sf(xlim = c(146, 156), ylim = c(-39, -28)) +
  	geom_text_repel(data = locs, 
  		aes(x = Longitude, y = Latitude, 
  			label = City), size = 3) +
    scalebar(x.min = 152, x.max = 155, y.min = -38, y.max = -37, transform = TRUE, 
      dist_unit = "km", dist = 150, st.dist = 0.4, st.size = 3, height = 0.2, border.size = 0.5)
plot.data

# Plot country inset with state boundaries
plot.inset <- ggplot() + theme_void() + 
	geom_sf(data = oz_sf_states, colour = "darkgray", fill = "lightgray", size = 0.3) +
	geom_rect(aes(xmin = 146, xmax = 156, ymin = -39, ymax = -28), # Same values as in the other plot to show inset!
		colour = "black", fill = NA) +
	theme(plot.background = element_rect(fill = "white", colour = NA)) +
	xlim(c(112.1539, 156.1)) + ylim(c(-45.014772, -9.395237))
plot.inset

# Combine the two plots
gg_inset_map2 <- ggdraw() +
  draw_plot(plot.data) +
  draw_plot(plot.inset, x = 0.145, y = 0.74, width = 0.25, height = 0.25)
gg_inset_map2


#=====================================================#
# 2. Working with line data: tracking humpback whales #
#=====================================================#

# Load whale location data
whale_locs <- read.csv("Whale_satellite.csv")
head(whale_locs)
summary(as.factor(whale_locs$ID))
whale_sf <- st_as_sf(whale_locs, coords = c("x", "y"), crs = 4326)
head(whale_sf)
plot(whale_sf)

# Use Azimuthal equidistant projection (centered on lon 0 and lat 0):
country.ae <- st_transform(country_sf, crs = "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
ggplot() + theme_minimal() + 
	geom_sf(data = country.ae, color = "darkgray", fill = "lightgray", alpha = 0.5)

# Crop only Antartica and South America: from latlon crs is easier!
country_cropped <- st_crop(country_sf, xmin = -90, xmax = -10, ymin = -90, ymax = 5)
ggplot() + theme_minimal() + 
	geom_sf(data = country_cropped, color = "darkgray", fill = "lightgray", alpha = 0.5)

# Transform longlat projection to Azimuthal equidistant
country.ae <- st_transform(country_cropped, crs = "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
ggplot() + theme_minimal() + 
	geom_sf(data = country.ae, color = "darkgray", fill = "lightgray", alpha = 0.5)

# Visualize whale tracks: as points per individual
ggplot() + theme_minimal() + 
	geom_sf(data = country.ae, color = "darkgray", fill = "lightgray", alpha = 0.5) +
	geom_sf(data = whale_sf, aes(colour = ID))

# Transform points to lines: and visualize by sex
whale_tracks <- whale_sf %>% group_by(ID, Sex) %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")
head(whale_tracks)
plot(whale_tracks)

# Plot per ID
ggplot() + theme_minimal() + 
	geom_sf(data = country.ae, color = "darkgray", fill = "lightgray", alpha = 0.7) +
	geom_sf(data = whale_tracks, aes(colour = ID)) 

# Plot per sex
ggplot() + theme_minimal() + 
	geom_sf(data = country.ae, color = "NA", fill = "darkgray", alpha = 0.7) +
	geom_sf(data = whale_tracks, aes(colour = Sex)) 


#====================================================================================#
# 3. Working with polygon data and doing spatial calculations: tracking tiger sharks #
#====================================================================================#

# Load shapefile of Australian Marine Regions
aus.marine <- shapefile("AUS Marine Regions/East_AUS_MarineRegions.shp") # Marine regions East coast of Australia
aus.marine
plot(aus.marine)

# Plot polygon data with ggplot2
aus.marine_df <- fortify(aus.marine) 
summary(aus.marine_df)
summary(as.factor(aus.marine_df$id)) # ????
  
  # Fix ID column:
  aus.marine_df$id[aus.marine_df$id == "0"] <- "Temperate East"
  aus.marine_df$id[aus.marine_df$id == "1"] <- "South East"
  aus.marine_df$id[aus.marine_df$id == "2"] <- "Coral Sea"
  aus.marine_df$id[aus.marine_df$id == "3"] <- "GBR Marine Park"
  aus.marine_df$id <- factor(aus.marine_df$id, levels = c("GBR Marine Park", "Coral Sea", "Temperate East", "South East"))
  summary(aus.marine_df$id)
  
ggplot() + theme_minimal() +
  	geom_polygon(data = aus.marine_df, aes(x = long, y = lat, fill = id), alpha = 0.8) +
  	labs(x = "Longitude", y = "Latitude", fill = "Marine Region") +
	geom_sf(data = oz_sf_country, colour = "darkgray", fill = "lightgray", size = 0.3) +
  	xlim(c(130, 165)) +
  scalebar(x.min = 155, x.max = 163, y.min = -40, y.max = -38, transform = TRUE, 
      dist_unit = "km", dist = 350, st.dist = 0.6, st.size = 3, height = 0.3, border.size = 0.3)


### Tiger shark data analysis:

# Load shark location data
tiger_locs <- read.csv("Shark_satellite.csv")
head(tiger_locs)

# Add Marine Region data
tiger_locs$MR <- NA # Empty column to add info about Marine Region
for (i in 1:nrow(tiger_locs)) {
	pts <- SpatialPoints(cbind(tiger_locs$x[i], tiger_locs$y[i]),
		proj4string = CRS(proj4string(aus.marine)))
	aux <- over(pts, aus.marine) # Overlay Marine Region shapefile and shark location
	tiger_locs$MR[i] <- aux$REGION
}
head(tiger_locs)
summary(as.factor(tiger_locs$MR)) # NA = outside MR!
tiger_locs$MR[which(is.na(tiger_locs$MR) == TRUE)] <- "Outside"
summary(as.factor(tiger_locs$MR)) 

# Sort dataset 
locs.type <- data.frame(MR = names(summary(as.factor(tiger_locs$MR))), 
	Locs = as.numeric(summary(as.factor(tiger_locs$MR))))  
locs.type
locs.type$MR <- factor(locs.type$MR, levels = c("Outside", "GBR Marine Park", "Coral Sea", "Temperate East", "South-east"))

# Plot location type data
plot.locs <- ggplot() + theme_bw() + labs(y = "Number of locations", x = "Marine Region") +
	geom_bar(data = locs.type, aes(x = MR, y = Locs, fill = MR), stat = "identity") +
	coord_cartesian(xlim = c(0.3, 5.7), ylim = c(0, 60), expand = FALSE) + 
	theme(legend.position = "none") +
	scale_fill_manual(values = c("lightgray", cmocean('haline')(8)[c(7, 5, 3, 1)]))
plot.locs

# Plot polygon and shark tracking data
tiger_sf <- st_as_sf(tiger_locs, coords = c("x", "y"), crs = 4326)
head(tiger_sf)
plot(tiger_sf)

plot.map <- ggplot() + theme_minimal() +
    geom_polygon(data = aus.marine_df, aes(x = long, y = lat, fill = id), alpha = 0.8) +
    scale_fill_manual(values = cmocean('haline')(8)[c(7, 5, 3, 1)]) +
  	geom_sf(data = oz_sf_states, colour = "darkgray", fill = "lightgray", size = 0.3) +
  	xlim(c(140, 165)) +
  	geom_sf(data = tiger_sf, aes(colour = ID, shape = Sex)) +
  	labs(x ="Longitude", y = "Latitude", fill = "Marine Region", colour = "Shark ID")
plot.map

((plot.locs / plot_spacer()) | plot.map) + plot_layout(guides = 'collect')


#==========================================================================================#
# 4. Working with rasters: Analysing remotely sensed data on Sea Surface Temperature (SST) #
#==========================================================================================#

# Load Sea Surface Temperature (SST) data: check variable units
nc <- nc_open("IMOS_SST.nc")
attributes(nc$dim)$names # Dimensions
attributes(nc$var)$names # Variable names
ncatt_get(nc, attributes(nc$var)$names[1]) # Variable units
nc_close(nc)

# Open the data using the raster package
nc <- brick("IMOS_SST.nc", varname = "sea_surface_temperature")
nc # Check raster data
plot(nc[[1]]) # Plots the first raster in the NetCDF data
nc <- aggregate(nc, fact = 10, # Aggregate raster data by mean to a 0.2° resolution (fact = 10 * 0.02 = 0.2)
  fun = mean, na.rm = TRUE) 
nc
plot(nc[[1]], main = "01 January 2019")

# Create auxiliary object for handling data:
names(nc)
indices <- as.numeric(format(as.Date(names(nc), format = "X%Y.%m.%d"), format = "%m"))
indices
summary(as.factor(indices)) # Check number of daily data in each month

# Obtain stacked monthly mean rasters
nc.sst <- stackApply(nc, indices = indices, fun = mean, na.rm = TRUE)
names(nc.sst) # Monthly rasters
plot(nc.sst[[1]], main = "January 2019")
nc.sst <- nc.sst - 273.15 # Transform temperature from Kelvin to Celsius
plot(nc.sst[[1]], main = "January 2019")


## Plotting the raster data with ggplot2:

# Convert raster to dataframe format
df.sst <- as.data.frame(nc.sst, xy = TRUE)
head(df.sst) # Wide format
df.sst <- melt(df.sst, id.vars = c("x", "y")) # Transform to long format: big dataset
head(df.sst)
names(df.sst) <- c("Lon", "Lat", "Month", "SST")
head(df.sst)

# Sort month variable
df.sst$Month <- lapply(df.sst$Month, gsub, pattern='index_', replacement='') # Exclude 'index_' prefix
df.sst$Month <- as.numeric(df.sst$Month)
head(df.sst)
summary(as.factor(df.sst$Month))

  # Change from numbers to month names:
  df.sst$Month[df.sst$Month == 1] <- "Jan"
  df.sst$Month[df.sst$Month == 2] <- "Feb"
  df.sst$Month[df.sst$Month == 3] <- "Mar"
  df.sst$Month[df.sst$Month == 4] <- "Apr"
  df.sst$Month[df.sst$Month == 5] <- "May"
  df.sst$Month[df.sst$Month == 6] <- "Jun"
  df.sst$Month[df.sst$Month == 7] <- "Jul"
  df.sst$Month[df.sst$Month == 8] <- "Aug"
  df.sst$Month[df.sst$Month == 9] <- "Sep"
  df.sst$Month[df.sst$Month == 10] <- "Oct"
  df.sst$Month[df.sst$Month == 11] <- "Nov"
  df.sst$Month[df.sst$Month == 12] <- "Dec"
  df.sst$Month <- factor(df.sst$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
    "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Plot a specific month:
ggplot() + theme_minimal() +
  geom_raster(data = subset(df.sst, Month == "Jan"), 
    aes(x = Lon, y = Lat, fill = SST)) + 
  scale_fill_gradientn(colours = cmocean('thermal')(100)) +
  geom_sf(data = oz_sf_country, color = "darkgray", fill = "lightgray", size = 0.3) +
  labs(x = "Longitude", y = "Latitude", title = "January 2019", fill = "SST (°C)") + xlim(c(140, 160))

# Plot all months together:
ggplot() + theme_minimal() +
  geom_raster(data = subset(df.sst), 
    aes(x = Lon, y = Lat, fill = SST)) + 
  scale_fill_gradientn(colours = cmocean('thermal')(100)) +
  geom_sf(data = oz_sf_country, color = "darkgray", fill = "lightgray", size = 0.3) +
  labs(x = "Longitude", y = "Latitude", fill = "SST (°C)", title = "2019 Satellite Sea Surface Temperature (SST)") +
  coord_sf(xlim = c(140, 160)) + scale_x_continuous(breaks = c(140, 150)) +
  facet_wrap(~ Month, ncol = 4) 


## Investigate for seasonality according to the Marine Regions: GBR + Temperate East + South-east

# Check spatial distribution of downloaded data
ggplot() + theme_minimal() +
  geom_raster(data = subset(df.sst, Month == "Jul"), 
    aes(x = Lon, y = Lat, fill = SST)) + 
  scale_fill_gradientn(colours = cmocean('thermal')(100)) +
  geom_polygon(data = aus.marine_df, aes(x = long, y = lat, colour = id), fill = NA) +
  geom_sf(data = oz_sf_country, color = "darkgray", fill = "lightgray", size = 0.3) +
  labs(x = "Longitude", y = "Latitude", title = "July 2019", fill = "SST (°C)", colour = "Marine Region") + xlim(c(140, 170))

# Use lapply instead of for loop: 113400 rows of data!
MR <- lapply(df.sst, function(x) {
  pts <- suppressWarnings(SpatialPoints(cbind(df.sst$Lon, df.sst$Lat),
    proj4string = CRS(proj4string(aus.marine))))
  aux <- over(pts, aus.marine) # Overlay Marine Region shapefile and shark location
  return(aux$REGION)
})
df.sst$MR <- MR$SST
summary(as.factor(df.sst$MR))
head(df.sst)

# Select only the areas of interest
df.sst <- subset(df.sst, MR == "GBR Marine Park" | MR == "Temperate East" | MR == "South-east")
summary(as.factor(df.sst$MR))

ggplot() + theme_minimal() +
  geom_raster(data = subset(df.sst, Month == "Jul"), 
    aes(x = Lon, y = Lat, fill = SST)) + 
  scale_fill_gradientn(colours = cmocean('thermal')(100)) +
  geom_polygon(data = aus.marine_df, aes(x = long, y = lat, colour = id), fill = NA) +
  geom_sf(data = oz_sf_country, color = "darkgray", fill = "lightgray", size = 0.3) +
  labs(x = "Longitude", y = "Latitude", title = "July", fill = "SST (°C)", colour = "Marine Region") + xlim(c(140, 165))

# Investigate seasonal variations as a function of Marine Region:
df.sst$MR <- factor(df.sst$MR, levels = c("GBR Marine Park", "Temperate East", "South-east"))

ggplot() + theme_bw() +
  geom_boxplot(data = df.sst, aes(x = Month, y = SST, colour = MR)) +
  scale_colour_manual(values = rev(cmocean('thermal')(4)[1:3])) +
  labs(y = "Sea Surface Temperature (°C)", colour = "Marine Region")


