# Data Carpentry
# Mel Moreno
# September 25, 2018
# Assignment 5 

library("raster")
library("rgdal")
library("ggplot2")
library("dplyr")
library("tidyverse")

# Exercise 1 Canopy Height from Space 

#1. Create two Canopy Height Models using simple raster math (chm = dsm - dtm), one for the HARV site (which was done during the lecture) and another for the SJER site.

# DSM SJER
dsm_sjer <- raster("assignment_5/data/SJER_dsmCrop.tif")

dsm_sjer_df<- as.data.frame(dsm_sjer, xy=TRUE)

# DTM SJER 
dtm_sjer <- raster("assignment_5/data/SJER_dtmCrop.tif")

dtm_sjer_df<- as.data.frame(dtm_sjer, xy=TRUE)

# DSM HARV
dsm_harv <- raster("assignment_5/data/HARV_dsmCrop.tif")

dsm_harv_df<- as.data.frame(dsm_harv, xy=TRUE)

# DTM HARV 
dtm_harv <- raster("assignment_5/data/HARV_dtmCrop.tif")

dtm_harv_df<- as.data.frame(dtm_harv, xy=TRUE)

# Using raster math to creat the Canopy Height Models 
chm_harv <- dsm_harv - dtm_harv

chm_harv_df<- as.data.frame(chm_harv, xy=TRUE)

chm_sjer <- dsm_sjer - dtm_sjer

chm_sjer_df<- as.data.frame(chm_sjer, xy=TRUE)

dtm_sjer <- raster("assignment_5/data/SJER_dtmCrop.tif")

dtm_sjer_df<- as.data.frame(dtm_sjer, xy=TRUE)

plots_harv <- readOGR("assignment_5/data/plot_locations", 
                      "HARV_plots")
plots_harv_df <- as.data.frame(plots_harv)

plots_sjer <- readOGR("assignment_5/data/plot_locations", 
                      "SJER_plots")
plots_sjer_df <- as.data.frame(plots_sjer)


#2 Create plots and histograms of canopy heights for both of the sites using ggplot.

#Creating plots for HARV and SJER
ggplot() +
  geom_raster(data= chm_harv_df, aes(x= x, y=y, fill = layer))

ggplot() +
  geom_raster(data= chm_sjer_df, aes(x= x, y=y, fill = layer))


# Creating a histogram, and changing the bins
ggplot() +
  geom_histogram(data= chm_harv_df, aes(x=layer), bins= 100)

ggplot() +
  geom_histogram(data= chm_sjer_df, aes(x=layer), bins= 100)


# 3 Add corresponding points from plot_locations folder to each site plot.
plots_harv <- readOGR("assignment_5/data/plot_locations","HARV_plots")
plots_harv_df <- as.data.frame(plots_harv)

plots_sjer <- readOGR("assignment_5/data/plot_locations","SJER_plots")
plots_sjer_df <- as.data.frame(plots_sjer)


chm_sjer<- dsm_sjer - dtm_sjer

chm_sjer_df<- as.data.frame(chm_sjer, xy=TRUE)

ggplot() +
  geom_raster(data= chm_harv_df, aes(x= x, y=y, fill = layer))

# Checking the coordinate reference system
crs(chm_harv)
crs(plots_harv)
crs(chm_sjer)
crs(plots_sjer)


# Give the reference for the plots harv, and then make chm the same
plots_harv_utm <- spTransform(plots_harv, crs(chm_harv))
plots_harv_utm_df = as.data.frame(plots_harv_utm)

plots_sjer_utm <- spTransform(plots_sjer, crs(chm_sjer))
plots_sjer_utm_df = as.data.frame(plots_sjer_utm)


#Plotting the raster with the locations

plots_sjer_utm <- spTransform(plots_sjer, crs(chm_harv))
plots_sjer_utm_df = as.data.frame(plots_sjer_utm)

ggplot() +
  geom_raster(data= chm_harv_df, aes(x= x, y=y, fill = layer)) +
  geom_point(data = plots_harv_utm_df, 
             aes(x = coords.x1, y = coords.x2), color = "yellow")

ggplot() +
  geom_raster(data= chm_sjer_df, aes(x= x, y=y, fill = layer)) +
  geom_point(data = plots_sjer_utm_df, 
             aes(x = coords.x1, y = coords.x2), color = "yellow")

#4 Create a single dataframe with two columns, one of the maximum canopy heights for each point at the HARV site and one for the SJER points’ maximum canopy heights. When extracting the canopy height values, use a buffer of 10.
plots_chm_harv <- raster::extract(chm_harv, plots_harv_utm)

plots_chm_sjer <- raster::extract(chm_sjer, plots_sjer_utm)

#Order of values lines up with plots_harv_utm$plot_id.
plots_harv_utm$plot_id
plots_chm_harv <- data.frame(plot_num = plots_harv_utm$plot_id, plot_value = plots_chm_harv)

plots_sjer_utm$plot_id
plots_chm_sjer <- data.frame(plot_num = plots_sjer_utm$plot_id, plot_value = plots_chm_sjer)


#To get an max of the values in a nearby region use buffer
max_canopy<-data.frame(harv_max=raster::extract(chm_harv, plots_harv_utm, buffer=10, fun = max),
                            sjer_max=raster::extract(chm_sjer, plots_sjer_utm, buffer=10, fun = max))

max_canopy


# Exercise 2 Phenology from Space 

#1. Plot the whole-raster mean NDVI (cellStats()) for Harvard Forest and SJER through time using different colors for the two sites. To do this:

#Load the files for each site as a raster stack
harv_ndvi_files<-list.files("assignment_5/data/HARV_NDVI/", full.names= TRUE)

sjer_ndvi_files<-list.files("assignment_5/data/SJER_NDVI/", full.names= TRUE)

ndvi_rasters_harv<- stack(harv_ndvi_files)
ndvi_rasters_sjer<- stack(sjer_ndvi_files)

plot(ndvi_rasters_harv)
plot(ndvi_rasters_sjer)

raster::extract(ndvi_rasters_harv,plots_harv_utm)
raster::extract(ndvi_rasters_sjer,plots_sjer_utm)

#Use cellStats() to calculate the mean values for each raster in the stack. Call the outputs harv_avg and sjer_avg

t(ndvi_rasters_harv)
t(ndvi_rasters_sjer)

harv_avg<- cellStats(ndvi_rasters_harv, mean)
harv_avg

sjer_avg<- cellStats(ndvi_rasters_sjer, mean)
sjer_avg

#Create a vector of sampling periods for each site: e.g., samp_period = c(1:length(harv_avg), 1:length(sjer_avg))

samp_period = c(1:length(harv_avg), 1:length(sjer_avg))
samp_period

#Create a vector of site names for each site: e.g., site_name = c(rep("harv", length(harv_avg)), rep("sjer", length(sjer_avg)))

site_name = c(rep("harv", length(harv_avg)), rep("sjer", length(sjer_avg)))

site_name 

#Make a data frame that includes columns for site name, sampling period, and the average NDVI values (concatenate the two vectors using c()).

ndvi_avg<-data.frame(samp_period, site_name,"avg"= c(harv_avg,sjer_avg))
ndvi_avg

#Graph the trends through time using ggplot
ggplot (data= ndvi_avg, aes(x= samp_period, y=avg, color= site_name)) +
  geom_point()

# 2 Extract the NDVI values from all rasters for the HARV_plots and SJER_plotsin NEON-airborne/plot_locations. Running extract() on a raster stack results in a matrix with one column per raster and one row per point. To more easily work with this data, we want to have one column with the raster names and one column per point, which you can do by transposing the matrix with the t() function. Then make this into a dataframe and turn the rownames into a column using tibble::rownames_to_column(your_matrix, var = "date"). Do this for both HARV and SJER.

#HARV
ndvi_files_harv = list.files("assignment_5/data/HARV_NDVI/",
                        full.names = TRUE,
                        pattern = "HARV_NDVI.*.tif")
ndvi_rasters_harv <- stack(ndvi_files_harv)
plots_harv_2 <- readOGR("assignment_5/data/plot_locations","HARV_plots")
plots_harv_utm_ndvi<- spTransform(plots_harv_2 , crs(ndvi_rasters_harv))
plots_harv_ndvi<-raster::extract(ndvi_rasters_harv, plots_harv_utm_ndvi)

plots_harv_ndvi_df<-as.data.frame(t(plots_harv_ndvi))
plots_harv_ndvi_df<- tibble::rownames_to_column(plots_harv_ndvi_df, var= "date")
plots_harv_ndvi_df

#SJER
ndvi_files_sjer = list.files("assignment_5/data/SJER_NDVI/",
                             full.names = TRUE,
                             pattern = "SJER_NDVI.*.tif")
ndvi_rasters_sjer <- stack(ndvi_files_sjer)
plots_sjer_2 <- readOGR("assignment_5/data/plot_locations","SJER_plots")
plots_sjer_utm_sjer<- spTransform(plots_sjer_2 , crs(ndvi_rasters_sjer))
plots_sjer_ndvi<-raster::extract(ndvi_rasters_sjer, plots_sjer_utm_sjer)

plots_sjer_ndvi_df<-as.data.frame(t(plots_sjer_ndvi))
plots_sjer_ndvi_df<- tibble::rownames_to_column(plots_sjer_ndvi_df, var= "date")
plots_sjer_ndvi_df

# Exercise 3 
library ("spocc")

#1 Get banner-tailed kangaroo rat occurrences from GBIF, the Global Biodiversity Information Facility, using the spocc R package, which is designed to retrieve species occurrence data from various openly available data resources. Use the following code to do so:

dipo_df = occ(query = "Dipodomys spectabilis", 
              from = "gbif",
              limit = 1000,
              has_coords = TRUE)
dipo_df = data.frame(dipo_df$gbif$data)

#2 Clean up the data by:

#Using the rename function from dplyr to rename the second and third columns of this dataset to longitude and latitude

dipo_df<-dipo_df %>% 
  rename(longitude= 2, latitude= 3)

dipo_df

#Filter the data to only include those specimens with Dipodomys_spectabilis.basisOfRecord that is PRESERVED_SPECIMEN and a Dipodomys_spectabilis.countryCode that is US

dipo_df<-dipo_df %>% 
  filter(Dipodomys_spectabilis.basisOfRecord =="PRESERVED_SPECIMEN" & Dipodomys_spectabilis.countryCode == "US")
dipo_df

#Remove points with values of 0 for latitude or longitude
dipo_df<-
dipo_df %>% 
  filter(!(latitude== "0" & longitude == "0"))
dipo_df


#Remove all of the columns from the dataset except latitude and longitude using select
dipo_df<-
  dipo_df %>% 
  select(latitude,longitude)
dipo_df

#Use head() function to show the top few rows of this cleaned dataset

head(dipo_df)


# 3 Do the following to display the locations of these points on a map of the United States:

library("maps")

points_crs <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

points_spat <- SpatialPointsDataFrame(
  dipo_df[c('longitude', 'latitude')], 
  dipo_df, 
  proj4string = points_crs)

crs(points_spat)


#Get data for a US map using usmap = map_data("usa")

usmap <- map_data("usa")

#Plot it using geom_polygon. In the aesthetic use group = group to avoid weird lines cross your graph. Use fill = "white" and color = "black".

ggplot() +
  geom_polygon(data = usmap, 
               aes(x = long, y = lat, group = group),fill = "white", color= "black")

#Plot the kangaroo rat locations
points_spat_df <- as.data.frame(points_spat)

ggplot() +
  geom_polygon(data = usmap, aes(x = long, y = lat, group = group),fill= "white", color= "black") +
  geom_point(data = points_spat_df, aes(x = longitude, y = latitude))

#Use coord_quickmap() to automatically use a reasonable spatial projection
ggplot() +
  geom_polygon(data = usmap, aes(x = long, y = lat, group = group),fill= "white", color= "black") +
  geom_point(data = points_spat_df, aes(x = longitude, y = latitude)) +
  coord_quickmap()
