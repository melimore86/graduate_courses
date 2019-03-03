# Assignment 6
# Mel Moreno
# Data Carpentry


library("ggplot2")
library("raster")
library("maps")
library("spocc")
library("dplyr")
library("viridis")

#Exercise 1 


#Climate data data is available from the WorldClim dataset. Using #getData('worldclim', var = 'bio', res = 10) (from the raster #package) will download all of the bioclim variables. 
world_clim<-getData('worldclim', var = 'bio', res = 10)

#The two #variables you need are bio1 (temperature) and bio12 (precipitation).

#There are over 500,000 global data points which can make plotting slow. You can choose to plot a random subset of 10,000 points (e.g., using sample_n from the dplyr package) to limit the time it takes to generate.

#Have to make this a data frame to make the dplyr (sample_n) work
world_clim_df<-as.data.frame (world_clim, xy=TRUE)

world_samp<-world_clim_df %>% 
  dplyr::select(bio1, bio12) %>% 
  na.omit() %>% 
  sample_n(size= 10000)
  
#Choose good labels and make the points transparent to see their density.

ggplot(world_samp, aes(x= bio1, y=bio12))+
  xlab("Mean Annual Temperature (C)") +
  ylab("Mean Annual Precipitation (mm)") +
  geom_point(alpha=0.5)

#You might notice that the temperature values seem large. Storing decimal values uses more space than integers, so the WorldClim creators provide temperature values multiplied by 10. For example, 19.5 is stored as 195. Make sure to display the actual temperatures, not the raw values provided. See more information about WorldClim units here.
world_samp$temp<- (world_samp$bio1 /10)
world_samp$temp

#Species occurrence data is available from GBIF using the spocc package. An example of how to get the data you need is available in the Species Occurrences Map exercise.

# World climate data
world_clim_rast<-getData('worldclim', var = 'bio', res = 10)
world_clim_rast_df<-as.data.frame (world_clim_rast)

world_clim_rast_df<-world_clim_rast_df%>% 
  dplyr::select(bio1, bio12) %>% 
  dplyr::rename(temperature=bio1, precipitation=bio12) %>%
  na.omit() %>% 
  sample_n(size= 10000)

world_clim_rast_df$temperature<- (world_clim_rast_df$temperature /10)

# Quercus alba
querc_df = occ(query = "Quercus alba", 
              from = "gbif",
              limit = 1000,
              has_coords = TRUE)

querc_df = data.frame(querc_df$gbif$data)

querc_df_df<-querc_df %>% 
  dplyr::rename(longitude= 2, latitude= 3) %>% 
  filter(!is.na(longitude), !is.na(latitude)) %>% 
  dplyr::filter(latitude != 0 & longitude != 0)%>% 
  dplyr::select(longitude,latitude)

# Extraacting from the world clim raster and querc data
querc_clim<-raster::extract(world_clim_rast, querc_df_df)
querc_clim<-as.data.frame(querc_clim)

querc_clim<-querc_clim %>% 
  dplyr::rename(temperature=1, precipitation=12) %>% 
  filter(!is.na(temperature), !is.na(precipitation)) %>%
  mutate(temperature=temperature/10)

# Plotting 
ggplot()+
  labs(title= "Quercus alba")+
  geom_point(data= world_clim_rast_df ,aes(x=temperature , y=precipitation),alpha=0.3) +
  geom_point(data=querc_clim,aes(x=temperature , y=precipitation), color= "red") +
  xlab("Mean Annual Temperature (C)") +
  ylab("Mean Annual Precipitation (mm)")



# Picea glauca
picea_df = occ(query = "Picea glauca", 
               from = "gbif",
               limit = 1000,
               has_coords = TRUE)

picea_df = data.frame(picea_df$gbif$data)

picea_df_df<-picea_df %>% 
  dplyr::rename(longitude= 2, latitude= 3) %>% 
  filter(!is.na(longitude), !is.na(latitude)) %>% 
  dplyr::filter(latitude != 0 & longitude != 0)%>% 
  dplyr::select(longitude,latitude)

# Extraacting from the world clim raster and querc data
picea_clim<-raster::extract(world_clim_rast, picea_df_df)
picea_clim<-as.data.frame(picea_clim)

picea_clim<-picea_clim %>% 
  dplyr::rename(temperature=1, precipitation=12) %>% 
  filter(!is.na(temperature), !is.na(precipitation)) %>%
  mutate(temperature=temperature/10)


# Plotting 
ggplot()+
  labs(title= "Picea glauca")+
  geom_point(data= world_clim_rast_df ,aes(x=temperature , y=precipitation),alpha=0.3) +
  geom_point(data=picea_clim,aes(x=temperature , y=precipitation), color= "white") +
  xlab("Mean Annual Temperature (C)") +
  ylab("Mean Annual Precipitation (mm)")


# Ceiba pentandra
ceiba_df = occ(query = "Ceiba pentandra", 
               from = "gbif",
               limit = 1000,
               has_coords = TRUE)

ceiba_df = data.frame(ceiba_df$gbif$data)

ceiba_df_df<-ceiba_df %>% 
  dplyr::rename(longitude= 2, latitude= 3) %>% 
  filter(!is.na(longitude), !is.na(latitude)) %>% 
  dplyr::filter(latitude != 0 & longitude != 0)%>% 
  dplyr::select(longitude,latitude)

# Extraacting from the world clim raster and querc data
ceiba_clim<-raster::extract(world_clim_rast, ceiba_df_df)
ceiba_clim<-as.data.frame(ceiba_clim)

ceiba_clim<-ceiba_clim %>% 
  dplyr::rename(temperature=1, precipitation=12) %>% 
  filter(!is.na(temperature), !is.na(precipitation)) %>%
  mutate(temperature=temperature/10)


# Plotting 
ggplot()+
  labs(title= "Ceiba pentandra")+
  geom_point(data= world_clim_rast_df ,aes(x=temperature , y=precipitation),alpha=0.3) +
  geom_point(data=ceiba_clim,aes(x=temperature , y=precipitation), color= "blue") +
  xlab("Mean Annual Temperature (C)") +
  ylab("Mean Annual Precipitation (mm)")


# Challenge
ggplot()+
  labs(title= "Ceiba, Quercus, and Pieca compared")+
  geom_point(data= world_clim_rast_df ,aes(x=temperature , y=precipitation),alpha=0.3) +
  geom_point(data=ceiba_clim,aes(x=temperature , y=precipitation), color= "blue") +
  geom_point(data=picea_clim,aes(x=temperature , y=precipitation), color= "white") +
  geom_point(data=querc_clim,aes(x=temperature , y=precipitation), color= "red") +
  xlab("Mean Annual Temperature (C)") +
  ylab("Mean Annual Precipitation (mm)")


# Exercise 2 

#Import the data into R. As with most real world data there are a number of issues with this dataset. Try to spot and clean them up during the import process, but understand that it is common to not discover some data issues until you start analyzing the data. Data cleaning is often an iterative process. Print out the structure of the resulting data frame.


mammal <- read.table("http://www.esapubs.org/archive/ecol/E084/094/MOMv3.3.txt", sep = "\t", stringsAsFactors = F, na.strings = c("-999", "-999.00"))


mammal<- mammal %>% 
  dplyr::rename(continent=1, status=2, order=3, family=4, genus=5, species= 6, long_mass= 7, combined_mass=8, reference=9 )


mammal %>%
  group_by(continent) %>%
  summarise(total.count=n()) 
# There is an Af instead of AF

#changed the continent name to AF
mammal$continent[mammal$continent == "Af"] <- "AF"

mammal %>%
  group_by(status) %>%
  summarise(total.count=n()) 
# looks good 

order<-mammal %>%
  group_by(order) %>%
  summarise(total.count=n()) 
order
# looks good

family<-mammal %>%
  group_by(family) %>%
  summarise(total.count=n()) 
family
#there are 10 with an NA, going to remove the NA

mammal<- mammal %>%
  filter(!is.na(family))
mammal
#removed the 10 with NAs in the family 

genus<-mammal %>%
  group_by(genus) %>%
  summarise(total.count=n()) 
genus
# seems fine, there is at least one mammal in each genus

species<-mammal %>%
  group_by(species) %>%
  summarise(total.count=n()) 
# seems fine, there is at least one mammal in each species


#Now to find out if the long_mass is out of bounds
long<-mammal %>% group_by(family) %>%
  summarise(max = max(long_mass))

#found that there are several long_mass that are not written out 

mammal<- mammal %>%
  filter(!is.na(long_mass))
mammal

# Now to find the min of the long_mass
min<-mammal %>% group_by(family) %>%
              summarise(min = min(long_mass))
#looks good

#Now to look at the compared_mass
compare_max<-mammal %>% group_by(family) %>%
  summarise(max = max(combined_mass))
# very large range, but that could be normal
compare_max

# Now to find the min of the long_mass
compare_min<-mammal %>% group_by(family) %>%
  summarise(min = min(combined_mass))
compare_min
# Also a large range here, but seems pretty normal 

#The structure of the mammal data set
str(mammal)

#2 Create a plot showing histograms of masses for extant mammals and those that went extinct during the pleistocene (extant and extinct in the status column). There should be one sub-plot for each continent and that sub-plot should show the histograms for both groups. Don’t include islands (Insular and Oceanic in the `continent column) and only include continents with species that went extinct in the pleistocene. Scale the x-axis logarithmically and stack the sub-plots vertically like in the original paper (but don’t worry about the order of the subplots being the same). Use good axis labels.


mammal_plot<-mammal %>% 
  filter(continent=="AF" | continent== "AUS"| continent== "NA"  | continent=="SA") %>% 
  filter(status=="extinct"| status== "extant") %>% 
  select(continent, status, combined_mass, species) 
  
ggplot(data= mammal_plot) +
  xlab("Mass(g)")+
  ylab("Number of Species") +
  labs(fill = "Status") +
  geom_histogram(aes(x= combined_mass, fill= status), bins=30) +
  scale_x_log10() +
  facet_wrap(~continent, ncol=1)

#3 The 2nd figure in the original paper looks in more detail at two orders, Xenarthra and Carnivora, which showed extinctions in North and South America. Create a figure similar to the one in Part 2, but that shows 4 sub-plots, one for each order on each of the two continents.

mammal_plot_xencar<-mammal %>% 
  filter(order== "Xenarthra" | order == "Carnivora") %>% 
  filter(continent=="NA" | continent== "SA") %>% 
  filter(status=="extinct"| status== "extant") %>% 
  select(continent, status, combined_mass, species, order) 


ggplot(data= mammal_plot_xencar) +
  xlab("Mass(g)")+
  ylab("Number of Species") +
  labs(fill = "Status") +
  geom_histogram(aes(x= combined_mass, fill= status), bins=30) +
  scale_x_log10() +
  facet_wrap(~order + continent, ncol=1)

#4 The 3rd figure in the original paper explores Australia as a case study. Australia is interesting because there is good data on both Pleistocene extinctions (extinct in the status column) and more modern extinctions occuring over the last 300 years (historical in the status column). Make a plot similar to the previous plots that compares these three different categories extinct, extant, and historical). Has the size pattern in exinctions changed for more modern extinctions?


mammal_plot_aus<-mammal %>% 
  filter(continent=="AUS") %>% 
  filter(status=="extinct"| status== "extant" | status== "historical") %>% 
  select(status, combined_mass) 


ggplot(data= mammal_plot_aus) +
  xlab("Mass(g)")+
  ylab("Number of Species") +
  labs(fill = "Status") +
  geom_histogram(aes(x= combined_mass, fill= status), bins=30) +
  scale_x_log10()

