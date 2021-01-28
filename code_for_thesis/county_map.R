library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("maps")
library("tools")
library("dplyr")
library("stringr")
library("tidyverse")
library("ggrepel")
library("ggspatial")
library("cowplot")

county_pop<- read.csv("county_data.csv", header=TRUE)

county_pop$Population<- as.numeric(county_pop$Population)

cbPalette <- c("#0072B2", "#D55E00", "#CC79A7")


p1<-ggplot(county_pop, aes(x= Year, y= Population, color= County))+
  #geom_point(size= 2.5) +
  geom_line(size=1.5) +
  scale_color_manual(values =cbPalette) +
  theme(legend.position="top",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"))



#making notes of whatever
world <- ne_countries(scale = "medium", returnclass = "sf")

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))

counties <- subset(counties, grepl("florida", counties$ID))

counties$area <- as.numeric(st_area(counties))

counties <- cbind(counties, st_coordinates(st_centroid(counties)))


counties$ID<- str_sub(counties$ID, start= 9)

counties$ID <- as.character(counties$ID)
counties$ID <- toTitleCase(counties$ID)

dixie<- counties %>% 
  filter(ID == "Dixie")

franklin<- counties %>% 
  filter(ID == "Franklin")

taylor<- counties %>% 
  filter(ID == "Taylor")


#rivers50 <- ne_download(scale = 50, type = 'rivers_lake_centerlines', category = 'physical', returnclass = "sf")
rivers <- st_read(
  "Waterways_Florida.shp")

rivers_map<- rivers %>% #control shift m
filter(NAME == "Suwannee River - Lower" | NAME == "Suwannee River - Upper"| NAME == "Suwannee River - Middle" | NAME == "Suwannee River - Above Big Shoals" | NAME == "Suwannee River")



p2<-ggplot() +
  labs(x = "Longitude", y= "Latitude") +
  geom_sf(data = states, fill = "lightgreen") + 
  #geom_sf(data = counties, fill = NA, color = gray(.5)) +
  geom_sf(data= dixie, fill= "#0072B2") +
  #geom_text(data = dixie, aes(X, Y, label = ID, hjust= -.85), size = 3) +
  geom_sf(data= franklin, fill= "#D55E00") +
  #geom_text(data = franklin, aes(X, Y, label = ID,vjust = 1, hjust= -.25), size = 3) +
  geom_sf(data= taylor, fill= "#CC79A7") +
  #geom_text(data = taylor, aes(X, Y, label = ID), size = 3) +
  geom_sf(data = rivers_map, size= 1.5) + 
  coord_sf(xlim = c(-88, -79), ylim = c(24.5, 31.5), expand = FALSE)+
  annotation_scale(location = "bl", width_hint = .50) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"))


p3<-ggplot() +
  labs(x = "Longitude", y= "Latitude") +
  geom_sf(data = states, fill = "lightgreen") + 
  #geom_sf(data = counties, fill = NA, color = gray(.5)) +
  geom_sf(data= dixie, fill= "#0072B2") +
  geom_text(data = dixie, aes(X, Y, label = ID, hjust= -.85), size = 3) +
  geom_sf(data= franklin, fill= "#D55E00") +
  geom_text(data = franklin, aes(X, Y, label = ID,vjust = 1.5, hjust= -.35), size = 3) +
  geom_sf(data= taylor, fill= "#CC79A7") +
  geom_text(data = taylor, aes(X, Y, label = ID), size = 3) +
  geom_sf(data = rivers_map, size= 1.5) + 
  coord_sf(xlim = c(-86, -81.5), ylim = c(28, 31.5), expand = FALSE) +
  annotate("text", x = -83, y = 30.9, label = "Suwannee River",size = 3) +
  annotation_scale(location = "bl", width_hint = .50) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"))




p4<-plot_grid(p2, p3, p1, labels = c('A', 'B', 'C'))
