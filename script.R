library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(tidyverse)

#Defining paths
out.dir <- (".\\Monit_Trichi_2023\\Mapas\\")

#loading grid UTM 10x10km ETRS89 / Portugal TM06
grid_pt <- st_read(".\\Monit_Trichi_2023\\Monit_Trichilogaster_2023\\Fishnet_10km.shp")

#Loading monitored points
db <- read.csv("C:\\Users\\LENOVO\\Downloads\\registo-de-trichilogaster-acaciaelongifoliae-csv(13)\\form-1__registo-galhas-de-trichilogaster.csv", header=T, sep=",") %>% 
  as.data.frame() %>%
  mutate(X = as.numeric(long_2_Localizao), 
         Y = as.numeric(lat_2_Localizao)) %>%
  filter(!is.na(X)) %>%
  st_as_sf(coords = c("long_2_Localizao","lat_2_Localizao"), crs = 4326, remove = FALSE)
db <- st_transform(db, crs = 3763 ) #Projecting to ETRS89

#Joining cell grid codes from UTM to database with monitored points
db <- st_join(db, grid_pt)

#PRESENCE OF TRICHILOGASTER
with_galls <- db %>% 
  filter(X8_A_planta_que_est_a == "Sim") #Filtering points with presence only of Trichilogaster

with_galls_grids<- grid_pt[with_galls,] #Filtering cells with presence only of Trichilogaster

plot(st_geometry(grid_pt[pi, ])) #Plotting on subsetted map to doublecheck

st_write(with_galls_grids, dsn=paste0(out.dir, "with_galls",".shp"),driver = "ESRI Shapefile") #Saving on shapefile

#ABSENCE OF TRICHILOGASTER
without_galls <- db %>% 
  filter(X8_A_planta_que_est_a == "Não") #Filtering points without presence of Trichilogaster

without_galls_grids<- grid_pt[without_galls,] #Filtering cells without presence of Trichilogaster

plot(st_geometry(grid_pt[without_galls, ])) #Plotting on subsetted map to doublecheck

st_write(without_galls_grids, dsn=paste0(out.dir, "without_galls",".shp"),driver = "ESRI Shapefile") #Saving on shapefile



#Plotting map
ggplot() + 
  geom_sf(data = grid_pt, fill="white") +
  geom_sf(data = without_galls_grids, fill="red") +
  geom_sf(data = with_galls_grids, fill="green") +
  theme(panel.grid.major = element_line(colour = "transparent"),
        panel.border=element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "italic", size = 14)) +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book") +
  
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.15, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_minimal(
      #fill = c("grey40", "white"),
      fill = "grey40",
      line_col = "grey20",
      text_family = "ArcherPro Book") )
