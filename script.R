library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(tidyverse)
library(xlsx)

#Defining paths
out.dir <- (".\\Monit_Trichi_2023\\Mapas\\")

#LOADING DATASETS ----

#Loading grid UTM 10x10km ETRS89 / Portugal TM06
grid_pt <- st_read(".\\Monit_Trichi_2023\\Monit_Trichilogaster_2023\\Fishnet_10km.shp")

#Loading monitored points
db <- read.xlsx(".\\GallsTrichi_Volunteer_20230824.xlsx", "GallsTrichi_Volunteer", header = T, encoding ="UTF-8") %>% 
  as.data.frame() %>%
  select(c(1:36)) %>%
  #Filtering only Epicollect 5 records during the Monitoring campaign
  filter(Atividade == "Monitorizacao",
         Source == "Epicollect 5",
         between(Date, as.Date('2023-07-01'), as.Date('2023-08-24'))) %>% 
  #Transforming variables to factor, and latitude and longitude to numeric
  mutate_at(c(5,13:17,19,20:23), as.factor) %>%
  mutate_at(c("Y", "X"), as.numeric) %>%
         #Sorting factor levels
  mutate(Density = fct_relevel(Density, "Uma", "Poucas", "Mancha até 100m2", "Mancha de 100m2 a 1ha", "Mancha maior do que 1ha"),
         Size = fct_relevel(Size, "Plântula (menor que 1 palmo)", "Planta jovem (até +- 1m)", "Planta adulta"),
         #Cleaning and sorting factor levels
         X.Cob_Galls = fct_recode(X.Cob_Galls, "0%"="0%","1%-10%"="1-10%","10%-25%"="10-25%","25%-50%"="25%-50%","50%-75%"="50%-75%","75%-90%"="75%-90%","﹥90%"="﹥90% " ),
         X.Cob_Galls = fct_relevel(X.Cob_Galls, "0%","1%-10%","10%-25%","25%-50%","50%-75%","75%-90%","﹥90%"), 
         X.Cob_Phyllodes = fct_recode(X.Cob_Phyllodes, "0%"="0%","1%-10%"="1%-10%","10%-25%"="10%-25%","25%-50%"="25%-50%","50%-75%"="50%-75%","75%-90%"="75%-90%","﹥90%"="﹥90% " ),
         X.Cob_Phyllodes = fct_relevel(X.Cob_Phyllodes,  "0%","1%-10%","10%-25%","25%-50%","50%-75%","75%-90%","﹥90%"),
         X.Cob_Flowers = fct_recode(X.Cob_Flowers, "0%"="0%","1%-10%"="1%-10%","10%-25%"="10%-25%","25%-50%"="25%-50%","50%-75%"="50%-75%","75%-90%"="75%-90%","﹥90%"="﹥90% " ),
         X.Cob_Flowers = fct_relevel(X.Cob_Flowers,  "0%","1%-10%","10%-25%","25%-50%","50%-75%","75%-90%","﹥90%"),
         X.Cob_Pods = fct_recode(X.Cob_Pods, "0%"="0%","1%-10%"="1%-10%","10%-25%"="10%-25%","25%-50%"="25%-50%","50%-75%"="50%-75%","75%-90%"="75%-90%","﹥90%"="﹥ 90% " ),
         X.Cob_Pods = fct_relevel(X.Cob_Pods,  "0%","1%-10%","10%-25%","25%-50%","50%-75%","75%-90%","﹥90%"),
         #Creating new quantitative variables from each level range
         Perc_galls = fct_recode(X.Cob_Galls, "0"="0%","5.5"="1%-10%","17.5"="10%-25%","37.5"="25%-50%","62.5"="50%-75%","82.5"="75%-90%","105"="﹥90%" ),
         Perc_phylo = fct_recode(X.Cob_Phyllodes, "0"="0%","5.5"="1%-10%","17.5"="10%-25%","37.5"="25%-50%","62.5"="50%-75%","82.5"="75%-90%","105"="﹥90%" ),
         Perc_flow = fct_recode(X.Cob_Flowers, "0"="0%","5.5"="1%-10%","17.5"="10%-25%","37.5"="25%-50%","62.5"="50%-75%","82.5"="75%-90%","105"="﹥90%" ),
         Perc_pods = fct_recode(X.Cob_Pods, "0"="0%","5.5"="1%-10%","17.5"="10%-25%","37.5"="25%-50%","62.5"="50%-75%","82.5"="75%-90%","105"="﹥90%" )) %>% 
  #Transforming qualitative new variables to numeric       
  mutate_at(c("Perc_galls", "Perc_phylo", "Perc_flow", "Perc_pods"), as.numeric) %>% 
  #Transforming data frame to spatial data frame and projecting from WGS84 to ETRS89 / Portugal TM06
  filter(!is.na(X)) %>%
  st_as_sf(coords = c("X","Y"), crs = 4326, remove = FALSE) %>%
  st_set_crs("+proj=longlat +datum=WGS84") %>%
  st_transform(crs = 3763) %>%
  #Joining cell grid codes from UTM to database with monitored points
  st_join(grid_pt)

save(db,grid_pt, file = "dataset_monitoring_2023.rds")


#CREATING MAPS ----

#PRESENCE OF TRICHILOGASTER
with_galls <- db %>% 
  filter(X8_A_planta_que_est_a == "Sim") #Filtering points with presence only of Trichilogaster

with_galls_grids <- grid_pt[with_galls,] #Filtering cells with presence only of Trichilogaster

plot(st_geometry(grid_pt[pi, ])) #Plotting on subsetted map to doublecheck

st_write(with_galls_grids, dsn = paste0(out.dir, "with_galls",".shp"),driver = "ESRI Shapefile") #Saving on shapefile

#ABSENCE OF TRICHILOGASTER
without_galls <- db %>% 
  filter(X8_A_planta_que_est_a == "Não") #Filtering points without presence of Trichilogaster

without_galls_grids<- grid_pt[without_galls,] #Filtering cells without presence of Trichilogaster

plot(st_geometry(grid_pt[without_galls, ])) #Plotting on subsetted map to doublecheck

st_write(without_galls_grids, dsn=paste0(out.dir, "without_galls",".shp"),driver = "ESRI Shapefile") #Saving on shapefile


with_galls_m <- with_galls %>% 
  select(Perc_galls, Perc_phylo, Perc_flow, Perc_pods, ID_grid) %>% 
  group_by(ID_grid) %>% 
  mutate(perc_galls_mean = mean(Perc_galls))

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
