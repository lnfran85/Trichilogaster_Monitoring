library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(tidyverse)
library(xlsx)
library(leaflet)
library(htmlwidgets)

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
         between(Date, as.Date('2023-06-01'), as.Date('2023-08-24'))) %>%
  droplevels() %>%
  #Excluding people not involved on monitoring campaign
  filter(User != "António Correia ",
         User != "Dora Oliveira (ICNF)",
         User != "Mário Reis",
         User != "Catarina Gregorio (ICNF)",
         User != "Isa Teixeira (ICNF)") %>%
  droplevels() %>%
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
  droplevels() %>%
  #Transforming qualitative new variables to numeric       
  mutate_at(c("Perc_galls", "Perc_phylo", "Perc_flow", "Perc_pods"), as.numeric) %>% 
  #Transforming data frame to spatial data frame and projecting from WGS84 to ETRS89 / Portugal TM06
  filter(!is.na(X)) %>%
  st_as_sf(coords = c("X","Y"), crs = 4326, remove = FALSE) %>%
  st_set_crs("+proj=longlat +datum=WGS84") %>%
  st_transform(crs = 3763) %>%
  #Joining cell grid codes from UTM to database with monitored points
  st_join(grid_pt)

#Loading dataset with the no visited places with previous records of Trichilogaster
db1<- read.csv(".\\Pres_previa_trichi_sem_confirm.csv", header = T, encoding ="UTF-8") %>% 
  as.data.frame() %>% 
  #Transforming data frame to spatial data frame and projecting from WGS84 to ETRS89 / Portugal TM06
  filter(!is.na(X)) %>%
  st_as_sf(coords = c("X","Y"), crs = 4326, remove = FALSE) %>%
  st_set_crs("+proj=longlat +datum=WGS84") %>%
  st_transform(crs = 3763) %>%
  #Joining cell grid codes from UTM to database with monitored points
  st_join(grid_pt)  
  
#Loading dataset with non visited places
db2<- read.csv(".\\Lugares_no_visit_sin_pres_previa_trichi.csv", header = T, encoding ="UTF-8") %>% 
  as.data.frame() %>% 
  #Transforming data frame to spatial data frame and projecting from WGS84 to ETRS89 / Portugal TM06
  filter(!is.na(X)) %>%
  st_as_sf(coords = c("X","Y"), crs = 4326, remove = FALSE) %>%
  st_set_crs("+proj=longlat +datum=WGS84") %>%
  st_transform(crs = 3763) %>%
  #Joining cell grid codes from UTM to database with monitored points
  st_join(grid_pt)  


save(db, db1, db2, grid_pt, file = "datasets_monitoring_2023.rds")


#CREATING MAPS ----

#PRESENCE OF TRICHILOGASTER
#Filtering points with presence only of Trichilogaster
with_galls <- db %>%
  filter(Species == "Acacia longifolia",
         Presence == "Sim") 
with_galls$Species <- droplevels(with_galls$Species) 
with_galls$Presence <- droplevels(with_galls$Presence)
#Filtering cells with presence only of Trichilogaster
with_galls_grids <- grid_pt[with_galls,] 
#Plotting on subsetted map to double check
plot(st_geometry(grid_pt[with_galls, ])) 
#Exporting to shapefile
st_write(with_galls_grids, dsn = paste0(out.dir, "with_galls",".shp"),driver = "ESRI Shapefile") #Saving on shapefile


#Filtering cells with presence only of Trichilogaster but which there were not visited during the campaign
Pl_wTrichi_grids <- grid_pt[db1,] 
#Plotting on subsetted map to double check
plot(st_geometry(grid_pt[db1, ])) 
#Exporting to shapefile
st_write(Pl_wTrichi_grids, dsn = paste0(out.dir, "Places_with_galls_WithoutVis",".shp"),driver = "ESRI Shapefile") #Saving on shapefile

#ABSENCE OF TRICHILOGASTER
#Filtering points without presence of Trichilogaster
without_galls <- db %>% 
  filter(Species == "Acacia longifolia",
         Presence == "Não") 
without_galls$Species <- droplevels(without_galls$Species) 
without_galls$Presence <- droplevels(without_galls$Presence)
#Filtering cells without presence of Trichilogaster
without_galls_grids<- grid_pt[without_galls,] 
#Plotting on subsetted map to double check
plot(st_geometry(grid_pt[without_galls, ])) 
#Exporting to shapefile
st_write(without_galls_grids, dsn=paste0(out.dir, "without_galls",".shp"),driver = "ESRI Shapefile") #Saving on shapefile


#Filtering cells with unknown presence of Trichilogaster 
Pl_not_visited_grids <- grid_pt[db2,] 
#Plotting on subsetted map to double check
plot(st_geometry(grid_pt[db2, ])) 
#Exporting to shapefile
st_write(Pl_not_visited_grids, dsn = paste0(out.dir, "Places_WithoutVis",".shp"),driver = "ESRI Shapefile") #Saving on shapefile


#Plotting map (simple)
ggplot() + 
  geom_sf(data = grid_pt, fill="white") +
  geom_sf(data = without_galls_grids, fill="red") +
  geom_sf(data = Pl_not_visited_grids, fill="black") +
  geom_sf(data = Pl_wTrichi_grids, fill="orange") +
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


#Creating the more complex and apelative map
#Transforming overlaped cells (pres and absence) in presences
overlaped <- intersect(with_galls_grids,without_galls_grids) #solapadas
only_pres <- setdiff(with_galls_grids,without_galls_grids) #solo pres
only_abs <- setdiff(without_galls_grids,with_galls_grids) #solo aus

with_galls_grids <- rbind(overlaped, only_pres)
without_galls_grids <- only_abs

#Projecting data sets to WGS84 from ETRS89 / Portugal TM06
grid_pt1 <- grid_pt %>%
  st_transform(crs = 4326)

without_galls_grids1 <- without_galls_grids %>%
  st_transform(crs = 4326)

Pl_not_visited_grids1 <- Pl_not_visited_grids %>%
  st_transform(crs = 4326)

Pl_wTrichi_grids1 <- Pl_wTrichi_grids %>%
  st_transform(crs = 4326)

with_galls_grids1 <- with_galls_grids %>%
  st_transform(crs = 4326)

#Creating the map
m <- leaflet(options = leafletOptions(minZoom = 6, maxZoom = 18 , preferCanvas = TRUE) ) %>%
  addProviderTiles("OpenStreetMap", group="Open Street Map") %>% 
  addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
  setView(-7.121931, 39.909736, zoom = 6) %>%
  
  addPolygons(data = grid_pt1, 
              color = "white", fillColor = NULL, fillOpacity = 0, dashArray = "3",
              group = "Grelha UTM10x10km") %>%
  addPolygons(data = without_galls_grids1, 
              color = "red", fillColor = "red", fillOpacity = 0.4,dashArray = "3",
              group = "Galhas não detectadas") %>%
  addPolygons(data = Pl_not_visited_grids1, 
              color = "black", fillColor = "black", fillOpacity = 0.4,dashArray = "3",
              group = "Quadrículas com acácia não visitadas") %>%
  addPolygons(data = Pl_wTrichi_grids1, 
              color = "orange", fillColor = "orange", fillOpacity = 0.4,dashArray = "3",
              group = "Quadrículas com Trichilogaster não visitadas") %>%
  addPolygons(data = with_galls_grids1, 
              color = "green", fillColor = "green", fillOpacity = 0.4,dashArray = "3",
              group = "Galhas detectadas") %>%
  
  addLayersControl(
    baseGroups = c("Open Street Map", "Satellite"),
    #overlayGroups = c("marcadores", "clusteres", "Freguesias com Presença", "Freguesias com Libertação"),
    #overlayGroups = c("Censo", "Áreas Protegidas"),
    overlayGroups = c("Grelha UTM10x10km"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to Level 10",
    onClick=JS("function(btn, map){ map.setZoom(10); }"))) %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
  
  addMeasure(primaryLengthUnit = "kilometers",
             primaryAreaUnit = "hectares",
             localization = "pt_PT")
#Printing and exporting the map
m  
saveWidget(m, file = ".//mapa_leaflet.html")






with_galls_m <- with_galls %>% 
  select(Perc_galls, Perc_phylo, Perc_flow, Perc_pods, ID_grid) %>% 
  group_by(ID_grid) %>% 
  mutate(perc_galls_mean = mean(Perc_galls))
