library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(tidyverse)
library(xlsx)
library(leaflet)
library(htmlwidgets)
library(smplot2)

#Defining paths
out.dir <- (".\\Monit_Trichi_2023\\Mapas\\")

#LOADING DATASETS ----
  #load("datasets_monitoring_2023.rds")
#Loading grid UTM 10x10km ETRS89 / Portugal TM06
grid_pt <- st_read(".\\Monit_Trichi_2023\\Monit_Trichilogaster_2023\\Fishnet_10km.shp") #the whole grid
grid_pt30 <- st_read(".\\Monit_Trichi_2023\\Monit_Trichilogaster_2023\\Fishnet_10km_clip.shp") #the clipped grid with 30km from the coastline

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
  mutate_at(c("Perc_galls", "Perc_phylo", "Perc_flow", "Perc_pods"), as.character) %>% 
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


save(db, db1, db2, grid_pt, grid_pt30, file = "datasets_monitoring_2023.rds")


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


##Creating the more complex and apelative map ----
#Transforming overlaped cells (pres and absence) in presences
overlaped <- intersect(with_galls_grids,without_galls_grids) #solapadas
only_pres <- setdiff(with_galls_grids,without_galls_grids) #solo pres
only_abs <- setdiff(without_galls_grids,with_galls_grids) #solo aus

with_galls_grids <- rbind(overlaped, only_pres)
without_galls_grids <- only_abs

#Projecting data sets to WGS84 from ETRS89 / Portugal TM06
grid_pt1 <- grid_pt %>%
  st_transform(crs = 4326)
grid_pt301 <- grid_pt30 %>%
  st_transform(crs = 4326)


without_galls_grids1 <- without_galls_grids %>%
  st_transform(crs = 4326)

Pl_not_visited_grids1 <- Pl_not_visited_grids %>%
  st_transform(crs = 4326)

Pl_wTrichi_grids1 <- Pl_wTrichi_grids %>%
  st_transform(crs = 4326)

with_galls_grids1 <- with_galls_grids %>%
  st_transform(crs = 4326)

st_write(grid_pt1, dsn=paste0(out.dir, "grid_ptwgs84",".shp"),driver = "ESRI Shapefile") #Saving on shapefile
st_write(grid_pt301, dsn=paste0(out.dir, "grid_pt30wgs84",".shp"),driver = "ESRI Shapefile") #Saving on shapefile
st_write(with_galls_grids1, dsn=paste0(out.dir, "presencewgs84",".shp"),driver = "ESRI Shapefile") #Saving on shapefile
st_write(without_galls_grids1, dsn=paste0(out.dir, "absencewgs84",".shp"),driver = "ESRI Shapefile") #Saving on shapefile
st_write(Pl_not_visited_grids1, dsn=paste0(out.dir, "pl_not_visitedwgs84",".shp"),driver = "ESRI Shapefile") #Saving on shapefile
st_write(Pl_wTrichi_grids1, dsn=paste0(out.dir, "pl_withtrichiwgs84",".shp"),driver = "ESRI Shapefile") #Saving on shapefile


#Creating the map
m <- leaflet(options = leafletOptions(minZoom = 6, maxZoom = 18 , preferCanvas = TRUE) ) %>%
  addProviderTiles("OpenStreetMap", group="Open Street Map") %>% 
  addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
  setView(-7.121931, 39.909736, zoom = 6) %>%
  

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
  addPolygons(data = grid_pt1, 
              color = "grey", fillColor = NULL, fillOpacity = 0, dashArray = "3",
              group = "Grelha UTM10x10km") %>%
  addPolygons(data = grid_pt301, 
              color = "darkblue", fillColor = NULL, fillOpacity = 0, dashArray = "3",
              group = "Grelha 30km UTM10x10km") %>%
  
  addLayersControl(
    baseGroups = c("Open Street Map", "Satellite"),
    #overlayGroups = c("marcadores", "clusteres", "Freguesias com Presença", "Freguesias com Libertação"),
    #overlayGroups = c("Censo", "Áreas Protegidas"),
    overlayGroups = c("Grelha UTM10x10km","Grelha 30km, UTM10x10km", "Galhas detectadas", "Galhas não detectadas", "Quadrículas com Trichilogaster não visitadas", "Quadrículas com acácia não visitadas"),
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
saveWidget(m, file = ".\\Monit_Trichi_2023\\Mapas\\mapa_pres_aus_leaflet.html")


#CALCULATING MEANS ----
#Filtering points with presence only of Trichilogaster
with_galls_m <- db %>%
  filter(Species == "Acacia longifolia",
         Presence == "Sim") 
with_galls_m$Species <- droplevels(with_galls_m$Species) 
with_galls_m$Presence <- droplevels(with_galls_m$Presence)
with_galls_m <- with_galls_m %>% 
  select(Perc_galls, Perc_phylo, Perc_flow, Perc_pods, ID_grid, geometry, X, Y) %>%
  group_by(ID_grid) %>% 
  summarise(
    perc_galls_sample.size = n(),
    perc_galls_mean = mean(Perc_galls),
    sd.perc_galls=sd(Perc_galls),
    error.perc_galls=sd.perc_galls/sqrt(perc_galls_sample.size), 
    perc_phylo_sample.size = n(),
    perc_phylo_mean = mean(Perc_phylo),
    sd.perc_phylo=sd(Perc_phylo),
    error.perc_phylo=sd.perc_phylo/sqrt(perc_phylo_sample.size),
    perc_flow_sample.size = n(),
    perc_flow_mean = mean(Perc_flow),
    sd.perc_flow=sd(Perc_flow),
    error.perc_flow=sd.perc_flow/sqrt(perc_flow_sample.size),
    perc_pods_sample.size = n(),
    perc_pods_mean = mean(Perc_pods),
    sd.perc_pods=sd(Perc_pods),
    error.perc_pods=sd.perc_pods/sqrt(perc_pods_sample.size)) %>% 
  mutate(Perc_galls_cat=cut(perc_galls_mean, breaks=c(-Inf, 0, 25, 75, Inf)),
         Perc_phylo_cat=cut(perc_phylo_mean, breaks=c(-Inf, 0, 25, 75, Inf)),
         Perc_flow_cat=cut(perc_flow_mean, breaks=c(-Inf, 0, 25, 75, Inf)),
         Perc_pods_cat=cut(perc_pods_mean, breaks=c(-Inf, 0, 25, 75, Inf)))


#saving table
write.csv2(as.data.frame(with_galls_m), 'Tabela_medias_monitor_Trichi_2023.csv')

#Creating the map

#Filtering cells with presence only of Trichilogaster
with_galls_m_grids <- grid_pt[with_galls_m,] 
#Plotting on subsetted map to double check
plot(st_geometry(grid_pt[with_galls, ])) 

with_galls_m_grids1 <- with_galls_m_grids %>%
  st_transform(crs = 4326)

without_galls_grids1 <- without_galls_grids %>%
  st_transform(crs = 4326)

Pl_not_visited_grids1 <- Pl_not_visited_grids %>%
  st_transform(crs = 4326)

Pl_wTrichi_grids1 <- Pl_wTrichi_grids %>%
  st_transform(crs = 4326)

conpal_galls <- colorNumeric(palette = "Greens", domain = with_galls_m$perc_galls_mean, na.color = "gray")
conpal_phylo <- colorNumeric(palette = "Greens", domain = with_galls_m$perc_phylo_mean, na.color = "gray")
conpal_flow <- colorNumeric(palette = "Greens", domain = with_galls_m$perc_flow_mean, na.color = "gray")
conpal_pods <- colorNumeric(palette = "Greens", domain = with_galls_m$perc_pods_mean, na.color = "gray")

dispal_galls <- colorFactor(c("white", "yellow","palegreen","darkgreen" ), domain = with_galls_m$Perc_galls_cat, na.color = "black")
dispal_phylo <- colorFactor(c("white", "yellow","palegreen","darkgreen" ), domain = with_galls_m$Perc_phylo_cat, na.color = "black")
dispal_flow <- colorFactor(c("white", "yellow","palegreen","darkgreen" ), domain = with_galls_m$Perc_flow_cat, na.color = "black")
dispal_pods <- colorFactor(c("white", "yellow","orange","darkred" ), domain = with_galls_m$Perc_pods_cat, na.color = "black")



m <- leaflet(options = leafletOptions(minZoom = 6, maxZoom = 18 , preferCanvas = TRUE) ) %>%
  addProviderTiles("OpenStreetMap", group="Open Street Map") %>% 
  addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
  setView(-7.121931, 39.909736, zoom = 6) %>%
  
  addPolygons(data = with_galls_m_grids1, 
              color = "green", fillColor = "green", fillOpacity = 0.4,dashArray = "3",
              group = "Galhas detectadas") %>%
  addPolygons(data = without_galls_grids1, 
              color = "red", fillColor = "red", fillOpacity = 0.4,dashArray = "3",
              group = "Galhas não detectadas") %>%
  addPolygons(data = Pl_wTrichi_grids1, 
              color = "orange", fillColor = "orange", fillOpacity = 0.4,dashArray = "3",
              group = "Quadrículas com Trichilogaster não visitadas") %>% 
  addPolygons(data = Pl_not_visited_grids1, 
              color = "black", fillColor = "black", fillOpacity = 0.4,dashArray = "3",
              group = "Quadrículas com acácia não visitadas") %>%
  addPolygons(data = with_galls_m_grids1, 
              color = ~dispal_galls(with_galls_m$Perc_galls_cat), fillColor = ~dispal_galls(with_galls_m$Perc_galls_cat), 
              popup = paste("Quadrícula: ", with_galls_m_grids1$ID_grid, "<br>",
                            "Número de acácias avaliadas: ", with_galls_m$perc_galls_sample.size, "<br>", 
                            "% Cobertura galhas: ", with_galls_m$perc_galls_mean, "\u00b1", round(with_galls_m$error.perc_galls, 1), "SME","<br>"),
              fillOpacity = 0.9,dashArray = "3",
              group = "Classe % Cobertura galhas") %>%
  addPolygons(data = with_galls_m_grids1, 
              color = ~dispal_phylo(with_galls_m$Perc_phylo_cat), fillColor = ~dispal_phylo(with_galls_m$Perc_phylo_cat), 
              popup = paste("Quadrícula: ", with_galls_m_grids1$ID_grid, "<br>",
                            "Número de acácias avaliadas: ", with_galls_m$perc_phylo_sample.size, "<br>", 
                            "% Cobertura filódios: ", with_galls_m$perc_phylo_mean, "\u00b1", round(with_galls_m$error.perc_phylo, 1), "SME","<br>"),
              fillOpacity = 0.9,dashArray = "3",
              group = "Classe % Cobertura filódios") %>%
  addPolygons(data = with_galls_m_grids1, 
              color = ~dispal_flow(with_galls_m$Perc_flow_cat), fillColor = ~dispal_flow(with_galls_m$Perc_flow_cat), 
              popup = paste("Quadrícula: ", with_galls_m_grids1$ID_grid, "<br>",
                            "Número de acácias avaliadas: ", with_galls_m$perc_flow_sample.size, "<br>", 
                            "% Cobertura flores: ", with_galls_m$perc_flow_mean, "\u00b1", round(with_galls_m$error.perc_flow, 1), "SME","<br>"),
              fillOpacity = 0.9,dashArray = "3",
              group = "Classe % Cobertura flores") %>%
  addPolygons(data = with_galls_m_grids1, 
              color = ~dispal_pods(with_galls_m$Perc_pods_cat), fillColor = ~dispal_pods(with_galls_m$Perc_pods_cat), 
              popup = paste("Quadrícula: ", with_galls_m_grids1$ID_grid, "<br>",
                            "Número de acácias avaliadas: ", with_galls_m$perc_pods_sample.size, "<br>", 
                            "% Cobertura vagens: ", with_galls_m$perc_pods_mean, "\u00b1", round(with_galls_m$error.perc_pods, 1), "SME","<br>"),
              fillOpacity = 0.9,dashArray = "3",
              group = "Classe % Cobertura vagens") %>%
  
  addPolygons(data = with_galls_m_grids1, 
              color = ~conpal_galls(with_galls_m$perc_galls_mean), fillColor = ~conpal_galls(with_galls_m$perc_galls_mean), 
              popup = paste("Quadrícula: ", with_galls_m_grids1$ID_grid, "<br>",
                            "Número de acácias avaliadas: ", with_galls_m$perc_galls_sample.size, "<br>", 
                            "% Cobertura galhas: ", with_galls_m$perc_galls_mean, "\u00b1", round(with_galls_m$error.perc_galls, 1), "SME","<br>"),
              fillOpacity = 0.9,dashArray = "3",
              group = "% Cobertura galhas") %>%  
  addPolygons(data = with_galls_m_grids1, 
              color = ~conpal_phylo(with_galls_m$perc_phylo_mean), fillColor = ~conpal_phylo(with_galls_m$perc_phylo_mean),
              popup = paste("Quadrícula: ", with_galls_m_grids1$ID_grid, "<br>",
                            "Número de acácias avaliadas: ", with_galls_m$perc_phylo_sample.size, "<br>",
                            "% Cobertura filódios: ", with_galls_m$perc_phylo_mean, "\u00b1", round(with_galls_m$error.perc_phylo, 1), "SME", "<br>"),
              fillOpacity = 0.9,dashArray = "3",
              group = "% Cobertura filódios") %>%
  addPolygons(data = with_galls_m_grids1, 
              color = ~conpal_flow(with_galls_m$perc_flow_mean), fillColor = ~conpal_flow(with_galls_m$perc_flow_mean), 
              popup = paste("Quadrícula: ", with_galls_m_grids1$ID_grid, "<br>",
                            "Número de acácias avaliadas: ", with_galls_m$perc_flow_sample.size, "<br>",
                            "% Cobertura flores: ", with_galls_m$perc_flow_mean, "\u00b1", round(with_galls_m$error.perc_flow, 1), "SME", "<br>"),
              fillOpacity = 0.9,dashArray = "3",
              group = "% Cobertura flores") %>%
  addPolygons(data = with_galls_m_grids1, 
              color = ~conpal_pods(with_galls_m$perc_pods_mean), fillColor = ~conpal_pods(with_galls_m$perc_pods_mean), 
              popup = paste("Quadrícula: ", with_galls_m_grids1$ID_grid, "<br>",
                            "Número de acácias avaliadas: ", with_galls_m$perc_pods_sample.size, "<br>",
                            "% Cobertura vagens: ", with_galls_m$perc_pods_mean, "\u00b1", round(with_galls_m$error.perc_pods, 1), "SME", "<br>"),
              fillOpacity = 0.9,dashArray = "3",
              group = "% Cobertura vagens") %>%
  addPolygons(data = grid_pt1, 
              color = "grey", fillColor = NULL, fillOpacity = 0.3, dashArray = "3",
              group = "Grelha UTM10x10km") %>%
  addPolygons(data = grid_pt301, 
              color = "darkblue", fillColor = NULL, fillOpacity = 0.3, dashArray = "3",
              group = "Grelha 30km UTM10x10km") %>%

  
  addLayersControl(
    baseGroups = c("Open Street Map", "Satellite"),
    #overlayGroups = c("marcadores", "clusteres", "Freguesias com Presença", "Freguesias com Libertação"),
    #overlayGroups = c("Censo", "Áreas Protegidas"),
    overlayGroups = c("Galhas detectadas", "Galhas não detectadas","Quadrículas com Trichilogaster não visitadas", "Quadrículas com acácia não visitadas", "Classe % Cobertura galhas", "Classe % Cobertura filódios", "Classe % Cobertura flores", "Classe % Cobertura vagens", "% Cobertura galhas", "% Cobertura filódios", "% Cobertura flores", "% Cobertura vagens", "Grelha UTM10x10km", "Grelha 30km UTM10x10km"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  
  addLegend(position = "bottomleft", pal = conpal_galls, values = with_galls_m$perc_galls_mean,
            title = "% Cobertura de galhas",
            opacity = 0.3, group="% Cobertura galhas") %>% 
  addLegend(position = "bottomleft", pal = conpal_phylo, values = with_galls_m$perc_phylo_mean,
            title = "% Cobertura de filódios",
            opacity = 0.3, group="% Cobertura filódios") %>% 
  addLegend(position = "bottomleft", pal = conpal_flow, values = with_galls_m$perc_flow_mean,
            title = "% Cobertura de flores",
            opacity = 0.3, group="% Cobertura flores") %>% 
  addLegend(position = "bottomleft", pal = conpal_pods, values = with_galls_m$perc_pods_mean,
            title = "% Cobertura de vagens",
            opacity = 0.3, group="% Cobertura vagens") %>% 
  addLegend(position = "bottomleft", pal = dispal_galls, values = with_galls_m$Perc_galls_cat,
            title = "Classe % Cobertura galhas",
            opacity = 0.3, group="Classe % Cobertura galhas") %>%
  addLegend(position = "bottomleft", pal = dispal_phylo, values = with_galls_m$Perc_phylo_cat,
            title = "Classe % Cobertura filódios",
            opacity = 0.3, group="Classe % Cobertura filódios") %>%
  addLegend(position = "bottomleft", pal = dispal_flow, values = with_galls_m$Perc_flow_cat,
            title = "Classe % Cobertura flores",
            opacity = 0.3, group="Classe % Cobertura flores") %>%
  addLegend(position = "bottomleft", pal = dispal_pods, values = with_galls_m$Perc_pods_cat,
            title = "Classe % Cobertura vagens",
            opacity = 0.3, group="Classe % Cobertura vagens") %>%
  hideGroup(c("Grelha 30km UTM10x10km", "Galhas detectadas", "Galhas não detectadas", "Quadrículas com Trichilogaster não visitadas", "Quadrículas com acácia não visitadas", "Classe % Cobertura filódios", "Classe % Cobertura flores", "Classe % Cobertura vagens", "% Cobertura galhas", "% Cobertura filódios", "% Cobertura flores", "% Cobertura vagens")) %>% 
  
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
saveWidget(m, file = ".\\Monit_Trichi_2023\\Mapas\\mapa_perc_leaflet.html")


## Correlations

with_galls_m$

ggplot(data=with_galls_m, aes(perc_galls_mean, perc_pods_mean))+
  geom_point(size=4, color="grey")+
  #geom_smooth(data=with_galls_m, mapping = aes(perc_galls_mean, perc_pods_mean))+
  sm_statCorr(color = '#0f993d', corr_method = 'spearman',size=2,
              linetype = 'dashed')+
  xlab("% médio de cobertura de galhas") +
  ylab("% médio de cobertura de vagens") +
  ylim(c(0,100))+
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.line = element_line(),
        panel.border=element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "italic", size = 14)) 
cor.test(with_galls_m$perc_galls_mean, with_galls_m$perc_pods_mean)

ggplot(data=with_galls_m, aes(perc_galls_mean, perc_phylo_mean))+
  geom_point(size=4, color="grey")+
  #geom_smooth(data=with_galls_m, mapping = aes(perc_galls_mean, perc_pods_mean))+
  sm_statCorr(color = '#0f993d', corr_method = 'spearman',size=2,
              linetype = 'dashed')+
  xlab("% médio de cobertura de galhas") +
  ylab("% médio de cobertura de filódios") +
  ylim(c(0,100))+
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.line = element_line(),
        panel.border=element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "italic", size = 14)) 
cor.test(with_galls_m$perc_galls_mean, with_galls_m$perc_phylo_mean)

ggplot(data=with_galls_m, aes(perc_galls_mean, perc_flow_mean))+
  geom_point(size=4, color="grey")+
  #geom_smooth(data=with_galls_m, mapping = aes(perc_galls_mean, perc_pods_mean))+
  sm_statCorr(color = '#0f993d', corr_method = 'spearman',size=2,
              linetype = 'dashed')+
  xlab("% médio de cobertura de galhas") +
  ylab("% médio de cobertura de inflorescências") +
  ylim(c(0,100))+
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.line = element_line(),
        panel.border=element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "italic", size = 14)) 
cor.test(with_galls_m$perc_galls_mean, with_galls_m$perc_flow_mean)

#DIRECT NO-TARGET EFFECTS ----
without_galls_m <- db %>%
  filter(Species != "Acacia longifolia") 
without_galls_m$Species <- droplevels(without_galls_m$Species)


tt1 <-count(without_galls_m, Species)
tt1 <- as.data.frame(tt1)
tt1 <- tt1[,c(1:2)]
write.csv2(as.data.frame(tt1), "species_nao_alvo.csv")
