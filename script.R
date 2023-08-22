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
variables <- c(User = "X1_Usurio", 
               Data = "X3_Data", 
               Habitat = "X4_Tipo_de_habitat_co", 
               Species = "X5_Espcie_que_est_a_r", 
               Density = "X6_Selecione_a_densid", 
               Size = "X7_Qual_o_tamanho_dom", 
               Gall_presence = "X8_A_planta_que_est_a", 
               Gall_quantity = "X9_Quantas_galhas_obs", 
               Gall_type = "X10_As_galhas_que_obs")

db <- read.csv("C:\\Users\\LENOVO\\Downloads\\registo-de-trichilogaster-acaciaelongifoliae-csv(14)\\form-1__registo-galhas-de-trichilogaster.csv", header=T, sep=",") %>% 
  as.data.frame() %>%
  
  rename(all_of(variables)) %>%
  
  mutate_at(c("X12_Observando_a_copa", "X13_Observando_a_copa", "X14_Observando_a_copa", "X15_Observando_a_copa",
              "User", "Habitat", "Species", "Density", "Size", "Gall_presence", "Gall_type"), as.factor) %>%
  
  mutate(X = as.numeric(long_2_Localizao), 
         Y = as.numeric(lat_2_Localizao),
         Data = as.Date(Data, format="%d/%m/%Y"),
         Gall_quantity = as.numeric(Gall_quantity),
         
         Density = fct_relevel(Density, "Uma", "Poucas", "Mancha até 100m2", "Mancha de 100m2 a 1ha", "Mancha maior do que 1ha"),
         Size = fct_relevel(Size, "Plântula (menor que 1 palmo)", "Planta jovem (até +- 1m)", "Planta adulta"),
         
         X12_Observando_a_copa = fct_recode(X12_Observando_a_copa, "0%"="0%","1%-10%"="1-10%","10%-25%"="10-25%","25%-50%"="25%-50%","50%-75%"="50%-75%","75%-90%"="75%-90%","﹥90%"="﹥90% " ), 
         X13_Observando_a_copa = fct_recode(X13_Observando_a_copa, "0%"="0%","1%-10%"="1%-10%","10%-25%"="10%-25%","25%-50%"="25%-50%","50%-75%"="50%-75%","75%-90%"="75%-90%","﹥90%"="﹥90% " ),
         X14_Observando_a_copa = fct_recode(X14_Observando_a_copa, "0%"="0%","1%-10%"="1%-10%","10%-25%"="10%-25%","25%-50%"="25%-50%","50%-75%"="50%-75%","75%-90%"="75%-90%","﹥90%"="﹥90% " ), 
         X15_Observando_a_copa = fct_recode(X15_Observando_a_copa, "0%"="0%","1%-10%"="1%-10%","10%-25%"="10%-25%","25%-50%"="25%-50%","50%-75%"="50%-75%","75%-90%"="75%-90%","﹥90%"="﹥ 90% " ) , 
         
         X12_Observando_a_copa = fct_relevel(X12_Observando_a_copa, "0%","1%-10%","10%-25%","25%-50%","50%-75%","75%-90%","﹥90%"), 
         X13_Observando_a_copa = fct_relevel(X13_Observando_a_copa,  "0%","1%-10%","10%-25%","25%-50%","50%-75%","75%-90%","﹥90%"),
         X14_Observando_a_copa = fct_relevel(X14_Observando_a_copa,  "0%","1%-10%","10%-25%","25%-50%","50%-75%","75%-90%","﹥90%"), 
         X15_Observando_a_copa = fct_relevel(X15_Observando_a_copa,  "0%","1%-10%","10%-25%","25%-50%","50%-75%","75%-90%","﹥90%") , 
         
         Perc_galls = fct_recode(X12_Observando_a_copa, "0"="0%","5.5"="1%-10%","17.5"="10%-25%","37.5"="25%-50%","62.5"="50%-75%","82.5"="75%-90%","105"="﹥90%" ),
         Perc_phylo = fct_recode(X13_Observando_a_copa, "0"="0%","5.5"="1%-10%","17.5"="10%-25%","37.5"="25%-50%","62.5"="50%-75%","82.5"="75%-90%","105"="﹥90%" ),
         Perc_flow = fct_recode(X14_Observando_a_copa, "0"="0%","5.5"="1%-10%","17.5"="10%-25%","37.5"="25%-50%","62.5"="50%-75%","82.5"="75%-90%","105"="﹥90%" ),
         Perc_pods = fct_recode(X15_Observando_a_copa, "0"="0%","5.5"="1%-10%","17.5"="10%-25%","37.5"="25%-50%","62.5"="50%-75%","82.5"="75%-90%","105"="﹥90%" )) %>% 
         
  mutate_at(c("Perc_galls", "Perc_phylo", "Perc_flow", "Perc_pods"), as.numeric) %>% 

  filter(!is.na(X)) %>%
  st_as_sf(coords = c("long_2_Localizao","lat_2_Localizao"), crs = 4326, remove = FALSE)
db <- st_transform(db, crs = 3763 ) #Projecting to ETRS89

#Joining cell grid codes from UTM to database with monitored points
db <- st_join(db, grid_pt)

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
