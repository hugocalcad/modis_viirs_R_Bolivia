## punto de Calor agosto Agosto 2019
library(raster)
library(fasterize)
library(ggplot2)
library(sf)
library(rgdal)
library(tidyverse)
library(tmap)

#Establecemos carpeta de trabajo
setwd('C:/Users/migue/Documents/UMSA/Focos_calor')

#Open VIIRS data obtenidos de https://firms.modaps.eosdis.nasa.gov/download/ del 1 de agosto de 2019
#al 2 de septiembre de 2019
pcalor_Viirs <- read.csv2("fire_nrt_V1_74210.csv", stringsAsFactors = F, sep = ',')
pcalor_Viirs$longitude <- as.numeric(pcalor_Viirs$longitude)
pcalor_Viirs$latitude <- as.numeric(pcalor_Viirs$latitude)
head(pcalor_Viirs)
tail(pcalor_Viirs)


#Open shapefiles limite de Bolivia, areas protegidas nacionales, municipales y departamentales
sf_boldep <- st_read('shp/Bolivia_Departaentos.shp')
sf_area_prot <- st_read('shp/aps_nacionales_2017.shp')
sf_ap_dep <- st_read('shp/Aps Departamentales.shp')
sf_ap_mun <- st_read('shp/Aps Municipales.shp')

#Subset para cubrir el area de Santa Cruz y Beni
sf_SC_beni <- subset(sf_boldep,sf_boldep$NOM_DEP %in% c('SANTA CRUZ','BENI'))

#Cargar los datos de Viirs del 1 de agosto de 2019 a 2 de septiembre de 2019
sf_pcalor <- st_as_sf(pcalor_Viirs, coords = c("longitude", "latitude"), 
                      crs = 4326, agr = "constant")

#Hacer un recorte para cubrir el area de Beni y Santa Cruz
b = sapply(st_intersects(sf_pcalor, sf_SC_beni),function(x){length(x)==0})
sf_pcalor_sc_be <- sf_pcalor[!b,]
sf_pcalor_sc_be <- sf_pcalor_sc_be[,-c(1:12)]

#HAcemos un plot para verificar los datos
ggplot(sf_boldep) +
  geom_sf() +
  geom_sf(data = sf_pcalor_sc_be, size = 0.2, shape = 15, color = "red") +
  geom_sf(data= sf_area_prot, fill= 'green', alpha = 0.2) +
  geom_sf(data= sf_ap_dep, fill= 'yellow', alpha = 0.2) +
  geom_sf(data= sf_ap_mun, fill= 'blue', alpha = 0.2) +
  scale_color_manual(values=c('green', 'yellow', 'blue'),
                     labels=c('Aps NAcionales','Aps departamentales','Aps Municipales'))

#Creamos un raster con la extension y projeccion de los shpa de Beni y Santa Cruz con una 
#resolucion de 250 a 375 m aproximadamente
ras_pcalor <- raster(ext = extent(sf_SC_beni), crs = crs(sf_SC_beni))
res(ras_pcalor) <- c(0.00338225,0.00338225)

#Generamos el raster con los puntos de calor recortados
ras_pcalor <- rasterize(sf_pcalor_sc_be, ras_pcalor, fun = mean)

#reclasificacamos para volverlo binario 1 donde hay focos de calor
ras2 <-reclassify(ras_pcalor, c(-Inf,0,0,0.1, Inf,1))

#publicacion utilizando tmap Utilizando los puntos en formato shape  con las capas de departamentos
#y areas protegidas
mapa <- tm_shape(sf_boldep) + tm_borders() +
  tm_shape(sf_SC_beni) + tm_fill() + tm_borders() +
  tm_shape(sf_pcalor_sc_be) + tm_symbols(col = 'red', shape=15, scale=0.125)+
  tm_shape(sf_area_prot) + tm_fill(col = 'green', alpha = 0.5) +
  tm_shape(sf_ap_dep) + tm_fill(col = 'yellow', alpha = 0.5) +
  tm_shape(sf_ap_mun) + tm_fill(col = 'blue', alpha = 0.5) 
#Agregar una peque;a leyenda
leyenda <- tm_add_legend('fill',
                         labels=c('APs Nacionales', 'Aps Departamentales', 'Aps Municipales'),
                         col = c('green', 'yellow', 'blue'))
#publicacion o ploteo
mapa + leyenda +  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 1) 
