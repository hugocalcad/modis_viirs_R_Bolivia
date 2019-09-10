## VIIRS https://drive.google.com/file/d/1uZpKO2wFBwjvLvpGFN4HNGHtU5vODn3B/view?usp=sharing viirs
## MODIS https://drive.google.com/file/d/1WgVwJ5aP803sCL4ROSqsQC_tCg3O6CnM/view?usp=sharing modis

library(sf)
library(tidyverse)
library(here)
library(ggplot2)
library(lubridate)

# load .rdas
load('data/modis_all_09092019.Rda')
load('data/viirs_all_09092019.Rda')

# ver la estructura
str(modis_final)
str(viirs_final)

# convirtiendo la columnas de las fechas
modis_final$acq_date <- as.Date(modis_final$acq_date, format = '%Y-%m-%d')
viirs_final$acq_date <- as.Date(viirs_final$acq_date, format = '%Y-%m-%d')

#load shapes
#Open shapefiles limite de Bolivia, areas protegidas nacionales, municipales y departamentales
sf_boldep <- st_read('shp/Bolivia_Departaentos.shp')
sf_area_prot <- st_read('shp/aps_nacionales_2017.shp')
sf_ap_dep <- st_read('shp/Aps Departamentales.shp')
sf_ap_mun <- st_read('shp/Aps Municipales.shp')

#Subset para cubrir el area de Santa Cruz y Beni
sf_sc_beni <- subset(sf_boldep,sf_boldep$NOM_DEP %in% c('SANTA CRUZ','BENI'))

##convertir a shapefile los datos viirs y modis
sf_modis <- st_as_sf(modis_final,  coords = c("longitude", "latitude"), 
                     crs = 4326, agr = "constant")

sf_viirs <- st_as_sf(viirs_final,  coords = c("longitude", "latitude"), 
                     crs = 4326, agr = "constant")

##contando datos por mes
viirs_mensual <- viirs_final %>% 
  group_by(month=floor_date(acq_date, 'month')) %>%
  tally()

ggplot(viirs_mensual, aes(month, n)) + geom_line() +
  scale_x_date(date_labels = "%b-%Y") + xlab("") + ylab("Nro Fuegos")
  
sf_ecorregiones <- st_read('C:/Users/migue/Documents/UMSA/Biologia/Cons_/Mapas_base/shp2/ecorre_ibish/Ecoregiones_FAN.shp')
sf_navarro <- st_read('C:/Users/migue/Documents/UMSA/Biologia/Cons_/Mapas_base/shps/Navarro/veg_nv_bol_geo.shp')

names(sf_navarro)[3] <- 'Navarro_veg'
names(sf_ecorregiones)[4] <- 'Ecoreg_ibich'
names(sf_boldep)[5] <- 'Departamento'
names(sf_area_prot)[8] <- 'ap_nacional'
names(sf_ap_dep)[9] <- 'ap_departamental'
names(sf_ap_mun)[11] <- 'ap_municipal'

sf_modis_ecor <- st_join(sf_modis, sf_ecorregiones["Ecoreg_ibich"], left = T)
sf_modis_ecor <- st_join(sf_modis_ecor, sf_navarro['Navarro_veg'], left = T)
sf_modis_ecor <- st_join(sf_modis_ecor, sf_boldep['Departamento'], left = T)
sf_modis_ecor <- st_join(sf_modis_ecor, sf_area_prot['ap_nacional'], left = T)
sf_modis_ecor <- st_join(sf_modis_ecor, sf_ap_dep['ap_departamental'], left = T)
sf_modis_ecor <- st_join(sf_modis_ecor, sf_ap_mun['ap_municipal'], left = T)

sf_viirs_ecor <- st_join(sf_viirs, sf_ecorregiones["Ecoreg_ibich"], left = T)
sf_viirs_ecor <- st_join(sf_viirs_ecor, sf_navarro['Navarro_veg'], left = T)
sf_viirs_ecor <- st_join(sf_viirs_ecor, sf_boldep['Departamento'], left = T)
sf_viirs_ecor <- st_join(sf_viirs_ecor, sf_area_prot['ap_nacional'], left = T)
sf_viirs_ecor <- st_join(sf_viirs_ecor, sf_ap_dep['ap_departamental'], left = T)
sf_viirs_ecor <- st_join(sf_viirs_ecor, sf_ap_mun['ap_municipal'], left = T)

df_viirs_new <- as.data.frame(sf_viirs_ecor)
df_modis_new <- as.data.frame(sf_modis_ecor)

save(df_viirs_new, file='viirs_all_cols_090920109.Rda')
save(df_modis_new), file='modis_all_cols_090920109.Rda'