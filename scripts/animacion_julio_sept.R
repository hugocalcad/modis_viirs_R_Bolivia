## Animacion rscript

## VIIRS https://drive.google.com/file/d/1uZpKO2wFBwjvLvpGFN4HNGHtU5vODn3B/view?usp=sharing viirs
## MODIS https://drive.google.com/file/d/1WgVwJ5aP803sCL4ROSqsQC_tCg3O6CnM/view?usp=sharing modis

library(sf)
library(tidyverse)
library(here)
library(ggplot2)
library(lubridate)
library(tmap)

# load .rdas
load('data/modis_all_09092019.Rda')
load('data/viirs_all_09092019.Rda')

# ver la estructura
str(modis_final)
str(viirs_final)

# convirtiendo la columnas de las fechas
modis_final$acq_date <- as.Date(modis_final$acq_date, format = '%Y-%m-%d')
viirs_final$acq_date <- as.Date(viirs_final$acq_date, format = '%Y-%m-%d')

#Abrimos la capa y hacemos subset
sf_boldep <- st_read('shp/Bolivia_Departaentos.shp')
#Subset para cubrir el area de Santa Cruz y Beni
sf_SC_beni <- subset(sf_boldep,sf_boldep$NOM_DEP %in% c('SANTA CRUZ','BENI'))

bbox_sc_be <- st_bbox(sf_SC_beni)

#viirs select 1 de julio al 9 de septiembre
viirs_julio_sep <- viirs_final %>%
  filter(acq_date > '2019-06-30') %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant") %>%
  st_crop(bbox_sc_be)

##Establecemos las Fechas de inicio y de final
start <- as.Date("2019-07-01",format="%Y-%m-%d")
end   <- as.Date("2019-09-09",format="%Y-%m-%d")


tmap_mode('plot')

c_osm <- tmaptools::read_osm(tmaptools::bb(sf_SC_beni), ext = 1.5)

### \____ Function for GIF ----
plot.map.calor <- function(){
  theDate <- start
  while(theDate <= end) {
    if(theDate == start){  
      dia <- viirs_julio_sep %>%
        filter(acq_date == theDate)
      ant <- dia
    }else{
      dia <- viirs_julio_sep %>%
        filter(acq_date %in% c(theDate - 1, theDate))
      ant <- viirs_julio_sep %>%
        filter(acq_date < theDate)
    }
    # mapa <- ggplot() +
    #   geom_sf(data = sf_boldep)+
    #   geom_sf(data = dia,  fill = "black", color = "red", alpha = .2) +
    #   ggtitle(paste0('Dia :', theDate))
    mapa <-  tm_shape(c_osm) +
      tm_rgb()  +
      tm_shape(sf_SC_beni) + 
      tm_polygons(alpha = 0,border.col = "black", lwd = 2)+
      tm_shape(ant) +
      tm_symbols(col = 'orange', shape = 15, scale = 0.35, alpha = 0.8 ) +
      tm_shape(dia) +
      tm_symbols(col = 'red', scale = 0.5, shape = 15 ) +
      tm_layout(main.title = "Fuegos: Julio - Septiembre",
                main.title.position = "center",
                main.title.color = "blue",
                panel.labels = c(paste0('Dia :', theDate)),
                panel.label.color = "purple",
                legend.text.color = "brown")
    theDate <- theDate + 1
    message(paste0('Dia   ',theDate))
    print(mapa)
  }
}

gif_file <-
  gifski::save_gif(
    expr = plot.map.calor(),
    gif_file = "calor_animation.gif",
    delay = 0.75, width = 738, height = 788, res = 100
  )

utils::browseURL(gif_file)