## Animacion rscript

## VIIRS https://drive.google.com/file/d/1uZpKO2wFBwjvLvpGFN4HNGHtU5vODn3B/view?usp=sharing viirs
## MODIS https://drive.google.com/file/d/1WgVwJ5aP803sCL4ROSqsQC_tCg3O6CnM/view?usp=sharing modis

library(sf)
library(tidyverse)
library(here)
library(ggplot2)
library(lubridate)
library(tmap)
library(grid)
library(rnaturalearth)

# load .rdas
load('data/viirs_09092019_19092019.Rda')
viirs_1 <- viirs_final
load('data/viirs_all_09092019.Rda')
viirs_final <- viirs_final %>% bind_rows(viirs_1) %>% distinct()

# convirtiendo la columnas de las fechas
viirs_final$acq_date <- as.Date(viirs_final$acq_date, format = '%Y-%m-%d')

#Abrimos la capa y hacemos subset
sf_boldep <- st_read('shp/Bolivia_Departaentos.shp')
#Subset para cubrir el area de Santa Cruz y Beni
sf_SC_beni <- subset(sf_boldep,sf_boldep$NOM_DEP %in% c('SANTA CRUZ'))

#El area que cubrira los mapas reesultantes y para los cortes
bbox_sc_be <- st_bbox(sf_SC_beni)
bbox_sc_be[1] <- bbox_sc_be[1] - 0.5
bbox_sc_be[2] <- bbox_sc_be[2] - 0.5
bbox_sc_be[3] <- bbox_sc_be[3] + 0.5
bbox_sc_be[4] <- bbox_sc_be[4] + 0.5


#viirs select 1 de julio al 9 de septiembre de 2019
viirs_julio_sep <- viirs_final %>%
  filter(acq_date > '2019-06-30') %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant") %>%
  st_crop(bbox_sc_be)

#viirs select 1 de julio al 9 de septiembre de 2018
viirs_julio_sep_2018 <- viirs_final %>%
  filter(between(acq_date,as.Date('2018-07-01'),as.Date('2018-09-19'))) %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant") %>%
  st_crop(bbox_sc_be)

tmap_mode('plot')

c_osm <- tmaptools::read_osm(bbox_sc_be, ext = 1)

sf_south_america <- ne_countries(continent = 'South America', returnclass = 'sf')

mapa_south_ame <- tm_shape(sf_south_america) +
  tm_polygons(alpha = 0, border.col = "grey10") +
  tm_shape(sf_SC_beni) +
  tm_fill(col = 'grey')

### \____ Function for GIF ----
plot.map.calor <- function(){
  ##Establecemos las Fechas de inicio y de final
  start <- as.Date("2019-07-01",format="%Y-%m-%d")
  end   <- as.Date("2019-09-19",format="%Y-%m-%d")
  
  start2018 <- as.Date("2018-07-01",format="%Y-%m-%d")
  end2018   <- as.Date("2018-09-19",format="%Y-%m-%d")
  
  dates_list_2019 <- ymd(start-1) + days(seq(as.numeric(end-start)+1))
  dates_list_2018 <- ymd(start2018-1) + days(seq(as.numeric(end2018-start2018)+1))
  
  for(i in seq(length(dates_list_2019)))
  {
      dia <- viirs_julio_sep %>%
        filter(acq_date %in% c(dates_list_2019[i] - 1, dates_list_2019[i]))
      ant <- viirs_julio_sep %>%
        filter(acq_date < dates_list_2019[i])
      
      dia2018 <- viirs_julio_sep_2018 %>%
        filter(acq_date %in% c(dates_list_2018[i] - 1, dates_list_2018[i]))
      ant2018 <- viirs_julio_sep_2018 %>%
        filter(acq_date < dates_list_2018[i])
      
      if(i==1){
        ant <-dia
        ant2018 <- dia2018
      }

    mapa2019 <-  tm_shape(c_osm) +
      tm_rgb()  +
      tm_shape(sf_SC_beni) + 
      tm_polygons(alpha = 0,border.col = "grey", lwd = 2) +
      tm_shape(ant) +
      tm_symbols(col = 'red', shape = 15, scale = 0.3, alpha = 0.8) +
      tm_shape(dia) +
      tm_symbols(col = 'purple', scale = 0.4, shape = 15 ) +
      tm_layout(main.title = "VIIRS FIRE DETECTION",
                main.title.position = "center",
                main.title.color = "black",
                main.title.size = 2,
                panel.labels = c(paste0('Day: ', dates_list_2019[i])),
                panel.label.color = "blue",
                panel.label.size = 1.5,
                legend.text.color = "brown")
  
    mapa2018 <-  tm_shape(c_osm) +
      tm_rgb()  +
      tm_shape(sf_SC_beni) + 
      tm_polygons(alpha = 0,border.col = "grey", lwd = 2)+
      tm_shape(ant2018) +
      tm_symbols(col = 'red', shape = 15, scale = 0.3, alpha = 0.8) +
      tm_shape(dia2018) +
      tm_symbols(col = 'purple', scale = 0.4, shape = 15 ) +
      tm_layout(main.title = "VIIRS FIRE DETECTION",
                main.title.position = "center",
                main.title.color = "black",
                main.title.size = 2,
                panel.labels = c(paste0('Day: ', dates_list_2018[i])),
                panel.label.color = "blue",
                panel.label.size = 1.5,
                legend.text.color = "brown")
    
    viewport_south_america <- viewport(x = .92, y = .73, width = .18, height = .28)
    viewport_south_america2 <- viewport(x = .42, y = .73, width = .18, height = .28)
    
    print(tmap_arrange(mapa2018, mapa2019,ncol=2))
    print(mapa_south_ame, vp = viewport_south_america) 
    print(mapa_south_ame, vp = viewport_south_america2) 
    
    message(paste0('Dia   ',dates_list_2019[i]))
  }
  
}

gif_file <-
  gifski::save_gif(
    expr = plot.map.calor(),
    gif_file = "calor_animation5.gif",
    delay = 0.75, width = 2100, height = 1100, res = 120
  )

utils::browseURL(gif_file)