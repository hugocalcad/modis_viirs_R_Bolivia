## Animacion rscript

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

sf_boldep <- st_read('shp/Bolivia_Departaentos.shp')

#viirs select 1 de julio al 9 de septiembre
viirs_julio_sep <- viirs_final %>%
  filter(acq_date > '2019-06-30') %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant")

start <- as.Date("2019-07-01",format="%Y-%m-%d")
end   <- as.Date("2019-09-09",format="%Y-%m-%d")

### \____ Function for GIF ----
plot.map.calor <- function(){
  theDate <- start
  while(theDate <= end) {
    dia <- viirs_julio_sep %>%
      filter(acq_date == theDate)
    mapa <- ggplot() +
      geom_sf(data = sf_boldep)+
      geom_sf(data = dia,  fill = "black", color = "red", alpha = .2) +
      ggtitle(theDate)
    theDate <- theDate + 1
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
