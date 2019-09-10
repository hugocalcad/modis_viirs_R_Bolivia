## VIIRS https://drive.google.com/file/d/1uZpKO2wFBwjvLvpGFN4HNGHtU5vODn3B/view?usp=sharing viirs
## MODIS https://drive.google.com/file/d/1WgVwJ5aP803sCL4ROSqsQC_tCg3O6CnM/view?usp=sharing modis

library(sf)
library(tidyverse)

load('viirs_all_09092019.Rda')
load('modis_all_09092019.Rda')

##ver la estructura
str(modis_final)
str(viirs_final)

#conveitiendo la columnas de las fechas
modis_final$acq_date <- as.Date(modis_final$acq_date, format = '%Y-%m-%d')
viirs_final$acq_date <- as.Date(viirs_final$acq_date, format = '%Y-%m-%d')
