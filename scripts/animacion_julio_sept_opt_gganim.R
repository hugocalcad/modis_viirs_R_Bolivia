library(gganimate)
viirs_julio_sep <- viirs_final %>%
  filter(acq_date > '2019-06-30') %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant")

start <- as.Date("2019-09-01",format="%Y-%m-%d")
end   <- as.Date("2019-09-09",format="%Y-%m-%d")

p<- ggplot() + 
  geom_sf(data = sf_boldep) +
  geom_sf(data = viirs_julio_sep,  fill = "black", color = "red", alpha = .2) 

q <- p + transition_time(acq_date) +
  labs(title = "Fecha: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)
  

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
