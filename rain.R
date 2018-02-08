#' Some falling raindrops

source("util.R")
library(ggplot2)
library(dplyr)

PATH = "img/animation/rain_frame_{i}.png"

M = 0.09


raindrop = function() {
  x = runif(min = -5, max = 5, n = 1)
  v_x = 0
  a_x = 0
  y = runif(min = 50, max = 70, n = 1)
  v_y = runif(min = 0, max = 1, n = 1)
  a_y = -8

  data.frame(x, v_x, a_x, y, v_y, a_y)
}


create_raindrops = function(n) {
  new_rain = data.frame()
  for (i in 1:n) {
    new_rain = bind_rows(new_rain, raindrop())
  }
  new_rain
}


rain = create_raindrops(20)

for (i in 0:300) {
  plt =
    rain %>%
    ggplot() +
    geom_point(aes(x, y), size = 0.2, color = "skyblue", alpha = 0.7) +
    theme_physics()

  save_frame(plt, path = PATH, i = i)

  rain = rain %>%
    update_projectiles() %>%
    filter(y > 0) %>%
    bind_rows(create_raindrops(round(runif(min = 5, max = 10, n = 1))))
}
