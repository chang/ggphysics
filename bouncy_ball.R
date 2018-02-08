#' A little animation of a bouncy ball with a color gradient mapped to the height

source("util.R")
library(ggplot2)
library(dplyr)

PATH = "img/animation/bouncy_ball_frame_{i}.png"  # path to save the images

M = 0.10  # fps multiplier
DECAY = 0.75


ball = function() {
  x = -5
  v_x = 0.5
  a_x = 0
  y = 50
  v_y = 0
  a_y = -8

  data.frame(x, v_x, a_x, y, v_y, a_y)
}

#' Creates next frame for projectiles
update_ball = function(ball) {
  if (is_valid_dataframe(ball)) {
    new = update_projectiles(ball)
    if (new$y < 0) {
      new = mutate(new, v_y = DECAY *  -1 * v_y)
    }
    new
  }
}

# Main render loop
ball = ball()
color_anchor = data.frame(y = c(0, 50))  # we need two invisible points at min_y and max_y to "anchor" the color scale

for (i in 0:200) {
  plt =
    ball %>% bind_rows(color_anchor) %>%
    ggplot() +
    geom_point(aes(x, y, color = y), size = 2) +
    scale_color_gradient(low = "gold", high = "navy") +
    theme_physics()

  # write plot
  save_frame(plt, path = PATH, i = i)

  print(ball)
  ball = update_ball(ball)
}
