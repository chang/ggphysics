#' A little animation of a bouncy ball with a color gradient mapped to the height

source("util.R")
library(ggplot2)
library(dplyr)

M = 0.10  # fps multiplier
DECAY = 0.75
DIRECTORY = '~/Downloads/bouncy_ball/bouncy_ball_frame_{i}.png'  # file path for images


#' Returns a random projectile: defined by position, velocity, and acceleration
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
color_anchor = data.frame(y = c(0, 50))  # we need two invisible at min_y and max_y to "anchor" the color scale

for (i in 0:200) {
  plt =
    ball %>% bind_rows(color_anchor) %>% 
    ggplot() +
    geom_point(aes(x, y, color = y), size = 2) +
    xlim(-5, 5) +
    ylim(0, 50) +
    theme(axis.title = element_blank(),
          legend.position = "none") +
    scale_color_gradient(low = "gold", high = "navy")
  
  # write plot
  fpath = glue::glue(DIRECTORY, i=i)
  ggsave(filename = fpath, plot = plt, width = 1.5, height = 1.5)
  message(fpath)
  
  print(ball)
  ball = update_ball(ball)
}
