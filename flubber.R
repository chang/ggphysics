source("util.R")
library(ggplot2)
library(dplyr)
library(purrrlyr)

M = 0.11
DECAY = 1.2
MAX_SPEED = 55
N_BALLS = 10
DIRECTORY = '~/Downloads/flubber/flubber_{i}.png'


#' Returns a random ball
random_ball = function() {
  x = runif(min = -5, max = 5, n = 1)
  v_x = rnorm(mean = 0, sd = 2.5, n = 1)
  a_x = 0
  y = runif(min = 10, max = 20, n = 1)
  v_y = rnorm(mean = 0, sd = 2, n = 1)
  a_y = -8
  color = random_color()
  
  data.frame(x, v_x, a_x, y, v_y, a_y, color)
}


#' Creates next frame for projectiles
update_balls_df = function(balls) {
  if (is_valid_dataframe(balls)) {
    purrrlyr::by_row(balls, update_ball_row, .collate = "rows", .labels = F) %>% 
      select(-.row)
  }
}


#' Updates a single row of the dataframe
#' 
#' TODO: generalize this function to
update_ball_row = function(row) {
  new = update_projectiles(row)
  
  # bounce off of y walls
  if (new$y < 0 || new$y > 50) {
    new$y = round(new$y)  # set to 0 to avoid clipping
    new$v_y = DECAY *  -1 * new$v_y
  }
  
  # max speed
  if (new$v_y > MAX_SPEED) {
    new$v_y = MAX_SPEED
  } else if (new$v_y < -MAX_SPEED) {
    new$v_y = -MAX_SPEED
  }
  
  # bounce off of x walls
  if (new$x <= -5 || new$x >= 5) {
    new$x = round(new$x)
    new$v_x = -1 * new$v_x
  }
  
  new
}


balls = data.frame()
for (i in 1:N_BALLS) {
  new_ball = random_ball()
  balls = bind_rows(balls, new_ball)
}


for (i in 0:300) {
  plt =
    balls %>% bind_rows(color_anchor) %>% 
    ggplot() +
    geom_point(aes(x, y, color = color), size = 2) +
    theme_physics()
  
  # write plot
  fpath = glue::glue(DIRECTORY, i=i)
  ggsave(filename = fpath, plot = plt, width = 1.5, height = 1.5)
  message(fpath)
  
  print(balls)
  balls = update_balls_df(balls)
}
