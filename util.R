#' General utilities for creating physics-based animations in ggplot2

#' Returns a random color as a string
random_color = function() {
  COLORS = c('firebrick', 'skyblue', 'green', 'purple', 'yellow', 'blue', 'red')
  sample(COLORS, 1)
}


#' Checks if a dataframe is valid
is_valid_dataframe = function(obj) {
  return (is.data.frame(obj) && nrow(obj) > 0)
}


#' General update function for projectiles
#' 
#' Assumes that position, velocity, and acceleration fields for x
#' are named x, v_x, and a_x, and similarly for y. TODO: add assertion
update_projectiles = function(projectiles) {
  if (is_valid_dataframe(projectiles)) {
    projectiles %>%
      mutate(x = x + v_x * M,
             v_x = v_x + a_x * M,
             y = y + v_y * M,
             v_y = v_y + a_y * M)
  }
}