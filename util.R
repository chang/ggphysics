#' General utilities for creating physics-based animations in ggplot2

#' Installs packages if needed
install_required_packages = function() {
  required = c('ggplot2', 'dplyr', 'purrr', 'glue')
  for (pkg in required) {
    if (!(pkg %in% installed.packages())) {
      install.packages(pkg)
    }
  }
}


install_required_packages()
library(ggplot2)
library(dplyr)
library(purrr)


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


#' Theme element
.theme_physics = list(
    xlim(-5, 5),
    ylim(0, 50),
    theme(axis.title = element_blank(),
          legend.position = "none") 
)

theme_physics = function() {
  .theme_physics
}


#' Grabs the directory path from a file path
dir_path = function(path) {
  sep = file.path("", "")  # gets file path separator in an OS agnostic way
  directory = strsplit(path, sep) %>% .[[1]] %>% .[-length(.)] %>% as.list()
  do.call(file.path, directory)
}


#' Save and print message
save_frame = function(plt, path, i) {
  dir_ = dir_path(path)
  if (!dir.exists(dir_)) {
    message(glue::glue("Creating directory at: {dir_}", dir_ = dir_))
    dir.create(dir_)
  }

  fpath = glue::glue(path, i=i)
  ggsave(filename = fpath, plot = plt, width = 1.5, height = 1.5)
  message(fpath)
}
