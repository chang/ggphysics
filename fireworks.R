#' Fireworks using ggplot2

source("util.R")
library(ggplot2)
library(dplyr)

M = 0.10  # fps multiplier
TRAIL_DECAY = 6  # rate of decay for firework trails
SIZE_BASE = 8  # base size for projectiles
DIRECTORY = '~/Downloads/firework/firework_{i}.png'  # file path for images
NEW_PROJECTILE_FRAMES = c(10, 25, 30, 40)  # frames on which to introduce new firework
SIZE_RANGE = c(0, 1)  # ggplot2 size scale range


#' Returns a random projectile: defined by position, velocity, and acceleration
random_projectile = function() {
  x = runif(min = -4, max = 4, n = 1)
  v_x = rnorm(mean = 0, sd = 0.2, n = 1)
  a_x = 0
  y = 0
  v_y = rnorm(mean = 22, sd = 3, n = 1)
  a_y = rnorm(mean = -8, sd = 1, n = 1)
  size = SIZE_BASE
  color = factor(random_color(), levels = COLORS)

  projectiles = data.frame(x, v_x, a_x, y, v_y, a_y, size, color)

  projectiles
}


#' Creates new trails
new_trails = function(projectiles) {
  if (is_valid_dataframe(projectiles)) {
    projectiles %>%
      mutate(x = x,  # + rnorm(mean = 0, sd = 0.05, n = 1),
             v_x = 0,
             a_x = 0,
             v_y = 0,
             a_y = 0,
             size = SIZE_BASE)
  }
}


#' Creates single explosion particle
explode_particle = function(projectile) {
  projectile %>%
    mutate(v_x = rnorm(mean = 0, sd = 0.4, n = 1),
           v_y = rnorm(mean = 9, sd = 2, n = 1),
           size = rnorm(mean = 0.15, sd = 0.05, n = 1))  # particle size
}


#' Explode a projectile
explode = function(projectile, n = 20) {
  particles = data.frame()
  for (i in 1:n) {
    particles = bind_rows(particles, explode_particle(projectile))
  }
  particles
}


#' Creates next frame for particles
update_particles = function(particles) {
  if (is_valid_dataframe(particles)) {
    update_projectiles(particles) %>% 
      mutate(size = size - 0.5)
  }
}
  


#' Creates next frame for trails
update_trails = function(trails) {
  if (is_valid_dataframe(trails)) {
    trails %>%
      mutate(size = size - TRAIL_DECAY * M) %>%
      filter(size > 0)
  }
}


# Main render loop
projectiles = bind_rows(random_projectile(), random_projectile())
trails = data.frame()
particles = data.frame()

for (i in 0:150) {
  plt =
    bind_rows(projectiles, trails, particles) %>%
      ggplot() +
        geom_point(aes(x, y, size = size, alpha = size)) +
        scale_size(range = SIZE_RANGE) +
        xlim(-5, 5) +
        ylim(0, 50) +
        theme(axis.title = element_blank(),
              legend.position = "none")

  # write plot
  fpath = glue::glue(DIRECTORY, i=i)
  ggsave(filename = fpath, plot = plt, width = 1.5, height = 1.5)
  message(fpath)

  # update projectiles
  if (i %in% NEW_PROJECTILE_FRAMES) {
    projectiles = projectiles %>% rbind(random_projectile())
  }

  projectiles = update_projectiles(projectiles)

  # check for exploding fireworks
  if (any(projectiles$v_y < 0)) {
    exploded = filter(projectiles, v_y < 0)
    for (i in 1:nrow(exploded)) {
      particles = bind_rows(particles, explode(exploded[i,]))
    }
    projectiles = filter(projectiles, v_y > 0)
  }
  message(nrow(particles))
  particles = update_particles(particles)

  trails = update_trails(trails)
  trails = bind_rows(trails, new_trails(projectiles))
}
