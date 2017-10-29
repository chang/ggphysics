library(ggplot2)
library(dplyr)

setwd("~/Downloads/")
DIRNAME = "sqrt_plot"
XLIM = 200

# make
x <- seq(1, XLIM, by = 2)
y <- sqrt(x)
dy <- 1 / (2 * sqrt(x))
dat <- data.frame(x, y, dy)


plot_point <- function(i) {
  # plots the sqrt function with point (i, sqrt(i)) and its tangent line highlighted
  intercept <- dat$y[i] - (dat$x[i] * dat$dy[i])
  
  dat %>% 
    ggplot(aes(x, y)) +
      geom_point(size = 1) +
      geom_abline(aes(slope = dat$dy[i], intercept = intercept), color = "skyblue") +
      geom_point(data = slice(dat, i), color = "red", size = 3) +
      ylab("sqrt(x)")
}

save_all_plots <- function(path) {
  dir.create(DIRNAME)
  
  for (i in 1:XLIM) {
    plt <- plot_point(i)
    file <- paste("plot_", i, ".png", sep = "")
    ggsave(filename = file.path(DIRNAME, file),
           plot = plt,
           device = "png")

    if (i %% 10 == 0) {
      message(glue::glue("{i} / {n} plots saved.", i = i, n = XLIM))
    }
  }
}

save_all_plots(DIRNAME)
