# library(dplyr)
library(ggplot2)

geom_hotdog <- function(length = 10) {
  
  x = length / 2
  
  bun <- function(x, y, color = "#CF8F41") {
    list(
      geom_segment(aes(x = -x + 1.5, xend = x - 1.5,
                       y = y, yend = y),
                   size = 60, lineend = "round", color = color)
    )
    }
  
  bun_back <- bun(x, 0.9)
  bun_back_inside <- bun(0.97 * x, 0.6, color = "#FFE6CC")
  bun_front <- bun(x, -2)
  bun_front_shadow <- bun(x, -2.3, color = "#A27139")
    
  hotdog <- list(
    geom_segment(aes(x = -x, xend = x, y = 0, yend = 0), size = 50, lineend = "round", color = "brown"),
    geom_function(fun = function(x) 0.7 * sin(2 * x),
                  xlim = c(-1.1 * x, 1.1 * x),
                  position = position_nudge(y = 1.2),
                  color = "#ffdb58", size = 8, lineend = "round")
  )
  
  limits <- list(
    xlim(-10, 10),
    ylim(-6, 6)
  )
  
  return(c(bun_back, bun_back_inside, hotdog, bun_front_shadow, bun_front, limits))
}

ggplot() +
  geom_hotdog() +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#7BE8E9", color = NA))
