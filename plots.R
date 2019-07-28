source("spirals.R")
library(tidyverse)
library(primes)

#### co-ordinate evolution ####

end_length <- 1000
coords <- xy_coords(end_length)
loop_ends <- total_length(seq(1:which_loop(end_length)))

# plot of x coords with path length
tibble(length = seq_along(coords$x)-1,
       x_coord = coords$x,
       y_coord = coords$y) %>%
  ggplot(aes(x = length)) +
  geom_vline(xintercept = loop_ends, color="grey85") +
  geom_point(aes(y = x_coord)) +
  geom_line(aes(y = x_coord)) +
  geom_point(aes(y = y_coord), color="blue") +
  geom_line(aes(y = y_coord), color="blue") +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  ylab("coordinate") + xlim(0, end_length)

ggsave("pics/coord_evol.png", dpi=300, width = 6, height = 4, units = "in")


#### Ulam spirals ####
end_length <- 1000000
coords <- xy_coords(end_length)
plot_data <- tibble(length = seq_along(coords$x)-1, 
                    x = coords$x,
                    y = coords$y,
                    prime_or_not = is_prime(length))

range_scale <- max(abs(c(coords$x, coords$y)))

plot_data %>%
  ggplot(aes(x = x, y = y)) +
  # geom_path(color = "grey90") +
  geom_raster(aes(fill = prime_or_not)) +
  scale_fill_manual(values = c(rgb(1, 1, 1, alpha = 0), rgb(0, 0, 0, alpha = 1))) +
  theme_bw() + 
  theme(panel.grid = element_blank(), legend.position = "none") 

ggsave("pics/ulam1000000.png", width=8, height=8, units="in")


#### colourful and composites ####

end_length <- 99999
coords <- xy_coords(end_length)
plot_data <- tibble(length = seq_along(coords$x)-1, 
                    x = coords$x,
                    y = coords$y,
                    factors = sapply(length, num_factors))

plot_data %>% 
  # head(10000) %>%
  ggplot(aes(x = x, y = y)) +
  geom_raster(aes(fill = factors)) +
  scale_fill_viridis_c(direction = 1, option = "magma") +
  theme_bw() + 
  theme(panel.grid = element_blank(), legend.position = "bottom")

ggsave("pics/magma_composites.png", width=8, height=9, units="in")
