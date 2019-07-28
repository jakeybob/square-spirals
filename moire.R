source("spirals.R")
library(tidyverse)
library(primes)
library(foreach)
library(doParallel)
registerDoParallel(parallel::detectCores())

end_length <- 200000
fps <- 60
length <- 30
pic_dir <- "pics"
output_video <- "output.mp4"

coords <- xy_coords(end_length)

plot_data <- tibble(length = seq_along(coords$x)-1, 
                    x = coords$x,
                    y = coords$y,
                    prime_or_not = is_prime(length))
range_scale <- max(abs(c(coords$x, coords$y)))

thetas <- seq(from=0, to=2*pi, length.out = length*fps) # rot
# thetas <- seq(from=-range_scale, to=range_scale, length.out = length*fps) # vertical / horiz

rotated <- plot_data %>%
  filter(prime_or_not == TRUE) %>%
  mutate(x1 = x - .5,
         y1 = y + .5,
         x2 = x + .5,
         y2 = y + .5,
         x3 = x - .5,
         y3 = y - .5,
         x4 = x + .5,
         y4 = y - .5) %>%
  select(length, x1, x2, x3, x4, y1, y2, y3, y4)

a <- rotated %>% select(length, x1) %>% rename(x = x1) %>% 
  bind_rows(rotated %>% select(length, x2) %>% rename(x = x2)) %>%
  bind_rows(rotated %>% select(length, x4) %>% rename(x = x4)) %>%
  bind_rows(rotated %>% select(length, x3) %>% rename(x = x3))

b <- rotated %>% select(length, y1) %>% rename(y = y1) %>% 
  bind_rows(rotated %>% select(length, y2) %>% rename(y = y2)) %>%
  bind_rows(rotated %>% select(length, y4) %>% rename(y = y4)) %>%
  bind_rows(rotated %>% select(length, y3) %>% rename(y = y3))


foreach (i = 1:length(thetas)) %dopar% {
  filename <- file.path(pic_dir, paste0("pic_", i, ".png"))
  
  flat <- a %>% bind_cols(b) %>% select(-length1) %>%
    mutate(x2 = (x*cos(thetas[i]) + y*sin(thetas[i])),
           y2 = (-x*sin(thetas[i]) + y*cos(thetas[i])))
    # mutate(x2 = x+thetas[i],
    #        y2 = y)
  # mutate(x2 = x+thetas[i],
  #        y2 = y+thetas[i])
    # mutate(x2 = x,
    #        y2 = y+thetas[i])
  
  p <- flat %>%
    ggplot() +
    geom_polygon(aes(x = x, y = y, group=length ), fill = rgb(1,1,1, alpha=.5)) +
    geom_polygon(aes(x = x2, y = y2, group=length ), fill = rgb(1,1,1, alpha=.5)) +  
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          legend.position = "none", 
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill="black"),
          plot.margin = margin(0, 0, -8, -8, unit = "pt")) +
    xlim(-range_scale, range_scale) + ylim(-range_scale, range_scale)
  
  ggsave(plot = p, filename = filename, width = 10.8, height = 10.8, dpi = 100)
}

#### FFMPEG ####
command <- paste0("ffmpeg -y -r ", fps, " -f image2 -s 1080x1080 -i ", pic_dir,
                  "/pic_%d.png -vcodec libx264 -crf 18 -pix_fmt yuv420p ", output_video)
system(command = command)
