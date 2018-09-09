# Sun Sep  9 15:09:05 2018 --------- Marius D. Pascariu ---
remove(list = ls())
library(magick)
library(tidyverse)



P = image_read("DeFM_red.png") %>% 
  image_trim() %>% 
  image_transparent('#ec1c24') %>%
  image_write("DeFM.png")

hexSticker::sticker("DeFM.png", filename = "MortalityGaps_logo.png",
                     package = "MortalityGaps", 
                     p_color = 1, p_size = 6, p_x = 1, p_y = 1.3, # package
                     h_fill = 'lightcyan1', h_color = '#dadaeb',  # hexagon
                     s_x = 1, s_y = 0.7, s_width = .72) # subplot

