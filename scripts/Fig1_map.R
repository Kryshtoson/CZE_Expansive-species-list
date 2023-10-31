library(tidyverse)
library(sf)

regiony <- read_sf(r'(C:/Users/chytryk/OneDrive - MUNI/2022_Expanzky/vyroba_seznamu/Regiony_shp/Regiony_wgs2.shp)')

regiony |>
  left_join(
    tibble(name = list.files(r'(C:/Users/chytryk/OneDrive - MUNI/2022_Expanzky/vyroba_seznamu/Dotazniky)',
                             pattern = '.xlsx', full.names = F)) |>
      mutate(name = gsub('\\.xlsx', '', name)) |>
      separate(name, c(NA, 'author', 'region'), sep = '-') |>
      group_by(id = region) |>
      mutate(id = as.numeric(id)) |>
      count(name = 'n')) |>
  ggplot() +
  geom_sf(aes(fill = n), show.legend = F) +
  geom_sf_text(aes(label = paste('n =', n))) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA,
                      name = '# Questionnaires') +
  theme_void() +
  theme(plot.title = element_text(face = 'italic'),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.95, 1),
        legend.justification = c(1, 1))

ggsave('figures\\Figure_1.png', width = 10, height = 6)
