library(gridExtra)
library(tidyverse)
library(sf)

final_list <- read_xlsx(r'(C:/Users/chytryk/OneDrive - MUNI/2022_Expanzky/expanzni-druhy-cz-regionalita-2023-09-27.xlsx)')
# regiony <- read_sf(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/vyroba_seznamu/Regiony_shp/Regiony_wgs2.shp)')

final_list |> names()
final_list |>
  select(species, `Bohemian Thermophyticum`:`Carpathian Oreophyticum`) |>
  pivot_longer(-1, values_to = 'expansive', names_to = 'id') |>
  filter(expansive == 1) |>
  group_by(id) |>
  count() |>
  ggplot(aes(fct_reorder(id, n), n)) +
  coord_flip() +
  geom_bar(stat = 'identity', fill = '#FFC300') +
  geom_text(aes(label = n, y = n + 3), size = 3) +
  theme_bw() +
  scale_y_continuous(expand = c(0,0,.1,0)) +
  labs(y = 'Number of expansive species') +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

ggsave('figures\\Figure_2.png', width = 8, height = 3)
