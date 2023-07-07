library(gridExtra)
library(tidyverse)
library(sf)

final_list <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/Expanzky_final-list_2023-05-09.xlsx)')
# regiony <- read_sf(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/vyroba_seznamu/Regiony_shp/Regiony_wgs2.shp)')


regions_old <- paste(rep(c('ceske', 'moravske'), each = 3), c('termo', 'mezo', 'oreo'), sep = '_')
regions_new  <- paste(rep(c('Bohemian', 'Carpathian'), each = 3), c('Thermophyticum', 'Mesophyticum', 'Oreophyticum'),
                      sep = '\n')

final_list |>
  select(species, ceske_termo:moravske_oreo) |>
  pivot_longer(-1, values_to = 'expansive', names_to = 'id') |>
  mutate(id = factor(id, levels = regions_old,
                labels = regions_new)) |>
  drop_na() |>
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

ggsave('figures\\Regions.png', width = 8, height = 3)
