library(tidyverse)
library(readxl)
library(googlesheets4)
library(googledrive)

googledrive::drive_deauth()
cout <- drive_download("https://docs.google.com/spreadsheets/d/1GRepQ4oohCt-UF9LiGSVcCKJ8tXebeDf/edit#gid=1150720028",
              type = 'csv', overwrite = T)

habitat <- read_xlsx(cout$local_path)[,1:28]

habitat |> pivot_longer(-1) |>
  filter(value == 1) |>
  group_by(name) |>
  count() |>
  arrange(-n) |>
  ggplot(aes(fct_reorder(name, n), n)) +
  coord_flip() +
  geom_bar(stat = 'identity', fill = '#FFC300') +
  geom_text(aes(label = n, y = n + 3), size = 3) +
  theme_bw() +
  scale_y_continuous(expand = c(0,0,.1,0)) +
  labs(y = 'Number of expansive species') +
  theme(axis.title.y = element_blank())

ggsave('figures\\Habitats.png', width = 8, height = 8)