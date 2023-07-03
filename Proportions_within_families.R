library(tidyverse)
library(readxl)

final_list <- read_xlsx(r'(C:\Users\krystof\OneDrive - MUNI\2022_Expanzky\Expanzky_final-list_2023-05-09.xlsx)')
traity <- read_xlsx(r'(C:\Users\krystof\OneDrive - MUNI\2022_Expanzky\Expanzky_traity_2023-05-09.xlsx)')

traity |>
  group_by(family, expansive) |>
  count(name = 'n') |>
  filter((expansive == 'expansive' & n > 0) | (expansive == 'no' & n > 5)) |>
  pull(family) |>
  unique() -> selected

bind_rows(traity,
          traity |> filter(expansive == 'expansive') |>
  mutate(expansive = 'no')) |>
  filter(family %in% selected) |>
  group_by(family, expansive) |>
  count(name = 'n') |>
  group_by(expansive) |>
  mutate(prop = (n / sum(n))* 100,
         prop_plot = ifelse(expansive == 'expansive', prop, -prop)) |>
  ggplot(aes(reorder(family, prop_plot, 'min'), prop_plot)) +
  geom_bar(stat = 'identity', aes(fill = expansive), show.legend = F) +
  geom_text(aes(label = paste0(family, ' (n=',n,', ', round(prop, 1), '%)'),
                hjust = ifelse(expansive != 'expansive', 1.05, -.05)), size = 2.5) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks = c(0, 5, -5, 10, -10, 15, -15, 20, -20, 25, -25),
                     labels = c('0%', rep(c('5%', '10%', '15%', '20%', '25%'), each = 2)),
  expand = c(.3, .3)) +
  theme_bw() +
  labs(y = 'Proportion of species') +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave('figures\\Families.png', width = 8, height = 8)