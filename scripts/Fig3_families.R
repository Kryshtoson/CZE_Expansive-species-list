library(tidyverse)
library(readxl)

traity <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/expanzky_traity_2023-09-27.xlsx)')

traity |>
  select(species, family, origin, expansive) |>
  filter(origin == 'native') |>
  group_by(family, name = expansive) |>
  count(name = 'value') |>
  pivot_wider() |>
  ungroup() |>
  filter(no > 5 | !is.na(expansive)) |>
  mutate(no = replace_na(no, 0)) |>
  pivot_longer(-1) |>
  left_join(traity |>
  filter(origin == 'native') |>
  group_by(name = expansive) |>
  count()) |>
  mutate(prop = round(value/n*100, 2),
         prop_plot = ifelse(name == 'expansive', prop, -prop)) |>
  left_join(traity |>
  filter(origin == 'native') |>
  group_by(family) |>
  count(name = 'total_count')) |>
  ggplot(aes(reorder(family, -total_count), prop_plot)) +
  geom_bar(stat = 'identity', aes(fill = name), show.legend = T) +
  #  geom_text(aes(label = paste0(family, ' (n=', n, ', ', round(prop, 1), '%)'),
  #                hjust = ifelse(expansive != 'expansive', 1.05, -.05)), size = 2.5) +
  geom_text(aes(label = ifelse(name == 'expansive',
                               paste0('(n=', value, ', ', round(prop, 1), '%)'),
                               paste0(family, ' (n=', value, ', ', round(prop, 1), '%)')),
                hjust = ifelse(name != 'expansive', 1.05, -.05)), size = 2.5) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks = c(0, 5, -5, 10, -10, 15, -15, 20, -20, 25, -25),
                     labels = c('0%', rep(c('5%', '10%', '15%', '20%', '25%'), each = 2)),
                     expand = c(.3, .3)) +
  scale_fill_manual(values = c('#FFC300', 'grey88'),
                    labels = c('Expansive', 'Native')) +
  theme_bw() +
  labs(y = 'Proportion of species') +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.background = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_line(size = 0.3, linetype = "solid", colour = "grey20"),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave('figures\\Figure_3.png', width = 8, height = 8)