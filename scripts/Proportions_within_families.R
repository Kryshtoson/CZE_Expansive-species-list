library(tidyverse)
library(readxl)

traity <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/Expanzky_traity_2023-07-03.xlsx)')

traity |>
  group_by(family, expansive) |>
  count(name = 'n') |>
  filter((expansive == 'expansive' & n > 0) | (expansive == 'no' & n > 5)) |>
  pull(family) |>
  unique() -> selected

bind_rows(traity,
          traity |>
            filter(expansive == 'expansive') |>
            mutate(expansive = 'no')) |>
  group_by(family, expansive) |>
  count(name = 'n') |>
  group_by(expansive) |>
  mutate(prop = (n / sum(n)) * 100,
         prop_plot = ifelse(expansive == 'expansive', prop, -prop)) |>
  filter(family %in% selected) |>
  ggplot(aes(reorder(family, prop_plot, 'min'), prop_plot)) +
  geom_bar(stat = 'identity', aes(fill = expansive), show.legend = T) +
  #  geom_text(aes(label = paste0(family, ' (n=', n, ', ', round(prop, 1), '%)'),
  #                hjust = ifelse(expansive != 'expansive', 1.05, -.05)), size = 2.5) +
  geom_text(aes(label = ifelse(expansive == 'expansive',
                               paste0('(n=', n, ', ', round(prop, 1), '%)'),
                               paste0(family, ' (n=', n, ', ', round(prop, 1), '%)')),
                hjust = ifelse(expansive != 'expansive', 1.05, -.05)), size = 2.5) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks = c(0, 5, -5, 10, -10, 15, -15, 20, -20, 25, -25),
                     labels = c('0%', rep(c('5%', '10%', '15%', '20%', '25%'), each = 2)),
                     expand = c(.3, .3)) +
  scale_fill_manual(values = c('#FFC300', 'grey88'),
                    labels = c('Expansive species', 'Czech flora')) +
  theme_bw() +
  labs(y = 'Proportion of species') +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave('figures\\Families.png', width = 8, height = 8)