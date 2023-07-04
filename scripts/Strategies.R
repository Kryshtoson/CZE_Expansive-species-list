library(readxl)
library(tidyverse)
library(ggpubr)

traity <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/Expanzky_traity_2023-07-03.xlsx)')

traity |>
  mutate(`S-PIERCE` = as.numeric(`S-PIERCE`)) |>
  filter(`S-PIERCE` > .5 & expansive == 'expansive') |>
  select(species, `S-PIERCE`) |>
  arrange(`S-PIERCE`) |>
  view()

bind_rows(traity,
          traity |>
            filter(expansive == 'expansive') |>
            mutate(expansive = 'no')) |>
  mutate(expansive = factor(expansive, levels = c('no', 'expansive'),
                            labels = c('Czech flora', 'Expansive species'))) |>
  select(species, expansive, `C-PIERCE`:`R-PIERCE`) |>
  pivot_longer(`C-PIERCE`:`R-PIERCE`) |>
  mutate(name = gsub('-PIERCE', '', name),
         value = as.numeric(value)) |>
  ggplot(aes(expansive, value)) +
  geom_violin(aes(fill = expansive), alpha = 1, show.legend = F) +
  #geom_boxplot(aes(fill = expansive), ) +
  geom_boxplot(width = 0.1, alpha = .2, notch = T) +
  scale_fill_manual(values = c('grey88', '#FFC300')) +
  stat_compare_means(method = 't.test', hjust = 0, size = 3) +
  facet_grid(~name, scales = 'free') +
  expand_limits(y = 110) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 13, face = 'bold'),
        axis.title = element_blank())

ggsave('figures/Strategies.png', height = 4, width = 7)