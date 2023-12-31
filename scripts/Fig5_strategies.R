library(readxl)
library(tidyverse)
library(ggpubr)

traity <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/expanzky_traity_2023-09-27.xlsx)')

traity |>
  filter(origin == 'native') |>
  mutate(expansive = factor(expansive, levels = c('no', 'expansive'),
                            labels = c('Non expansive', 'Expansive'))) |>
  select(species, expansive, `C-PIERCE`:`R-PIERCE`) |>
  pivot_longer(`C-PIERCE`:`R-PIERCE`) |>
  mutate(name = gsub('-PIERCE', '', name),
         value = as.numeric(value),
         name_caption = factor(name, levels = c('C', 'S', 'R'),
                               labels = c('Competitors', 'Stress tolerators','Ruderals'))) |>
  ggplot(aes(expansive, value)) +
  geom_violin(aes(fill = expansive), alpha = 1, show.legend = F) +
  #geom_boxplot(aes(fill = expansive), ) +
  geom_boxplot(width = 0.1, alpha = .2, notch = T) +
  scale_fill_manual(values = c('grey88', '#FFC300')) +
  stat_compare_means(hjust = 0, size = 3) +
  facet_grid(~name_caption, scales = 'free') +
  expand_limits(y = 110) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 13, face = 'bold'),
        axis.title = element_blank())

ggsave('figures/Figure_5.png', height = 4, width = 7)