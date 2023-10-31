library(ggpubr)
library(tidyverse)
library(readxl)

traity <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/expanzky_traity_2023-09-27.xlsx)')
order <- c('Tree', 'Shrub', 'Chamaephyte', 'Hemicryptophyte', 'Geophyte', 'Hydrophyte', 'Therophyte')

traity |>
  filter(origin == 'native') |>
  select(species, expansive, `Tree`:`Therophyte`, height = Height) |>
  pivot_longer(-c(species, expansive, height)) |>
  mutate(height = as.numeric(height)) |>
  mutate(expansive = factor(expansive, levels = c('expansive', 'no'),
                            labels = c('Expansive', 'Native'))) |>
  filter(value == 1) |>
  mutate(name = factor(name, levels = order))-> step

step |> group_by(name, expansive) |> count()

step |> ggplot(aes(expansive, height)) +
  geom_violin(aes(fill = expansive), alpha = 1, show.legend = F) +
  geom_boxplot(data = step |>
    filter(!(expansive == 'Expansive' & name %in% c('Hydrophyte', 'Chamaephyte'))),
               width = 0.1, alpha = .2, notch = T) +
  geom_jitter(data = step |>
    filter(expansive == 'Expansive'  & name %in% c('Hydrophyte', 'Chamaephyte')),
               width = 0, height = 0) +
  scale_fill_manual(values = c('#FFC300', 'grey88')) +
  stat_compare_means(hjust = 1, size = 3,
                     label.y.npc = 1, label.x = 2.3) +
  facet_wrap(~name, scales = 'free', ncol = 2) +
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 13, face = 'bold'),
        axis.title = element_blank())

ggsave('figures\\Figure_6.png', width = 8, height = 10)

