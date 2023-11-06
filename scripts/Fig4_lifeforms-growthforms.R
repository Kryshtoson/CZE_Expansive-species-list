library(tidyverse)
library(readxl)

traity <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/expanzky_traity_2023-09-27.xlsx)')

traity |>
  filter(origin == 'native') |>
  select(species, expansive, `Tree`:`Therophyte`) |>
  pivot_longer(-c(species, expansive)) |>
  group_by(expansive, name) |>
  count(value) |>
  filter(value == 1) |>
  group_by(expansive) |>
  mutate(prop = (n / sum(n)) * 100,
         prop_plot = ifelse(expansive == 'expansive', prop, -prop)) |>
  mutate(kind = '(A) Life forms') -> lifeforms

bind_rows(traity,
          traity |>
            filter(expansive == 'expansive') |>
            mutate(expansive = 'no')) |>
  select(species, expansive, `annual`:`perennial-polycarpic`) |>
  pivot_longer(-c(species, expansive)) |>
  group_by(expansive, name) |>
  count(value) |>
  filter(value == 1) |>
  group_by(expansive) |>
  mutate(prop = (n / sum(n)) * 100,
         prop_plot = ifelse(expansive == 'expansive', prop, -prop)) |>
  mutate(kind = '(B) Life history') -> lifehistory

bind_rows(lifeforms, lifehistory) |>
  ggplot(aes(reorder(name, prop_plot, 'min'), prop_plot)) +
  geom_bar(stat = 'identity', aes(fill = expansive)) +
  geom_text(aes(label = ifelse(expansive == 'expansive',
                               paste0('(n=', n, ', ', round(prop, 1), '%)'),
                               paste0(name, ' (n=', n, ', ', round(prop, 1), '%)')),
                hjust = ifelse(expansive != 'expansive', 1.05, -.05)), size = 2.5) +
  coord_flip() +
  facet_wrap(~kind, scales = 'free', ncol = 1) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks = c(0, 50, -50, 100, -100),
                     labels = c('0%', rep(c('50%', '100%'), each = 2)),
                     expand = c(1.3, 1.3)) +
  scale_fill_manual(values = c('#FFC300', 'grey88'),
                    labels = c('Expansive', 'Non expansive')) +
  theme_bw() +
  labs(y = 'Proportion of species') +
  theme(panel.border = element_blank(),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        axis.line.x = element_line(size = 0.3, linetype = "solid", colour = "grey20"),
        strip.text = element_text(size = 13, face = 'bold', hjust = 0.5),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave('figures\\Figure_4.png', width = 5, height = 6)