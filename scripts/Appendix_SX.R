library(gridExtra)
library(tidyverse)
library(sf)

final_list <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/expanzni-druhy-cz-regionalita-2023-09-27.xlsx)')
regiony <- read_sf(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/vyroba_seznamu/Regiony_shp/Regiony_wgs2.shp)')

long_list <- final_list |>
  select(species, `1`:`17`) |>
  pivot_longer(-1, values_to = 'expansive', names_to = 'id') |>
  mutate(expansive = ifelse(expansive == 1, TRUE, FALSE),
         id = as.numeric(id))

loop_over <- sort(unique((long_list$species)))

ls <- list()

for (i in loop_over) {

  ls[[i]] <- regiony |>
    left_join(long_list[long_list$species == i, c('id', 'expansive')]) |>
    mutate(expansive = factor(expansive, c(F, T))) |>
    ggplot() +
    geom_sf(aes(fill = expansive)) +
    #geom_sf_text(aes(label = paste0('<', id, '>\n', pomer))) +
    scale_fill_manual(breaks = c(TRUE, FALSE),
                      labels = c('Expanding', 'Not expanding'),
                      values = c('#FFC300', 'grey88'), drop = F) +
    theme_void() +
    labs(title = i,
         x = '', y = '') +
    theme(plot.title = element_text(face = 'italic'),
          panel.background = element_blank(),
          axis.title = element_text(),
    legend.position = c(1,1),
          legend.justification = c(1,1),
    legend.title = element_blank())
}

m <- marrangeGrob(ls, ncol = 1, nrow = 1)
ggsave('figures\\Appendix_Sx_maps.pdf', m, width = 10, height = 6, dpi = 150)
