# The list of expansive plants of the Czech Republic

* I keep archeophytes in.

## Scripts

* `Proportion_within_families.R`: creates `Families.png` 
  * Proportion of expansive species vs proportions of all species within families in Czech flora. 
  * Families included: `filter((expansive == 'expansive' & n > 0) | (expansive == 'no' & n > 5))`

* `Lifeforms_growthforms.R`: creates `Lifeforms.png`
  * Proportion of individual lifeforms of expansive species vs all species in Czech flora.
  * All families included.

* `Strategies.R`: creates `Strategies.png`
  * Boxplots of Pierce's indices
  * All families included.
  * Tested T-test

* `New_maps.R`: creates `Expansive_plants.pdf`
  * For Appendix, I suppose
  * All species included
  * Add legend
  * Remove page number

* `Vegetative_height`: creates `...`
  * Facets per individual growth-forms

* `Fytoregiony.R`: creates `...`
  * Counts per fytoregions
  * Tested with Anova

* `Dotaznik_counts.R`: creates `...`
  * Map of frequency of questionnaires

* `Habitats.R`: creates `...`
  * Number of invasive species per habitat sorted by frequency
