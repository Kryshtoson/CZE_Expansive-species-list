read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/Expanzky_final-list_2023-05-09.xlsx)')
Sys.setlocale(locale = 'Czech')

tibble(path = list.files(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/vyroba_seznamu/Dotazniky)',
                         pattern = '.xlsx', full.names = T),
       name = list.files(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/vyroba_seznamu/Dotazniky)',
                         pattern = '.xlsx', full.names = F)) |>
  #select(name) |>
  mutate(name = gsub('\\.xlsx', '', name)) |>
  separate(name, c(NA, 'author', 'region'), sep = '-') |>
  mutate(data = path |> map(~read_xlsx(.x, 2))) -> stuff

stuff |>
  unnest() |>
  rename(species = `...2`) |>
  filter(str_count(species, "\\w+") > 1) |>
  mutate(species = gsub('\\*', '', species)) |>
  select(species, region, `Vodní vegetace`:`Synantropní dřevinná vegetace, paseky a nálety`) |>
  pivot_longer(-c(species, region)) |>
  drop_na() |>
  left_join(read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2022_Expanzky/HabitatNames-2023-07-03.xlsx)') |>
  rename(name = Jmeno, habitat = Name))