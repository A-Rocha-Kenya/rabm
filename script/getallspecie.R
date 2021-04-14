library(rabm)
library(dplyr)

countries <- c("kenya", "southafrica", "nigeria", "botswana", "namibia", "zimbabwe", "lesotho", "swaziland", "mozambique")

for (c in countries){
  get_species_list(c)
  bind_rows
}

species <- map_dfr(countries,get_species_list)
list <- species %>%
  unique() %>%
  unite(name,Common_species,Common_group, sep = ' ') %>%
  select(Spp,name) %>%
  arrange(Spp) %>%
  write_csv(path = 'C:/Users/rnussba1/Documents/GitHub/rabm/Shiny/species_list.csv')

