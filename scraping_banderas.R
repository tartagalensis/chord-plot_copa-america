# Scraping Banderas - Copa América
# Autor: Franco Galeano
# Fecha: 14 de Junio

library(tidyverse)
library(magick)
library(rsvg)


w <- read_csv("./data/wiki_players_copamerica.csv", guess_max = 1e5) %>% 
  print()

n0 <- w %>%
  select(nat_team, nat_team_alpha3) %>%
  distinct() %>%
  rename(label = nat_team, 
         alpha3 = nat_team_alpha3) %>% 
  print()

d0 <- w %>%
  select(club_country_harm, club_alpha3, club_country_flag, year) %>%
  filter(club_alpha3 %in% n0$alpha3) %>%
  group_by_at(1:3) %>%
  mutate(year_min = min(year), 
         year_max = max(year),
         years = paste0(unique(sort(year)), collapse = ",")) %>%
  ungroup() %>%
  select(-year) %>%
  distinct() %>%
  rename(label = club_country_harm, 
         alpha3 = club_alpha3,
         flag_url = club_country_flag) %>%
  drop_na() %>%
  arrange(label) %>%
  mutate(label = ifelse(str_detect(string = flag_url, pattern = "Wales"), 
                        "Wales", label),
         alpha3 = ifelse(str_detect(string = flag_url, pattern = "Wales"), 
                         "GB-WLS", alpha3),
         flag_url = paste0("https:", flag_url)) 


# use most upto date flag
d2 <- d0 %>%
  bind_rows(d1) %>%
  arrange(alpha3, year_min) %>%
  group_by(alpha3) %>%
  slice(n())

b <- image_blank(width = 100, height = 75, color = "grey40")

for(i in 1:nrow(d2)){
  message(d2$alpha3[i])
  f <- image_read_svg(path = d2$flag_url[i])
  f %>%
    image_resize("100x75") %>%
    image_composite(image = b, composite_image = ., gravity = "center") %>%
    image_write(path = paste0("./flag/",d2$alpha3[i], ".png"))
}
