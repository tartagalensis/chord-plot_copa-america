# Scraping Competiciones - Copa América
# Autor: Franco Galeano
# Fecha: 14 de Junio

library(tidyverse)
library(rvest)
library(countrycode)
library(janitor)

#Armo un tibble con las últimas 15 competiciones, equipos y link a wiki
d <- tibble(
  year = c(1987, 1989,1991,1993,1995,1997,1999,2001,2004,2007,2011,2015,2019,2021),
  teams = c(10,10,10,12,12,12,12,12,12,12,12,12,12,10),
  url = paste0("https://en.wikipedia.org/wiki/", year,"_Copa_América")
)

# Copa america centenario tienen un link diferente
centenario <- tibble(
  year = 2016,
  teams = 16,
  url = "https://en.wikipedia.org/wiki/Copa_América_Centenario")

# Funcion obtener logo
get_logo <- function(u){
  # u = d$url[1]
  h <- read_html(u)
  h %>%
    html_nodes(".infobox-image img") %>%
    html_attr("src")
}

# Equipos participantes
c1987 <- tibble(year = 1987,
       team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                "Brazil","Chile","Venezuela","Colombia","Bolivia"),
       url = "https://en.wikipedia.org/wiki/1987_Copa_América",
       teams = 10) %>% 
  print()

c1989 <- tibble(year = 1989,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia"),
                url = "https://en.wikipedia.org/wiki/1989_Copa_América",
                teams = 10) %>% 
  print()

c1991 <- tibble(year = 1991,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia"),
                url = "https://en.wikipedia.org/wiki/1991_Copa_América",
                teams = 10) %>% 
  print()

c1993 <- tibble(year = 1993,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia",
                         "United States","Mexico"),
                url = "https://en.wikipedia.org/wiki/1993_Copa_América",
                teams = 12) %>% 
  print()

c1995 <- tibble(year = 1995,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia",
                         "United States","Mexico"),
                url = "https://en.wikipedia.org/wiki/1995_Copa_América",
                teams = 12) %>% 
  print()

c1997 <- tibble(year = 1997,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia",
                         "Costa Rica","Mexico"),
                url = "https://en.wikipedia.org/wiki/1995_Copa_América",
                teams = 12) %>% 
  print()

c1999 <- tibble(year = 1999,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia",
                         "Japan","Mexico"),
                url = "https://en.wikipedia.org/wiki/1999_Copa_América",
                teams = 12) %>% 
  print()
 

c2001 <- tibble(year = 2001,
                team = c("Honduras", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia",
                         "Costa Rica","Mexico"),
                url = "https://en.wikipedia.org/wiki/2001_Copa_América",
                teams = 12) %>% 
  print()

c2004 <- tibble(year = 2004,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia",
                         "Costa Rica","Mexico"),
                url = "https://en.wikipedia.org/wiki/2004_Copa_América",
                teams = 12) %>% 
  print()


c2007 <- tibble(year = 2007,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia",
                         "United States","Mexico"),
                url = "https://en.wikipedia.org/wiki/2007_Copa_América",
                teams = 12) %>% 
  print()

c2011 <- tibble(year = 2011,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia",
                         "Costa Rica","Mexico"),
                url = "https://en.wikipedia.org/wiki/2011_Copa_América",
                teams = 12) %>% 
  print()


c2015 <- tibble(year = 2015,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia",
                         "Jamaica","Mexico"),
                url = "https://en.wikipedia.org/wiki/2015_Copa_América",
                teams = 12) %>% 
  print()

c2016 <- tibble(year = 2016,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia",
                         "United States","Jamaica","Mexico","Costa Rica","Haiti","Panama"),
                url = "https://en.wikipedia.org/wiki/Copa_América_Centenario",
                teams = 16) %>% 
  print()

c2019 <- tibble(year = 2019,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia",
                         "Japan","Qatar"),
                url = "https://en.wikipedia.org/wiki/2019_Copa_América",
                teams = 12) %>% 
  print()

c2021 <- tibble(year = 2021,
                team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                         "Brazil","Chile","Venezuela","Colombia","Bolivia"),
                url = "https://en.wikipedia.org/wiki/2021_Copa_América",
                teams = 10) %>% 
  print()


d <- bind_rows(c1987,c1989,c1991,c1993,c1995,c1997,c1999,
          c2001,c2004,c2007,c2011,c2015,c2016,c2019,
          c2021) %>%
  mutate(url_comp_logo = map(.x = url, .f = ~get_logo(u = .x))) %>% 
  print()

equipos <- tibble(team = c("Argentina", "Peru", "Ecuador","Uruguay","Paraguay",
                 "Brazil","Chile","Venezuela","Colombia","Bolivia",
                 "United States","Mexico",
                 "Japan","Costa Rica",
                 "Honduras","Jamaica",
                 "Haiti","Panama","Qatar"),
        url_team = c("/wiki/Argentina_national_football_team","/wiki/Peru_national_football_team","/wiki/Ecuador_national_football_team","/wiki/Uruguay_national_football_team","/wiki/Paraguay_national_football_team",
                     "/wiki/Brazil_national_football_team","/wiki/Chile_national_football_team","/wiki/Venezuela_national_football_team","/wiki/Colombia_national_football_team","/wiki/Bolivia_national_football_team",
                     "/wiki/United_States_men%27s_national_soccer_team","/wiki/Mexico_national_football_team",
                     "/wiki/Japan_national_football_team","/wiki/Costa_Rica_national_football_team",
                     "/wiki/Honduras_national_football_team","/wiki/Jamaica_national_football_team",
                     "/wiki/Haiti_national_football_team","/wiki/Panama_national_football_team","/wiki/Qatar_national_football_team")) %>% 
  print()

wiki_copaamerica_comp <- d %>% left_join(equipos) %>%
  unnest(url_comp_logo) %>%
  mutate(team_alpha3 = countrycode(sourcevar = team, origin = "country.name", 
                                   destination = "iso3c")) %>% 
  print()

write_excel_csv(wiki_copaamerica_comp,  "./data/wiki_comp_copamerica.csv")
