# Scraping Banderas - Copa América
# Autor: Franco Galeano
# Fecha: 14 de Junio

library(tidyverse)
library(tweenr)
library(circlize)
library(migest)
library(animation)
library(magick)
library(grid)


w1 <- read_csv("./data/wiki_players_copamerica.csv")
w2 <- read_csv("./data/wiki_colours_copamerica.csv")
w3 <- read_csv("./data/wiki_comp_copamerica.csv")

# club_country to nat_team data frame
d1 <- w1 %>%
  select(year, nat_team, club_country_harm, contains("alpha3")) %>%
  rename(club_country = club_country_harm,
         nat_alpha3 = nat_team_alpha3) %>%
  replace_na(list(club_country = "No Club")) %>%
  group_by(club_country, nat_team, year, club_alpha3, nat_alpha3) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  left_join(w2, by = c("nat_alpha3" = "team_alpha3")) %>%
  select(-url_team) %>%
  arrange(year) %>% 
  print()

n0 <- d1 %>%
  select(contains("nat_"), contains("kit")) %>%
  distinct() %>%
  mutate(nat_team = factor(x = nat_team, levels = unique(d1$nat_team))) %>%
  arrange(nat_team) %>%
  mutate(nat_team = as.character(nat_team)) %>%
  rename(lab = nat_team, 
         alpha3 = nat_alpha3) %>% 
  print()

c0 <- d1 %>%
  select(contains("club_")) %>%
  distinct() %>%
  filter(!(club_country %in% n0$lab)) %>%
  mutate(kit_shirt = "transparent", 
         kit_shorts = "transparent", 
         kit_socks = "transparent",
         club_country = factor(x = club_country, levels = unique(d1$club_country)),
         club_country = fct_rev(club_country)) %>%
  arrange(club_country) %>%
  mutate(club_country = as.character(club_country)) %>%
  rename(lab = club_country,
         alpha3 = club_alpha3) %>% 
  print()


d0 <- n0 %>%
  bind_rows(c0) %>%
  mutate(label = str_wrap(string = lab, width = 10)) %>%
  separate(col = label, into = c("lab1", "lab2"), sep = "\n", 
           fill = "right", remove = FALSE) %>%
  mutate(y = ifelse(test = !is.na(lab2), yes = 1, no = 0.6),
         label = ifelse(test = alpha3 %in% c("DEU", "GB-NIR", "IRL", "CZE"),
                        label, lab),
         label = ifelse(label == "Czechoslovakia", "Czecho-\nslovakia", label),
         label = ifelse(label == "Bosnia and Herzegovina", "Bosnia and Herz.", label),
         # label = ifelse(label == "Switzerland", "Switzer-\nland", label),
         gap = ifelse(kit_shirt == "transparent", 2.5, 1.5),
         label2020 = ifelse(lab == "Bosnia and Herzegovina", "Bosnia", label),
         label2020 = ifelse(lab == "North Macedonia", "North\nMacedonia", label2020)) %>% 
  print()


z <- expand_grid(year = unique(sort(d1$year)),
                 nat_team = d0$lab,
                 club_country = d0$lab) %>%
  mutate(n = ifelse(nat_team == club_country, 0.01, 0),
         kit_shirt = "transparent", 
         kit_shorts = "transparent", 
         kit_socks = "transparent",
         kit_away = "transparent") %>% 
  print()

d2 <- d1 %>%
  bind_rows(z) %>%
  group_by(year, nat_team, club_country) %>%
  filter(n == max(n)) %>%
  ungroup() %>%
  arrange(year) %>%
  select(-contains("alpha3")) %>% 
  print()


m0 <- d2 %>%
  filter(year == 2021) %>% 
  complete(club_country, nat_team, fill = list(n = 0)) %>%
  sum_turnover(orig_col = "club_country", dest_col = "nat_team",
               flow_col = "n", drop_diagonal = FALSE,
               include_net = FALSE) %>%
  mutate(tot = tot_in + tot_out) %>%
  group_by(region) %>%
  filter(tot == max(tot)) %>%
  ungroup() %>%
  #select(-year) %>%
  distinct() %>%
  select(region, tot) %>%
  deframe() %>% 
  print()

# tween
d3 <- d2 %>%
  mutate(corridor = paste(club_country, nat_team, sep = " -> ")) %>%
  select(-club_country, -nat_team, -contains("kit"), kit_shirt) %>%
  mutate(ease = "linear") %>%
  tween_elements(time = "year", group = "corridor", ease = "ease", 
                 nframes = diff(range(d1$year)) * 4) %>%
  as_tibble() %>%
  separate(col = .group, into = c("club_country", "nat_team"), sep = " -> ") %>%
  relocate(club_country, nat_team, n) %>% 
  mutate(kit_shirt = ifelse(nat_team == "Uruguay", "#95C9FC", kit_shirt)) %>% 
  print()

# Copa America 2021
copamerica2021 <- w1 %>%
  filter(year == 2021) %>%
  pull(squad) %>%
  unique() %>% 
  print()



df <- d3 %>% 
  filter(year == 2021,
         n > 0.5) %>% 
  mutate(kit_shirt = ifelse(nat_team == "Uruguay", "#95C9FC", kit_shirt)) %>% 
  print()

pdf(file = "./plot/copa_america2021.pdf", height = 7, width = 7, useDingbats = FALSE)

par(mar = rep(0, 4), bg = "grey40", lheight = 0.8)  
circos.clear()
circos.par(track.margin=c(0.01, -0.01), points.overflow.warning = FALSE,
           gap.degree = 2.5,
           start.degree = 90)
chordDiagram(x = select(df, nat_team, club_country, n),
                 col = pull(df, kit_shirt),
                 order = c(copamerica2021, d0$lab[!d0$lab %in% copamerica2021]),
                 grid.col = d0 %>%
                   select(lab, kit_shirt) %>%
               mutate(kit_shirt = ifelse(lab == "United States", "transparent", kit_shirt)) %>%
               mutate(kit_shirt = ifelse(lab == "Mexico", "transparent", kit_shirt)) %>% 
               mutate(kit_shirt = ifelse(lab == "Uruguay", "#95c9fc", kit_shirt)) %>% 
                   deframe(),
                 transparency = 0.1,
                 directional = -1, direction.type = c("diffHeight", "arrows"),
                 link.arr.type = "big.arrow", diffHeight  = -0.02,
                 link.sort = TRUE,
                 link.largest.ontop = TRUE,
                 h.ratio = 0.6,
                 xmax = m0,
                 annotationTrack = "grid",
                 annotationTrackHeight = 0.02,
                 preAllocateTracks = list(track.height = 0.2))


# plot the chord diagram
circos.trackPlotRegion(
    track.index = 1, 
    bg.border = NA, 
    panel.fun = function(x, y) {
      s <- get.cell.meta.data("sector.index")
      dd <- filter(d0, lab == s)
      xx <- get.cell.meta.data("xlim") %>%
        mean()
      theta <- circlize(mean(xx), 1.3)[1, 1] %% 360
      ff <- ifelse(theta < 90 || theta > 270, 
                   "clockwise", "reverse.clockwise")
      aa <- c(1, 0.5)
      if(theta < 90 || theta > 270)  
        aa <- c(0, 0.5)
      
      flag_disp <- TRUE
      if(is.na(dd$alpha3))
        flag_disp <- FALSE
      if(dd$alpha3 %in% c0$alpha3)
        flag_disp <- FALSE
      # euro2020 drop flags for past teams
      if(!dd$lab %in% copamerica2021)
         flag_disp <- FALSE
      
      if(flag_disp){
        flag_rot <- ifelse(theta < 90 || theta > 270, -90, 90)
        flag <- dd$alpha3 %>%
          paste0("./flag/", . ,".png") %>%
          image_read() %>%
          image_rotate(degrees = flag_rot)
        circos.raster(image = flag, x = mean(xx), y = 0.2, 
                      width = "0.3cm", facing = "inside")
        
      }
      circos.text(x = xx, y = ifelse(flag_disp, 0.38, 0.1), 
                  labels = dd$label2020,
                  # labels = dd$label, 
                  facing = ff, adj = aa,
                  col = "white", cex = 0.8)
    }
  )
  y <- d4$year[1]
  if(y %in% w3$year){
    logo <- w3 %>%
      filter(year == y) %>%
      slice(1) %>%
      pull(url_comp_logo) %>%
      paste0("https:", .) %>%
      image_read()
    
    w <- h <- NULL
    if(y != 2000)
      w <- 0.14
    if(y == 2000)
      h <- 0.18
    grid.raster(image = logo, x = 0.99, y = 0.99, 
                width = w, height = h, 
                hjust = 1, vjust = 1)
  }
  
  if(y %% 4 != 0)
    y <- NULL
  text(-1.1,1.02, paste0("Planteles Copa América 2021", y), col="white", cex=1.5, pos=4)
  text(-1.1,0.96,"De ligas a selecciones", col="white", cex=0.75, pos=4)
  text(-1.1,0.91,"", col="white", cex=0.75, pos=4)
  

  text(-1.1,yy-0.03*10, "@Tartagalensis, replica de @guyabelguyabel", col="white", cex=0.75, pos=4)

dev.off()
file.show("./plot/copa_america2021.pdf")
# file.show("./plot/euro2020.pdf")





##
## placeholder image
##
# p <- pp[length(pp)]
p <- image_read_pdf(path = "plot/copa_america2021.pdf")
png(filename = "./plot/copamerica.png", width = 2100, height = 2100)
par(mar = rep(0,4))
plot(as.raster(p))
dev.off()
