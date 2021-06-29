library(tidyverse); library(ggplot2); library(leaflet); library(sf); library(ggrepel); library(gganimate)

AL1617 <- read.csv("data/AdultAL1617MSOA.csv")
AL1718 <- read.csv("data/AdultAL1718MSOA.csv")
AL1819 <- read.csv("data/AdultALNov1819MSOA.csv")
msoaNames <- read.csv("data/MSOA Names.csv")
msoa <- st_read("data/GM_MSOA.geojson")
las <- st_read("data/GM_LADS.geojson")


AL1617 <- AL1617 %>% left_join(select(msoaNames, Laname, msoa11nm), by = "msoa11nm") %>%
  filter(Laname %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan"))
AL1617$Release <- "2016/17"  
AL1718 <- AL1718 %>% left_join(select(msoaNames, Laname, msoa11nm), by = "msoa11nm") %>%
  filter(Laname %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan"))
AL1718$Release <- "2017/18"
AL1819 <- AL1819 %>% subset(select = c(1:4)) %>% 
  left_join(select(msoaNames, Laname, msoa11nm), by = "msoa11nm") %>%
  filter(Laname %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan"))
AL1819$Release <- "2018/19"

ALMSOA <- bind_rows(AL1617, AL1718, AL1819)

msoaNames <- msoaNames %>%
  filter(Laname %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan"))
msoa <- left_join(msoa, msoaNames, by = c("MSOA11CD" = "msoa11cd"))

ALMSOA <- left_join(msoa, ALMSOA, by = c("MSOA11CD" = "msoa11"))
ALMSOA1819 <- ALMSOA %>% filter(Release == "2018/19")
ALMSOA1718 <- ALMSOA %>% filter(Release == "2017/18")
ALMSOA1617 <- ALMSOA %>% filter(Release == "2016/17")

MSOA1819 <- ggplot() +
  geom_sf(data = ALMSOA1819, aes(fill = Inactive)) +
  theme_GS() +
  scale_fill_gradient2(
    low = "#FFFFFF",
    mid = "#D21C60",
    high = "#5B2D86",
    midpoint = 25,
    guide = "legend"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +   
  geom_sf(data = las, 
          color = "#3c3c3b", 
          size = 1,
          fill = NA)
  
MSOA1819 +
  geom_sf_text(data = ALMSOA1819, aes(label = msoa11hclnm))


MSOA1718 <- ggplot() +
  geom_sf(data = ALMSOA1718, aes(fill = Inactive)) +
  theme_GS() +
  scale_fill_gradient2(
    low = "#FFFFFF",
    mid = "#D21C60",
    high = "#5B2D86",
    midpoint = 25,
    guide = "legend"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +   
  geom_sf(data = las, 
          color = "#3c3c3b", 
          size = 1,
          fill = NA)


MSOA1718 +
  geom_sf_text(data = ALMSOA1718, aes(label = msoa11hclnm))

MSOA1617 <- ggplot() +
  geom_sf(data = ALMSOA1617, aes(fill = Inactive)) +
  theme_GS() +
  scale_fill_gradient2(
    low = "#FFFFFF",
    mid = "#D21C60",
    high = "#5B2D86",
    midpoint = 25,
    guide = "legend"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +   
  geom_sf(data = las, 
          color = "#3c3c3b", 
          size = 1,
          fill = NA)


MSOA1617 +
  geom_sf_text(data = ALMSOA1617, aes(label = msoa11hclnm))


# Oldham Mapping ----------------------------------------------------------
ALMSOA1819Oldham <- ALMSOA1819 %>% filter(Laname.x == "Oldham")
ALMSOA1718Oldham <- ALMSOA1718 %>% filter(Laname.x == "Oldham")
ALMSOA1617Oldham <- ALMSOA1617 %>% filter(Laname.x == "Oldham")

MSOA1819Oldham <- ggplot() +
  geom_sf(data = ALMSOA1819Oldham, aes(fill = Inactive)) +
  theme_GS() +
  scale_fill_gradient2(
    low = "#FFFFFF",
    mid = "#D21C60",
    high = "#5B2D86",
    midpoint = 25,
    guide = "legend"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +   
  geom_sf_text(data = ALMSOA1819Oldham, aes(label = msoa11hclnm))


MSOA1718Oldham <- ggplot() +
  geom_sf(data = ALMSOA1718Oldham, aes(fill = Inactive)) +
  theme_GS() +
  scale_fill_gradient2(
    low = "#FFFFFF",
    mid = "#D21C60",
    high = "#5B2D86",
    midpoint = 25,
    guide = "legend"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +   
  geom_sf_text(data = ALMSOA1718Oldham, aes(label = msoa11hclnm))

MSOA1617Oldham <- ggplot() +
  geom_sf(data = ALMSOA1617Oldham, aes(fill = Inactive)) +
  theme_GS() +
  scale_fill_gradient2(
    low = "#FFFFFF",
    mid = "#D21C60",
    high = "#5B2D86",
    midpoint = 25,
    guide = "legend"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  geom_sf_text(data = ALMSOA1617Oldham, aes(label = msoa11hclnm))


# Animated ----------------------------------------------------------------

ALMSOAOldham <- ALMSOA %>%
  filter(Laname.x == "Oldham")

OldhamMSOAAL <- ggplot(data = ALMSOAOldham, aes(fill = Inactive)) +
  theme_GS() +
  scale_fill_gradient2(
    low = "#FFFFFF",
    mid = "#D21C60",
    high = "#5B2D86",
    midpoint = 25,
    guide = "legend"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) 

OldhamMSOAALAnimate <- OldhamMSOAAL +
  transition_states(Release,
                    transition_length = 2,
                    state_length = 1) 

animate(OldhamMSOAALAnimate, renderer = gifski_renderer())

