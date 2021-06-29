library(sf); library(leaflet); library(gganimate)

la <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_December_2017_Boundaries/MapServer/3/query?where=1%3D1&outFields=*&outSR=4326&f=json")
gm_la <- la %>%
  filter(lad17nm %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale", "Salford","Stockport","Tameside","Trafford","Wigan"))

AL_LA <- ActiveLives %>%
  filter(Area != "Greater Manchester"| Area != "England") 

AL_LA$Release <- factor(AL_LA$Release, levels = 
                                c("Nov 15-16", "May 16-17", "Nov 16-17",
                                  "May 17-18", "Nov 17-18", "May 18-19",
                                  "Nov 18-19", "May 19-20", "Nov 19-20"), ordered = TRUE)



AL_LA <-right_join(gm_la, AL_LA, by = c("lad17nm" = "Area"))

AL_LA_Inactive <- AL_LA %>% 
  filter(ActivityLevel == "Inactive") %>%
  filter(Demographic == "Total Population")

AL_LA_Inactive %>% filter(Release == "Nov 19-20") %>%
  ggplot() +
  geom_sf(aes(fill = Value)) +
  scale_fill_gradient(low = "#FFFFFF", 
                      high = "#5B2D86",
                      aesthetics = "fill") +
  theme_GMM() +
  theme(axis.title = element_blank(),
       axis.text = element_blank(),
       axis.ticks = element_blank())

map <- ggplot(data = AL_LA_Inactive, aes(fill = Value*100)) +
  geom_sf() +
  scale_fill_gradient(low = "#FFFFFF", 
                      high = "#5B2D86",
                      aesthetics = "fill") +
  labs(caption = "**Source:** *Sport England, Active Lives Adult Survey*") +
  theme_GS() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.15,0.18),
        legend.title = element_text(color = "#3c3c3b"),
        plot.caption = element_markdown()) +
  labs(fill = "Inactivity Rate (%)")

map_animate <- map +
  transition_manual(factor(Release, levels = 
                             c("Nov 15-16", "May 16-17", "Nov 16-17",
                               "May 17-18", "Nov 17-18", "May 18-19",
                               "Nov 18-19", "May 19-20", "Nov 19-20"))) +
  labs(title = "Inactivity In Greater Manchester",
       subtitle = '<br>**Release:** {current_frame}'
  ) +
  theme(plot.title = element_text(size= 20),
        plot.subtitle = element_markdown(size = 16))

animated <- animate(map_animate, renderer = gifski_renderer(), fps = 5)

anim_save("ALAnimation2.gif", animated, height = 1080, width = 1080)  


# November only -----------------------------------------------------------

mapNov <- 
  AL_LA_Inactive %>%
  filter(
    str_detect(Release, "Nov")
  ) %>%
  ggplot(aes(fill = Value*100)) +
  geom_sf() +
  scale_fill_gradient(low = "#FFFFFF", 
                      high = "#5B2D86",
                      aesthetics = "fill") +
  labs(caption = "**Source:** *Sport England, Active Lives Adult Survey*") +
  theme_GS() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.15,0.18),
        legend.title = element_text(color = "#3c3c3b"),
        plot.caption = element_markdown()) +
  labs(fill = "Inactivity Rate (%)")

map_animateNov <- mapNov +
  transition_manual(factor(Release, levels = 
                             c("Nov 15-16", "Nov 16-17",
                               "Nov 17-18", "Nov 18-19", "Nov 19-20"))) +
  labs(title = "Inactivity In Greater Manchester",
       subtitle = '<br>**Release:** {current_frame}'
  ) +
  theme(plot.title = element_text(size= 20),
        plot.subtitle = element_markdown(size = 16))

animated <- animate(map_animateNov, renderer = gifski_renderer(), fps = 10)

anim_save("ALAnimationNov.gif", animated, height = 1080, width = 1080)  

