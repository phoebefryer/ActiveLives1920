library(readxl); library(tidyverse); library(dplyr); library(ggplot2); library(xlsx); 
library(extrafont); library(ggridges); library(data.table); library(ggrepel)


# data wrangling ----------------------------------------------------------

AdultAL <- read_excel("data/Active Lives Template v20.xlsx", 
                                        sheet = "Full Adult Active Lives")
AdultAL$Inactive <- AdultAL$Inactive*100
AdultALFiltered <- AdultAL %>% filter(Release == "Nov 18-19") %>%
  rename(Borough = Area) %>%
  filter(Borough %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>%
  group_by(Borough) %>%
  arrange(desc(Borough))
AdultALFiltered$Borough <- factor(AdultALFiltered$Borough, levels = AdultALFiltered$Borough)
AdultALNov1819MSOA <- read.csv("data/AdultALNov1819MSOA.csv")
AdultALNov1819MSOA <- AdultALNov1819MSOA %>% rename(Borough = Laname) %>%
  filter(Borough %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>%
  group_by(Borough) %>%
  arrange(desc(Borough))
AdultALNov1819MSOA$msoa11nm <- factor(AdultALNov1819MSOA$msoa11nm, levels = AdultALNov1819MSOA$msoa11nm)
EngAvg <- AdultAL %>% filter(Release == "Nov 18-19") %>%
  rename(Borough = Area) %>%
  filter(Borough == "England")
EngAvg <- EngAvg$Inactive

max <- setDT(AdultALNov1819MSOA)[, .SD[which.max(Inactive)], by = Borough]
min <- setDT(AdultALNov1819MSOA)[, .SD[which.min(Inactive)], by = Borough]
diff <- left_join(max, min, by = "Borough")  
diff$diff <- diff$Inactive.x - diff$Inactive.y
bigDiff <- diff[, .SD[which.max(diff)]]
smallDiff <- diff[, .SD[which.min(diff)]]


ActiveLives <- read.csv("data/ALSimple2.csv")

ActiveLives$DemographicGroup[ActiveLives$Demographic %in% c("Male", "Female")] <- "Gender"
ActiveLives$DemographicGroup[ActiveLives$Demographic %in% 
                               c("16-34", "35-54", "55-74", "75+", "16-54", "55+")] <- "Age"
ActiveLives$DemographicGroup[ActiveLives$Demographic %in% 
                               c("White British", "White Other", "South Asian",
                                 "Black", "Chinese", "Mixed", "Other ethnic group")] <- "Ethnicity"
ActiveLives$DemographicGroup[ActiveLives$Demographic %in% 
                               c("LT limiting disability", "No LT limiting disability")] <- "Disability"
ActiveLives$DemographicGroup[ActiveLives$Demographic %in% 
                               c("NS SEC 1-2: Higher social groups", "NS SEC 3-5: Middle social groups",
                                 "NS SEC 6-8: Lower social groups", "NS SEC 9: Students and other")] <- "Socio-Economic"

ActiveLives$DemographicGroup[ActiveLives$Demographic %in% 
                               c("Working full or part time", "Unemployed",
                                 "Not working - retired", "Not working - looking after house/children",
                                 "Not working - long term sick or disabled", "Student full or part time",
                                 "Other working status")] <- "Employment"

ActiveLives$Release <- factor(ActiveLives$Release, levels = 
                                c("Nov 15-16", "May 16-17", "Nov 16-17",
                                  "May 17-18", "Nov 17-18", "May 18-19",
                                  "Nov 18-19", "May 19-20"))

ActiveLivesIGM <- ActiveLives %>%
  filter(Activity.Level == "Inactive") %>%
  filter(!is.na(DemographicGroup)) %>%
  filter(Area == "Greater Manchester") %>%
  filter(Demographic != "55+") %>%
  filter(Demographic != "16-54") %>%
  filter(DemographicGroup != "Employment")

# Differences -------------------------------------------------------------
groups <- c("Socio-Economic", "Gender", "Ethnicity", "Disability", "Age")

a <- c("Nov 15-16", "May 16-17", "Nov 16-17",
       "May 17-18", "Nov 17-18", "May 18-19",
       "Nov 18-19", "May 19-20")

max <- ActiveLivesIGM %>% group_by(DemographicGroup, Release) %>% 
  summarise(Max = max(Value, na.rm = TRUE))

min <- ActiveLivesIGM %>% group_by(DemographicGroup, Release) %>% 
  summarise(Min = min(Value, na.rm = TRUE))

diff <- cbind(max, min) %>%
  mutate(Difference = Max-Min) %>%
  rename(DemographicGroup = DemographicGroup...1) %>%
  rename(Release = Release...2)

diffLabels <- diff %>%
  filter(Release %in% c("Nov 15-16", "May 19-20"))

# Process -----------------------------------------------------------------

ggplot(AdultALNov1819MSOA, aes(x = Borough, y =  Inactive)) +
  geom_boxplot() + 
  scale_x_discrete(limits = rev)


ggplot(AdultALNov1819MSOA, aes(x = Borough, y =  Inactive)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(limits = c(0,45))+
  scale_x_discrete(limits = rev)

ggplot(AdultALNov1819MSOA, aes(x = Borough, y =  Inactive, color = Borough)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(limits = c(10,45), expand = c(0.005, 0.005)) +
  scale_x_discrete(limits = rev) +
  theme_light(base_size = 18) +
  labs(x = NULL,
       y = "Inactivity Rate (%)") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank(),
    panel.border = element_blank()
  )


# Finalising --------------------------------------------------------------


g <- ggplot(AdultALNov1819MSOA, aes(x = Borough, y =  Inactive, color = Borough)) +
  coord_flip() +
  scale_y_continuous(limits = c(10,45), expand = c(0.005, 0.005)) +
  theme_light(base_size = 14, base_family = "PT Sans") +
  labs(x = NULL,
       y = "Inactivity Rate (%)",
       caption = "Data: Sport England, Active Lives Adult Survey, Nov 18/19",
       title = "Inactivity rates across Greater Machester") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 11),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
  )


g_text <- g +
  geom_hline(yintercept = EngAvg, color = "#3C3C3B", size = 1) +
  geom_point(data = AdultALFiltered, size = 5) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  annotate(
    "text", x = 9.5, y = 30, size = 2.8, family = "PT Sans", color = "gray20", lineheight = .9,
    label = glue::glue("England average:\n{round(EngAvg, 1)}% classified as inactive")
  ) +
  annotate(
    "text", x = 3, y = 41.5, size = 2.8, family = "PT Sans", color = "gray20",
    label = "Borough average"
  ) + 
  annotate(
    "text", x = 6, y = 15,  size = 2.8, family = "PT Sans", color = "gray20",
    label = "MSOA's (Middle Layer\nSuper Output Areas)\nper borough"
  )

arrows <-
  tibble(
    x1 = c(9.1, 3, 5.4, 5.4),
    x2 = c(8.4, 3.8, 4, 4.1),
    y1 = c(30, 38, 15, 15),
    y2 = c(EngAvg, 33, 21.3, 18.3)
  )

g_text +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = -0.3
  )

finalALInactivity <- g_text +
  geom_segment(data = AdultALFiltered, aes(x = Borough, xend = Borough,
               y = EngAvg, yend = Inactive),
               size = 0.8) +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = -0.3
  ) +
  labs(
    title = "Inactivity rates across Greater Manchester",
    subtitle = paste(bigDiff$Borough, " has the largest difference in activity rates at an MSOA level, whilst ", 
                     smallDiff$Borough, " has the smallest difference.")
  ) +
  scale_x_discrete(limits = rev) +
  theme(plot.caption = element_text(size = 9, color = "gray50"),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(size = 12),
        plot.title.position = "plot"
        )

ggplot(AdultALNov1819MSOA, aes(x = Inactive, y = Borough, fill = Borough)) +
  geom_density_ridges(alpha = 0.5) +
  theme_light(base_size = 14, base_family = "PT Sans") +
  scale_y_discrete(limits = rev) +
  labs(x = "Inactivity Rate (%)",
       y = NULL,
       caption = "Data: Sport England, Active Lives Adult Survey, Nov 18/19") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 11),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
  )

ggplot(AdultALNov1819MSOA, aes(x = Inactive, y = Borough, fill = Borough)) +
  geom_violin() +
  theme_light(base_size = 14, base_family = "PT Sans") +
  scale_y_discrete(limits = rev) +
  labs(x = "Inactivity Rate (%)",
       y = NULL,
       caption = "Data: Sport England, Active Lives Adult Survey, Nov 18/19") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 11),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
  )

##Branded up
base <- ggplot(AdultALNov1819MSOA, aes(x = Borough, y =  Inactive, color = Borough)) +
  coord_flip() +
  scale_y_continuous(limits = c(10,45), expand = c(0.005, 0.005)) +
  theme_GS() +
  theme(
    legend.position = "none"
  ) +
  labs(x = NULL,
       y = "Inactivity Rate (%)",
       caption = "Data: Sport England, Active Lives Adult Survey, Nov 18/19",
       title = "Inactivity rates across Greater Machester") 

baseText <- base +
  geom_hline(yintercept = EngAvg, color = "#3C3C3B", size = 1) +
  geom_point(data = AdultALFiltered, size = 5) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  scaleColGS(palette = "boroughs_10") +
  annotate(
    "text", x = 9.5, y = 30, size = 2.8, lineheight = .9,
    label = glue::glue("England average:\n{round(EngAvg, 1)}% classified as inactive")
  ) +
  annotate(
    "text", x = 3, y = 41.5, size = 2.8, 
    label = "Borough average"
  ) + 
  annotate(
    "text", x = 6, y = 15,  size = 2.8, 
    label = "MSOA's (Middle Layer\nSuper Output Areas)\nper borough"
  )

baseText +
  geom_segment(data = AdultALFiltered, aes(x = Borough, xend = Borough,
                                           y = EngAvg, yend = Inactive),
               size = 0.8) +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = -0.3
  ) +
  labs(
    title = "Inactivity rates across Greater Manchester",
    subtitle = paste(bigDiff$Borough, " has the largest difference in activity rates at an MSOA level, whilst ", 
                     smallDiff$Borough, " has the smallest difference.")
  ) +
  scale_x_discrete(limits = rev) +
  theme(
        plot.title.position = "plot"
  )






ggplot(diff, aes(x = Release, y = Difference*100, 
                 group = DemographicGroup, color = DemographicGroup)) +
  geom_line(size = 1) +
  geom_text_repel(data = diffLabels, aes(label = paste(format(round(Difference*100,2)),"%"))) +
  theme_GS() +
  scaleColGS(palette = "mixed") +
  labs(title = "Inactivity Change by Demographics",
       caption = "Data: Sport England, Active Lives Adult Survey, Nov 15/16 to Nov 18/19",
       x = NULL,
       y = "Difference (%)"
  )
