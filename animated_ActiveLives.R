library(tidyverse); library(ggplot2); library(ggrepel); library(openxlsx); library(gganimate)


# Data Wrangling ----------------------------------------------------------


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


ActiveLivesLatest <- ActiveLives %>%
  filter(Release == "May 19-20")

ActiveLivesLatestI <- ActiveLivesLatest %>%
  filter(Activity.Level == "Inactive") %>%
  filter(!is.na(DemographicGroup))

ActiveLivesLatestIGM <- ActiveLivesLatestI %>%
  filter(Area == "Greater Manchester")

ActiveLivesLatestIGM.2 <- ActiveLivesLatestIGM %>%
  group_by(DemographicGroup) %>%
  mutate(highlight = (min(Value) == Value | max(Value) == Value)) %>%
  filter(highlight == TRUE)


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


# Graphing ----------------------------------------------------------------


ggplot(ActiveLivesLatestI, aes(x = DemographicGroup, y = Value, color = Area)) + 
  geom_point() +
  coord_flip()

ggplot(ActiveLivesLatestIGM, aes(x = DemographicGroup, y = Value)) + 
  geom_jitter() +
  coord_flip()

DemographicsPlotGM <- ggplot(ActiveLivesLatestIGM, aes(x = DemographicGroup, y = Value, color = DemographicGroup)) + 
  theme_GS() +
  geom_point() +
  geom_text_repel(data = . %>%
               group_by(DemographicGroup) %>%
               filter(Value == max(Value)| Value == min(Value)),
             aes(label = paste(Demographic, round(Value*100, digits = 2), "%")),
             nudge_x = .3,
             box.padding = 0.5) +
  scaleColGS(palette = "mixed") +
  coord_flip() +
  theme(
    legend.position = "none"
  )

# Lets get animated -------------------------------------------------------
geom_text(data = diff, aes(x = DemographicGroup, y = Difference, 
                           label = paste(Difference*100))) +
  

DemographicsPlot <- ggplot(ActiveLivesIGM, aes(x = DemographicGroup, y = Value*100, color = DemographicGroup)) + 
  theme_GS() +
  geom_point(size = 2.5) +
  geom_text(data = diff, aes(x = DemographicGroup, 
                             label = paste0((format(round(Difference*100,2))),"%"), 
                             y = 55, vjust = -1)) +
  scaleColGS(palette = "mixed") +
  coord_flip() +
  labs(y = "Inactivity Rate (%)") +
  scale_y_continuous(limits = c(0,60), expand = c(0.005, 0.005)) +
  theme(
    legend.position = "none"
  )

DemographicsAnimate <- DemographicsPlot +
  transition_states(Release,
                    transition_length = 2,
                    state_length = 1) +
  labs(title = "Inactivity Change by Demographics",
      subtitle = 'Release: {next_state}',
      caption = "Data: Sport England, Active Lives Adult Survey, Nov 15/16 to Nov 18/19",
      x = NULL
  )

animate(DemographicsAnimate, renderer = gifski_renderer())


DemographicsLine <- ggplot(diff, 
                           aes(x = Release, y = Difference*100, group = DemographicGroup)) +
  geom_line() +
  theme_GS() +
  scaleColGS(palette = "mixed") +
  transition_states(Release,
                    transition_length = 2,
                    state_length = 1) +
  labs(title = "Inactivity Change by Demographics",
       subtitle = 'Release: {next_state}',
       caption = "Data: Sport England, Active Lives Adult Survey, Nov 15/16 to Nov 18/19",
       x = NULL
  )

animate(DemographicsLine, renderer = gifski_renderer())

  
DemographicsLine <- ActiveLivesIGM %>% filter(DemographicGroup == "Age") %>%
  filter(!is.na(Value)) %>%
  ggplot(aes(x = Release, y = Value*100, group = Demographic, color = Demographic)) +
  geom_point() +
  geom_line() +
  theme_GS() +
  scaleColGS(palette = "mixed") +
  transition_states(Release,
                    transition_length = 2,
                    state_length = 1) +
  labs(title = "Inactivity Change by Demographics",
       subtitle = 'Release: {next_state}',
       caption = "Data: Sport England, Active Lives Adult Survey, Nov 15/16 to Nov 18/19",
       x = NULL
  ) +
  shadow_mark(past = T, future=F, alpha=0.3) 

animate(DemographicsLine, renderer = gifski_renderer())