library(tidyverse); library(readxl); library(extrafont); library(ggtext); library(ggrepel);
library(ggfittext)

# Import and Wrangle ------------------------------------------------------

ethnicity <- read_excel("data/GM Ethnicity Secondary Demographics.xlsx",
                        sheet = "Inactive")

ethnicity <- pivot_longer(ethnicity, 2:18)

ethnicity <- ethnicity %>%
  mutate(DemographicType = 
           case_when(
             str_detect(name, "Male") ~"Gender",
             str_detect(name, "Female") ~"Gender",
             str_detect(name, "Non ") ~"Ethnicity",
             str_detect(name, "White") ~"Ethnicity",
             str_detect(name, "Black") ~"Ethnicity",
             str_detect(name, "Asian") ~"Ethnicity",
             str_detect(name, "Chinese") ~"Ethnicity",
             str_detect(name, "Mixed") ~"Ethnicity",
             str_detect(name, "ethnic") ~"Ethnicity",
             str_detect(name, "isability") ~"Disability",
             str_detect(name, "NS SEC") ~"Socio-Economic",
             str_detect(name, "orking") ~"Employment",
             str_detect(name, "Unemployed") ~"Employment",
             str_detect(name, "Student") ~"Employment",
             str_detect(name, "Level") ~"Qualification",
             str_detect(name, "qualification") ~"Qualification",
             str_detect(name, "4") ~"Age",
             str_detect(name, "5+") ~"Age",
             str_detect(name, "Single") ~"Living Arrangements",
             str_detect(name, "parent") ~"Living Arrangements",
             str_detect(name, "Couple") ~"Living Arrangements",
             str_detect(name, "household") ~"Living Arrangements",
             str_detect(name, "Other") ~"Gender",
             str_detect(name, "Christian") ~"Religion",
             str_detect(name, "Buddhist") ~"Religion",
             str_detect(name, "Hindu") ~"Religion",
             str_detect(name, "Jewish") ~"Religion",
             str_detect(name, "Muslim") ~"Religion",
             str_detect(name, "Sikh") ~"Religion",
             str_detect(name, "religion") ~"Religion",
             NULL
           ))

ethnicity$value <- ethnicity$value*100

# Graphs ------------------------------------------------------------------

#Gender ==========

ethnicity %>%
  filter(DemographicType == "Gender") %>%
  filter(name != "Other") %>%
  filter(Demographic != "Other ethnic group") %>%
  filter(Demographic != "Mixed") %>%
  filter(Demographic != "Chinese") %>%
  ggplot(aes(y = str_wrap(Demographic, 10), x = value, fill = name, 
             label=paste(sprintf("%0.1f", round2(value, 1)),"%"))) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_text(color = "white", 
            family = "Verdana",
            fontface = "bold",
            position = position_dodge(0.9),
            hjust = 1) +
  theme_GS() +
  scale_fill_manual(values = GS_cols %>% unname) +
  xlab("Inactivity rate (%)")  +
  ylab("") +
  labs(title = "Inactivity Rates by Ethnicity and Gender",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.78, 0.91),
        legend.key.height=unit(0.5, "cm"),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

#Age ====

ethnicity %>%
  filter(DemographicType == "Age") %>%
  filter(Demographic != "Other ethnic group") %>%
  filter(Demographic != "Mixed") %>%
  filter(Demographic != "Chinese") %>%
  filter(Demographic != "Black") %>%
  ggplot(aes(y = str_wrap(Demographic, 10), x = value, fill = name, 
             label=paste(sprintf("%0.1f", round2(value, 1)),"%"))) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_text(color = "white", 
            family = "Verdana",
            fontface = "bold",
            position = position_dodge(0.9),
            hjust = 1) +
  theme_GS() +
  scale_fill_manual(values = GS_cols %>% unname) +
  xlab("Inactivity rate (%)")  +
  ylab("") +
  labs(title = "Inactivity Rates by Ethnicity and Age",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.78, 0.91),
        legend.key.height=unit(0.5, "cm"),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

#Disability ====

ethnicity %>%
  filter(DemographicType == "Disability") %>%
  filter(Demographic != "Other ethnic group") %>%
  filter(Demographic != "Mixed") %>%
  filter(Demographic != "Chinese") %>%
  filter(Demographic != "Black") %>%
  filter(Demographic != "White Other") %>%
  ggplot(aes(y = str_wrap(Demographic, 10), x = value, fill = name, 
             label=paste(sprintf("%0.1f", round2(value, 1)),"%"))) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_text(color = "white", 
            family = "Verdana",
            fontface = "bold",
            position = position_dodge(0.9),
            hjust = 1) +
  theme_GS() +
  scale_fill_manual(values = GS_cols %>% unname) +
  xlab("Inactivity rate (%)")  +
  ylab("") +
  labs(title = "Inactivity Rates by Ethnicity and Disability Status",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.78, 0.91),
        legend.key.height=unit(0.5, "cm"),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))