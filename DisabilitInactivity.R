library(tidyverse); library(readxl); library(extrafont); library(ggtext); library(ggrepel);
library(ggfittext)


# Import and Format -------------------------------------------------------


Disability <- read_excel("data/NGM Disability Secondary Demographics.xlsx", 
                                                                 sheet = "Inactive", nmax = 2)

Disability <- pivot_longer(Disability, 2:34)

Disability <- Disability %>%
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
             str_detect(name, "disability") ~"Disbility",
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

Disability$value <- Disability$value*100

DisabilityTypes <- read_excel("data/Disability.xlsx",
                              sheet = "Conditions")

DisabilityTypes <- pivot_longer(DisabilityTypes,2:14)

DisabilityTypes$value <- DisabilityTypes$value*100

DisabilityLimit <- read_excel("data/Disability.xlsx", sheet = "Sheet2")
DisabilityLimit <- pivot_longer(DisabilityLimit, 2:4)
DisabilityLimit$value <- DisabilityLimit$value*100

# Graphing ----------------------------------------------------------------

#Gender ==========
Disability %>%
  filter(DemographicType == "Gender") %>%
  filter(name != "Other") %>%
  ggplot(aes(y = str_wrap(name, 10), x = value, fill = Demographic, 
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
  labs(title = "Inactivity Rates by Disability and Gender",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.78, 0.15),
        legend.key.height=unit(0.5, "cm"),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

##Age =====

Disability %>%
  filter(DemographicType == "Age") %>%
  ggplot(aes(y = str_wrap(name, 10), x = value, fill = str_wrap(Demographic, 20), 
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
  labs(title = "Inactivity Rates by Disability and Age",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.8, 0.25),
        legend.key.height=unit(0.5, "cm"),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

##Qualification====
##REORDER

DisabilityQual <- Disability %>%
  filter(DemographicType == "Qualification") %>%
  filter(name != "Level 1 and below") %>%
  filter(name != "Another type of qualification")

DisabilityQual$name <- str_wrap(DisabilityQual$name, 12)

DisabilityQual$name <- factor(DisabilityQual$name, levels = c(
  "Level 4 or\nabove",
  "Level 3 and\nequivalents",
  "Level 2 and\nequivalents",
  "No\nqualifications"
))

DisabilityQual %>%
  ggplot(aes(y = name, x = value, fill = Demographic, 
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
  ylab("")  +
  xlab("Inactivity rate (%)") +
  labs(title = "Inactivity Rates by Disability and Qualifications",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.73, 0.1),
        legend.key.height=unit(0.5, "cm"),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

##Employment =========

Disability %>%
  filter(DemographicType == "Employment") %>%
  filter(name != "Not working - looking after house/children") %>%
  filter(name != "Not working - long term sick or disabled") %>%
  filter(name != "Other working status") %>%
  filter(name != "Student full or part time") %>%
  ggplot(aes(y = str_wrap(name, 10), x = value, fill = Demographic, 
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
  labs(title = "Inactivity Rates by Disability and Employment Status",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.7, 0.85),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

##Living Arrangements =======

Disability %>%
  filter(DemographicType == "Living Arrangements") %>%
  filter(name != "Multi-generational household") %>%
  ggplot(aes(y = str_wrap(name, 10), x = value, fill = Demographic, 
             label=paste(sprintf("%0.1f", round2(value, 1)),"%"))) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_text(color = "white", 
            family = "Verdana",
            fontface = "bold",
            position = position_dodge(0.9),
            hjust = 1) +  theme_GS() +
  scale_fill_manual(values = GS_cols %>% unname) +
  xlab("Inactivity rate (%)")  +
  ylab("") +
  labs(title = "Inactivity Rates by Disability and Living Arrangements",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = "bottom",
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))



# Conditions --------------------------------------------------------------

DisabilityTypes %>%
  filter(ActivityLevel == "Inactive") %>%
  ggplot(aes(x = value, y = reorder(name, -value), fill = ActivityLevel,
             label = paste(sprintf("%0.1f", round2(value, 1)),"%"))) +
  geom_bar(stat = "identity") +
  geom_bar_text(color = "white") +
  scale_y_discrete(limits = rev,
                   expand = c(0,0)) +
  labs(title = "Inactivity by Health Condition",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"
  ),
  legend.position = "none",
  plot.caption = element_markdown(),
  panel.grid.major.y = element_blank(), 
  panel.grid.major.x = element_line(color = "#a3a3a2"),
  axis.title.x = element_text(hjust = 1, vjust = 1)) +
  scale_fill_manual(values = GS_cols %>% unname,
                    breaks = c("Active", "Fairly Active")) +
  xlab("Inactivity Rate (%)")  +
  ylab("") 


# Limiting ----------------------------------------------------------------

DisabilityLimit %>%
  filter(ActivityLevel == "Inactive") %>%
  ggplot(aes(x = value, y = reorder(name, -value), fill = ActivityLevel,
             label = paste(sprintf("%0.1f", round2(value, 1)),"%"))) +
  geom_bar(stat = "identity") +
  geom_bar_text(color = "white") +
  scale_y_discrete(limits = rev,
                   expand = c(0,0)) +
  labs(title = "Inactivity by Disability Status",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"
  ),
  legend.position = "none",
  plot.caption = element_markdown(),
  panel.grid.major.y = element_blank(), 
  panel.grid.major.x = element_line(color = "#a3a3a2"),
  axis.title.x = element_text(hjust = 1, vjust = 1)) +
  scale_fill_manual(values = GS_cols %>% unname,
                    breaks = c("Active", "Fairly Active")) +
  xlab("Inactivity Rate (%)")  +
  ylab("") 

