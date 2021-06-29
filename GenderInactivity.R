library(tidyverse); library(readxl); library(extrafont); library(ggtext); library(ggrepel);
library(ggfittext)


# Import and Wrangle ------------------------------------------------------


Gender <- read_excel("data/GM Gender Secondary demographics.xlsx", 
                     sheet = "Inactive", n_max = 2)

Gender <- pivot_longer(Gender, 2:32)

Gender <- Gender %>%
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

Gender$value <- Gender$value*100

Gender <- Gender %>% rename( "Demographic" = "...1")


# Graphing ----------------------------------------------------------------

# Age ======

Gender %>%
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
  labs(title = "Inactivity Rates by Gender and Age",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.76, 0.12),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

# Disability =====

Gender %>%
  filter(DemographicType == "Disability") %>%
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
  labs(title = "Inactivity Rates by Gender and Disability Status",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

# Ethnicity ====

Gender %>%
  filter(DemographicType == "Ethnicity") %>%
  filter(name != "Other ethnic group") %>%
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
  labs(title = "Inactivity Rates by Gender and Ethnicity",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

# Socio-Econ ====

SocioEconGender <- Gender %>%
  filter(DemographicType == "Socio-Economic")

SocioEconGender$name <- str_wrap(SocioEconGender$name, 20)

SocioEconGender$name <- factor(SocioEconGender$name, levels = c(
  "NS SEC 1-2: Higher\nsocial groups",
  "NS SEC 3-5: Middle\nsocial groups",
  "NS SEC 6-8: Lower\nsocial groups",
  "NS SEC 9: Students\nand other /\nunclassified"
))

SocioEconGender %>%
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
  xlab("Inactivity rate (%)")  +
  ylab("") +
  labs(title = "Inactivity Rates by Gender and Socio-Economic Status",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

# Living Arrangements ====

Gender %>%
  filter(DemographicType == "Living Arrangements") %>%
  filter(name != "Multi-generational household") %>%
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
  labs(title = "Inactivity Rates by Gender and Living Arrangement",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

# Employmnet ====

Gender %>%
  filter(DemographicType == "Employment") %>%
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
  labs(title = "Inactivity Rates by Gender and Employment Status",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

# Qualification ####

GenderQual <- Gender %>%
  filter(DemographicType == "Qualification") 

GenderQual$name <- str_wrap(GenderQual$name, 10)

GenderQual$name <- factor(GenderQual$name, levels = c(
  "No\nqualifications",
  "Another\ntype of\nqualification",
  "Level 1\nand below",
  "Level\n2 and\nequivalents",
  "Level\n3 and\nequivalents",
  "Level 4 or\nabove"
))

GenderQual %>%
  filter(name !=   "Level 1\nand below") %>%
  ggplot(aes(y = name, x = value, fill = str_wrap(Demographic, 20), 
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
  labs(title = "Inactivity Rates by Gender and Qualification Level",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))