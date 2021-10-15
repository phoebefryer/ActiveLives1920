library(tidyverse); library(readxl); library(extrafont); library(ggtext); library(ggrepel);
library(ggfittext)


# Import and Wrangle ------------------------------------------------------

Age <- read_excel("data/GM Age Secondary Demographics.xlsx",
                  sheet = "Inactivity")

Age <- pivot_longer(Age, 2:32)

Age <- Age %>%
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

Age$value <- Age$value*100

ageSmall <- read.csv("data/AgeSmall.csv")

ageSmall$Inactive <- ageSmall$Inactive*100

# Graphs ------------------------------------------------------------------

# Disability ====

Age %>%
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
  labs(title = "Inactivity Rates by Age and Disability Status",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

# Ethnicity ====

Age %>%
  filter(DemographicType == "Ethnicity") %>%
  filter(name != "Other ethnic group") %>%
  filter(name != "Mixed") %>%
  filter(name != "Chinese") %>%
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
  labs(title = "Inactivity Rates by Age and Ethnicity",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

#Living Arrangements ====

Age %>%
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
  labs(title = "Inactivity Rates by Age and Ethnicity",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

# Employment ====

Age %>%
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
  labs(title = "Inactivity Rates by Age and Employment Status",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

# Qualification ====

AgeQual <- Age %>%
  filter(DemographicType == "Qualification")

AgeQual$name <- str_wrap(AgeQual$name, 10)

AgeQual$name <- factor(AgeQual$name, levels = c(
  "No\nqualifications",
  "Another\ntype of\nqualification",
  "Level 1\nand below",
  "Level\n2 and\nequivalents",
  "Level\n3 and\nequivalents",
  "Level 4 or\nabove"
))

AgeQual %>%
  filter(name != "Level 1\nand below") %>%
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
  labs(title = "Inactivity Rates by Age and Qualification Level",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.9, 0.88),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

# Socio-Economic ====

AgeSocioEcon <-  Age %>%
  filter(DemographicType == "Socio-Economic")

AgeSocioEcon$name <- str_wrap(AgeSocioEcon$name, 15)


AgeSocioEcon$name <- factor(AgeSocioEcon$name, levels = c(
  "NS SEC 1-2:\nHigher social\ngroups",
  "NS SEC 3-5:\nMiddle social\ngroups",
  "NS SEC 6-8:\nLower social\ngroups",
  "NS SEC 9:\nStudents\nand other /\nunclassified"
))

AgeSocioEcon %>%
  filter(Demographic != "75+") %>%
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
  labs(title = "Inactivity Rates by Age and Socio-Economic Status",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.84, 0.2),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))


# 5 Year Breaks -----------------------------------------------------------

ageSmall %>%
  ggplot(aes(x = Age, y = Inactive, 
             label=paste(sprintf("%0.1f", round2(Inactive, 1)),"%"))) +
  geom_bar(stat = "identity",
           fill = "#5B2D86") +
  geom_text(color = "white", 
            family = "Verdana",
            fontface = "bold",
            angle = 90,
            hjust = 1.1) +
  theme_GS() +
  scale_fill_manual(values = GS_cols %>% unname) +
  xlab("")  +
  ylab("Inactivity Rate (%)") +
  labs(title = "Inactivity Rates by Age",
       caption = "**Source:** Sport England, Active Lives Survey, November 2019-20") +
  theme(plot.caption = element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))


# Older adults ------------------------------------------------------------

age5070 <- read.csv("data/age5070.csv")

age5070 %>%
  filter(ActivityLevel == "Inactive") %>%
  ggplot(aes(x = Demographic, y = Percent, fill = Release, label = paste(Percent, "%"))) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_text(color = "white", 
            family = "Verdana",
            fontface = "bold",
            position = position_dodge(0.9),
            vjust = 1.2) +
  theme_GS() +
  ylab("Inactivity Rate (%)") +
  xlab("") +
  scale_fill_manual(values = GS_cols %>% unname) +
  labs(title = "Inactivity Rates Amongst Older Adults in Greater Manchester",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.1,0.9))
