library(tidyverse); library(readxl); library(extrafont); library(ggtext); library(ggrepel);
library(ggfittext)


# Import and Format -------------------------------------------------------


SocioEcon <- read_excel("data/NS-SEC Secondary demongraphics.xlsx", 
                        sheet = "Simple Inactive")

SocioEcon <- pivot_longer(SocioEcon, 2:32)

SocioEcon <- SocioEcon %>%
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

SocioEcon$value <- SocioEcon$value*100

SocioEcon$Demographic <- factor(SocioEcon$Demographic, levels = c(
  "NS SEC 1-2: Higher social groups",
  "NS SEC 3-5: Middle social groups",
  "NS SEC 6-8: Lower social groups",
  "NS SEC 9: Students and other / unclassified"
))

# Graphing ----------------------------------------------------------------

#Gender ==========

SocioEconGender <- SocioEcon %>%
  filter(DemographicType == "Gender") %>%
  filter(name != "Other")

SocioEconGender %>%
  ggplot(aes(y = str_wrap(name, 10), x = value, fill = str_wrap(Demographic, 15), 
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
  labs(title = "Inactivity Rates",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = "bottom",
        legend.key.height=unit(0.5, "cm"),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))


##Age =====

SocioEcon %>%
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
  labs(title = "Inactivity Rates by Socio-Economic Status and Age",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.8, 0.25),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

##Qualification====

SocioEcon %>%
  filter(DemographicType == "Qualification") %>%
  filter(name != "Level 1 and below") %>%
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
  ylab("Inactivity rate (%)")  +
  xlab("") +
  labs(title = "Inactivity Rates by Socio-Economic Status and Qualification Level",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.73, 0.29),
        legend.key.height=unit(0.5, "cm"),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

##Employment =========

SocioEcon %>%
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
  ylab("Inactivity rate (%)")  +
  xlab("") +
  labs(title = "Inactivity Rates",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.7, 0.85),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))

##Living Arrangements =======
##fix legend
SocioEcon %>%
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
  ylab("Inactivity rate (%)")  +
  xlab("") +
  labs(title = "Inactivity Rates by Socio-Economic Status and Living Arrangements",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = c(0.32, 0.9),
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))


# Disability --------------------------------------------------------------
SocioEconDisability <- SocioEcon %>%
  filter(DemographicType == "Disability")


SocioEconDisability %>%
  ggplot(aes(y = name, x = value, fill = str_wrap(Demographic, 15), 
             label=paste(sprintf("%0.1f", round2(value, 1)),"%"))) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_text(color = "white", 
            family = "Verdana",
            fontface = "bold",
            position = position_dodge(0.9),
            hjust = 1) +  theme_GS() +
  scale_fill_manual(values = GS_cols %>% unname) +
  scale_y_discrete(limits = rev) +
  xlab("Inactivity rate (%)")  +
  ylab("") +
  labs(title = "Inactivity Rates by Socio-Economic Status and Disability",
       caption = "**Source:** *Sport England, Active Lives Survey, November 2019-20*",
       fill = "Activity Level") +
  theme(plot.caption = element_markdown(),
        legend.position = "bottom",
        axis.title.x = element_text(hjust = 1, vjust = 1),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2"))
