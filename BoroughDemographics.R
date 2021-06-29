library(tidyverse); library(readxl); library(extrafont); library(ggtext); library(ggrepel);
library(ggfittext)

# Rounding ----------------------------------------------------------------

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Data Import and Wrangle -------------------------------------------------

##Import
ActiveLives <- read_excel("data/Active Lives Template v22.xlsx", 
                          sheet = "Full Adult Active Lives")

##Wrangle
ActiveLives <- ActiveLives %>%
  filter(Release != "Total change") %>%
  subset(select = -c(95:98, 102:108))

ActiveLives <- pivot_longer(ActiveLives, 3:97)

ActiveLives$Release <- factor(ActiveLives$Release, levels = 
                                c("Nov 15-16", "May 16-17", "Nov 16-17",
                                  "May 17-18", "Nov 17-18", "May 18-19",
                                  "Nov 18-19", "May 19-20", "Nov 19-20"), ordered = TRUE)



##Columns
ActiveLives <- ActiveLives %>%
  mutate(ActivityLevel = 
           ifelse(
             grepl("Fairly Active", name), "Fairly Active",
             ifelse(
               grepl("FA", name), "Fairly Active",
               ifelse(
                 grepl("Inactive", name), "Inactive",
                 ifelse(
                   grepl("I", name), "Inactive",
                   ifelse(
                     grepl("Moving", name), "Moving",
                     ifelse(
                       grepl("alking", name), "Walking", "Active"
                     )))))))


ActiveLives <- ActiveLives %>%
  mutate(DemographicType = 
           case_when(
             str_detect(name, "ale") ~"Gender",
             str_detect(name, "LT") ~"Disability",
             str_detect(name, "White") ~"Ethnicity",
             str_detect(name, "Black") ~"Ethnicity",
             str_detect(name, "Asian") ~"Ethnicity",
             str_detect(name, "Chinese") ~"Ethnicity",
             str_detect(name, "Mixed") ~"Ethnicity",
             str_detect(name, "ethnic") ~"Ethnicity",
             str_detect(name, "NS SEC") ~"Socio-economic",
             str_detect(name, "orking") ~"Employment",
             str_detect(name, "Unemployed") ~"Employment",
             str_detect(name, "Student") ~"Employment",
             str_detect(name, "4") ~"Age",
             str_detect(name, "5+") ~"Age",
             NULL
           ))

##Simplify column values
ActiveLives$name <- gsub(" FA", "", as.character(ActiveLives$name))
ActiveLives$name <- gsub(" I", "", as.character(ActiveLives$name))
ActiveLives$name <- gsub("Active - ", "", as.character(ActiveLives$name))
ActiveLives$name <- gsub("Fairly Active - ", "", as.character(ActiveLives$name))
ActiveLives$name <- gsub("Inactive - ", "", as.character(ActiveLives$name))
ActiveLives$name <- gsub("Moving - ", "", as.character(ActiveLives$name))
ActiveLives$name <- gsub("Inactive", "Total Population", as.character(ActiveLives$name))
ActiveLives$name <- gsub("Fairly Active", "Total Population", as.character(ActiveLives$name))
ActiveLives$Area <- gsub("GM", "Greater Manchester", as.character(ActiveLives$Area))
ActiveLives$name <- gsub("Fairly ", "", as.character(ActiveLives$name))


ActiveLives$Area <- factor(ActiveLives$Area, levels = 
                             c("England", "Greater Manchester",
                               "Bolton", "Bury", "Manchester",
                               "Oldham", "Rochdale", "Salford",
                               "Stockport", "Tameside",
                               "Trafford", "Wigan"), ordered = TRUE)

##Column rename
ActiveLives <- ActiveLives %>%
  rename("Demographic" = "name",
         "Value" = "value")


# Bolton ------------------------------------------------------------------

##Disability

ActiveLives %>%
  filter(Area == "Bolton") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability") %>%
  ggplot(       aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
    geom_line(size = 1) +
    geom_text(data = ActiveLives %>%
                filter(Area == "Bolton") %>%
                filter(ActivityLevel == "Inactive") %>%
                filter(DemographicType == "Disability") %>% 
                filter(Release == "Nov 15-16"),
              aes(label = Demographic),
              hjust = 0,
              vjust = -1,
              fontface = "bold") +
    geom_text(data = ActiveLives %>%
                filter(Area == "Bolton") %>%
                filter(ActivityLevel == "Inactive") %>%
                filter(DemographicType == "Disability") %>% 
                filter(Release == "Nov 19-20"),
              aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
              nudge_x = 0.3) +
    ylim(20,60) +
    theme_GS() +
    scale_color_manual(values = GS_cols %>% unname) +
    labs(title = "Bolton Inactivity Rate By Disability Status",
         y = "Inactivity Rate (%)",
         caption = "**Source:** Sport England Active Lives Survey") +
    theme(legend.position = "none",
          plot.caption = element_markdown())

##Gender
ActiveLives %>%
  filter(Area == "Bolton") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
    geom_line(size = 1) +
    geom_text(data = ActiveLives %>%
                filter(Area == "Bolton") %>%
                filter(ActivityLevel == "Inactive") %>%
                filter(DemographicType == "Gender")  %>% 
                filter(Release == "Nov 15-16"),
              aes(label = Demographic),
              hjust = 0,
              vjust = -1,
              fontface = "bold") +
    geom_text(data = ActiveLives %>%
                filter(Area == "Bolton") %>%
                filter(ActivityLevel == "Inactive") %>%
                filter(DemographicType == "Gender") %>% 
                filter(Release == "Nov 19-20"),
              aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
              nudge_x = 0.3) +
    ylim(20,40) +
    theme_GS() +
    scale_color_manual(values = GS_cols %>% unname) +
    labs(title = "Bolton Inactivity Rate By Gender",
         y = "Inactivity Rate (%)",
         caption = "**Source:** Sport England Active Lives Survey") +
    theme(legend.position = "none",
          plot.caption = element_markdown())

##Age

ActiveLives %>%
  filter(Area == "Bolton") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Age") %>% 
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
    geom_line(size = 1) +
    geom_text(data = ActiveLives %>%
                filter(Area == "Bolton") %>%
                filter(ActivityLevel == "Inactive") %>%
                filter(DemographicType == "Age") %>%
                filter(Demographic != "16-54") %>%
                filter(Demographic != "55+") %>%
                filter(Release == "Nov 15-16"),
              aes(label = Demographic),
              hjust = 0,
              vjust = -1,
              fontface = "bold") +
    geom_text(data = ActiveLives %>%
                filter(Area == "Bolton") %>%
                filter(ActivityLevel == "Inactive") %>%
                filter(DemographicType == "Age") %>%
                filter(Demographic != "16-54") %>%
                filter(Demographic != "55+") %>%
                filter(Release == "Nov 19-20"),
              aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
              nudge_x = 0.3) +
    ylim(10,70) +
    theme_GS() +
    scale_color_manual(values = GS_cols %>% unname) +
    labs(title = "Bolton Inactivity Rate By Age Group",
         y = "Inactivity Rate (%)",
         caption = "**Source:** Sport England Active Lives Survey") +
    theme(legend.position = "none",
          plot.caption = element_markdown())

##Socio-Economic

ActiveLives %>%
  filter(Area == "Bolton") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
    geom_line(size = 1) +
    geom_text(data = ActiveLives %>%
                filter(Area == "Bolton") %>%
                filter(ActivityLevel == "Inactive") %>%
                filter(DemographicType == "Socio-economic") %>%
                filter(Demographic != "NS SEC 9: Students and other") %>% 
                filter(Release == "Nov 15-16"),
              aes(label = c(
                "NS-SEC 1-2",
                "NS SEC 3-5",
                "NS SEC 6-8"
              )),
              hjust = 0,
              vjust = -1,
              fontface = "bold") +
    geom_text(data = ActiveLives %>%
                filter(Area == "Bolton") %>%
                filter(ActivityLevel == "Inactive") %>%
                filter(DemographicType == "Socio-economic") %>%
                filter(Demographic != "NS SEC 9: Students and other") %>%
                filter(Release == "Nov 19-20"),
              aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
              nudge_x = 0.3) +
    ylim(15,60) +
    theme_GS() +
    scale_color_manual(values = GS_cols %>% unname) +
    labs(title = "Bolton Inactivity Rate By Socio-Economic Group",
         y = "Inactivity Rate (%)",
         caption = "**Source:** Sport England Active Lives Survey") +
    theme(legend.position = "none",
          plot.caption = element_markdown())


# Bury --------------------------------------------------------------------

##Disability

ActiveLives %>%
  filter(Area == "Bury") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability") %>%
  ggplot(       aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = DisabilityInactive %>% filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Bury") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,50) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Bury Inactivity Rate By Disability Status",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Gender
ActiveLives %>%
  filter(Area == "Bury") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Bury") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Bury") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,30) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Bury Inactivity Rate By Gender",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Age

ActiveLives %>%
  filter(Area == "Bury") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Age") %>% 
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Bury") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text_repel(data = ActiveLives %>%
              filter(Area == "Bury") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,60) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Bury Inactivity Rate By Age Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Socio-Economic

ActiveLives %>%
  filter(Area == "Bury") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Bury") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = c(
              "NS-SEC 1-2",
              "NS SEC 3-5",
              "NS SEC 6-8"
            )),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Bury") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(15,45) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Bury Inactivity Rate By Socio-Economic Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())


# Manchester --------------------------------------------------------------

##Disability

ActiveLives %>%
  filter(Area == "Manchester") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability") %>%
  ggplot(       aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Manchester") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Manchester") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,50) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Manchester Inactivity Rate By Disability Status",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Gender
ActiveLives %>%
  filter(Area == "Manchester") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Manchester") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender")  %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text_repel(data = ActiveLives %>%
              filter(Area == "Manchester") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,35) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Manchester Inactivity Rate By Gender",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Age

ActiveLives %>%
  filter(Area == "Manchester") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Age") %>% 
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Manchester") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Manchester") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,75) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Manchester Inactivity Rate By Age Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Socio-Economic

ActiveLives %>%
  filter(Area == "Manchester") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Manchester") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = c(
              "NS-SEC 1-2",
              "NS SEC 3-5",
              "NS SEC 6-8"
            )),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Manchester") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,50) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Manchester Inactivity Rate By Socio-Economic Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())


# Oldham ------------------------------------------------------------------

##Disability

ActiveLives %>%
  filter(Area == "Oldham") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability") %>%
  ggplot(       aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Oldham") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Oldham") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,60) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Oldham Inactivity Rate By Disability Status",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Gender
ActiveLives %>%
  filter(Area == "Oldham") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Oldham") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender")  %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Oldham") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,40) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Oldham Inactivity Rate By Gender",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Age

ActiveLives %>%
  filter(Area == "Oldham") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Age") %>% 
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Oldham") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Oldham") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic == "16-34") %>%
              filter(Release == "May 16-17"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text_repel(data = ActiveLives %>%
              filter(Area == "Oldham") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,70) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Oldham Inactivity Rate By Age Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey
       \nDue to insufficient sample size inactivity rates for 16-34 year olds could not be calculated
       in May 2016-17.") +
  theme(legend.position = "none",
        plot.caption = element_markdown(),
        plot.subtitle = element_markdown())

##Socio-Economic

ActiveLives %>%
  filter(Area == "Oldham") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Oldham") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = c(
              "NS-SEC 1-2",
              "NS SEC 3-5",
              "NS SEC 6-8"
            )),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Oldham") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(15,50) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Oldham Inactivity Rate By Socio-Economic Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())



# Rochdale ----------------------------------------------------------------

##Disability

ActiveLives %>%
  filter(Area == "Rochdale") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability") %>%
  ggplot(       aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Rochdale") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Rochdale") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,60) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Rochdale Inactivity Rate By Disability Status",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Gender
ActiveLives %>%
  filter(Area == "Rochdale") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Rochdale") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender")  %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Rochdale") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,40) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Rochdale Inactivity Rate By Gender",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Age

ActiveLives %>%
  filter(Area == "Rochdale") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Age") %>% 
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text_repel(data = ActiveLives %>%
              filter(Area == "Rochdale") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Rochdale") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,70) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Rochdale Inactivity Rate By Age Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Socio-Economic

ActiveLives %>%
  filter(Area == "Rochdale") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Rochdale") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = c(
              "NS-SEC 1-2",
              "NS SEC 3-5",
              "NS SEC 6-8"
            )),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Rochdale") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(15,50) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Rochdale Inactivity Rate By Socio-Economic Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())



# Salford -----------------------------------------------------------------

##Disability

ActiveLives %>%
  filter(Area == "Salford") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability") %>%
  ggplot(       aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Salford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Salford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,60) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Salford Inactivity Rate By Disability Status",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Gender
ActiveLives %>%
  filter(Area == "Salford") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Salford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender")  %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Salford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,40) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Salford Inactivity Rate By Gender",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Age

ActiveLives %>%
  filter(Area == "Salford") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Age") %>% 
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Salford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Salford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,70) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Salford Inactivity Rate By Age Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Socio-Economic

ActiveLives %>%
  filter(Area == "Salford") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Salford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = c(
              "NS-SEC 1-2",
              "NS SEC 3-5",
              "NS SEC 6-8"
            )),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Salford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,50) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Salford Inactivity Rate By Socio-Economic Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())


# Stockport ---------------------------------------------------------------

##Disability

ActiveLives %>%
  filter(Area == "Stockport") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability") %>%
  ggplot(       aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Stockport") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Stockport") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,50) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Stockport Inactivity Rate By Disability Status",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Gender
ActiveLives %>%
  filter(Area == "Stockport") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Stockport") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender")  %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Stockport") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,30) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Stockport Inactivity Rate By Gender",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Age

ActiveLives %>%
  filter(Area == "Stockport") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Age") %>% 
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Stockport") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-34") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Stockport") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic == "16-34") %>%
              filter(Release == "May 18-19"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Stockport") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,60) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Stockport Inactivity Rate By Age Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey
       \nDue to insufficient sample size inactivity rates for 16-34 year olds could not be calculated
       for all releases.") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Socio-Economic

ActiveLives %>%
  filter(Area == "Stockport") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Stockport") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = c(
              "NS-SEC 1-2",
              "NS SEC 3-5",
              "NS SEC 6-8"
            )),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Stockport") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic == "NS SEC 6-8: Lower social groups") %>% 
              filter(Release == "Nov 16-17"),
            aes(label = "NS SEC 6-8"),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Stockport") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,40) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Stockport Inactivity Rate By Socio-Economic Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey
       \nDue to insufficient sample size inactivity rates for NS-SEC 6-8 could not be calculated
       for all releases.") +
  theme(legend.position = "none",
        plot.caption = element_markdown())


# Tameside ----------------------------------------------------------------

##Disability

ActiveLives %>%
  filter(Area == "Tameside") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability") %>%
  ggplot(       aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Tameside") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Tameside") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,60) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Tameside Inactivity Rate By Disability Status",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Gender
ActiveLives %>%
  filter(Area == "Tameside") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Tameside") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender")  %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Tameside") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,40) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Tameside Inactivity Rate By Gender",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Age

ActiveLives %>%
  filter(Area == "Tameside") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Age") %>% 
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Tameside") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Tameside") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,70) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Tameside Inactivity Rate By Age Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Socio-Economic

ActiveLives %>%
  filter(Area == "Tameside") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Tameside") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = c(
              "NS-SEC 1-2",
              "NS SEC 3-5",
              "NS SEC 6-8"
            )),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Tameside") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,50) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Tameside Inactivity Rate By Socio-Economic Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())



# Trafford ----------------------------------------------------------------

##Disability

ActiveLives %>%
  filter(Area == "Trafford") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability") %>%
  ggplot(       aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Trafford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Trafford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,60) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Trafford Inactivity Rate By Disability Status",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Gender
ActiveLives %>%
  filter(Area == "Trafford") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Trafford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender")  %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text_repel(data = ActiveLives %>%
              filter(Area == "Trafford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,40) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Trafford Inactivity Rate By Gender",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Age

ActiveLives %>%
  filter(Area == "Trafford") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Age") %>% 
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Trafford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Trafford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "16-34") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,70) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Trafford Inactivity Rate By Age Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey
       \nDue to insufficient sample size inactivity rates for 16-34 year olds could not be calculated
       for all releases.") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Socio-Economic

ActiveLives %>%
  filter(Area == "Trafford") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Trafford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = c(
              "NS-SEC 1-2",
              "NS SEC 3-5",
              "NS SEC 6-8"
            )),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Trafford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic == "NS SEC 6-8: Lower social groups") %>% 
              filter(Release == "Nov 16-17"),
            aes(label = "NS SEC 6-8"),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Trafford") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,60) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Trafford Inactivity Rate By Socio-Economic Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey
       \nDue to insufficient sample size inactivity rates for NS-SEC 6-8 could not be calculated
       for all releases.") +
  theme(legend.position = "none",
        plot.caption = element_markdown())



# Wigan -------------------------------------------------------------------

##Disability

ActiveLives %>%
  filter(Area == "Wigan") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability") %>%
  ggplot(       aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Wigan") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Wigan") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Disability") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,60) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Wigan Inactivity Rate By Disability Status",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Gender
ActiveLives %>%
  filter(Area == "Wigan") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Wigan") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender")  %>% 
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Wigan") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Gender") %>% 
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,40) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Wigan Inactivity Rate By Gender",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Age

ActiveLives %>%
  filter(Area == "Wigan") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Age") %>% 
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Wigan") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Wigan") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Age") %>%
              filter(Demographic != "16-54") %>%
              filter(Demographic != "55+") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,70) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Wigan Inactivity Rate By Age Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey
       \nDue to insufficient sample size inactivity rates for 16-34 year olds could not be calculated
       for all releases.") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

##Socio-Economic

ActiveLives %>%
  filter(Area == "Wigan") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLives %>%
              filter(Area == "Wigan") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>% 
              filter(Release == "Nov 15-16"),
            aes(label = c(
              "NS-SEC 1-2",
              "NS SEC 3-5",
              "NS SEC 6-8"
            )),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLives %>%
              filter(Area == "Wigan") %>%
              filter(ActivityLevel == "Inactive") %>%
              filter(DemographicType == "Socio-economic") %>%
              filter(Demographic != "NS SEC 9: Students and other") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(10,50) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Wigan Inactivity Rate By Socio-Economic Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())


# Faceting ----------------------------------------------------------------

##Disability 

facetDisabilty <- ActiveLives %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability") %>%
  filter(!is.na(Release)) %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) 

facetDisabilty + facet_wrap(vars(Area)) +
  labs(title = "Inactivity Rate By Disability Status",
       y = "Inactivity Rate (%)",
       x = "",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(plot.caption = element_markdown(),
        axis.text.x = element_text(angle = 90),
        legend.position = c(0.75,1.15),
        legend.direction = "horizontal")

##Gender

facetGender <- ActiveLives %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender") %>%
  filter(!is.na(Release)) %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname)

facetGender + facet_wrap(vars(Area)) +
  labs(title = "Inactivity Rate By Gender",
       y = "Inactivity Rate (%)",
       x = "",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(plot.caption = element_markdown(),
        axis.text.x = element_text(angle = 90),
        legend.position = c(0.75,1.15),
        legend.direction = "horizontal")

##Age

facetAge <- ActiveLives %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(!is.na(Release)) %>%
  filter(Demographic == "16-34" | Demographic == "75+") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname)

facetAge + facet_wrap(vars(Area)) +
  labs(title = "Inactivity Rate By Age",
       y = "Inactivity Rate (%)",
       x = "",
       caption = "**Source:** Sport England Active Lives Survey
       \nDue to insufficient sample size inactivity for 16-34 year olds can't be calculated for all releases.") +
  theme(plot.caption = element_markdown(),
        axis.text.x = element_text(angle = 90),
        legend.position = c(0.75,1.15),
        legend.direction = "horizontal")

##Socio Economic

facetSocioEcon <- ActiveLives %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(!is.na(Release)) %>%
  filter(Demographic == "NS SEC 1-2: Higher social groups" | Demographic == "NS SEC 6-8: Lower social groups") %>%
  ggplot(aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname,
                     labels = c("NS-SEC 1-2", "NS-SEC 6-8"))

facetSocioEcon + facet_wrap(vars(Area)) +
  labs(title = "Inactivity Rate By Socio-Economic Group",
       y = "Inactivity Rate (%)",
       x = "",
       caption = "**Source:** Sport England Active Lives Survey
       \nDue to insufficient sample size inactivity for NS-SEC 6-8 can't be calculated for all releases.") +
  theme(plot.caption = element_markdown(),
        axis.text.x = element_text(angle = 90),
        legend.position = c(0.75,1.15),
        legend.direction = "horizontal")