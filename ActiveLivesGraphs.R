library(tidyverse); library(readxl); library(extrafont); library(ggtext); library(ggrepel);
library(ggfittext)


# Rounding ----------------------------------------------------------------
#This is just a function, this doesn't need editing just run it

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Data Import and Wrangle -------------------------------------------------
## This is to set up all the data you will need for the graphs.
## Most of this you can just run straight away only a couple of pieces of code need changing
## these are generally to do with releases and changing for the latest one

## Import base data ----
### This can be updated to the latest version or wherever you have it saved, if you are copying
### a file location you need to reverse the \ e.g. Documents\Active Lives becomes Documents/Active Lives

ActiveLives <- read_excel("data/Active Lives Template v22.xlsx", 
                          sheet = "Full Adult Active Lives")

### Wrangle, this is dropping certain rows and columns that we don't need
ActiveLives <- ActiveLives %>%
  filter(Release != "Total change") %>%
  subset(select = -c(95:98, 102:108))

### Switch from wide to long
ActiveLives <- pivot_longer(ActiveLives, 3:97)

### remove non-release values
ActiveLives <- ActiveLives %>%
  filter(Release != "12 monthchange") %>%
  filter(Release != "Total change")


### Columns ----
#### Adding in a new column based on activity level
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


#### New column to group demographics
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

#### Simplify column values
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

#### Column rename
ActiveLives <- ActiveLives %>%
  rename("Demographic" = "name",
         "Value" = "value")

### Release order ----
#### You need to add new releases
ActiveLives$Release <- factor(ActiveLives$Release, levels = 
                                c("Nov 15-16", "May 16-17", "Nov 16-17",
                                  "May 17-18", "Nov 17-18", "May 18-19",
                                  "Nov 18-19", "May 19-20", "Nov 19-20"), ordered = TRUE)

### Area order ----
ActiveLives$Area <- factor(ActiveLives$Area, levels = 
                             c("England", "Greater Manchester",
                               "Bolton", "Bury", "Manchester",
                               "Oldham", "Rochdale", "Salford",
                               "Stockport", "Tameside",
                               "Trafford", "Wigan"), ordered = TRUE)

## Percent dataset ----------------------------------------------------------------

ActiveLivesPercents <- ActiveLives %>%
  filter(Demographic != "Numbers")

ActiveLivesPercents$Value <- ActiveLivesPercents$Value*100

### Latest and earliest
### Amend latest release
ActiveLivesSummary <- ActiveLives %>%
  filter(Release == "Nov 15-16" | Release == "Nov 19-20")

### Inactive only ----

ActiveLivesInactivePop <- ActiveLives %>% 
  filter(ActivityLevel == "Inactive") %>%
  filter(Demographic == "Total Population") 

ActiveLivesInactivePop$Value <- ActiveLivesInactivePop$Value*100

ActiveLivesSummaryInactivePop <- ActiveLivesSummary %>% 
  filter(ActivityLevel == "Inactive") %>%
  filter(Demographic == "Total Population")

#### Latest data ----
# Need to amend the latest release
inactiveDataEnd <-  ActiveLives %>% 
  filter(ActivityLevel == "Inactive") %>%
  filter(Demographic == "Total Population") %>%
  filter(Release == "Nov 19-20")

## GM Inactive - all demographics ----

ActiveLivesGM <- ActiveLives %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(Area == "Greater Manchester") %>%
  filter(Demographic != "Numbers") %>%
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+")

## Demographics ----
### NS-SEC ----

NSSECInactive <- ActiveLives %>%
  filter(Area == "Greater Manchester") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other")

NSSECInactive$Value <- NSSECInactive$Value*100 

NSSECInactiveStudent <- ActiveLives %>%
  filter(Area == "Greater Manchester") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") 

NSSECInactiveStudent$Value <- NSSECInactiveStudent$Value*100 

NSSECInactiveArea <- ActiveLives %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Socio-economic") %>%
  filter(Demographic != "NS SEC 9: Students and other")

NSSECInactiveArea$Value <- NSSECInactiveArea$Value*100 

  
### Gender ----

GenderInactive <- ActiveLives %>%
  filter(Area == "Greater Manchester") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Gender")

GenderInactive$Value <- GenderInactive$Value*100 

### Age ----

AgeInactive <- ActiveLives %>%
  filter(Area == "Greater Manchester") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Age") %>%
  filter(Demographic != "16-54") %>%
  filter(Demographic != "55+")

AgeInactive$Value <- AgeInactive$Value*100 

### Disability ----

DisabilityInactive <- ActiveLives %>%
  filter(Area == "Greater Manchester") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Disability")

DisabilityInactive$Value <- DisabilityInactive$Value*100 

### Ethnicity ----

EthnicityInactive <- ActiveLives %>%
  filter(Area == "Greater Manchester") %>%
  filter(ActivityLevel == "Inactive") %>%
  filter(DemographicType == "Ethnicity") %>%
  filter(Demographic != "Chinese" ) %>%
  filter(Demographic != "Mixed")

EthnicityInactive$Value <- EthnicityInactive$Value*100 


## Gaps Import -------------------------------------------------------------

ActiveLivesGaps <- read_excel("data/Active Lives Template v22.xlsx", 
                          sheet = "Full Adult Active Lives")

## Wrangle and drop all unnecessary columns
ActiveLivesGaps <- ActiveLivesGaps %>%
  filter(Release != "Total change") %>%
  subset(select = -c(3:94, 99:108))

ActiveLivesGaps <- pivot_longer(ActiveLivesGaps, 3:6)

ActiveLivesGaps$Area <- gsub("GM", "Greater Manchester", as.character(ActiveLivesGaps$Area))

### Release order ----
ActiveLivesGaps$Release <- factor(ActiveLivesGaps$Release, levels = 
                                c("Nov 15-16", "May 16-17", "Nov 16-17",
                                  "May 17-18", "Nov 17-18", "May 18-19",
                                  "Nov 18-19", "May 19-20", "Nov 19-20"), ordered = TRUE)

### Area order ----
ActiveLivesGaps$Area <- factor(ActiveLivesGaps$Area, levels = 
                             c("England", "Greater Manchester",
                               "Bolton", "Bury", "Manchester",
                               "Oldham", "Rochdale", "Salford",
                               "Stockport", "Tameside",
                               "Trafford", "Wigan"), ordered = TRUE)

## Column rename
ActiveLivesGaps <- ActiveLivesGaps %>%
  rename("Gap" = "name",
         "Value" = "value")

ActiveLivesGaps$Value <- ActiveLivesGaps$Value*-100


# Graphs ------------------------------------------------------------------

## GM and England comparison ----
#### GreaterSport Branding ----

ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "England") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "England") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "England") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  # ylim() sets the y-axis limits so this may need changing on some graphs where new releases
  # fall outside the exisiting limits. This can be removed with no issue and won't break
  # anything it just makes for neater graphs in some instances
  ylim(20, 35) +
  labs(title = "Inactivity Levels Over Time",
    caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"),
    plot.caption = element_markdown(),
    legend.position = "none") +
  scale_color_manual(values = GS_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")

#### GMM Branding ----

ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "England") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "England") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "England") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  ylim(20, 35) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GMM2() +
  theme(plot.title = element_text(),
    plot.caption = element_markdown(),
    legend.position = "none") +
  scale_color_manual(values = GMM_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")

#### GMM Line Yearly Releases ----
##### Just shows annual data, each November release
##### GMM branding

ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "England") %>%
  filter(Release != "May 16-17") %>%
  filter(Release != "May 17-18") %>%
  filter(Release != "May 18-19") %>%
  filter(Release != "May 19-20") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "England") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "England") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  ylim(20, 35) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GMM2() +
  theme(plot.title = element_text(),
        plot.caption = element_markdown(),
        legend.position = "none") +
  scale_color_manual(values = GMM_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")

## Borough Comparison ----

ActiveLivesPercents %>% filter(Release == "Nov 19-20") %>%
  filter(ActivityLevel == "Active" | ActivityLevel == "Fairly Active") %>%
  filter(Demographic == "Total Population") %>%
  ggplot(aes(x = Value, y = Area, fill = ActivityLevel,
             label = paste(sprintf("%0.1f", round2(Value, 1)),"%"))) +
  geom_bar(stat = "identity") +
  geom_bar_text(color = "white") +
  geom_text(data = ActiveLivesPercents %>%
              filter(Release == "Nov 19-20") %>%
              filter(Demographic == "Moving"),
            aes(label = paste(sprintf("%0.1f", round2(Value, 1)),"%")),
            hjust = -0.1,
            fontface = "bold") +
  scale_y_discrete(limits = rev,
                   expand = c(0,0)) +
  scale_x_continuous(limits = c(0,100),
                     expand = c(0,2)) +
  labs(title = "Moving Rates by Area",
       caption = "**Source:** Sport England, Active Lives Survey, November 2019-20",
       fill = "Activity Level") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"
  ),
  legend.position = c(0.9, 0.87),
  panel.grid.major.x = element_line(color = "#a3a3a2"), 
  panel.grid.major.y = element_blank()
  ) +
  scale_fill_manual(values = GS_cols %>% unname,
                    breaks = c("Active", "Fairly Active")) +
  xlab("Activity rate (%)")  +
  ylab("") 

## Borough Lines -----------------------------------------------------------

### Bolton ----

ActiveLivesInactivePop %>%
  filter(Area == "Bolton"|Area == "Greater Manchester") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Bolton"|Area == "Greater Manchester") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Bolton") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -0.25,
            fontface = "bold") +
  ylim(25, 40) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"),
    plot.caption = element_markdown(),
    legend.position = "none") +
  scale_color_manual(values = GS_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")

### Bury ----

ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "Bury") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Bury") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4,
            hjust = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Bury") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  ylim(20, 35) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"),
    plot.caption = element_markdown(),
    legend.position = "none") +
  scale_color_manual(values = GS_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")


### Manchester ---- 

ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "Manchester") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Manchester") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4,
            nudge_x = 0.2) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Manchester") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  ylim(20, 35) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"),
    plot.caption = element_markdown(),
    legend.position = "none") +
  scale_color_manual(values = GS_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")


### Oldham ---- 

ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "Oldham") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Oldham") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Oldham") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  ylim(20, 35) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"),
    plot.caption = element_markdown(),
    legend.position = "none") +  
  scale_color_manual(values = GS_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")

### Rochdale ----


ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "Rochdale") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Rochdale") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Rochdale") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  ylim(20, 40) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"),
    plot.caption = element_markdown(),
    legend.position = "none") +
  scale_color_manual(values = GS_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")

### Salford ----

ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "Salford") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Salford") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Salford") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  ylim(20, 35) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"),
    plot.caption = element_markdown(),
    legend.position = "none") +
  scale_color_manual(values = GS_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")

### Stockport ----

ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "Stockport") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Stockport") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Stockport") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  ylim(15, 35) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"),
    plot.caption = element_markdown(),
    legend.position = "none") +
  scale_color_manual(values = GS_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")

### Tameside ----

ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "Tameside") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Tameside") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.5,
            nudge_x = 0.2) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Tameside") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  ylim(20, 35) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"),
    plot.caption = element_markdown(),
    legend.position = "none") +
  scale_color_manual(values = GS_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")

### Trafford ----

ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "Trafford") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Trafford") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Trafford") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -0.7,
            fontface = "bold") +
  ylim(20, 35) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"),
    plot.caption = element_markdown(),
    legend.position = "none") +
  scale_color_manual(values = GS_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")

### Wigan ----

ActiveLivesInactivePop %>%
  filter(Area == "Greater Manchester"|Area == "Wigan") %>%
  ggplot(aes(x = Release, y = Value, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Wigan") %>%
              filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.4) +
  geom_text(data = ActiveLivesInactivePop %>%
              filter(Area == "Greater Manchester"|Area == "Wigan") %>%
              filter(Release == "Nov 15-16"),
            aes(label = Area),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  ylim(20, 40) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** Sport England, Active Lives Survey") +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"),
    plot.caption = element_markdown(),
    legend.position = "none") +
  scale_color_manual(values = GS_cols %>% unname) +
  ylab("Inactivity rate (%)") +
  xlab("")


## Demographics -----------------------------------------------------

### NS-SEC ----

#### GM ----

ggplot(data = NSSECInactive,
       aes(x = Release, y = Value, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = NSSECInactive %>% filter(Release == "Nov 15-16"),
            aes(label = c(
              "NS-SEC 1-2",
              "NS SEC 3-5",
              "NS SEC 6-8"
            )),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = NSSECInactive %>% filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(15,45) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Greater Manchester Inactivity Rate By Socio-Economic Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

#### Borough Gap Comparison ----

ActiveLivesGaps %>%
  filter(Release == "Nov 19-20") %>%
  filter(Gap == "Inactive Socio-Economic Gap") %>%
  filter(Area != "Trafford") %>%
  ggplot(aes(x = Value, y = Area, fill = Gap,
             label = paste(sprintf("%0.1f", round2(Value, 1)),"%"))) +
  geom_bar(stat = "identity") +
  geom_bar_text(color = "white") +
  labs(title = "Socio-Economic Inactivity Gap by Area",
       caption = "**Source:** Sport England, Active Lives Survey, November 2019-20",
       fill = "Activity Level") +
  scale_y_discrete(limits = rev,
                   expand = c(0,0)) +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"
  ),
  legend.position = c(0.9, 0.87),
  plot.caption = element_markdown(),
  panel.grid.major.y = element_blank(), 
  panel.grid.major.x = element_line(color = "#a3a3a2"),
  axis.title.x = element_text(hjust = 1, vjust = 1)) +
  scale_fill_manual(values = GS_cols %>% unname,
                    breaks = c("Active", "Fairly Active")) +
  xlab("Inactivity Gap")  +
  ylab("") 



### Age ----

ggplot(data = AgeInactive,
       aes(x = Release, y = Value, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = AgeInactive %>% filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = AgeInactive %>% filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(15,60) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Greater Manchester Inactivity Rate By Age Group",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

#### Borough Comparison ----

ActiveLivesGaps %>%
  filter(Release == "Nov 19-20") %>%
  filter(Gap == "Inactivity Age Gap") %>%
  ggplot(aes(x = Value, y = Area, fill = Gap,
             label = paste(sprintf("%0.1f", round2(Value, 1)),"%"))) +
  geom_bar(stat = "identity") +
  geom_bar_text(color = "white") +
  labs(title = "Inactivity Age Gap by Area",
       caption = "**Source:** Sport England, Active Lives Survey, November 2019-20",
       fill = "Activity Level") +
  scale_y_discrete(limits = rev,
                   expand = c(0,0)) +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"
  ),
  legend.position = c(0.9, 0.87),
  plot.caption = element_markdown(),
  panel.grid.major.y = element_blank(), 
  panel.grid.major.x = element_line(color = "#a3a3a2"),
  axis.title.x = element_text(hjust = 1, vjust = 1)) +
  scale_fill_manual(values = GS_cols %>% unname,
                    breaks = c("Active", "Fairly Active")) +
  xlab("Inactivity Gap")  +
  ylab("") 

### Gender ----

ggplot(data = GenderInactive,
       aes(x = Release, y = Value, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = GenderInactive %>% filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = GenderInactive %>% filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,35) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Greater Manchester Inactivity Rate By Gender",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

#### Borough Comparison ----

ActiveLivesGaps %>%
  filter(Release == "Nov 19-20") %>%
  filter(Gap == "Inactive Gender Gap") %>%
  ggplot(aes(x = abs(Value), y = Area, fill = Gap,
             label = paste(sprintf("%0.1f", round2(abs(Value), 1)),"%"))) +
  geom_bar(stat = "identity") +
  geom_bar_text(color = "black") +
  labs(title = "Gender Inactivity Gap by Area",
       caption = "**Source:** Sport England, Active Lives Survey, November 2019-20",
       fill = "Activity Level") +
  scale_y_discrete(limits = rev,
                   expand = c(0,0)) +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"
  ),
  legend.position = c(0.9, 0.87),
  plot.caption = element_markdown(),
  panel.grid.major.y = element_blank(), 
  panel.grid.major.x = element_line(color = "#a3a3a2"),
  axis.title.x = element_text(hjust = 1, vjust = 1)) +
  scale_fill_manual(values = GS_cols %>% unname,
                    breaks = c("Active", "Fairly Active")) +
  xlab("Inactivity Gap")  +
  ylab("") 

### Disability ----

ggplot(data = DisabilityInactive,
       aes(x = Release, y = Value, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = DisabilityInactive %>% filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = DisabilityInactive %>% filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,50) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Greater Manchester Inactivity Rate By Disability Status",
       y = "Inactivity Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

#### Borough Comparison ----

ActiveLivesGaps %>%
  filter(Release == "Nov 19-20") %>%
  filter(Gap == "Inactive Disability Gap") %>%
  ggplot(aes(x = Value, y = Area, fill = Gap,
             label = paste(sprintf("%0.1f", round2(Value, 1)),"%"))) +
  geom_bar(stat = "identity") +
  geom_bar_text(color = "white") +
  labs(title = "Disability Inactivity Gap by Area",
       caption = "**Source:** Sport England, Active Lives Survey, November 2019-20",
       fill = "Activity Level") +
  scale_y_discrete(limits = rev,
                   expand = c(0,0)) +
  theme_GS() +
  theme(plot.title = element_text(
    color = "#5B2D86"
  ),
  legend.position = c(0.9, 0.87),
  plot.caption = element_markdown(),
  panel.grid.major.y = element_blank(), 
  panel.grid.major.x = element_line(color = "#a3a3a2"),
  axis.title.x = element_text(hjust = 1, vjust = 1)) +
  scale_fill_manual(values = GS_cols %>% unname,
                    breaks = c("Active", "Fairly Active")) +
  xlab("Inactivity Gap")  +
  ylab("") 

### Ethnicity ----

ggplot(data = EthnicityInactive,
       aes(x = Release, y = Value, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = EthnicityInactive %>% filter(Release == "Nov 15-16"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = EthnicityInactive %>% filter(Release == "Nov 19-20"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_x = 0.3) +
  ylim(20,50) +
  theme_GS() +
  scale_color_manual(values = c("#5B2D86", "#D21C60", "#8A6EAD", "#D7697A",
                                 "#5197D1", "#FCCA58", "#3C3C3B", "#73FC48")) +
  labs(title = "Greater Manchester Inactivity Rate By Ethnicity",
       y = "Inactivity Rate (%)",
       x = "",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

# Not necessary
# ggplot(data = ActiveLives %>%
#          filter(Area == "Greater Manchester") %>%
#          filter(!is.na(Release)) %>%
#          filter(ActivityLevel == "Inactive") %>%
#          filter(DemographicType == "Ethnicity") %>%
#          filter(Demographic != "Chinese" ) %>%
#          filter(Demographic != "Mixed")
#          ,
#        aes(x = Release, y = Value*100, color = Demographic, group = Demographic)) +
#   geom_line(size = 1) +
#   geom_text(data = EthnicityInactive %>% filter(Release == "Nov 15-16"),
#             aes(label = Demographic),
#             hjust = 0,
#             vjust = -0.4,
#             fontface = "bold") +
#   geom_text_repel(data = EthnicityInactive %>% filter(Release == "Nov 19-20"),
#             aes(label = paste(sprintf("%0.1f", round(Value*100, digits = 2)),"%")),
#             nudge_x = 0.3) +
#   ylim(0,50) +
#   theme_GS() +
#   scale_color_manual(values = c("#5B2D86", "#D21C60", "#8A6EAD", "#D7697A",
#                                 "#5197D1", "#FCCA58", "#3C3C3B", "#73FC48")) +
#   labs(title = "Greater Manchester Inactivity Rate By Ethnicity",
#        subtitle = "Due to the small sample size there is no data on inactivity for those from 'other ethnic\ngroups' in November 2018/19. Data on those who identified as Chinese or Mixed\nhave been removed from this graph due to thenumber of releases without data.",
#        y = "Inactivity Rate (%)",
#        caption = "**Source:** Sport England Active Lives Survey") +
#   theme(legend.position = "none",
#         axis.title.x = element_text(hjust = 1, vjust = 1),
#         plot.caption = element_markdown())

# Walking -----------------------------------------------------------------

## Import data ----
Walking <- ActiveLives %>%
  filter(ActivityLevel == "Walking") %>%
  filter(Release != "Nov 15-16") %>%
  filter(Release != "May 16-17") %>%
  filter(Release != "Nov 16-17") %>%
  filter(Release != "May 17-18") %>%
  filter(Area != "England")

Walking$Area <- str_wrap(Walking$Area, width = 10)

Walking$Value <- Walking$Value*100

### Area order ----
Walking$Area <- factor(Walking$Area, levels = 
                             c("England", "Greater\nManchester",
                               "Bolton", "Bury", "Manchester",
                               "Oldham", "Rochdale", "Salford",
                               "Stockport", "Tameside",
                               "Trafford", "Wigan"), ordered = TRUE)

## Walking graphs ----

### By activity ----
#### All walking types ----
Walking %>%
  filter(Area == "Greater\nManchester") %>%
  ggplot(aes(x = Release, y = Value, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(data = Walking %>% filter(Release == "Nov 17-18") %>%
              filter(Area == "Greater\nManchester"),
            aes(label = Demographic),
            hjust = 0,
            vjust = -1,
            fontface = "bold") +
  geom_text(data = Walking %>% filter(Release == "Nov 19-20") %>%
              filter(Area == "Greater\nManchester"),
            aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_x = 0.2) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Walking Rates in Greater Manchester",
       subtitle = "Adults who have walked at least twice in the past 28 days",
       y = "Participation Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

#### Walking for Leisure ----
Walking  %>%
  filter(Area == "Greater\nManchester") %>%
  filter(Demographic == "Walking for leisure") %>%
  ggplot(aes(x = Release, y = Value, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.5) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Walking Rates in Greater Manchester",
       subtitle = "Adults who have walked for leisure at least twice in the past 28 days",
       y = "Participation Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

#### Walking for Travel ----
Walking  %>%
  filter(Area == "Greater\nManchester") %>%
  filter(Demographic == "Walking for travel") %>%
  ggplot(aes(x = Release, y = Value, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.5) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Walking Rates in Greater Manchester",
       subtitle = "Adults who have walked for travel at least twice in the past 28 days",
       y = "Participation Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

#### All walking ----
Walking  %>%
  filter(Area == "Greater\nManchester") %>%
  filter(Demographic == "All walking") %>%
  ggplot(aes(x = Release, y = Value, color = Demographic, group = Demographic)) +
  geom_line(size = 1) +
  geom_text(aes(label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%")),
            nudge_y = 0.5) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  labs(title = "Walking Rates in Greater Manchester",
       subtitle = "Adults who have walked at least twice in the past 28 days",
       y = "Participation Rate (%)",
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown())

### Borough Comparisons ----

#### All Walking ----
Walking  %>%
  filter(Release == "Nov 19-20") %>%
  filter(Demographic == "All walking") %>%
  ggplot(aes(y = Area, x = Value, fill = Demographic, group = Demographic,
             label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%"))) +
  geom_bar(stat = 'identity') +
  geom_bar_text(color = 'white', fontface = 'bold') +
  theme_GS() +
  labs(title = "All Walking Rates by Area",
       subtitle = "Adults who have walked at least twice\nin the past 28 days",
       x = "Participation Rate (%)",
       y = '',
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown(),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2")) +
  scale_fill_manual(values = GS_cols %>% unname,
                    breaks = c("Active", "Fairly Active")) +
  scale_y_discrete(limits = rev)

#### Walking for leisure ----
Walking  %>%
  filter(Release == "Nov 19-20") %>%
  filter(Demographic == "Walking for leisure") %>%
  ggplot(aes(y = Area, x = Value, fill = Demographic, group = Demographic,
             label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%"))) +
  geom_bar(stat = 'identity') +
  geom_bar_text(color = 'white', fontface = 'bold') +
  theme_GS() +
  labs(title = "Walking for Leisure Rates by Area",
       subtitle = "Adults who have walked for leisure at least twice\nin the past 28 days",
       x = "Participation Rate (%)",
       y = '',
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown(),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2")) +
  scale_fill_manual(values = GS_cols %>% unname,
                    breaks = c("Active", "Fairly Active")) +
  scale_y_discrete(limits = rev)

#### Walking for travel ----
Walking  %>%
  filter(Release == "Nov 19-20") %>%
  filter(Demographic == "Walking for travel") %>%
  ggplot(aes(y = Area, x = Value, fill = Demographic, group = Demographic,
             label = paste(sprintf("%0.1f", round(Value, digits = 2)),"%"))) +
  geom_bar(stat = 'identity') +
  geom_bar_text(color = 'white', fontface = 'bold') +
  theme_GS() +
  labs(title = "Walking for Travel Rates by Area",
       subtitle = "Adults who have walked for travel at least twice\nin the past 28 days",
       x = "Participation Rate (%)",
       y = '',
       caption = "**Source:** Sport England Active Lives Survey") +
  theme(legend.position = "none",
        plot.caption = element_markdown(),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "#a3a3a2")) +
  scale_fill_manual(values = GS_cols %>% unname,
                    breaks = c("Active", "Fairly Active")) +
  scale_y_discrete(limits = rev)
