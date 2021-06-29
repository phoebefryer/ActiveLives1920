library(tidyverse); library(readxl); library(ggtext)

monthlyAL <- read_xlsx("data/Monthly Change.xlsx")

monthlyAL <- pivot_longer(monthlyAL, 3:62)

monthYrAL <- read_xlsx("data/monthlyChangeYr.xlsx")

monthlyALGM <- monthlyAL %>%
  filter(Area == "Greater Manchester")

monthYrAL$Year <- as.factor(monthYrAL$Year)
monthYrAL$name <- str_wrap(monthYrAL$name, width = 10)
monthYrAL$name <- factor(monthYrAL$name, levels = 
                           c("Mid Nov to\nMid Dec", "Mid Dec to\nMid Jan",
                             "Mid Jan to\nMid Feb", "Mid Feb to\nMid Mar",
                             "Mid Mar to\nMid Apr", "Mid Apr to\nMid May",
                             "Mid May to\nMid Jun", "Mid Jun to\nMid Jul",
                             "Mid Jul to\nMid Aug", "Mid Aug to\nMid Sep",
                             "Mid Sep to\nMid Oct", "Mid Oct to\nMid Nov"), ordered = TRUE)

  

monthlyAL %>%
  filter(`Activity Rate` == "Inactive") %>%
  ggplot(aes(x = name, y = value, color = Area)) +
  geom_line(aes(group = 1))

monthlyALGM %>%
  filter(`Activity Rate` == "Inactive") %>%
  ggplot(aes(x = name, y = value*100)) +
  geom_line(aes(group = 1))

monthYrAL %>%
  filter(Area == "Greater Manchester") %>%
  filter(`Activity Rate` == "Inactive") %>%
  ggplot(aes(x = name, y = value*100, group = Year, color = Year, size = Year)) +
  geom_line() +
  scale_size_manual(values = c(0.1, 0.1, 0.1, 0.1, 1)) +
  theme_GMM() +
  scale_color_manual(values = GMM_cols %>% unname)

monthYrAL %>%
  filter(Area == "Greater Manchester") %>%
  filter(`Activity Rate` == "Inactive") %>%
  ggplot(aes(x = name, y = value*100, group = Year, color = Year, size = Year)) +
  geom_line() +
  scale_size_manual(values = c(1, 1, 1, 1, 1.5)) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  theme(legend.position = c(0.15, 0.1),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1),
        plot.caption = element_markdown()) +
  guides(color = guide_legend(direction = "horizontal"),
         size = guide_legend(direction = "horizontal")) +
  labs(title = "Greater Manchester Monthly Inactivity Rates by Survey Year",
       caption = "**Source:** *Sport England Active Lives Survey*",
       y = "Inactivity Rate (%)",
       x = "")

EngGMTime <- monthYrAL %>%
  filter(Year == "5") %>%
  filter(`Activity Rate` == "Inactive") %>%
  ggplot(aes(x = name, y = value*100, group = Area, color = Area)) +
  geom_line(size = 1) +
  ylim(23, 37) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  theme(legend.position = c(0.1, 0.9),
        legend.key = element_blank(),
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        plot.caption = element_markdown()) +
  labs(title = "Inactivity Levels Over Time",
       caption = "**Source:** *Sport England, Active Lives Survey*",
       y = "Inactivity Rate (%)",
       x = "")

EngGMTimeTiers <- EngGMTime +
  annotate("rect", xmin = 5, xmax = 7, ymin = 23, ymax = Inf,
           fill = "#FF0000", alpha= 0.2) +
  annotate("rect", xmin = 8, xmax = 11, ymin = 23, ymax = Inf,
           fill = "#F7FF00", alpha = 0.2) +
  annotate("rect", xmin = 11, xmax = 12, ymin = 23, ymax = Inf,
           fill = "#FF7000", alpha = 0.2)


EngGMTimeTiers +
  labs(subtitle = "2020 saw the Covid-19 pandemic take hold across the world.
       In England this resulted in a national lockdown in<br><span style='color:#FF0000;'>March,</span> areas
       of <span style='color:#FFFF00;'>Greater Manchester</span> were then in some form of localised restrictions
       for much of the year. <span style='color:#FFA500;'><br>Nationally restrictions</span> were then reintroduced from mid September.") +
  theme(plot.subtitle = element_markdown())


EngGMTimeTiers +
  labs(subtitle = "2020 saw the Covid-19 pandemic take hold across the world.
       In England this resulted in a national lockdown in<br><span style='color:#FF0000;'>March,</span> 
        <span style='color:#FFA500;'>restrictions were then reintroduced from mid-September.</span>
        Meanwhile, in  <span style='color:#FFFF00;'>Greater Manchester<br> there were localised restrictions</span>
        for much of the year.") +
  theme(plot.subtitle = element_markdown())

EngGMTimeTiers +
  geom_richtext(aes(x = 6, y = 36, label = "National lockdown"), color = "#3c3c3b",
                fill = NA, label.color = NA, show.legend = F) +
  geom_richtext(aes(x = 9.5, y = 36, label = "Greater Manchester<br>local restrictions"), color = "#3c3c3b",
                fill = NA, label.color = NA, show.legend = F) +
  geom_richtext(aes(x = 11.5, y = 36, label = "National<br>restrictions"),  color = "#3c3c3b",
                fill = NA, label.color = NA, show.legend = F)


# Simplified --------------------------------------------------------------

monthYrAL %>%
  filter(Area == "Greater Manchester") %>%
  filter(`Activity Rate` == "Inactive") %>%
  ggplot(aes(x = name, y = value*100, group = Year, color = Year, size = Year),) +
  geom_line() +
  scale_alpha_discrete(c(0.3,0.3,0.3,0.3,1)) +
  scale_size_manual(values = c(0.7, 0.7, 0.7, 0.7, 1.5)) +
  stat_summary(data =  monthYrAL %>%
                 filter(Area == "Greater Manchester") %>%
                 filter(`Activity Rate` == "Inactive") %>%
                 filter(Year != "5"), 
               aes(colour = "Year 1-4 average", group = 1),
               fun = mean, geom = "line", size = 1.2,
               show.legend = FALSE) +
  theme_GS() +
  scale_color_manual(values = GS_cols %>% unname) +
  theme(legend.position = c(0.16, 0.18),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        plot.caption = element_markdown(),
        legend.title = element_text(
          family = "Verdana",
          color = "#3c3c3b",
          size = 14,
          face = "bold")) +
  guides(color = guide_legend(direction = "horizontal",
                              title.position = "top"),
         size = FALSE,
         alpha = FALSE) +
  labs(title = "Greater Manchester Monthly Inactivity Rates by Survey Year",
       caption = "**Source:** *Sport England Active Lives Survey*",
       y = "Inactivity Rate (%)",
       x = "",
       color = "Survey Year")





