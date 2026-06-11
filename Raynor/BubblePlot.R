#Bubble Plot for Question 2
#Chaely Raynor
# 9 June 2026

#Libraries
library(tidyverse)
library(ggrepel)

#Import the data
wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/wwc_shots.csv")

#4 most common play patterns
common_play_patterns <- wwc_shots |>
  filter(play_pattern.name == "From Corner" |
           play_pattern.name == "From Free Kick" |
           play_pattern.name == "Regular Play" |
           play_pattern.name == "From Throw In"|
           play_pattern.name == "From Counter")|>
  mutate(play_pattern.name = case_when(play_pattern.name == "From Corner" ~"Corner",
                                       play_pattern.name == "From Free Kick" ~ "Free Kick",
                                       play_pattern.name == "From Throw In" ~ "Throw In",
                                       play_pattern.name == "From Counter" ~ "Counter",
                                       play_pattern.name == "Regular Play" ~ "Regular Play"))

#Median time in possesion by play pattern
medianTimeInPoss <- common_play_patterns|>
  group_by(play_pattern.name)|>
  summarize(median.TimeInPoss = median(TimeInPoss))

#Goal percentage for each type of play pattern
play_patterns_goals <-common_play_patterns|>
  group_by(play_pattern.name)|>
  count(goal)|>
  pivot_wider(id_cols = play_pattern.name, names_from = goal, values_from = n)|>
  ungroup()|>
  mutate(Total = (Goal + `No Goal`),
         goal.pct = Goal/Total)

#Median density by play pattern
median.density <- common_play_patterns|>
  group_by(play_pattern.name)|>
  summarize(median_density.incone = median(density.incone))

#Merging into one 
common_play_pattern.data <- merge(medianTimeInPoss, play_patterns_goals, by= "play_pattern.name")
common_play_pattern.data <- merge(common_play_pattern.data, median.density, by= "play_pattern.name")


####Bubble Plot####
common_play_pattern.data|>
  ggplot(aes(x=median.TimeInPoss, y=goal.pct, size=Total,
             color = median_density.incone,
             label = play_pattern.name))+
  geom_point()+
  scale_size(range= c(5, 20), name="Total Shot Attempts")+
  theme_bw()+
  scale_y_continuous(limits = c(0, .18), labels = scales::label_percent())+
  scale_x_continuous(limits = c(0,22))+
  geom_text_repel(size=3.5, nudge_y = -0.0125, 
                  segment.color = NA, family = "mono")+
  labs(x="Median Time In Possession",
       y= "Goal Percentage",
       title = "Time in Possession by Goal Percentage",
       caption = "Data courtesy of StatsBomb",
       color = "Median Density in Cone")+
  scale_color_gradient(low="#69000C",
                       high="#FF999E")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold",
                                  size = 24),
        plot.caption = element_text(size=10),
        text = element_text(family = "mono"))
