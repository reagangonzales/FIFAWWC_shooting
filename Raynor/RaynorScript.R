#EDA Project
#FIFAWWC Shooting
#Chaely Raynor

#Libraries
library(tidyverse)
library(zoo)
library(ggridges)

#Import the data
wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/wwc_shots.csv")

###Data Overview###
glimpse(wwc_shots)
head(wwc_shots)
#1544 observations
#45 variables
length(unique(wwc_shots$player.name)) #386 players
length(unique(wwc_shots$possession_team.name)) #32 teams
#number of observed shots by team
wwc_shots|>
  group_by(possession_team.name)|>
  count(possession_team.name)|>
  arrange(desc(n))|>
  print(n=32)

###Some data cleaning###
#Adding a goal binary variable
wwc_shots <- wwc_shots|>
  mutate(goal = case_when(shot.outcome.name == "Goal" ~ "Goal",
                          shot.outcome.name != "Goal" ~ "No Goal"))

#Making the under_pressure true/false
wwc_shots$under_pressure = na.fill(wwc_shots$under_pressure, fill = FALSE)

#Adding a top 4 team binary variable
wwc_shots<- wwc_shots|>
  mutate(top4binary = case_when(wwc_shots$possession_team.name != "Spain Women's"&
                                  wwc_shots$possession_team.name != "England Women's"&
                                  wwc_shots$possession_team.name != "Australia Women's"& 
                                  wwc_shots$possession_team.name != "Sweden Women's" ~ FALSE,
                                TRUE ~ TRUE
                                ))

#Removing "Women's" from team names since we know it is only women's teams
#names_working = strsplit(wwc_shots$possession_team.name, " ")
#wwc_shots$team_name=0
#for(i in 1:length(names_working)){
#  wwc_shots$team_name[i] = names_working[[i]][1]
#} #Need to resolve for countries with more than 1 word names




###Helpful Subsets###
#4 most common play patterns
common_play_patterns <- wwc_shots |>
  filter(play_pattern.name == "From Corner" |
           play_pattern.name == "From Free Kick" |
           play_pattern.name == "Regular Plat" |
           play_pattern.name == "From Throw In")


###Questions###


common_play_patterns |>
  ggplot(aes(x = DistToGoal, fill= shot.outcome.name)) +
  geom_histogram(bins = 15, alpha=.5) +
  facet_wrap(~shot.outcome.name)+
  labs(x= "Distance to Goal")+
  scale_fill_viridis_d()+
  theme(legend.position = "none")

#joy plot of dist_to_goal for shots by the teams
wwc_shots|>
  ggplot(aes(x=DistToGoal, y=possession_team.name, fill=possession_team.name))+
  geom_density_ridges(scale=1.5)+
  theme(legend.position = "none")+
  scale_fill_viridis_d()
#Separated by whether they were a top 4 finisher
wwc_shots|>
  ggplot(aes(x=DistToGoal, y=possession_team.name, fill=possession_team.name))+
  geom_density_ridges(scale=1.5)+
  facet_wrap(~top4binary)+
  theme(legend.position = "none")+
  scale_fill_viridis_d()
