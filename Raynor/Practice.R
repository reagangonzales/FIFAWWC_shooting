#Practice script
#Chaely Raynor
#4 June 2026

#Loading in the FIFAWWC shooting data
library(tidyverse)
library(zoo)

wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/wwc_shots.csv")
View(wwc_shots)

#Questions

#What factors lead to a goal?
#Retaining team posession after shot attempt?
#Do certain teams have preferred shooting styles?
    #Under pressure v. not under pressure
    #Play type?
#Top scorers -> are there any common themes? same shot type?
#Use top 4 teams?
#Does having defenders in the cone change the goalkeepers position
#Distance to cone + defenders in cone + angle of shot = shot outcome?

ggplot(wwc_shots, aes(x=shot.outcome.name))+
  geom_bar()
length(unique(wwc_shots$player.name)) #386 players
length(unique(wwc_shots$possession_team.name)) #32 teams

goal_shots <- wwc_shots|>
  filter(shot.outcome.name=="Goal")

#Goalkeeper position when the outcome was a goal
goal_shots|>
  ggplot(aes(x=location.x.GK, y=location.y.GK))+
  geom_point()

wwc_shots|>
  filter(shot.outcome.name!="Goal")|>
  ggplot(aes(x=location.x.GK, y=location.y.GK))+
  geom_point()

wwc_shots|>
  mutate(goal = case_when(shot.outcome.name == "Goal" ~ "Goal",
                          shot.outcome.name != "Goal" ~ "No Goal"
                          ))|>
  ggplot(aes(x=location.x.GK, y=location.y.GK, color= goal))+
  geom_point(alpha=.5)
  
#Defense measurements that lead to a favorable outcome (!goal)
#Adding a goal binary variable
wwc_shots <- wwc_shots|>
  mutate(goal = case_when(shot.outcome.name == "Goal" ~ "Goal",
                          shot.outcome.name != "Goal" ~ "No Goal"
  ))
#need to 
wwc_shots$under_pressure = na.fill(wwc_shots$under_pressure, fill = FALSE)

table(wwc_shots$shot.technique.name,
      wwc_shots$shot.outcome.name)
table(wwc_shots$DefendersInCone,
      wwc_shots$shot.outcome.name)

table(wwc_shots$DefendersInCone,
      wwc_shots$goal)

names(wwc_shots)
library(tableone)
goal_table <- CreateTableOne(data=wwc_shots,
               vars= c("DefendersInCone", "under_pressure", "InCone.GK",
                       "distance.ToD1.360","distance.ToD2.360" ),
               strata= "goal")
print(goal_table)

#distribution of shots for each country
wwc_shots|>
  group_by(possession_team.name)|>
  count(shot.outcome.name)|>
  pivot_wider(names_from = shot.outcome.name, values_from = n)
  
team_patterns <- wwc_shots|>
  group_by(possession_team.name)|>
  count(play_pattern.name)|>
  pivot_wider(names_from = play_pattern.name, values_from = n)

#making sure the values are numeric in the new dataframe
names(team_patterns)
team_patterns <-team_patterns|>
  na.fill(fill =0)
team_patterns[,2] = as.numeric(team_patterns[,2])
team_patterns[,3] = as.numeric(team_patterns[,3])
team_patterns[,4] = as.numeric(team_patterns[,4])
team_patterns[,5] = as.numeric(team_patterns[,5])
team_patterns[,6] = as.numeric(team_patterns[,6])
team_patterns[,7] = as.numeric(team_patterns[,7])
team_patterns[,8] = as.numeric(team_patterns[,8])
team_patterns[,9] = as.numeric(team_patterns[,9])
team_patterns[,10] = as.numeric(team_patterns[,10])




#shot distance by outcome type
wwc_shots |>
  ggplot(aes(x = DistToGoal, fill= shot.outcome.name)) +
  geom_histogram(bins = 15, alpha=.5) +
  facet_wrap(~shot.outcome.name)+
  labs(x= "Distance to Goal")+
  scale_fill_viridis_d(option="inferno")+
  theme(legend.position = "none")

#shot distance by play_pattern.name
wwc_shots |>
  ggplot(aes(x = DistToGoal, fill= play_pattern.name)) +
  geom_histogram(bins = 15, alpha=.5) +
  facet_wrap(~play_pattern.name)+
  labs(x= "Distance to Goal")+
  scale_fill_viridis_d()+
  theme(legend.position = "none")

#filtering by 4 most common play patterns
common_play_patterns <- wwc_shots |>
  filter(play_pattern.name == "From Corner" |
           play_pattern.name == "From Free Kick" |
           play_pattern.name == "Regular Plat" |
           play_pattern.name == "From Throw In")

common_play_patterns |>
  ggplot(aes(x = DistToGoal, fill= shot.outcome.name)) +
  geom_histogram(bins = 15, alpha=.5) +
  facet_wrap(~shot.outcome.name)+
  labs(x= "Distance to Goal")+
  scale_fill_viridis_d()+
  theme(legend.position = "none")

#joy plot of dist_to_goal for shots by the teams
library(ggridges)
common_play_patterns|>
  ggplot(aes(x=DistToGoal, y=possession_team.name, fill=possession_team.name))+
  geom_density_ridges(scale=1.5)+
  theme(legend.position = "none")+
  scale_fill_viridis_d()
