#EDA Project
#FIFAWWC Shooting
#Chaely Raynor

#Libraries
library(tidyverse)
library(zoo)
library(ggridges)
library(tableone)
library(ggrepel)

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



###Helpful Subsets###
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


#Do certain play patterns create dangerous shots?
#shot distance by play_pattern.name
wwc_shots |>
  ggplot(aes(x = DistToGoal, fill= play_pattern.name)) +
  geom_histogram(bins = 15, alpha=.5) +
  facet_wrap(~play_pattern.name)+
  labs(x= "Distance to Goal")+
  scale_fill_viridis_d()+
  theme(legend.position = "none")

#time in possesion by play_pattern.name
wwc_shots |>
  ggplot(aes(x = TimeInPoss, fill= play_pattern.name)) +
  geom_histogram(bins = 15, alpha=.5) +
  facet_wrap(~play_pattern.name)+
  labs(x= "Time in Possession")+
  scale_fill_viridis_d()+
  theme(legend.position = "none")
#same thing but common play_patterns
common_play_patterns |>
  ggplot(aes(x = TimeInPoss, fill= play_pattern.name)) +
  geom_histogram(bins = 15, alpha=.5) +
  facet_wrap(~play_pattern.name)+
  labs(x= "Time in Possession")+
  scale_fill_viridis_d()+
  theme(legend.position = "none")

#Median time in possession based on play_pattern
medianTimeInPoss <- common_play_patterns|>
  group_by(play_pattern.name)|>
  summarize(median.TimeInPoss = median(TimeInPoss))
common_play_patterns|>
  group_by(play_pattern.name)|>
  summarize(mean(TimeInPoss))

#Goal percentage for each type of play pattern
play_patterns_goals <-common_play_patterns|>
  group_by(play_pattern.name)|>
  count(goal)|>
  pivot_wider(id_cols = play_pattern.name, names_from = goal, values_from = n)|>
  ungroup()|>
  mutate(Total = (Goal + `No Goal`),
         goal.pct = Goal/Total)

median.density <- common_play_patterns|>
  group_by(play_pattern.name)|>
  summarize(median_density.incone = median(density.incone))
common_play_patterns|>
  group_by(play_pattern.name)|>
  summarize(mean(density.incone))
common_play_patterns |>
  ggplot(aes(x = density.incone, fill= play_pattern.name)) +
  geom_histogram(bins = 15, alpha=.5) +
  facet_wrap(~play_pattern.name)+
  labs(x= "Density in Cone")+
  scale_fill_viridis_d()+
  theme(legend.position = "none")

common_play_pattern.data <- merge(medianTimeInPoss, play_patterns_goals, by= "play_pattern.name")
common_play_pattern.data <- merge(common_play_pattern.data, median.density, by= "play_pattern.name")

#Bubble Plot
common_play_pattern.data|>
  ggplot(aes(x=median.TimeInPoss, y=goal.pct, size=Total,
             color = median_density.incone,
             label = play_pattern.name))+
  geom_point()+
  scale_size(range= c(5, 20), name="Total")+
  theme_bw()+
  scale_y_continuous(limits = c(0, .18))+
  scale_x_continuous(limits = c(0,22))+
  geom_text_repel(size=3.5, nudge_y = -0.0125, 
                  segment.color = NA)+
  labs(x="Median Time In Possession",
       y= "Goal Percentage",
       title = "Play Pattern Goals",
       caption = "Data courtesy of StatsBomb",
       color = "Median Density in Cone")+
  scale_color_gradient(low= "navy",
                       high="goldenrod")



################################################################################
#table of play patterns and whether they result in a goal
play_patterntable <- CreateTableOne(data=wwc_shots,
               strata = "goal", 
               vars="play_pattern.name"
               )
print(play_patterntable)
#37.6% of goals comes from regular play
#Also a high percent of goals from corner and from throw in

complex.play_patterntable <- CreateTableOne(data=wwc_shots,
                                    strata = "goal", 
                                    vars=c("play_pattern.name", "TimeInPoss", 
                                           "DefendersInCone")
)
print(complex.play_patterntable)


###########################################################################
### k-means Clustering ###
std_wwc_shots_clusterfeatures<- wwc_shots|>
  select(DistToGoal, AngleToGoal, distance.ToD1.360, DefendersInCone,
         density.incone, TimeInPoss, avevelocity, DistSGK)|>
  scale(center=TRUE, scale=TRUE)

kmeans_many_features <- std_wwc_shots_clusterfeatures |>
  kmeans(algorithm= "Hartigan-Wong", 
         centers=4, nstart=30)


############################################################################
#Position on field by shot type
names(wwc_shots)
