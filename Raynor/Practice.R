#Practice script
#Chaely Raynor
#4 June 2026

#Loading in the FIFAWWC shooting data
library(tidyverse)

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

