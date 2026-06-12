# Load in dataset
library(tidyverse)

wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/wwc_shots.csv")

dim(wwc_shots)
names(wwc_shots)
table(wwc_shots$shot.outcome.name)

# Count and proportion of different shot outcomes
wwc_shots |>
  count(shot.outcome.name) |>
  mutate(prop = n / sum(n)) |>
  arrange(desc(prop))
# Count and proportion of different shot techniques
wwc_shots |>
  count(shot.technique.name) |>
  mutate(prop = n / sum(n)) |>
  arrange(desc(prop))

## Visualizations ##
# Teams with the most shots visualized
# Creating new data set in order to add images
library(ggimage)
wwc_shots2 <- wwc_shots |>
  mutate(possession_team.name = case_when(possession_team.name == "Spain Women's" ~ "Spain",
                                          possession_team.name == "Australia Women's" ~ 
                                            "Australia",
                                          possession_team.name =="England Women's" ~ "England",
                                          possession_team.name == "France Women's" ~ "France",
                                          possession_team.name == "United States Women's" ~
                                            "United States"))

# add image file column
wwc_shots2$image_file <- paste0("logo_images/", wwc_shots2$possession_team.name, ".png")
# create visualization
wwc_shots2 |>
  mutate(goal_outcome = "Shots") |>
  filter(possession_team.name %in% c("Spain", "Australia", "England", "France", 
                                     "United States")) |>
  group_by(possession_team.name, image_file) |> 
  count(goal_outcome) |>
  arrange(desc(n)) |>
  ggplot(aes(x = reorder(possession_team.name, -n), y = n)) +
  geom_col(aes(fill = possession_team.name), color = "black", show.legend = FALSE) + # color to add border around columns
  geom_image(aes(image = image_file, y = n), nudge_x = 0.0085, size = 0.08, asp = 1.5)+
  theme_bw() +
  scale_fill_manual(values = c(
    "Spain"         = "#FF0000",  # red
    "Australia"     = "#FFD700",  # gold
    "England"       = "#FFFFFF",  # white
    "France"        = "#003189",  # blue
    "United States" = "#002868"   # navy
  )) +
  coord_cartesian(ylim = c(0,160)) + # adjust y scale to fit Spain's logo
  labs(x = "", y = "Number of Shots",  
       caption = "Data courtesy of StatsBomb")



## Gaussian Clustering ##
# filter top 15 goal scores
wwc_top15 <- wwc_shots |>
  select(shot.outcome.name, player.name) |>
  group_by(player.name, shot.outcome.name) |>
  summarize(total_goals = sum(shot.outcome.name == "Goal")) |>
  arrange(desc(total_goals)) |>
  head(n = 15)

wwc_top15_shots <- wwc_shots |>
  filter(player.name %in% wwc_top15$player.name)

# Clustering
# Assuming velocity is in  24 yd/s
library(mclust)
wwc_mclust <- wwc_top15_shots |>
  select(DistToGoal, avevelocity) |>  
  filter(avevelocity <= 30) |> # realistically average women's shot at most around 24/25 yd/s
  Mclust()
summary(wwc_mclust)

# Visualize clusters
library(broom)
library(gt)
library(patchwork)
gt(tidy(wwc_mclust))

# Plot
library(NatParksPalettes)
plot <- wwc_mclust|>
  augment() |>
  ggplot(aes(x=DistToGoal,y=avevelocity, 
             color = .class, 
             size = .uncertainty)) +
  geom_point(alpha = 0.7) +
  labs(x = "Distance to Goal", y = "Average Velocity", size = "Uncertainty", color = "Cluster",
       caption = "Data courtesy  of StatsBomb") +
  scale_color_manual(values = natparks.pals("Torres", 3)) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 1, vjust = 2),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) 

# Table
tablet <- tidy(wwc_mclust) |>
  as_tibble() |>
  rename(`Component` = component, `Size` = size, `Proportion` = proportion,
         `Avg Distance To Goal` = mean.DistToGoal, `Avg Velocity` = mean.avevelocity) |>
  gt() |>
  cols_align(align = "center",
             columns = everything()) |>
  fmt_number(columns = c(Proportion, `Avg Distance To Goal`, `Avg Velocity`), decimals = 2) |>
  opt_stylize(style = 4, color = "blue")