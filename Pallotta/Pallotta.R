rm(list=ls())
library(tidyverse)
library(scales)
library(gt)
library(hexbin)
library(sportyR)
library(patchwork)

# load the data
wwc_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/wwc_shots.csv")

# clean the pressure variable and create a goal outcome variable 
wwc_shots <- wwc_shots |>
  mutate(under_pressure = case_when(under_pressure == TRUE ~ "Pressured",
                             is.na(under_pressure) ~ "Not Pressured")) |>
  mutate(goal = ifelse(shot.outcome.name == "Goal", 1, 0))

# calculate the number of goals, shot attempts, and goal rate by the pressure variable
wwc_shots <- wwc_shots |>
  group_by(under_pressure) |>
  mutate(n_shots = n()) |>
  mutate(n_goals = sum(goal)) |>
  mutate(sh_percentage = sum(goal)/n_shots) |>
  mutate(sh_percentage = label_percent(accuracy = 0.01)(sh_percentage))

# standardize the coordinates in meters to match the Women's World Cup field dimensions
wwc_shots <- wwc_shots %>%
  mutate(
    x_fifa = location.x * (105 / 120) - 52.5,
    y_fifa = location.y * (68 / 80) - 34
  )

# use the sportyr package to create soccer field
fifa_field <- geom_soccer("fifa", display_range = "offense",
                          pitch_updates = list(pitch_length = 105, pitch_width = 68), pitch_units = "m")

# create a hexbin plot faceted by the pressure attribute, colored by goal rate
plot <- fifa_field +
  stat_summary_hex(data = wwc_shots, aes(x = x_fifa, y = y_fifa, z = goal), binwidth = c(2, 2),
    fun = mean, color = "black") +
  facet_wrap(~under_pressure, nrow = 1) +
  scale_fill_gradient(low = "midnightblue", high = "goldenrod", labels = scales::percent) +
  labs(title = "Goal Percentage by Defensive Pressure",
       caption = "Data courtesy of StatsBomb",
       fill = "Goal %") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        strip.text = element_text(family = "mono", size = 16),
        strip.background = element_blank(),
        plot.title = element_text(family = "mono", hjust = 0.5, face = "bold", size = 24),
        plot.caption = element_text(family = "mono", hjust = 0.5, size = 10),
        legend.position = c(0.005, .935),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(5, 5, 5, 5),
        text = element_text(family = "mono"))

# create a gt table for the total counts by the pressure variable
shot_breakdown <- wwc_shots |> 
  select(n_goals, n_shots, sh_percentage) |>
  rename(`Shots` = n_shots, `Goals` = n_goals, `Goal %` = sh_percentage) |>
  arrange(desc(`Shots`)) |>
  slice(1) |>
  gt(groupname_col = "") |>
  cols_label(under_pressure = "") |>
  cols_align(align = "center",
             columns = everything()) |>
  opt_table_font(font = list(google_font(name = "mono")))

# center the gt table and combine the elements into one viz
centered_table <- plot_spacer() +
  wrap_elements(full = shot_breakdown) +
  plot_layout(widths = c(2, 1, 2)) 

centered_table

combined_layout <- plot / centered_table +
  plot_layout(heights = c(8, 1))

combined_layout

# save the viz to wd
ggsave(combined_layout, filename = "combined_layout.png", width = 9, height = 6.5, dpi = 300)
