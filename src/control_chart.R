library(tidyverse)
library(zoo)

team_name <- "Club Tijuana"
data <- read_csv("data/ligaMX_2023-24/2023-24/wyscout_matches.csv", show_col_types = FALSE)
team <- data |>
  filter(Team == team_name)

actual_team_data <- read_csv("data/ligaMX_2023-24/2024-25/wyscout_matches.csv", show_col_types = FALSE) |>
  filter(Date > lubridate::ymd("20240701"), Team == team_name) |>
  arrange(Date)

random_team <- team |>
  sample_n(nrow(team)) |>
  mutate(
    xG_mean = zoo::rollapply(xG, width = 4, mean, fill = NA, align = "left"),
    xG_sd = zoo::rollapply(xG, width = 4, sd, fill = NA, align = "left")
  )

team_mean <- random_team |>
  pull(xG_mean) |>
  mean(na.rm = TRUE)
team_sd <- random_team |>
  pull(xG_mean) |>
  sd(na.rm = TRUE)

chart <- team |>
  mutate(
    xG_mean = zoo::rollapply(xG, width = 4, mean, fill = NA, align = "left"),
    xG_sd = zoo::rollapply(xG, width = 4, sd, fill = NA, align = "left")
  ) |>
  select(c(1:8, "xG_mean"))

actual_team_chart <- actual_team_data |>
  mutate(
    xG_mean = zoo::rollapply(xG, width = 4, mean, fill = NA, align = "right"),
    xG_sd = zoo::rollapply(xG, width = 4, sd, fill = NA, align = "right")
  ) |>
  select(c(1:8, "xG_mean"))
