library(tidyverse)
data <- read_csv("data/bundesliga_2023-24/match_bundesliga_2023-24.csv", show_col_types = FALSE)
athletic <- data |>
  mutate(
    deep_bu = Passes / `Long passes_accurate`,
    patient = Shots * 100 / `Passes to final third`,
    central_p = Passes * 100 / Crosses,
    shot_q = xG / Shots,
    creation = xG * 90 / Duration,
    circulation = Passes / `Progressive passes`
  ) |>
  group_by(Team) |>
  summarize(
    deep_build_up = mean(deep_bu),
    possesion = mean(`Possession, %`),
    central_progession = mean(central_p),
    circulation = mean(circulation),
    intensity = mean(1 / PPDA),
    patient = mean(patient),
    creation = mean(creation),
    shot_quality = mean(shot_q)
  )

tilt <- read_csv("xG_build-up_ppda_tilt_78_2024_2023-24.csv", show_col_types = FALSE)
wheel_data <- athletic |>
  left_join(tilt, by = c("Team" = "team")) |>
  select(
    c("Team", "deep_build_up", "possesion", "central_progession", "circulation", "intensity", "patient", "creation", "shot_quality", "high_line", "press_resistance", "chance_prevention", "tilt")
  ) |>
  write_csv("wheel_data_bundesliga_2023-24.csv")
