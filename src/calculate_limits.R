library(tidyverse)
library(zoo)

metric_names <- list("passes_per_possession_minute" = "Tempo", "xg_shot" = "xG", "ppda" = "PPDA", "goal" = "Goles")
team_name <- "toluca"
the_team_path <- glue::glue("/workdir/data/{team_name}_team.csv")
data <- readr::read_csv(the_team_path, show_col_types = FALSE) |>
  dplyr::mutate(attack = 0.7 * xg_shot + 0.3 * goal)

matches_path <- glue::glue("/workdir/data/{team_name}_matches.csv")
matches <- readr::read_csv(matches_path, show_col_types = FALSE) |>
  dplyr::select(date, name, competition)

metric <- "ppda"
random_team <- data |>
  dplyr::sample_n(nrow(data)) |>
  dplyr::mutate(
    mean = zoo::rollapply(!!sym(metric), width = 4, mean, fill = NA, align = "left"),
    sd = zoo::rollapply(!!sym(metric), width = 4, sd, fill = NA, align = "left")
  )

team_mean <- random_team |>
  dplyr::pull(mean) |>
  mean(na.rm = TRUE)
team_sd <- random_team |>
  dplyr::pull(mean) |>
  sd(na.rm = TRUE)

statistics <- list(
  mean = team_mean,
  sd = team_sd
)

chart <- data |>
  dplyr::mutate(
    mean = zoo::rollapply(!!sym(metric), width = 4, mean, fill = NA, align = "left")
  )


to_plot <- cbind(chart, matches)
fecha_inicio_tendencia <- lubridate::ymd("2024-12-12")
y_label <- metric_names[[metric]]
fecha_inicio <- lubridate::ymd("2024-01-01")
to_plot |>
  dplyr::filter(date > fecha_inicio) |>
  ggplot(aes(x = date, y = mean)) +
  labs(
    title = glue::glue("Carta de control {y_label} (2022-2025)"),
    x = "Fecha",
    y = y_label
  ) +
  theme_minimal() + # Un tema limpio para el gráfico
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  geom_ribbon(aes(ymin = statistics$mean - 3 * statistics$sd, ymax = statistics$mean + 3 * statistics$sd), fill = "red") +
  geom_ribbon(aes(ymin = statistics$mean - 2 * statistics$sd, ymax = statistics$mean + 2 * statistics$sd), fill = "orange") +
  geom_ribbon(aes(ymin = statistics$mean - statistics$sd, ymax = statistics$mean + statistics$sd), fill = "yellow") +
  geom_line(color = "steelblue", size = 1) + # Agrega la línea
  geom_point(color = "darkblue", size = 2) + # Agrega los puntos para cada observación
  geom_hline(yintercept = team_mean, color = "darkolivegreen", size = 1) +
  geom_vline(xintercept = fecha_inicio_tendencia, color = "black", size = 1) +
  theme_classic()

ggsave(glue::glue("/workdir/results/{team_name}_{metric}_trend.png"), width = 10, height = 6)
