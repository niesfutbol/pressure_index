library(tidyverse)
library("comprehenr")
library(jsonlite)
library(wstools)

teams_names_by_league <- list(
  "39" = c("Liverpool", "Chelsea", "Manchester City", "Tottenham Hotspur", "Arsenal", "Brighton", "Crystal Palace", "Aston Villa", "Brentford", "Everton", "Fulham", "Manchester United", "Newcastle United", "Nottingham Forest", "Southampton", "West Ham United", "Wolverhampton Wanderers", "Bournemouth", "Burnley", "Luton_Town", "Sheffield_United"),
  "78" = c("Augsburg", "Bayer Leverkusen", "Bayern München", "Bochum", "Borussia Dortmund", "Borussia Mgladbach", "Darmstadt 98", "Eintracht Frankfurt", "Freiburg", "Heidenheim", "Hoffenheim", "Köln", "Mainz 05", "RB Leipzig", "Stuttgart", "Union Berlin", "Werder Bremen", "Wolfsburg"),
  "94" = c("Arouca", "Benfica", "Boavista", "Casa Pia AC", "Chaves", "Estoril", "Famalicão", "Gil Vicente", "Marítimo", "Paços de Ferreira", "Portimonense", "Porto", "Rio Ave", "Santa Clara", "Sporting Braga", "Sporting CP", "Vitória Guimarães", "Vizela", "Santos Laguna", "Tigres UANL", "Toluca"),
  "262" = c("América", "Atlas", "Atlético de San Luis", "Club Tijuana", "Cruz Azul", "Guadalajara", "Juárez", "León", "Mazatlán", "Monterrey", "Necaxa", "Pachuca", "Puebla", "Pumas UNAM", "Querétaro", "Santos Laguna", "Tigres UANL", "Toluca"),
  "263" = c("Alebrijes de Oaxaca", "Cancún", "Dorados", "Raya2", "Universidad Guadalajara", "Atlante", "Celaya", "Durango", "Tapatío", "Venados", "Atlético Morelia", "Cimarrones de Sonora", "Mineros de Zacatecas", "Tepatitlán de Morelos", "CA La Paz", "Correcaminos UAT", "Pumas Tabasco", "Tlaxcala"),
  "135" = c("Cremonese", "Sampdoria", "Internazionale", "Hellas Verona", "Monza", "Fiorentina", "Milan", "Lecce", "Juventus", "Empoli", "Sassuolo", "Napoli", "Spezia", "Lazio", "Roma", "Atalanta", "Bologna", "Torino", "Salernitana", "Udinese"),
  "140" = c("Almería", "Athletic Bilbao", "Atlético Madrid", "Barcelona", "Cádiz", "Celta de Vigo", "Deportivo Alavés", "Getafe", "Girona", "Granada", "Las Palmas", "Mallorca", "Osasuna", "Rayo Vallecano", "Real Betis", "Real Madrid", "Real Sociedad", "Sevilla", "Valencia", "Villarreal")
)
library(pression)

league <- "78_2024"
names <- obtain_files_names("/workdir/data/bundesliga_2023-24")
all_team_stats <- list()
all_rivals_team <- list()
for (team in names) {
  team_name <- str_replace_all(team, " ", "_")
  path <- glue::glue("/workdir/data/bundesliga_2023-24/{team_name}.csv")
  team_stats <- read_team_stats(path) |>
    #     filter_premier_league() |>
    add_tilt() |>
    filter(Team == team)
  all_team_stats[[team]] <- team_stats
  team_stats <- read_team_stats(path) |>
    #     filter_premier_league() |>
    filter(Team != team)
  all_rivals_team[[team]] <- team_stats
}
mean_team_passes <- to_vec(for (team in names) mean(all_team_stats[[team]]$Passes_accurate_percentage, na.rm = TRUE))
all_team_passes <- tibble(
  "team" = names,
  passes_accurate_percentage_rivals = mean_team_passes
)

for (team in names) {
  all_rivals_team[[team]] <- all_rivals_team[[team]] |>
    left_join(all_team_passes, by = c("Team" = "team")) |>
    mutate(chance_prevention = xG) |>
    mutate(high_line = Offsides + Counterattacks) |>
    mutate(build_up = passes_accurate_percentage_rivals - Passes_accurate_percentage) |>
    select(c(1, 2, 5, 12:14, 110:113, Sliding_tackles))
}
mean_team_xg <- to_vec(for (team in names) mean(all_team_stats[[team]]$xG, na.rm = TRUE))
mean_build_up_disruption <- to_vec(for (team in names) mean(all_rivals_team[[team]]$build_up, na.rm = TRUE))
mean_ppda <- to_vec(for (team in names) mean(all_team_stats[[team]]$PPDA, rm.rm = TRUE))
mean_c_p <- to_vec(for (team in names) mean(all_rivals_team[[team]]$chance_prevention, rm.rm = TRUE))
mean_h_l <- to_vec(for (team in names) mean(all_rivals_team[[team]]$high_line, rm.rm = TRUE))
mean_p_r <- to_vec(for (team in names) mean((all_team_stats[[team]]$Passes - all_team_stats[[team]]$Passes_to_final_third) / (all_team_stats[[team]]$Losses_Low + all_team_stats[[team]]$Losses_Medium), rm.rm = TRUE))
build_up_teams <- tibble(
  "team" = names,
  xG = mean_team_xg,
  build_up_disruption = mean_build_up_disruption,
  ppda = mean_ppda,
  high_line = mean_h_l,
  press_resistance = mean_p_r,
  chance_prevention = -mean_c_p
) |>
  arrange(-build_up_disruption) |>
  write_csv(glue::glue("build_up_and_ppda_{league}.csv"))
best_build_team <- build_up_teams$team[1]
