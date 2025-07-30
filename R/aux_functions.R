#' @export
obtain_files_names <- function(path_directory) {
  files_name <- list.files(path = path_directory, pattern = "csv$")
  .clean_files_name(files_name)
}

.clean_files_name <- function(files_name) {
  files_name |>
    .remove_suffixes()
}

.remove_prefixes <- function(files_name) {
  lists_names <- files_name |> stringr::str_split("Stats ")
  names_with_xlsx <- comprehenr::to_vec(for (team in lists_names) team[2])
}

.remove_suffixes <- function(names_with_xlsx) {
  lists_cleaned_names <- stringr::str_split(names_with_xlsx, "\\.")
  comprehenr::to_vec(for (team in lists_cleaned_names) team[1])
}

#' @export
obtain_json_files_names <- function(path_directory) {
  raw_files_name <- list.files(path = path_directory, pattern = "json$")
  files_name <- comprehenr::to_vec(for (team in raw_files_name) if (team != "datapackage.json") team)
  .clean_files_name(files_name)
}
