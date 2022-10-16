library(valorantr)
library(tibble)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)
library(fs)

events <- load_valorant('events')
series <- load_valorant('series')

series_ids <- series |> 
  distinct(id, .keep_all = TRUE) |> 
  rename(event_id = eventId, start_date = startDate) |> 
  inner_join(
    events |> 
      inner_join(get_all_region_names(), by = 'regionId') |> 
      filter(regionName %in% c('Europe', 'North America', 'International')) |> 
      distinct(event_id = id, region_name = regionName, event_name = name),
    by = 'event_id'
  ) |> 
  unnest_wider(c(team1, team2), names_sep = '_') |>
  select(
    id, 
    event_id, 
    event_name,
    region_name,
    start_date,
    team1_name,
    team2_name
  )

tool_dir <- file.path(getwd(), 'src')
setwd(tool_dir)
scrape_series <- function(series_id) {
  output_dir <- file.path('../data', series_id)
  if (fs::dir_exists(output_dir)) {
    message(sprintf('Returning early for %s.', series_id))
    return(NULL)
  }
  txt_path <- file.path(tool_dir, 'data.txt')
  data_dir <- file.path(tool_dir, 'data-pulled')
  on.exit(fs::file_delete(txt_path))
  on.exit(fs::dir_delete(data_dir))

  fs::dir_create(data_dir)
  write_lines(sprintf('https://rib.gg/series/%s', series_id), txt_path)
  res <- system2('webscrape.exe')
  fs::dir_copy(
    data_dir,
    output_dir
  )
  invisible(res)
}

series_ids |> 
  pull(id) |> 
  walk(scrape_series)
