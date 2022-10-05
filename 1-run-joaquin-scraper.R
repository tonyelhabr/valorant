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
      as_tibble() |> 
      filter(
        name |> str_detect('(Closed|Open) Qualifier', negate = TRUE)
      ) |> 
      distinct(event_id = id, region_id = regionId, event_name = name),
    by = 'event_id'
  ) |> 
  unnest_wider(c(team1, team2), names_sep = '_') |>
  select(
    id, 
    event_id, 
    event_name,
    region_id,
    start_date,
    team1_name,
    team2_name
  )

tool_dir <- "C://Users//antho//Downloads//RIB Web Scraper v0.5//RIB Web Scraper v0.5"
setwd(tool_dir)
scrape_series <- function(series_id) {
  output_dir <- sprintf("C://Users//antho//Downloads//data//%s", series_id)
  if (fs::dir_exists(output_dir)) {
    message(sprintf("Returning early for %s.", series_id))
    return(NULL)
  }
  txt_path <- file.path(tool_dir, "data.txt")
  data_dir <- file.path(tool_dir, "data-pulled")

  fs::dir_create(data_dir)
  write_lines(sprintf("https://rib.gg/series/%s", series_id), txt_path)
  res <- system2("webscrape.exe")
  fs::dir_copy(
    data_dir,
    sprintf("C://Users//antho//Downloads//ribgg//data//%s", series_id)
  )
  fs::file_delete(txt_path)
  fs::dir_delete(data_dir)
  invisible(res)
}

series_ids |> 
  # filter(region_id == 2) |> 
  pull(id) |> 
  walk(scrape_series)
