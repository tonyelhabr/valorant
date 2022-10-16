library(valorantr)
library(tibble)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)
library(fs)
library(janitor)

events <- load_valorant('events')
series <- load_valorant('series')
players <- load_valorant('players')

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
series_ids

read_valorant_csvs <- function(name, ids) {
  tibble(
    path = fs::dir_ls('data', regexp = sprintf('%s[.]csv', name), recurse = TRUE)
  ) |> 
    filter(basename(dirname(path)) %in% ids) |> 
    pull(path) |> 
    map_dfr(read_csv, show_col_types = FALSE) |> 
    clean_names()
}
kills <- read_valorant_csvs('kill', series_ids$id)
econ <- read_valorant_csvs('econ', series_ids$id)

long_kills_h2h <- bind_rows(
  kills |> 
    transmute(
      match_id,
      kill_id,
      round_id,
      player_id = killer_id,
      action_role = 'killer'
    ),
  kills |> 
    transmute(
      match_id,
      kill_id,
      round_id,
      player_id = victim_id,
      action_role = 'victim'
    )
) |> 
  left_join(
    econ |> 
      select(round_id, player_id, weapon, weapon_category),
    by = c('round_id', 'player_id')
  ) |> 
  left_join(
    players |> select(player_id = id, ign),
    by = 'player_id'
  )
long_kills_h2h

wide_kills_h2h <- long_kills_h2h |> 
  pivot_wider(
    names_from = action_role,
    values_from = c(weapon, weapon_category, player_id, ign)
  )

init_weapon_h2h_n <- wide_kills_h2h |> 
  count(weapon_killer, weapon_category_killer, weapon_victim, weapon_category_victim) |> 
  rename_all(~stringr::str_remove(.x, 'weapon_')) |> 
  mutate(
    killer_first = killer < victim,
    weapons_are_same = killer == victim,
    key = sprintf('%s-%s', ifelse(killer_first, killer, victim), ifelse(killer_first, victim, killer))
  ) |> 
  select(-killer_first)

weapon_h2h_n <- inner_join(
  init_weapon_h2h_n |> 
    select(key, weapons_are_same, weapon1 = killer, weapon_category1 = category_killer, n1 = n),
  init_weapon_h2h_n |> 
    select(key, weapon2 = killer, weapon_category2 = category_killer, n2 = n),
  by = 'key'
) |> 
  filter(weapons_are_same | weapon1 != weapon2) |> 
  mutate(
    n = n1 + n2
  ) |> 
  arrange(desc(n))

weapon_category_h2h_n <- weapon_h2h_n |> 
  group_by(weapon_category1, weapon_category2) |> 
  summarize(
    across(c(n1, n2, n), sum)
  ) |> 
  arrange(desc(n))

