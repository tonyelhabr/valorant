library(valorantr)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(janitor)
library(stringr)
library(readr)

prettify_df <- function(df) {
  df |> 
    as_tibble() |> 
    clean_names()
}

events <- load_valorant('events') |> prettify_df()
series <- load_valorant('series') |> prettify_df()

filt_series <- series |> 
  distinct(id, .keep_all = TRUE) |> 
  inner_join(
    events |> 
      filter(!is.na(child_label)) |> 
      filter(
        short_name |> str_detect('Open', negate = TRUE)
      ) |> 
      distinct(event_id = id, region_id),
    by = 'event_id'
  ) |> 
  unnest_wider(c(team1, team2), names_sep = '_') |>
  transmute(
    id,
    event_id,
    region_id,
    event_name,
    team1_name,
    team2_name,
    start_date
  )

matches <- load_valorant('matches')
matches[[1]] |> names()

map_pluck_matches <- function(element, f = map_int) {
  matches |> f(~pluck(.x, element))
}

tibble(
  match_id = map_pluck_matches('id'),
  team_1_id = map_pluck_matches('team1Id'),
  team_2_id = map_pluck_matches('team2Id')
)

match_details <- load_valorant('match_details')
match_ids <- match_details |> map_int(~pluck(.x, 'id'))
match_details <- match_details |> set_names(match_ids)

prettify_match_details <- function(match_details, element) {
  match_details |> 
    map_dfr(
      ~pluck(.x, element),
      .id = 'match_id'
    ) |> 
    prettify_df() |> 
    mutate(
      across(match_id, as.integer)
    )
}

## TODO: calculate player differential and number of players remaining on one side
events <- match_details |> 
  prettify_match_details('events') |> 
  mutate(
    across(c(impact, matches('win_probability')), as.double)
  )
events

economies <- match_details |> 
  prettify_match_details('economies') 

locations <- match_details |> prettify_match_details('locations')

match_locations <- events |> 
  select(
    match_id,
    round_id,
    round_number,
    round_time_millis,
    player_id,
    reference_player_id,
    kill_id,
    bomb_id,
    event_type,
    weapon_id,
    ability
  ) |> 
  left_join(
    economies |> 
      select(
        match_id,
        round_id,
        round_number,
        player_id,
        agent_id,
        score,
        economy_weapon_id = weapon_id,
        armor_id,
        loadout_value
      ),
    by = c('match_id', 'round_id', 'round_number', 'player_id')
  ) |> 
  left_join(
    locations |> 
      select(
        match_id,
        round_number,
        player_id,
        round_time_millis,
        location_x,
        location_y,
        view_radians
      ),
    by = c('match_id', 'round_number', 'round_time_millis', 'player_id')
  )

match_locations |> 
  filter(economy_weapon_id != weapon_id) |> 
  count(event_type)
