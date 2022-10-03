library(valorantr)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(janitor)

prettify_df <- function(df) {
  df |> 
    as_tibble() |> 
    clean_names()
}

events <- load_valorant('events') |> prettify_df()
events

series <- load_valorant('series') |> prettify_df()
series |> 
  distinct(id, .keep_all = TRUE)

series_urls <- series |> 
  distinct(id, .keep_all = TRUE) |> 
  mutate(
    ribgg_url = sprintf(
      'https://rib.gg/series/%s', 
      id
    )
  ) |> 
  unnest_wider(c(team1, team2), names_sep = '_') |> 
  select(id, team1_name, team2_name, start_date, ribgg_url)
series_urls
series_urls |> 
  filter(
    ribgg_url |> stringr::str_detect('faze-clan-vs-100-thieves')
  ) |> 
  pull(ribgg_url)
series |> unnest_wider(c(team1, team2), names_sep = '_') |> select(team1_name)
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
