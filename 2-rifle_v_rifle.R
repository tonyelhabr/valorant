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

events_na2022 <- events |> 
  inner_join(get_all_region_names(), by = 'regionId') |> 
  filter(regionName == 'North America' & !grepl('2021', name)) |> 
  distinct(event_id = id, event_name = name)

series_na2022 <- series |> 
  rename(event_id = eventId, start_date = startDate) |> 
  inner_join(
    events_na2022,
    by = 'event_id'
  ) |> 
  unnest_wider(c(team1, team2), names_sep = '_') |>
  select(
    series_id = id, 
    event_id, 
    team1_id,
    team1_name,
    team2_id,
    team2_name
  )
series_na2022

set_names_from_id_element <- function(x) {
  match_ids <- x |> map_int(~pluck(.x, "id"))
  x |> set_names(match_ids)
}
matches <- load_valorant("matches") |> set_names_from_id_element()
matches_na2022 <- matches[names(matches) %in% as.character(series_na2022$series_id)]

series_player_mapping_na2022 <- matches_na2022 |> 
  map_dfr(
    ~pluck(.x, "playerStats"),
    .id = "series_id"
  ) |> 
  distinct(series_id, team_number = teamNumber, player_id = playerId) |> 
  mutate(across(series_id, as.integer))

map_pluck_matches <- function(matches, element) {
  matches |> map_int(~pluck(.x, element))
}

series_team_mapping_na2022 <- tibble(
  series_id = matches_na2022 |> map_pluck_matches("id"),
  `1` = matches_na2022 |> map_pluck_matches("team1Id"),
  `2` = matches_na2022 |> map_pluck_matches("team2Id")
) |> 
  pivot_longer(
    -series_id,
    names_to = "team_number",
    values_to = "team_id"
  ) |> 
  mutate(
    across(team_number, as.integer)
  )

series_team_player_mapping_na2022 <- series_player_mapping_na2022 |> 
  inner_join(
    series_team_mapping_na2022 |> 
      transmute(series_id, team_number, team_id), 
    by = c("series_id", "team_number")
  ) |> 
  select(-team_number)

series_match_mapping_na2022 <- matches_na2022 |> 
  map_dfr(
    ~tibble(match_id = pluck(.x, "matches", "id")), 
    .id = "series_id"
  ) |> 
  mutate(
    across(series_id, as.integer)
  )

players <- load_valorant("players")
teams <- get_all_team_names()
weapon_ids <- get_all_weapon_names() |>
  select(
    weapon_id = weaponId, 
    weapon = weaponName, 
    weapon_category = weaponCategory
  )

## my data ----
match_details <- load_valorant("match_details") |> set_names_from_id_element()
match_details_na2022 <- match_details[names(match_details) %in% as.character(series_match_mapping_na2022$match_id)]

cleanly_pluck_from_match_details <- function(match_details, element) {
  match_details |> 
    map_dfr(~pluck(.x, element), .id = "match_id") |> 
    as_tibble() |> 
    mutate(
      across(match_id, as.integer)
    )
}

match_events_na2022_t <- match_details_na2022 |> cleanly_pluck_from_match_details("events")
econ_na2022_t <- match_details_na2022 |> cleanly_pluck_from_match_details("economies")

weapon_kills_na2022_t <- match_events_na2022_t |> 
  filter(!is.na(killId), damageType == "weapon") |> 
  select(
    match_id,
    round_id = roundId,
    round_number = roundNumber,
    kill_id = killId,
    killer_id = playerId, 
    victim_id = referencePlayerId,
    killer_weapon_id = weaponId
  ) |> 
  ## data doesn"t list victim"s actual weapon, so we use what they bought in their loadout
  left_join(
    econ_na2022_t |> 
      select(
        match_id,
        round_id = roundId,
        victim_id = playerId, 
        victim_weapon_id = weaponId
      ),
    by = c("match_id", "round_id", "victim_id")
  ) |> 
  left_join(
    series_match_mapping_na2022,
    by = "match_id"
  ) |> 
  left_join(
    series_team_player_mapping_na2022 |> 
      select(
        series_id, 
        killer_id = player_id, 
        killer_team_id = team_id
      ),
    by = c("series_id", "killer_id")
  ) |> 
  left_join(
    series_team_player_mapping_na2022 |> 
      select(
        series_id, 
        victim_id = player_id, 
        victim_team_id = team_id
      ),
    by = c("series_id", "victim_id")
  ) |>
  left_join(
    players |> select(killer_id = id, killer_ign = ign),
    by = "killer_id"
  ) |> 
  left_join(
    players |> select(victim_id = id, victim_ign = ign),
    by = "victim_id"
  ) |> 
  left_join(
    teams |> select(killer_team_id = id, killer_team_name = name),
    by = "killer_team_id"
  ) |> 
  left_join(
    teams |> select(victim_team_id = id, victim_team_name = name),
    by = "victim_team_id"
  ) |> 
  left_join(
    weapon_ids |> rename_all(~sprintf("killer_%s", .x)),
    by = "killer_weapon_id"
  ) |> 
  left_join(
    weapon_ids |> rename_all(~sprintf("victim_%s", .x)),
    by = "victim_weapon_id"
  )

## joaquin data ----
read_valorant_csvs <- function(name, ids) {
  tibble(
    path = fs::dir_ls('data', regexp = sprintf('%s[.]csv', name), recurse = TRUE)
  ) |> 
    filter(basename(dirname(path)) %in% ids) |> 
    pull(path) |> 
    map_dfr(read_csv, show_col_types = FALSE)
}

kills_na2022_j <- read_valorant_csvs('kill', series_na2022$series_id) |> 
  mutate(
    across(weapon, tolower)
  )
econ_na2022_j <- read_valorant_csvs('econ', series_na2022$series_id) |> 
  mutate(
    across(weapon, tolower)
  )

weapon_kills_na2022_j <- kills_na2022_j |> 
  filter(!is.na(killId), damageType == "weapon") |> 
  transmute(
    match_id = as.integer(matchId),
    round_id = roundId,
    round_number = roundNumber,
    kill_id = killId,
    killer_id = killerId, 
    victim_id = victimId,
    killer_weapon = weapon,
    killer_team_id = killerTeamId,
    victim_team_id = victimTeamId
  ) |> 
  ## data doesn"t list victim"s actual weapon, so we use what they bought in their loadout
  left_join(
    econ_na2022_j |> 
      transmute(
        round_id = roundId,
        victim_id = playerId, 
        victim_weapon = weapon
      ),
    by = c("round_id", "victim_id")
  ) |>
  left_join(
    series_match_mapping_na2022,
    by = "match_id"
  ) |>
  left_join(
    players |> select(killer_id = id, killer_ign = ign),
    by = "killer_id"
  ) |> 
  left_join(
    players |> select(victim_id = id, victim_ign = ign),
    by = "victim_id"
  ) |> 
  left_join(
    teams |> select(killer_team_id = id, killer_team_name = name),
    by = "killer_team_id"
  ) |> 
  left_join(
    teams |> select(victim_team_id = id, victim_team_name = name),
    by = "victim_team_id"
  ) |> 
  left_join(
    weapon_ids |> select(killer_weapon = weapon, killer_weapon_category = weapon_category),
    by = "killer_weapon"
  ) |> 
  left_join(
    weapon_ids |> select(victim_weapon = weapon, victim_weapon_category = weapon_category),
    by = "victim_weapon"
  )
weapon_kills_na2022_j

## compare ----
weapon_kills_na2022_j |> 
  distinct(match_id, round_id) |> 
  anti_join(
    weapon_kills_na2022_t |> 
      distinct(match_id, round_id),
    by = c('match_id', 'round_id')
  )

weapon_kills_na2022_t |> 
  distinct(match_id, round_id) |> 
  anti_join(
    weapon_kills_na2022_j |> 
      distinct(match_id, round_id),
    by = c('match_id', 'round_id')
  )

## finish ----
rifle_rifle_win_rates_by_player_na2022 <- bind_rows(
  weapon_kills_na2022_j |> 
    mutate(
      ign = killer_ign, 
      player_id = killer_id, 
      is_killer = TRUE
    ),
  weapon_kills_na2022_j |> 
    mutate(
      ign = victim_ign, 
      player_id = victim_id,
      is_killer = FALSE
    )
) |> 
  ## could due ALL rifles for real for real
  # filter(
  #   killer_weapon_category == "rifle" &
  #   victim_weapon_category == "rifle"
  # ) |>
  filter(
    (killer_weapon %in% c("phantom", "vandal")) &
      (victim_weapon %in% c("phantom", "vandal"))
  ) |>
  count(player_id, ign, is_killer, sort = TRUE) |> 
  group_by(player_id, ign) |> 
  mutate(total = sum(n), prop = n / total) |> 
  ungroup() |> 
  filter(is_killer) |>
  select(-is_killer) |> 
  arrange(desc(prop))
rifle_rifle_win_rates_by_player_na2022

last_player_team_mapping_na2022 <- series_team_player_mapping_na2022 |> 
  group_by(player_id) |> 
  slice_max(series_id, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  select(player_id, team_id) |> 
  left_join(
    players |> select(player_id = id, ign),
    by = "player_id"
  ) |> 
  left_join(
    teams |> 
      select(team_id = id, last_team = name),
    by = "team_id"
  )

top_rifle_rifle_win_rates_na2022 <- rifle_rifle_win_rates_by_player_na2022 |> 
  filter(total >= 200) |> 
  inner_join(
    last_player_team_mapping_na2022 |> select(player_id, last_team),
    by = "player_id"
  ) |> 
  transmute(
    rank = row_number(desc(prop)),
    ign,
    last_team,
    n,
    total,
    prop
  )
top_rifle_rifle_win_rates_na2022 |> head(20)
