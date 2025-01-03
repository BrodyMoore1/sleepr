#' Get Weekly Points
#'
#' Retrieves and calculates weekly points for a fantasy football league, including starter points, bench points, and optimized starter points.
#'
#' This function fetches weekly matchup data from the Sleeper API for a specified league, processes player and team data,
#' and computes various point summaries for each week. It leverages the \code{create_season_weekly_df},
#' \code{create_starter_df}, and \code{optimize_starter_points} functions to organize and optimize the data.
#'
#' @param league_id A string representing the ID of the fantasy football league (default: "1124848060283768832").
#' @param nfl_start_dt A string representing the start date of the NFL season (default: "2024-09-05").
#' @param player_df A dataframe containing player metadata. If not provided, the function dynamically retrieves player data using \code{get_player_df}.
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{full_points_df}}{A dataframe summarizing weekly points for each roster, including starter points, bench points, and win/loss outcomes.}
#'     \item{\code{long_points_df}}{A detailed dataframe containing individual player points for each week and roster.}
#'   }
#' @examples
#' \dontrun{
#' # Fetch weekly points for a league
#' weekly_points <- get_weekly_points(
#'   league_id = "1124848060283768832",
#'   nfl_start_dt = "2024-09-05"
#' )
#' full_points_df <- weekly_points$full_points_df
#' long_points_df <- weekly_points$long_points_df
#' print(full_points_df)
#' print(long_points_df)
#' }
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr left_join mutate filter arrange group_by ungroup summarise rowwise bind_rows select coalesce desc
#' @importFrom tidyr pivot_wider
#' @export
get_weekly_points <- function(league_id = "1124848060283768832", nfl_start_dt = "2024-09-05", player_df = NA) {

  # Check if player_df is provided; if not, fetch it dynamically
  if (!is.data.frame(player_df)) {
    player_df <- get_player_df()
  }

  long_points_df <- tibble::tibble()
  full_points_df <- tibble::tibble()

  nfl_start_dt <- as.Date(nfl_start_dt)
  nfl_weeks <- create_season_weekly_df(nfl_start_dt)

  # Iterate through weeks up to the current week
  for (week_num in 1:nfl_weeks$current_week$week) {
    message(paste0("Collecting data for week ", week_num))

    matchup_url <- paste0("https://api.sleeper.app/v1/league/", league_id, "/matchups/", week_num)
    matchup_response <- httr::GET(matchup_url)

    if (httr::status_code(matchup_response) == 200) {
      matchup_data <- httr::content(matchup_response, as = "text")
      matchup_data_parsed <- jsonlite::fromJSON(matchup_data)

      # Process matchup data
      matchup_setup <- matchup_data_parsed %>%
        dplyr::select(roster_id, matchup_id) %>%
        dplyr::arrange(matchup_id) %>%
        tidyr::drop_na()

      matchup_num <- matchup_setup %>%
        dplyr::distinct(matchup_id) %>%
        dplyr::mutate(rn = dplyr::row_number()) %>%
        dplyr::filter(rn == max(rn)) %>%
        dplyr::pull(rn)

      matchup_df <- matchup_setup %>%
        dplyr::mutate(stadium = rep(c("home_roster_id", "opponent_roster_id"), matchup_num)) %>%
        tidyr::pivot_wider(names_from = stadium, values_from = roster_id) %>%
        dplyr::bind_rows(
          matchup_setup %>%
            dplyr::mutate(stadium = rep(c("opponent_roster_id", "home_roster_id"), matchup_num)) %>%
            tidyr::pivot_wider(names_from = stadium, values_from = roster_id)
        ) %>%
        dplyr::select(-matchup_id) %>%
        dplyr::arrange(home_roster_id) %>%
        dplyr::mutate(
          home_roster_id = as.character(home_roster_id),
          opponent_roster_id = as.character(opponent_roster_id)
        )

      weekly_points <- as.data.frame(unlist(matchup_data_parsed$players_points)) %>%
        dplyr::mutate(player_id = rownames(.))

      pts_df <- dplyr::as_tibble(weekly_points) %>%
        dplyr::rename(player_points = `unlist(matchup_data_parsed$players_points)`) %>%
        dplyr::filter(!is.na(player_points)) %>%
        dplyr::mutate(is_alpha = grepl("[A-Za-z]", player_id)) %>%
        dplyr::mutate(
          roster_id = dplyr::if_else(is_alpha, as.numeric(gsub(".*?(\\d+)$", "\\1", player_id)), NA)
        ) %>%
        tidyr::fill(roster_id, .direction = "up") %>%
        dplyr::rowwise() %>%
        dplyr::mutate(new_player_id = gsub(paste0(roster_id, "$"), "", player_id)) %>%
        dplyr::select(roster_id, player_id = new_player_id, player_points) %>%
        dplyr::mutate(roster_id = as.character(roster_id)) %>%
        dplyr::ungroup()

      starter_df <- dplyr::bind_rows(lapply(1:length(matchup_data_parsed$roster_id), function(i) {
        create_starter_df(matchup_data_parsed$roster_id[i], matchup_data_parsed$starters[[i]])
      }))

      team_df <- pts_df %>%
        dplyr::left_join(starter_df, by = c("roster_id" = "roster_id", "player_id" = "starters")) %>%
        dplyr::mutate(is_starter = dplyr::coalesce(is_starter, "bench_points")) %>%
        dplyr::arrange(roster_id, dplyr::desc(is_starter), dplyr::desc(player_points)) %>%
        dplyr::mutate(week = week_num)

      optimal_list <- optimize_starter_points(team_df, player_df = player_df)

      team_optimized_df <- team_df %>%
        dplyr::left_join(optimal_list$optimizer_defined %>%
                           dplyr::select(roster_id, player_id, player_name, position_choices),
                         by = c("roster_id", "player_id")
        )

      aggregated_weekly_points <- team_optimized_df %>%
        dplyr::group_by(week, roster_id, is_starter) %>%
        dplyr::summarise(points_for = sum(player_points), .groups = 'drop') %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = is_starter, values_from = points_for)

      final_pts_df <- aggregated_weekly_points %>%
        dplyr::left_join(matchup_df, by = c("roster_id" = "home_roster_id")) %>%
        dplyr::left_join(aggregated_weekly_points %>%
                           dplyr::select(roster_id, opponent_starter_points = starter_points),
                         by = c("opponent_roster_id" = "roster_id")
        ) %>%
        dplyr::arrange(as.numeric(roster_id)) %>%
        dplyr::mutate(is_win = dplyr::if_else(starter_points > opponent_starter_points, 1, 0)) %>%
        dplyr::left_join(optimal_list$optimizer_aggregated, by = "roster_id")

      full_points_df <- dplyr::bind_rows(full_points_df, final_pts_df)
      long_points_df <- dplyr::bind_rows(long_points_df, team_df)

    } else {
      message(paste0("Matchup API Call Failed For Week ", week_num))
    }
  }

  list(
    "full_points_df" = full_points_df,
    "long_points_df" = long_points_df
  )
}
