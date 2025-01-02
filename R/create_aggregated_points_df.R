#' Create Aggregated Points Dataframe
#'
#' Aggregates and processes fantasy football league data, including weekly points, user information,
#' roster details, and season structure, into a single dataframe.
#'
#' This function combines multiple data sources, including league users, rosters, weekly points, and season schedule,
#' to produce a comprehensive dataframe with aggregated points and matchup details for each roster.
#'
#' @param league_id A string representing the ID of the fantasy football league (default: "1124848060283768832").
#' @param nfl_start_dt A string representing the start date of the NFL season (default: "2024-09-05").
#' @param player_df A dataframe containing player metadata. If not provided, the function dynamically retrieves player data using \code{get_player_df}.
#' @return A dataframe (tibble) containing aggregated league points and matchup details, including:
#'   \describe{
#'     \item{\code{month}}{The month corresponding to the week.}
#'     \item{\code{week}}{The week number of the NFL season.}
#'     \item{\code{roster_id}}{The ID of the roster.}
#'     \item{\code{display_name}}{The display name of the user.}
#'     \item{\code{team_name}}{The name of the team.}
#'     \item{\code{starter_points}}{The total points scored by the starters.}
#'     \item{\code{bench_points}}{The total points scored by the bench players.}
#'     \item{\code{optimal_starter_points}}{The total points scored by the optimal starters.}
#'     \item{\code{opponent_roster_id}}{The roster ID of the opponent.}
#'     \item{\code{opponent_name}}{The display name of the opponent.}
#'     \item{\code{opponent_starter_points}}{The total points scored by the opponent's starters.}
#'     \item{\code{is_win}}{A binary indicator of whether the roster won the matchup (1 = win, 0 = loss).}
#'   }
#' @examples
#' \dontrun{
#' # Fetch aggregated points for a league
#' aggregated_points_df <- create_aggregated_points_df(
#'   league_id = "1124848060283768832",
#'   nfl_start_dt = "2024-09-05"
#' )
#' print(aggregated_points_df)
#' }
#' @importFrom dplyr left_join select arrange mutate
#' @export
create_aggregated_points_df <- function(league_id = "1124848060283768832", nfl_start_dt = "2024-09-05", player_df = NA) {

  # Check if player_df is provided; if not, fetch it dynamically
  if (!is.data.frame(player_df)) {
    player_df <- get_player_df()
  }

  # Generate season dataframe
  season_list <- create_season_weekly_df(nfl_start_dt = nfl_start_dt)
  season_df <- season_list$season_df

  # Get weekly points
  points_list <- get_weekly_points(league_id = league_id, nfl_start_dt = nfl_start_dt, player_df = player_df)

  # Get user and roster data
  user_df <- create_user_df(league_id = league_id)
  roster_df <- create_roster_df(league_id = league_id)

  # Combine user and roster data
  name_df <- user_df %>%
    dplyr::left_join(roster_df, by = c("league_id" = "league_id", "user_id" = "owner_id")) %>%
    dplyr::select(
      roster_id, display_name, team_name
    ) %>%
    dplyr::arrange(roster_id) %>%
    dplyr::mutate(roster_id = as.character(roster_id))

  # Aggregate points and matchup details
  aggregated_df <- points_list$full_points_df %>%
    dplyr::left_join(name_df, by = "roster_id") %>%
    dplyr::left_join(name_df %>% dplyr::select(roster_id, opponent_name = display_name),
                     by = c("opponent_roster_id" = "roster_id")) %>%
    dplyr::left_join(season_df %>% dplyr::select(week, month), by = "week") %>%
    dplyr::select(
      month, week, roster_id, display_name, team_name, starter_points, bench_points,
      optimal_starter_points, opponent_roster_id, opponent_name,
      opponent_starter_points, is_win
    )

  return(aggregated_df)
}
