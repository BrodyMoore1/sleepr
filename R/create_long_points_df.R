#' Create Long Points Dataframe
#'
#' Generates a detailed dataframe of individual player points for each roster and week in a fantasy football league.
#'
#' This function combines weekly points, user information, roster details, and player metadata to produce a comprehensive
#' long-format dataframe with player-level details for each week.
#'
#' @param league_id A string representing the ID of the fantasy football league (default: "1124848060283768832").
#' @param nfl_start_dt A string representing the start date of the NFL season (default: "2024-09-05").
#' @param player_df A dataframe containing player metadata. If not provided, the function dynamically retrieves player data using \code{get_player_df}.
#' @return A dataframe (tibble) containing long-format player points data, including:
#'   \describe{
#'     \item{\code{week}}{The week number of the NFL season.}
#'     \item{\code{roster_id}}{The ID of the roster.}
#'     \item{\code{display_name}}{The display name of the user.}
#'     \item{\code{team_name}}{The name of the team.}
#'     \item{\code{player_id}}{The ID of the player.}
#'     \item{\code{player_name}}{The full name of the player.}
#'     \item{\code{player_position}}{The position of the player (e.g., QB, RB, WR, etc.).}
#'     \item{\code{player_points}}{The points scored by the player during the week.}
#'     \item{\code{is_starter}}{A designation indicating if the player is a starter or on the bench.}
#'   }
#' @examples
#' \dontrun{
#' # Fetch long-format player points data for a league
#' long_points_df <- create_long_points_df(
#'   league_id = "1124848060283768832",
#'   nfl_start_dt = "2024-09-05"
#' )
#' print(long_points_df)
#' }
#' @importFrom dplyr left_join select arrange mutate
#' @export
create_long_points_df <- function(league_id = "1124848060283768832", nfl_start_dt = "2024-09-05", player_df = NA) {

  # Check if player_df is provided; if not, fetch it dynamically
  if (!is.data.frame(player_df)) {
    player_df <- get_player_df()
  }

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

  # Combine all data into long-format player points dataframe
  long_points_df <- points_list$long_points_df %>%
    dplyr::left_join(name_df, by = "roster_id") %>%
    dplyr::left_join(player_df, by = "player_id") %>%
    dplyr::select(
      week,
      roster_id,
      display_name,
      team_name,
      player_id,
      player_name,
      player_position,
      player_points,
      is_starter
    )

  return(long_points_df)
}
