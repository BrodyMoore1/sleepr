#' Optimize Starter Points
#'
#' Optimizes the allocation of starter points for each roster based on player positions and their scores.
#'
#' This function takes player data and assigns "optimal" or "sub_optimal" designations to players
#' based on their position and points scored. It optionally fetches player information using the
#' `get_player_df` function if a `player_df` is not provided. It then aggregates the total points
#' for "optimal" starters for each roster.
#'
#' @param data A dataframe containing player information for each roster, including columns such as:
#'   \describe{
#'     \item{\code{player_id}}{The unique ID of the player.}
#'     \item{\code{roster_id}}{The ID of the roster the player belongs to.}
#'     \item{\code{player_position}}{The position of the player (e.g., QB, RB, WR, etc.).}
#'     \item{\code{player_points}}{The points scored by the player.}
#'   }
#' @param player_df A dataframe containing player metadata (default: \code{NA}). If not provided,
#'   the function calls \code{get_player_df()} to fetch the player data dynamically.
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{optimizer_defined}}{A dataframe indicating whether each player is "optimal" or "sub_optimal".}
#'     \item{\code{optimizer_aggregated}}{A dataframe summarizing total "optimal" starter points for each roster.}
#'   }
#' @examples
#' \dontrun{
#' # Example usage with dynamically fetched player data
#' player_data <- tibble::tibble(
#'   roster_id = c(1, 1, 1, 2, 2),
#'   player_id = c("8138", "11563", "11631", "9509", "GB"),
#'   player_position = c("RB", "RB", "QB", "WR", "D"),
#'   player_points = c(25, 15, 20, 10, 8),
#'   is_starter = c("starter_points", "starter_points", "starter_points", "bench_points", "bench_points"),
#'   week = rep(16, 5)
#' )
#'
#' optimized_data <- optimize_starter_points(player_data)
#' optimizer_defined <- optimized_data$optimizer_defined
#' optimizer_aggregated <- optimized_data$optimizer_aggregated
#' print(optimizer_defined)
#' print(optimizer_aggregated)
#' }
#' @importFrom dplyr left_join arrange group_by mutate ungroup filter row_number bind_rows summarise desc
#' @importFrom tidyr pivot_wider
#' @export
optimize_starter_points <- function(data, player_df = NA) {
  # Check if player_df is provided; if not, fetch it dynamically
  if (!is.data.frame(player_df)) {
    player_df <- get_player_df()
  }

  # Define starters for non-flex positions
  non_flex_defined <- data %>%
    dplyr::left_join(player_df, by = "player_id") %>%
    dplyr::arrange(roster_id, player_position, dplyr::desc(player_points)) %>%
    dplyr::group_by(roster_id, player_position) %>%
    dplyr::mutate(position_rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(roster_id) %>%
    dplyr::mutate(
      position_choices = dplyr::case_when(
        player_position %in% c("DEF", "K", "QB") & position_rank == 1 ~ "optimal",
        player_position %in% c("DEF", "K", "QB") & position_rank > 1 ~ "sub_optimal",
        player_position %in% c("TE") & position_rank == 1 ~ "optimal",
        player_position %in% c("WR", "RB") & position_rank <= 2 ~ "optimal",
        TRUE ~ "sub_optimal"
      )
    ) %>%
    dplyr::ungroup()

  # Define flex starters
  flex_defined <- non_flex_defined %>%
    dplyr::filter(is.na(position_choices)) %>%
    dplyr::arrange(roster_id, dplyr::desc(player_points)) %>%
    dplyr::group_by(roster_id) %>%
    dplyr::mutate(
      flex_rank = dplyr::row_number(),
      position_choices = dplyr::if_else(flex_rank <= 2, "optimal", "sub_optimal")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-flex_rank)

  # Combine non-flex and flex starters
  optimizer_defined <- non_flex_defined %>%
    dplyr::filter(!is.na(position_choices)) %>%
    dplyr::bind_rows(flex_defined)

  # Aggregate optimal starter points
  optimizer_aggregated <- optimizer_defined %>%
    dplyr::group_by(roster_id, position_choices) %>%
    dplyr::summarise(pts = sum(player_points), .groups = 'drop') %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(values_from = pts, names_from = position_choices) %>%
    dplyr::select(
      roster_id,
      optimal_starter_points = optimal
    )

  # Return the results as a list
  list(
    "optimizer_defined" = optimizer_defined,
    "optimizer_aggregated" = optimizer_aggregated
  )
}
