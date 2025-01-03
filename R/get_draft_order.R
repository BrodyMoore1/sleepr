#' Get Draft Order from Sleeper League
#'
#' This function retrieves and calculates the draft order for a specified Sleeper fantasy league. It integrates playoff and non-playoff team performances to generate the draft order based on team placements and total points scored.
#'
#' @param leauge_id A character string representing the ID of the Sleeper fantasy league. Defaults to "1069393183089586176".
#' @return A data frame containing the draft order, team names, the reason for their placement (playoff or non-playoff team), and a description of the reason.
#'
#' @details
#' The function performs the following steps:
#' - Retrieves aggregated points data for the league.
#' - Fetches playoff bracket information from the Sleeper API.
#' - Processes playoff results to determine team placements.
#' - Calculates placements for non-playoff teams based on total points scored.
#' - Combines playoff and non-playoff results into a final draft order.
#'
#' @examples
#' # Example usage:
#' draft_order <- get_draft_order("123456789012345678")
#' print(draft_order)
#'
#' @seealso
#' \code{\link[sleepr:create_aggregated_points_df]{create_aggregated_points_df}}
#' @importFrom dplyr distinct mutate filter arrange group_by summarise ungroup left_join select bind_rows
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @export
get_draft_order <- function(leauge_id = "1069393183089586176") {
  combined_df <- sleepr::create_aggregated_points_df(leauge_id)

  name_df <- combined_df %>%
    distinct(roster_id, display_name)

  bracket_response <- GET(paste0("https://api.sleeper.app/v1/league/", leauge_id,"/winners_bracket"))
  if (status_code(bracket_response) == 200) {
    message("Bracket API Successfully Called")
    bracket_content <- content(bracket_response, "text")
    bracket_content_parsed <- fromJSON(bracket_content)

    bracket_setup <- bracket_content_parsed %>%
      mutate(
        week_name = case_when(r == 1 ~ "quarter_finals",
                              r == 2 ~ "semi_finals",
                              r == 3 ~ "championship_week"),
        l = as.character(l),
        w = as.character(w)
      ) %>%
      left_join(name_df %>% select(roster_id, loser_name = display_name), by = c("l" = "roster_id") ) %>%
      left_join(name_df %>% select(roster_id, winner_name = display_name), by = c("w" = "roster_id") ) %>%
      filter(!is.na(p)) %>%
      select(winner_place = p, winner_name, loser_name) %>%
      arrange(winner_place) %>%
      mutate(loser_place = winner_place +1)

    playoff_group_order <- bracket_setup %>%
      select(place = winner_place, name = winner_name) %>%
      bind_rows(
        bracket_setup %>% select(place = loser_place, name = loser_name)
      ) %>%
      arrange(desc(place)) %>%
      mutate(
        reason = "playoff_team",
        reason_description = paste0("Placed ", place)
      )

    non_playoff_group_order <- combined_df %>%
      filter(!(display_name %in% (playoff_group_order %>% distinct(name) %>% pull()))) %>%
      filter(week <= 16) %>%
      group_by(display_name) %>%
      summarise(pts_for = sum(starter_points), .groups = "drop") %>%
      ungroup() %>%
      arrange(desc(pts_for)) %>%
      mutate(
        place = row_number(),
        reason = "non-playoff_team",
        reason_description = paste0("Scored ", pts_for, " points for")
      ) %>%
      select(place, name = display_name, reason, reason_description)

    non_playoff_group_order %>%
      bind_rows(playoff_group_order) %>%
      mutate(draft_order = row_number()) %>%
      select(draft_order, name, reason, reason_description)

  } else {
    message("Bracket API Failed")
  }
}
