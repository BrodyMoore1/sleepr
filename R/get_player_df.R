#' Retrieve NFL Player Dataframe
#'
#' Fetches NFL player data from the Sleeper API and organizes it into a dataframe.
#'
#' This function retrieves player data from the Sleeper API, extracts specific player
#' details such as player ID, name, position, and injury status, and organizes them
#' into a tidy dataframe. Additional player information can be added by modifying the
#' code to include other fields from the API response.
#'
#' @return A dataframe (tibble) containing NFL player information, including:
#'   \describe{
#'     \item{player_id}{The unique ID of the player.}
#'     \item{player_name}{The full name of the player.}
#'     \item{player_position}{The position of the player (e.g., QB, RB, WR, etc.).}
#'     \item{injury_status}{The current injury status of the player (if available).}
#'   }
#' @examples
#' \dontrun{
#' # Fetch player data and store it in a dataframe
#' player_data <- get_player_df()
#' print(player_data)
#' }
#' @export
get_player_df <- function() {
  # Bring in the player information
  player_response <- httr::GET("https://api.sleeper.app/v1/players/nfl",
                               config(ssl_verifypeer = 0))

  if (httr::status_code(player_response) == 200) {
    message("Player API call success")
    player_content <- httr::content(player_response, "text")
    player_parsed <- jsonlite::fromJSON(player_content)

    # Initialize an empty dataframe with specified columns
    player_ids <- names(player_parsed)
    player_df <- tibble::tibble(
      player_id = NA_character_,
      player_name = NA_character_,
      player_position = NA_character_,
      injury_status = NA_character_
    ) %>% tidyr::drop_na()

    # Loop through player IDs and extract relevant data
    for (i in seq_along(player_ids)) {
      player_of_interest <- player_ids[i]
      specific_player_data <- player_parsed[[player_of_interest]]

      new_row <- tibble::tibble(
        player_id = player_of_interest,
        player_name = specific_player_data[['full_name']],
        player_position = specific_player_data[['position']],
        injury_status = specific_player_data[['injury_status']]
      )

      player_df <- dplyr::bind_rows(player_df, new_row)
    }

    return(player_df)
  } else {
    stop(paste0("API call failed with status code ", httr::status_code(player_response)))
  }
}
