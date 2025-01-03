#' Create User Dataframe
#'
#' Fetches user information for a specified fantasy football league and organizes it into a dataframe.
#'
#' This function retrieves user details from the Sleeper API for a given league ID. It includes the user's display name,
#' team name, and user ID, with a fallback to a default team name if no custom name is provided.
#'
#' @param league_id A string representing the ID of the fantasy football league (default: "1124848060283768832").
#' @return A dataframe (tibble) containing user information, including:
#'   \describe{
#'     \item{\code{league_id}}{The ID of the league.}
#'     \item{\code{user_id}}{The ID of the user.}
#'     \item{\code{display_name}}{The display name of the user.}
#'     \item{\code{team_name}}{The team name, either as provided by the user or a default value ("Team <display_name>").}
#'   }
#' @examples
#' \dontrun{
#' # Fetch user data for a league
#' user_df <- create_user_df(league_id = "1124848060283768832")
#' print(user_df)
#' }
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate select
#' @export
create_user_df <- function(league_id = "1124848060283768832") {
  # API call to fetch user information
  user_response <- httr::GET(paste0("https://api.sleeper.app/v1/league/", league_id, "/users"))

  if (httr::status_code(user_response) == 200) {
    message("User API Successfully Called")

    # Parse API response
    user_content <- httr::content(user_response, "text")
    user_content_parsed <- jsonlite::fromJSON(user_content)

    # Process and return user dataframe
    user_df <- user_content_parsed %>%
      dplyr::select(league_id, user_id, display_name) %>%
      dplyr::mutate(
        team_name = dplyr::coalesce(user_content_parsed$metadata$team_name, paste0("Team ", display_name))
      )
    return(user_df)
  } else {
    stop("User API call failed.")
  }
}
