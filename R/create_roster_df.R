#' Create Roster Dataframe
#'
#' Fetches roster information for a specified fantasy football league and organizes it into a dataframe.
#'
#' This function retrieves roster details from the Sleeper API for a given league ID. It creates a mapping between
#' `owner_id` and `roster_id`, along with the associated league ID.
#'
#' @param league_id A string representing the ID of the fantasy football league (default: "1124848060283768832").
#' @return A dataframe (tibble) containing roster information, including:
#'   \describe{
#'     \item{\code{league_id}}{The ID of the league.}
#'     \item{\code{owner_id}}{The ID of the owner.}
#'     \item{\code{roster_id}}{The ID of the roster.}
#'   }
#' @examples
#' \dontrun{
#' # Fetch roster data for a league
#' roster_df <- create_roster_df(league_id = "1124848060283768832")
#' print(roster_df)
#' }
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select
#' @export
create_roster_df <- function(league_id = "1124848060283768832") {
  # API call to fetch roster information
  roster_response <- httr::GET(paste0("https://api.sleeper.app/v1/league/", league_id, "/rosters"), config(ssl_verifypeer = 0))

  if (httr::status_code(roster_response) == 200) {
    message("Roster API successfully called")

    # Parse API response
    roster_content <- httr::content(roster_response, "text")
    roster_content_parsed <- jsonlite::fromJSON(roster_content)

    # Process and return roster dataframe
    roster_df <- roster_content_parsed %>%
      dplyr::select(
        league_id,
        owner_id,
        roster_id
      )
    return(roster_df)
  } else {
    stop("Rosters API call failed.")
  }
}
