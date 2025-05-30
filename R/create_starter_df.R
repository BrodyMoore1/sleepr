#' Create a Starter Dataframe
#'
#' Generates a dataframe representing starters for a given roster.
#'
#' This function creates a dataframe with information about the starters for a specific roster.
#' Each row represents a starter and includes the roster ID, the starter's identifier, and a
#' column indicating that the player is a starter.
#'
#' @param roster_id A vector representing the ID(s) of the roster(s). This will be coerced to a character type.
#' @param starters A vector representing the player_ids for the starters.
#' @return A tibble (dataframe) with the following columns:
#'   \describe{
#'     \item{\code{roster_id}}{The ID of the roster, stored as a character string.}
#'     \item{\code{starters}}{The identifiers for the starters.}
#'     \item{\code{is_starter}}{A constant value "starter_points" indicating the player is a starter.}
#'   }
#' @examples
#' # Create a starter dataframe for a roster
#' create_starter_df(
#'   roster_id = 1,
#'   starters = c("11563", "9509", "8138",  "11631", "4039",  "4217",  "7496",  "11635", "1945" , "BAL")
#' )
#'
#' @export
create_starter_df <- function(roster_id, starters) {
  tibble::tibble(
    roster_id = as.character(roster_id),
    starters = starters,
    is_starter = "starter_points"
  )
}
