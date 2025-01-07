#' Create Season Weekly Dataframe
#'
#' Generates a dataframe representing weekly start and end dates for an NFL season.
#'
#' This function creates a dataframe with details for each of the 18 NFL weeks, including
#' the week number, start and end dates, and the month corresponding to each week. It also
#' identifies the current week based on the current date.
#'
#' @param nfl_start_dt A string representing the start date of the NFL season (default: "2024-09-05").
#'   This date is used to calculate the weekly start and end dates.
#' @return A list containing:
#'   \describe{
#'     \item{season_df}{A dataframe (tibble) with columns:
#'       \itemize{
#'         \item{\code{week}: The week number (1 to 18).}
#'         \item{\code{start_dt}: The start date of each week (Thursday).}
#'         \item{\code{end_dt}: The end date of each week (Monday).}
#'         \item{\code{month}: The month of the start date.}
#'       }
#'     }
#'     \item{current_week}{A dataframe (tibble) containing details of the current week based on today's date.}
#'   }
#' @examples
#' \dontrun{
#' # Create the season weekly dataframe and identify the current week
#' season_data <- create_season_weekly_df("2024-09-05")
#' season_df <- season_data$season_df
#' current_week <- season_data$current_week
#' print(season_df)
#' print(current_week)
#' }
#' @export
create_season_weekly_df <- function(nfl_start_dt = "2024-09-05") {

  # Convert the start date to a Date object
  nfl_start_dt <- lubridate::ymd(nfl_start_dt)

  # Create a dataframe for 18 NFL weeks
  season_df <- tibble::tibble(
    week = 1:17,
    start_dt = nfl_start_dt + lubridate::weeks(0:16),       # Start date of each week (every Thursday)
    end_dt = nfl_start_dt + lubridate::weeks(0:16) + lubridate::days(4) # End date of each week (every Monday)
  ) %>%
    dplyr::mutate(month = lubridate::month(start_dt))

  # Identify the current week based on today's date
  current_week <- season_df %>%
    dplyr::filter(end_dt <= lubridate::today()) %>%
    dplyr::filter(week == max(week))

  # Return the season dataframe and current week as a list
  list(
    "season_df" = season_df,
    "current_week" = current_week
  )
}
