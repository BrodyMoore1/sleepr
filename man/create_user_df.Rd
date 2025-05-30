% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_user_df.R
\name{create_user_df}
\alias{create_user_df}
\title{Create User Dataframe}
\usage{
create_user_df(league_id = "1124848060283768832")
}
\arguments{
\item{league_id}{A string representing the ID of the fantasy football league (default: "1124848060283768832").}
}
\value{
A dataframe (tibble) containing user information, including:
\describe{
\item{\code{league_id}}{The ID of the league.}
\item{\code{user_id}}{The ID of the user.}
\item{\code{display_name}}{The display name of the user.}
\item{\code{team_name}}{The team name, either as provided by the user or a default value ("Team <display_name>").}
}
}
\description{
Fetches user information for a specified fantasy football league and organizes it into a dataframe.
}
\details{
This function retrieves user details from the Sleeper API for a given league ID. It includes the user's display name,
team name, and user ID, with a fallback to a default team name if no custom name is provided.
}
\examples{
\dontrun{
# Fetch user data for a league
user_df <- create_user_df(league_id = "1124848060283768832")
print(user_df)
}
}
