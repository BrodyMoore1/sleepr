#' Create Awards
#'
#' Generates a comprehensive set of awards for a fantasy football league based on team and player performance.
#'
#' This function analyzes league data to calculate and assign awards for achievements like the highest individual
#' scoring player, most efficient manager, smallest margin of loss, and many others. The function processes weekly points,
#' aggregated team data, and player metadata to identify award winners and provide detailed descriptions of their accomplishments.
#'
#' @param league_id A string representing the ID of the fantasy football league (default: "1124848060283768832").
#' @param nfl_start_dt A string representing the start date of the NFL season (default: "2024-09-05").
#' @return A dataframe (tibble) containing awards, with the following columns:
#'   \describe{
#'     \item{\code{award_name}}{The name of the award.}
#'     \item{\code{award_winner}}{The name of the award recipient.}
#'     \item{\code{award_description}}{A brief description of the award.}
#'     \item{\code{award_details}}{Detailed information about why the award was given.}
#'   }
#' @examples
#' \dontrun{
#' # Generate awards for a league
#' awards_df <- create_awards(league_id = "1124848060283768832", nfl_start_dt = "2024-09-05")
#' print(awards_df)
#' }
#' @importFrom dplyr select arrange mutate group_by summarise ungroup bind_rows filter slice
#' @importFrom tidyr pivot_wider
#' @export
create_awards <- function(league_id = "1124848060283768832", nfl_start_dt = "2024-09-05") {
  # Load Data frames
  player_df <- get_player_df()
  combined_df <- create_aggregated_points_df(league_id = league_id, nfl_start_dt = nfl_start_dt, player_df = player_df)
  long_points_df <- create_long_points_df(league_id = league_id, nfl_start_dt = nfl_start_dt, player_df = player_df)


  award_selector <- function(data){data %>% select(award_name, award_winner = display_name, award_description, award_details)}

  #Highest Scoring Player
  nuclear_award <- long_points_df %>%
    arrange(desc(player_points)) %>%
    slice(1) %>%
    mutate(
      award_name = "The Nuclear Award",
      award_description = "Highest Individual Scoring Player in a Week",
      award_details = paste0(player_name, " in week ", week, " scored ", player_points, " points for ", display_name)
    ) %>%
    award_selector()

  # Most Efficient Manager
  efficiency_awards <- combined_df %>%
    group_by(display_name) %>%
    summarise(
      total_starter_points = sum(starter_points),
      total_optimal_points = sum(optimal_starter_points)
    ) %>%
    ungroup() %>%
    mutate(
      efficiency_pct =  total_starter_points / total_optimal_points
    ) %>%
    arrange(desc(efficiency_pct)) %>%
    mutate(
      award_name = case_when(efficiency_pct == max(efficiency_pct) ~ "The Shallow Bench Award",
                             efficiency_pct == min(efficiency_pct) ~ "Least Optimal Award",
                             TRUE ~ NA
      )
    ) %>%
    filter(!is.na(award_name)) %>%
    mutate(
      award_description = if_else(award_name == "The Shallow Bench Award", "Highest Efficiency in Setting Lineups", "Lowest Efficiency in Setting Lineups"),
      award_details = if_else(award_name == "The Shallow Bench Award", paste0(str_to_title(display_name), " had ", round(efficiency_pct*100,2),"% ", "in setting the optimal lineup"), paste0(str_to_title(display_name), " had ", round(efficiency_pct*100,2),"% ", "in setting the optimal lineup"))
    ) %>%
    award_selector()

  # Smallest Average Margin of loss // Smallest Overall margin of loss
  margin_of_loss_df_setup <- combined_df %>%
    filter(is_win == 0) %>%
    mutate(margin_of_loss = opponent_starter_points - starter_points)

  margin_of_loss_df <- margin_of_loss_df_setup %>%
    group_by(display_name) %>%
    summarise(
      avg_margin = mean(margin_of_loss),
      min_margin = min(margin_of_loss),
      max_margin = max(margin_of_loss)
    ) %>%
    ungroup()

  # Heart Breaker Award
  heartbreaker_award <- margin_of_loss_df %>%
    filter(avg_margin == min(avg_margin)) %>%
    mutate(
      award_name = "The Heartbreaker Award",
      award_description = "Smallest Average Margin of Loss",
      award_details = paste0(str_to_title(display_name), " had an average margin of loss of ", round(avg_margin, 2))
    ) %>%
    award_selector()

  # Get the Almost There Award
  smallest_margin <- margin_of_loss_df %>% filter(min_margin == min(min_margin)) %>% pull(min_margin)

  almost_there_award <- margin_of_loss_df_setup %>%
    filter(margin_of_loss == smallest_margin) %>%
    mutate(
      award_name = "The Almost There Award",
      award_description = "Smallest Overall Margin of Loss",
      award_details = paste0("In Week ", week," ", display_name, " lost by ", round(margin_of_loss, 2), " points against ", opponent_name)
    ) %>%
    award_selector()

  # Crash and Burn Award
  largest_margin <- margin_of_loss_df %>% filter(max_margin == max(max_margin)) %>% pull(max_margin)

  crash_and_burn_award <- margin_of_loss_df_setup %>%
    filter(margin_of_loss == largest_margin) %>%
    mutate(
      award_name = "The Crash and Burn Award",
      award_description = "Largest Overall Margin of Loss",
      award_details = paste0("In Week ", week," ", display_name, " lost by ", round(margin_of_loss, 2), " points against ", opponent_name)
    ) %>%
    award_selector()

  #Largest Overall Week / Smallest Overall Week
  goose_juggarnaut_award <- combined_df %>%
    filter(week <= 16) %>%
    arrange(desc(starter_points)) %>%
    mutate(
      top_rank = row_number()
    ) %>%
    arrange(starter_points) %>%
    mutate(
      bottom_rank = row_number()
    ) %>%
    filter(
      top_rank <= 10 | bottom_rank <= 10
    ) %>%
    filter(top_rank == 1 | bottom_rank == 1) %>%
    mutate(
      award_name = case_when(starter_points == min(starter_points) ~ "The Goose Egg Award",
                             starter_points == max(starter_points) ~ "The Juggernaut Award"),
      award_description = case_when(starter_points == min(starter_points) ~ "Smallest Overall Week",
                                    starter_points == max(starter_points) ~ "Largest Overall Week"),
      award_details = case_when(starter_points == min(starter_points) ~ paste0(str_to_title(display_name), " had ", starter_points, " in week ", week),
                                starter_points == max(starter_points) ~ paste0(str_to_title(display_name), " had ", starter_points, " in week ", week)),
    ) %>%
    award_selector()

  # Most Injury Prone Team (Based on Original Drafted players)

  injury_setup <- long_points_df %>%
    filter(week == 1) %>%
    select(roster_id, display_name, player_id) %>%
    left_join(player_df %>% select(player_id, player_name, injury_status), by = "player_id") %>%
    filter(!is.na(injury_status) & injury_status != "Questionable")

  ir_award_setup <- injury_setup %>%
    count(roster_id, display_name) %>%
    select(roster_id, display_name, num_injuries = n) %>%
    arrange(desc(num_injuries)) %>%
    filter(num_injuries == max(num_injuries)) %>%
    mutate(
      award_name = "The IR All-Stars Award",
      award_description = "Most Injury Prone Team (based from drafted players)"
    )

  ir_players <- injury_setup %>% filter(roster_id == (ir_award_setup %>% pull(roster_id))) %>% pull(player_name)

  ir_all_star_award <- ir_award_setup %>%
    mutate(
      award_details = paste0(str_to_title(display_name), " had ", num_injuries, " on IR or Out at the end of the season from his original draft class (", paste0(ir_players, collapse = ", "), ")")
    ) %>%
    award_selector()


  # Most Points Against (AKA most dunked on)
  most_dunked_on_award <- combined_df %>%
    group_by(display_name) %>%
    summarise(
      pts_against = sum(opponent_starter_points)
    ) %>%
    ungroup() %>%
    filter(pts_against == max(pts_against)) %>%
    mutate(
      award_name = "Most Dunked On",
      award_description = "Most Points Against",
      award_details = paste0(str_to_title(display_name), " had ", pts_against, " scored against them")
    ) %>%
    award_selector()

  # add most points kept on the bench

  bench_awards_setup <- combined_df %>%
    filter(week <= 16) %>%
    mutate(
      unrealized_points = optimal_starter_points - starter_points
    ) %>%
    group_by(display_name) %>%
    summarise(
      tot_unrealized_pts = sum(unrealized_points),
      tot_bench_pts = sum(bench_points)
    ) %>%
    ungroup() %>%
    arrange(desc(tot_bench_pts))

  poorest_decision_maker_award <- bench_awards_setup %>%
    filter(
      tot_unrealized_pts == max(tot_unrealized_pts)
    ) %>%
    mutate(
      award_name = "Poorest Decision Maker Award",
      award_description = "Most Unrealized Points",
      award_details = paste0(str_to_title(display_name), " left ", tot_unrealized_pts, " points on his bench that they could've had")
    ) %>%
    award_selector()

  benchwarmer_award <- bench_awards_setup %>%
    filter(
      tot_bench_pts == max(tot_bench_pts)
    ) %>%
    mutate(
      award_name = "The Benchwarmer's Regret Award",
      award_description = "Most Points on the Bench",
      award_details = paste0(str_to_title(display_name), " had ", tot_bench_pts, " points on his bench")
    ) %>%
    award_selector()

  # Highest Scoring Bench Player
  bench_mvp_award <- long_points_df %>%
    filter(week <= 16) %>%
    filter(is_starter == "bench_points") %>%
    arrange(desc(player_points)) %>%
    filter(player_points == max(player_points)) %>%
    mutate(
      award_name = "The Bench MVP Award",
      award_description = "Highest Scoring Bench Player",
      award_details = paste0(player_name, " scored ", player_points, " in week ", week, " on ", display_name, "'s bench")
    ) %>%
    award_selector()

  # Luckiest Team
  rabbits_foot_award <- combined_df %>%
    filter(week <= 16) %>%
    arrange(week, desc(starter_points)) %>%
    group_by(week) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    group_by(display_name, is_win) %>%
    summarise(
      avg_rank = mean(rank),
      avg_pts = mean(starter_points),
    ) %>%
    ungroup() %>%
    mutate(is_win = if_else(is_win == 1, "win", "loss")) %>%
    pivot_wider(
      names_from = is_win,
      values_from = c(avg_rank, avg_pts)
    ) %>%
    filter(avg_pts_win == min(avg_pts_win)) %>%
    mutate(
      award_name = "The Rabbit's Foot Award",
      award_description = "Luckiest Team based on average points in wins",
      award_details = paste0(str_to_title(display_name), " had the lowest average points in their wins with ", avg_pts_win, " points")
    ) %>%
    award_selector()

  # Combine them all
  nuclear_award %>%
    bind_rows(efficiency_awards) %>%
    bind_rows(heartbreaker_award) %>%
    bind_rows(almost_there_award) %>%
    bind_rows(crash_and_burn_award) %>%
    bind_rows(goose_juggarnaut_award) %>%
    bind_rows(ir_all_star_award) %>%
    bind_rows(most_dunked_on_award) %>%
    bind_rows(poorest_decision_maker_award) %>%
    bind_rows(benchwarmer_award) %>%
    bind_rows(bench_mvp_award) %>%
    bind_rows(rabbits_foot_award)

}
