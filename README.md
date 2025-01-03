# SleepR

Welcome to SleepR, the ultimate toolkit for diving deep into fantasy football stats and stories. If you're tired of juggling endless spreadsheets and scattered data, SleepR is here to transform your league's chaos into clarity. With just a few lines of code, you'll have all the insights you need to relive every matchup and crown your season's MVPs.

## Core Functions

Here’s what SleepR brings to the table:

### 1. create_aggregated_points_df
Transform your weekly chaos into a streamlined summary. This function aggregates data by week and by team, giving you a clear view of points scored and key matchup stats. Whether you’re analyzing trends or strategizing for next week, this function is your go-to.

### 2. create_long_points_df
Go granular with this long-form data frame! Dive into player-level scores for every week. Perfect for spotting breakout performances, identifying under-performers, or simply basking in the glory of that one week your star player crushed it.

### 3. create_awards
Every season has its heroes and heartbreaks. Use this function to create a dataframe of season awards. Whether it's "Top Scorer," "Biggest Blowout," or "Most Painful Loss," let the data tell the story of your league.

## Pro Tip
Planning to use multiple functions? Speed things up with get_player_df! This function creates a player dataframe that can be reused across different workflows. Pass it in where applicable and let SleepR handle the rest.

## Installation

Install the development version of the package from GitHub:

```R
devtools::install_github("BrodyMoore1/SleepR")
