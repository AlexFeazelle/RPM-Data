## Functions for weekly updated script

library(cfbfastR)
library(tidyverse)


qb_weekly_updates <- function(week,season) {
#call the player stats function in cfbfastR to get the passing statistics
  
  weekly_updates_qb <-  cfbd_game_player_stats(year = season, week = week, category = "passing") |> 
#filter to only FBS schools   
   filter(conference %in% c("Pac-12","Big Ten", "FBS Independents","SEC","Mid-American","Sun Belt","Mountain West",
                            "American Athletic","Conference USA","ACC","Big 12")) |>  
  select(athlete_name,passing_attempts,passing_yds,passing_avg,passing_td,passing_int,team,passing_qbr,passing_completions) |> 
    arrange(-passing_avg) |> filter(passing_attempts >= 12) |> 
    select(-passing_qbr) |> rename('yds/att' = 'passing_avg') 
  view(weekly_updates_qb)
  
}

#qb_weekly_updates(12,2023)

rb_weekly_updates <- function(week,season) {
weekly_updates_rb <-  cfbd_game_player_stats(year = season, week = week, category = "rushing") |>  filter(conference %in% c("Pac-12","Big Ten", "FBS Independents","SEC","Mid-American","Sun Belt","Mountain West","American Athletic","Conference USA","ACC","Big 12"))
rb_pos_wk6 <- cfbd_team_roster(year = 2023) |> filter(position == "RB") |> select('athlete_id','position')
rb_pos_wk6$athlete_id <- as.numeric(as.character(rb_pos_wk6$athlete_id )) 
rb_weekly_updates <- right_join(weekly_updates_rb,rb_pos_wk6,by = "athlete_id")|> 
  select(athlete_name,team,position,rushing_yds,rushing_avg,rushing_car,rushing_td) |> 
  arrange(-rushing_yds) |> 
  filter(rushing_car >= 8) |> 
  rename('yds/att' = 'rushing_avg')


view(rb_weekly_updates)
}

#rb_weekly_updates(12,2023)

wr_weekly_updates <- function(week,season) {
  weekly_updates_wr <-  cfbd_game_player_stats(year = season, week = week, category = "receiving") |>  
    filter(conference %in% c("Pac-12","Big Ten", "FBS Independents","SEC","Mid-American","Sun Belt","Mountain West","American Athletic",
                             "Conference USA","ACC","Big 12"))
  cfbd_game_player_stats(year = 2023, category = "receiving",week = 11)
  wr_pos_wk5 <- cfbd_team_roster(year = 2023) |> filter(position %in% c("WR", "CB")) |> 
    select('athlete_id','position')
  wr_pos_wk5$athlete_id <- as.numeric(as.character(wr_pos_wk5$athlete_id )) 
  wr_weekly_updates_2 <- right_join(weekly_updates_wr,wr_pos_wk5,by = "athlete_id") |> 
    select(athlete_name,team,position,receiving_yds,receiving_avg,receiving_rec,receiving_td) |> 
    arrange(-receiving_yds) |> filter(receiving_rec >= 4)
  view(wr_weekly_updates_2)
  
  
}

#wr_weekly_updates(12,2023)

te_weekly_updates <- function(week,season) {
  weekly_updates_te <-  cfbd_game_player_stats(year = season, week = week, category = "receiving") |>  
    filter(conference %in% c("Pac-12","Big Ten", "FBS Independents","SEC","Mid-American","Sun Belt","Mountain West",
                             "American Athletic","Conference USA","ACC","Big 12"))
  te_pos_wk6 <- cfbd_team_roster(year = 2023) |> filter(position == "TE") |> 
    select('athlete_id','position')
  te_pos_wk6$athlete_id <- as.numeric(as.character(te_pos_wk6$athlete_id)) 
  te_weekly_updates <- right_join(weekly_updates_te,te_pos_wk6,by = "athlete_id")|> 
    select(athlete_name,team,position,receiving_yds,receiving_avg,receiving_rec,receiving_td) |> 
    arrange(-receiving_yds) |> filter(receiving_rec > 2)  |> 
    rename('yds/att' = 'receiving_avg')
  view(te_weekly_updates)
}
  
#te_weekly_updates(12,2023)
