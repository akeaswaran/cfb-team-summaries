library(cfbfastR)
library(tidyverse)

ep_model = xgboost::xgb.load("./ep_model_1424.model")
weights <- c(7, -7, 3, -3, 2, -2, 0)

classes = c(
    "Touchdown", "Opp_Touchdown", "Field_Goal", "Opp_Field_Goal",
    "Safety", "Opp_Safety", "No_Score"
)

raw_play_df = cfbfastR::cfbd_plays(year = 2024, week = 14, team = "Georgia Tech", season_type = "regular")

drive_info <- cfbfastR::cfbd_drives(year = 2024, week = 14, team = "Georgia Tech", season_type = "regular")

clean_drive_df <- cfbfastR:::clean_drive_info(drive_info)

colnames(clean_drive_df) <- paste0("drive_", colnames(clean_drive_df))

play_df = raw_play_df %>%
    janitor::clean_names() %>%
    dplyr::mutate(drive_id = as.numeric(drive_id)) %>%
    dplyr::left_join(
        clean_drive_df,
         by = c(
             "drive_id" = "drive_drive_id",
             "game_id" = "drive_game_id"
         ),
         suffix = c("_play", "_drive")
    )

rm_cols <- c(
    "drive_game_id", "drive_id_drive", #' drive_drive_number',
    "drive_plays", "drive_start_yardline", "drive_end_yardline",
    "drive_offense", "drive_offense_conference",
    "drive_defense", "drive_defense_conference",
    "drive_start_time.hours", "drive_start_time.minutes", "drive_start_time.seconds",
    "drive_end_time.hours", "drive_end_time.minutes", "drive_end_time.seconds",
    "drive_elapsed.hours", "drive_elapsed.minutes", "drive_elapsed.seconds"
)

prep_epa_df_after <- function(dat) {
    ## --Play type vectors------
    turnover_play_type <- c(
        "Blocked Field Goal",
        "Blocked Field Goal Touchdown",
        "Blocked Punt",
        "Blocked Punt Touchdown",
        "Punt",
        "Punt Touchdown",
        "Punt Return Touchdown",
        "Sack Touchdown",
        "Field Goal Missed",
        "Missed Field Goal Return",
        "Missed Field Goal Return Touchdown",
        "Fumble Recovery (Opponent)",
        "Fumble Recovery (Opponent) Touchdown",
        "Interception",
        "Interception Return",
        "Interception Return Touchdown",
        "Pass Interception Return",
        "Pass Interception Return Touchdown",
        "Uncategorized Touchdown"
    )
    turnover_vec <- c(
        "Blocked Field Goal",
        "Blocked Field Goal Touchdown",
        "Blocked Punt",
        "Blocked Punt Touchdown",
        "Field Goal Missed",
        "Missed Field Goal Return",
        "Missed Field Goal Return Touchdown",
        "Fumble Recovery (Opponent)",
        "Fumble Recovery (Opponent) Touchdown",
        "Fumble Return Touchdown",
        "Defensive 2pt Conversion",
        "Interception",
        "Interception Return",
        "Interception Return Touchdown",
        "Pass Interception Return",
        "Pass Interception Return Touchdown",
        "Kickoff Team Fumble Recovery",
        "Kickoff Team Fumble Recovery Touchdown",
        "Punt Touchdown",
        "Punt Return Touchdown",
        "Sack Touchdown",
        "Uncategorized Touchdown"
    )
    normalplay <- c(
        "Rush",
        "Pass",
        "Pass Reception",
        "Pass Incompletion",
        "Pass Completion",
        "Sack"
    )
    penalty <- c(
        "Penalty",
        "Penalty (Kickoff)",
        "Penalty (Safety)"
    )
    offense_score_vec <- c(
        "Passing Touchdown",
        "Rushing Touchdown",
        "Field Goal Good",
        "Pass Reception Touchdown",
        "Fumble Recovery (Own) Touchdown",
        "Kickoff Return Touchdown",
        "Punt Touchdown",
        "Punt Team Fumble Recovery Touchdown"
    )
    defense_score_vec <- c(
        "Blocked Punt Touchdown",
        "Blocked Field Goal Touchdown",
        "Missed Field Goal Return Touchdown",
        "Punt Return Touchdown",
        "Fumble Recovery (Opponent) Touchdown",
        "Fumble Return Touchdown",
        "Defensive 2pt Conversion",
        "Safety",
        "Kickoff (Safety)",
        "Blocked Punt (Safety)",
        "Punt (Safety)",
        "Penalty (Safety)",
        "Sack Touchdown",
        "Interception Return Touchdown",
        "Pass Interception Return Touchdown",
        "Uncategorized Touchdown"
    )
    kickoff <- c(
        "Kickoff",
        "Kickoff Return (Offense)",
        "Kickoff Return Touchdown",
        "Kickoff Touchdown",
        "Kickoff Team Fumble Recovery",
        "Kickoff Team Fumble Recovery Touchdown",
        "Kickoff (Safety)",
        "Penalty (Kickoff)"
    )

    turnover_ind <- dat$play_type %in% turnover_play_type
    dat$turnover <- 0
    # define turnover on downs
    downs_turnover <- (dat$downs_turnover == 1)
    # data is ordered
    new_offense <- (dat$change_of_pos_team == 1)
    scoring_plays <- dat$play_type %in% offense_score_vec
    # end of half check as well
    end_of_half_plays <- !(dat$end_of_half == 0)
    # is specifically defined as a turnover
    turnover_play_check <- dat$play_type %in% turnover_vec
    # turnovers only occur on actual change of offense
    # but not scoring plays
    # and not at the end of half.
    # Turnovers now capture downs, when there is a change of offense after a fourth down normal play.
    t_ind <- (turnover_ind | (new_offense)) &
        !scoring_plays & !end_of_half_plays &
        (turnover_play_check | downs_turnover)

    dat$turnover[t_ind] <- 1

    dat <- dat %>%
        dplyr::ungroup() %>%
        dplyr::group_by(game_id, half) %>%
        dplyr::arrange(id_play, .by_group = TRUE) %>%
        dplyr::mutate(
            turnover_indicator =
                ifelse(
                    (play_type %in% defense_score_vec) |
                        (play_type %in% turnover_vec) |
                        ((play_type %in% normalplay | play_type == "Fumble Recovery (Own)") &
                             yards_gained < distance &
                             down == 4), 1, 0
                ),
            down = as.numeric(down),
            lead_down = as.numeric(lead_down),
            lead_down2 = as.numeric(lead_down2),
            lead_distance = as.numeric(lead_distance),
            yards_gained = as.numeric(yards_gained),
            #--New Down-----
            new_down = as.numeric(dplyr::case_when(
                #--- turnovers ---
                turnover == 1 ~ 1,
                play_type == "Timeout" ~ down,
                ## --Penalty Cases (new_down)-----
                # 8 cases with three T/F penalty flags
                # 4 cases in 1
                play_type %in% penalty & penalty_1st_conv ~ 1,
                # offsetting penalties, no penalties declined, no 1st down by penalty (1 case)
                play_type %in% penalty & !penalty_declined &
                    penalty_offset & !penalty_1st_conv ~ down,
                # offsetting penalties, penalty declined true, no 1st down by penalty
                # seems like it would be a regular play at that point (1 case, split in three)
                play_type %in% penalty & penalty_declined &
                    penalty_offset & !penalty_1st_conv &
                    yards_gained < distance & down <= 3 ~ down + 1,
                play_type %in% penalty & penalty_declined &
                    penalty_offset & !penalty_1st_conv &
                    yards_gained < distance & down == 4 ~ 1,
                play_type %in% penalty & penalty_declined &
                    penalty_offset & !penalty_1st_conv &
                    yards_gained >= distance ~ 1,
                # only penalty declined true, same logic as prior (1 case, split in three)
                play_type %in% penalty & penalty_declined &
                    !penalty_offset & !penalty_1st_conv &
                    yards_gained < distance & down <= 3 ~ down + 1,
                play_type %in% penalty & penalty_declined &
                    !penalty_offset & !penalty_1st_conv &
                    yards_gained < distance & down == 4 ~ 1,
                play_type %in% penalty & penalty_declined &
                    !penalty_offset & !penalty_1st_conv &
                    yards_gained >= distance ~ 1,
                # no other penalty flags true, lead on down (1 case)
                play_type %in% penalty & !penalty_declined &
                    !penalty_offset & !penalty_1st_conv & lead_down != 0 ~ lead_down,
                # no other penalty flags true, lead on down is 0 (1 case for end of period plays)
                play_type %in% penalty & !penalty_declined &
                    !penalty_offset & !penalty_1st_conv & lead_down == 0 ~ lead_down2,
                ## --Scores, kickoffs,  defensive scores----
                play_type %in% offense_score_vec ~ 1,
                play_type %in% kickoff ~ 1,
                play_type %in% defense_score_vec ~ 1,
                ## --Regular Plays----
                # regular play 1st down
                (play_type %in% normalplay | play_type == "Fumble Recovery (Own)") & yards_gained >= distance ~ 1,
                # iterate to next down due to not meeting the yards to gain
                (play_type %in% normalplay | play_type == "Fumble Recovery (Own)") &
                    yards_gained < distance & down <= 3 ~ as.integer(down) + 1,
                # turnover on downs
                (play_type %in% normalplay | play_type == "Fumble Recovery (Own)") &
                    yards_gained < distance & down == 4 ~ 1,
                play_type %in% c("Uncategorized", "placeholder") & lead_down != 0 ~ lead_down,
                play_type %in% c("Uncategorized", "placeholder") & lead_down == 0 ~ lead_down2,
                play_type %in% c("End Period", "End of Half", "End of Game") & !is.na(lead_down) ~ 0
            )),
            drive_start_yards_to_goal = as.numeric(drive_start_yards_to_goal),
            #--New Distance-----
            new_distance = as.numeric(dplyr::case_when(
                ## --turnovers, defensive scores, scores, kickoffs
                turnover == 1 ~ 10,
                ## --Penalty cases (new_distance)
                #--offsetting penalties, keep same distance
                play_type %in% penalty &
                    penalty_offset ~ as.numeric(distance),
                #--penalty first down conversions, 10 or to goal
                play_type %in% penalty &
                    penalty_1st_conv ~ as.numeric(ifelse(yards_to_goal - yards_gained <= 10 &
                                                                   yards_to_goal - yards_gained >= 0,
                                                               as.numeric(yards_to_goal), 10
                    )),
                #--penalty without first down conversion
                play_type %in% penalty & !penalty_declined &
                    !penalty_1st_conv &
                    !penalty_offset ~ as.numeric(ifelse((yards_gained >= distance) &
                                                                  (yards_to_goal - yards_gained <= 10) &
                                                                  yards_to_goal - yards_gained >= 0,
                                                              as.numeric(yards_to_goal), 10
                    )),
                ## --normal plays
                (play_type %in% normalplay | play_type == "Fumble Recovery (Own)") &
                    yards_gained >= distance &
                    (yards_to_goal - yards_gained >= 10) ~ 10,
                (play_type %in% normalplay | play_type == "Fumble Recovery (Own)") &
                    yards_gained >= distance &
                    (yards_to_goal - yards_gained <= 10) ~ as.numeric(yards_to_goal - yards_gained),
                (play_type %in% normalplay | play_type == "Fumble Recovery (Own)") &
                    yards_gained < distance & down <= 3 ~ as.numeric(distance - yards_gained),
                (play_type %in% normalplay | play_type == "Fumble Recovery (Own)") &
                    yards_gained < distance &
                    down == 4 & (100 - (yards_to_goal - yards_gained) >= 10) ~ 10,
                (play_type %in% normalplay | play_type == "Fumble Recovery (Own)") &
                    yards_gained < distance & down == 4 &
                    (100 - (yards_to_goal - yards_gained) <= 10) ~ as.numeric(100 - yards_to_goal),
                play_type %in% defense_score_vec ~ 10,
                play_type %in% offense_score_vec ~ 10,
                play_type %in% kickoff ~ 10,
                !is.na(lead_distance) & lead_play_type %in% c("Kickoff") ~ 10,
                !is.na(lead_distance) & lead_play_type %in% c("Timeout") &
                    !(lead_play_type2 %in% c("Timeout")) ~ as.numeric(lead_distance2),
                !is.na(lead_distance) & !(lead_play_type %in% c(
                    "Kickoff", "Timeout",
                    "End Period", "End of Half", "End of Game"
                )) ~ as.numeric(lead_distance)
            )),
            #--New Yardline----
            new_yardline = as.numeric(dplyr::case_when(
                downs_turnover == 1 & punt == 0 ~ 100 - yards_to_goal + yards_gained,
                play_type %in% penalty & penalty_offset & kickoff_play == 0 ~ yards_to_goal,
                play_type %in% penalty & !penalty_offset & kickoff_play == 0 ~ yards_to_goal - yards_gained,
                play_type %in% normalplay ~ yards_to_goal - yards_gained,
                play_type %in% offense_score_vec ~ 0,
                play_type %in% defense_score_vec ~ 0,
                play_type %in% kickoff ~ lead_yards_to_goal,
                play_type %in% c(
                    "Blocked Punt", "Punt", "Blocked Field Goal", "Fumble Recovery (Opponent)",
                    "Field Goal Missed", "Missed Field Goal Return",
                    "Fumble Recovery (Own)", "Interception Return", "Kickoff",
                    "Punt Team Fumble Recovery"
                ) ~ lead_yards_to_goal,
                play_type %in% turnover_vec ~ 100 - yards_to_goal + yards_gained,
                !is.na(lead_yards_to_goal) ~ lead_yards_to_goal
            )),
            new_TimeSecsRem = ifelse(!is.na(lead_TimeSecsRem), lead_TimeSecsRem, 0),
            # new_Goal_To_Go = ifelse(new_yardline <= new_distance, TRUE, FALSE),
            # new under two minute warnings
            # new_Under_two = new_TimeSecsRem <= 120,
            #----- Series/down-set variable creation --------
            # TODO - Add these variables to the documentation and select outputs
            row = 1:dplyr::n(),
            first_by_penalty = ifelse(play_type %in% penalty & penalty_1st_conv, 1,
                                      ifelse(play_type %in% penalty & penalty_declined &
                                                 penalty_offset & !penalty_1st_conv &
                                                 yards_gained > distance, 1,
                                             ifelse(play_type %in% penalty & penalty_declined &
                                                        !penalty_offset & !penalty_1st_conv &
                                                        yards_gained >= distance, 1, 0)
                                      )
            ),
            first_by_yards = ifelse((play_type %in% normalplay | play_type == "Fumble Recovery (Own)") &
                                        yards_gained >= distance, 1, 0),
            lag_first_by_penalty3 = dplyr::lag(first_by_penalty, 3),
            lag_first_by_penalty2 = dplyr::lag(first_by_penalty, 2),
            lag_first_by_penalty = dplyr::lag(first_by_penalty, 1),
            lag_first_by_penalty = ifelse(is.na(lag_first_by_penalty) & !(lag_play_type %in% penalty) |
                                              row == 1, 0, lag_first_by_penalty),
            lag_first_by_yards3 = dplyr::lag(first_by_yards, 3),
            lag_first_by_yards2 = dplyr::lag(first_by_yards, 2),
            lag_first_by_yards = dplyr::lag(first_by_yards, 1),
            lag_first_by_yards = ifelse(is.na(lag_first_by_yards) & !(lag_play_type %in% normalplay) &
                                            (lag_play_type != "Fumble Recovery (Own)") | row == 1, 0, lag_first_by_yards),
            new_series = dplyr::if_else(
                id_drive != dplyr::lag(id_drive, 1) |
                    lag_first_by_yards == 1 | lag_first_by_penalty == 1 |
                    row == 1, 1, 0
            )

            # end TODO
        ) %>%
        dplyr::mutate_at(vars(new_TimeSecsRem), ~ replace_na(., 0)) %>%
        dplyr::group_by(game_id, half, drive_id) %>%
        dplyr::arrange(id_play, .by_group = TRUE) %>%
        dplyr::mutate(
            # TODO - Add these variables to the documentation and select outputs
            firstD_by_kickoff = ifelse(kickoff_play == 1 & down == 1, 1, 0),
            # end TODO
            firstD_by_poss = dplyr::case_when(
                #---- Drive Definition ----
                # 1-L.I) start by play after kickoff
                !(lag_play_type %in% c("Timeout", "End Period")) & #     condition: has play event 1 row prior
                    drive_event_number == 2 & lag_kickoff_play == 1 &
                    kickoff_play == 0 ~ 1,
                (lag_play_type %in% c("Timeout", "End Period")) & #     condition: has non-play event 1 row prior, looks 2 rows back
                    !(lag_play_type2 %in% c("Timeout", "End Period")) &
                    drive_event_number == 3 & lag_kickoff_play2 == 1 &
                    kickoff_play == 0 ~ 1,
                # 2-L.I) start by change of pos_team
                !(lag_play_type %in% c("Timeout", "End Period")) & #     condition: has play event 1 row prior
                    lag_change_of_pos_team == 1 &
                    (lag_punt == 1 | lag_downs_turnover == 1 | lag_turnover_vec == 1) ~ 1,
                # 2-L.II) start by change of pos_team with 1 non-play event in between
                (lag_play_type %in% c("Timeout", "End Period")) & #     condition: has non-play event 1 row prior, looks 2 rows back
                    lag_change_of_pos_team2 == 1 &
                    !(lag_play_type2 %in% c("Timeout", "End Period")) &
                    (lag_punt2 == 1 | lag_downs_turnover2 == 1 | lag_turnover_vec2 == 1) ~ 1,
                # 3-L.I) start by opponent scoring play
                !(lag_play_type %in% c("Timeout", "End Period")) & #    condition: has play event 1 row prior
                    lag_scoring_play == 1 & kickoff_play != 1 ~ 1,
                # 3-L.II) start by opponent scoring play with 1 non-play event in between
                (lag_play_type %in% c("Timeout", "End Period")) & #     condition: has non-play event 1 row prior, looks 2 rows back
                    !(lag_play_type2 %in% c("Timeout", "End Period")) &
                    lag_scoring_play2 == 1 & kickoff_play != 1 ~ 1,
                # 4) start of half plays start drives
                (drive_event_number == 1 & kickoff_play != 1) ~ 1,
                TRUE ~ 0
            ),
            firstD_by_penalty = ifelse((lag_first_by_penalty == 1 & lag_change_of_pos_team != 1 &
                                            !(lag_play_type %in% c("Timeout", "End Period"))) |
                                           (lag_first_by_penalty2 == 1 & lag_change_of_pos_team2 != 1 &
                                                (lag_play_type %in% c("Timeout", "End Period"))) |
                                           (lag_first_by_penalty3 == 1 & lag_change_of_pos_team3 != 1 &
                                                (lag_play_type %in% c("Timeout", "End Period") & (lag_play_type2 %in% c("Timeout", "End Period")))), 1, 0),
            firstD_by_penalty = dplyr::case_when(
                (lag_first_by_penalty == 1 & lag_change_of_pos_team != 1 &
                     !(lag_play_type %in% c("Timeout", "End Period"))) ~ 1,
                lag_first_by_penalty2 == 1 & lag_change_of_pos_team2 != 1 &
                    (lag_play_type %in% c("Timeout", "End Period")) ~ 1,
                lag_first_by_penalty3 == 1 & lag_change_of_pos_team3 != 1 &
                    (lag_play_type %in% c("Timeout", "End Period") & (lag_play_type2 %in% c("Timeout", "End Period"))) ~ 1,
                TRUE ~ 0
            ),
            firstD_by_yards = ifelse((lag_first_by_yards == 1 & lag_change_of_pos_team != 1 &
                                          !(lag_play_type %in% c("Timeout", "End Period"))) |
                                         (lag_first_by_yards2 == 1 & lag_change_of_pos_team2 != 1 &
                                              (lag_play_type %in% c("Timeout", "End Period"))) |
                                         (lag_first_by_yards3 == 1 & lag_change_of_pos_team3 != 1 &
                                              (lag_play_type %in% c("Timeout", "End Period") & (lag_play_type2 %in% c("Timeout", "End Period")))), 1, 0),
            new_id = id_play
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(new_id, .by_group = TRUE) %>%
        # dplyr::select(-play, -half_play, -drive_play) %>%
        dplyr::mutate(
            new_yardline = ifelse(kickoff_play == 1 & kickoff_tb == 1, 75, new_yardline),
            new_yardline = ifelse(end_of_half == 1, 100, new_yardline),
            new_distance = ifelse(new_yardline <= 0, lead_distance2, new_distance),
            new_yardline = ifelse(new_yardline <= 0, lead_yards_to_goal, new_yardline),
            #--Punt Plays--------------------------
            new_down = ifelse(punt == 1 | kickoff_play == 1, 1, new_down),
            new_distance = ifelse(punt == 1, 10, new_distance),
            # new_log_ydstogo = ifelse(punt == 1, log(10), new_log_ydstogo),
            # new_Goal_To_Go = ifelse(punt == 1, FALSE, new_Goal_To_Go),
            # new_pos_score_diff_start = ifelse(change_of_pos_team == 0, pos_score_diff, -1 * pos_score_diff),
            new_down = ifelse(kickoff_play == 1, 1, new_down)
        )

    #--End of Half Plays--------------------------
    end_of_half_plays <- (dat$new_TimeSecsRem == 0 |
                              (dat$end_of_half == 1 & !(dat$play_type %in% c("End Period", "End of Half", "End of Game"))))

    if (any(end_of_half_plays)) {
        dat$new_yardline[end_of_half_plays] <- 100
        dat$new_down[end_of_half_plays] <- 4
        dat$new_distance[end_of_half_plays] <- 100
        dat$end_of_half[end_of_half_plays] <- 1
        # dat$new_log_ydstogo[end_of_half_plays] <- log(100)
        # dat$new_Under_two[end_of_half_plays] <- dat$new_TimeSecsRem[end_of_half_plays] <= 120
    }

    # missed field goal needs to be here
    # needs to go before the na check to set to 99
    dat <- dat %>%
        dplyr::mutate(
            new_yardline = dplyr::if_else(is.na(new_yardline) &
                                              play_type %in% c("Field Goal Missed", "Blocked Field Goal"),
                                          100 - (yards_to_goal - 9),
                                          new_yardline
            )
        )

    #--General weird plays that don't have an easy fix----
    na_yd_line <- which(is.na(dat$new_yardline) | dat$new_yardline >= 100)
    dat$new_yardline[na_yd_line] <- dat$yardline[na_yd_line + 1]
    neg_distance <- which(dat$new_distance < 0)
    dat$new_distance[neg_distance] <- dat$distance[neg_distance + 1]
    # dat$new_log_ydstogo[neg_distance] <- log(dat$new_distance[neg_distance])

    #--Missing yd_line Plays--------------------------
    missing_yd_line <- dat$new_yardline == 0
    dat$new_yardline[missing_yd_line] <- 100
    # dat$new_log_ydstogo[missing_yd_line] <- log(100)
    dat$new_down[missing_yd_line] <- 1
    dat$missing_yard_flag <- FALSE
    dat$missing_yard_flag[missing_yd_line] <- TRUE

    dat <- dat %>%
        dplyr::arrange(id_play) %>%
        dplyr::mutate(
            new_yardline = ifelse(end_of_half == 1 & is.na(new_yardline), 100, new_yardline),
            new_id = gsub(pattern = unique(game_id), "", x = new_id),
            new_id = as.numeric(new_id)
        ) %>%
        dplyr::mutate(
            new_half_seconds_remaining = new_TimeSecsRem,
            new_down1 = dplyr::if_else(new_down == 1, 1, 0),
            new_down2 = dplyr::if_else(new_down == 2, 1, 0),
            new_down3 = dplyr::if_else(new_down == 3, 1, 0),
            new_down4 = dplyr::if_else(new_down == 4, 1, 0),
            # new_distance
            new_yards_to_goal = new_yardline,
            new_is_home = dplyr::if_else((change_of_pos_team == 1), (def_pos_team == home), (pos_team == home)),

            new_pos_team_timeouts = dplyr::if_else((change_of_pos_team == 1), def_pos_team_timeouts, pos_team_timeouts),
            new_def_pos_team_timeouts = dplyr::if_else((change_of_pos_team == 1), pos_team_timeouts, def_pos_team_timeouts),
        )



    return(dat)
}


play_df <- play_df %>%
    dplyr::select(dplyr::setdiff(names(play_df), rm_cols)) %>%
    dplyr::rename(
        "drive_pts" = "drive_pts_drive",
        "drive_result" = "drive_drive_result",
        "orig_drive_number" = "drive_drive_number",
        "id_play" = "play_id",
        "offense_play" = "offense",
        "defense_play" = "defense",
        "clock.minutes" = "clock_minutes",
        "clock.seconds" = "clock_seconds",
    ) %>%
    dplyr::mutate(
        season = 2024,
        wk = 14
    ) %>%
    cfbfastR:::penalty_detection() %>%
    cfbfastR:::add_play_counts() %>%
    cfbfastR:::clean_pbp_dat() %>%
    cfbfastR:::clean_drive_dat() %>%
    cfbfastR:::add_yardage() %>%
    cfbfastR:::add_player_cols() %>%
    prep_epa_df_after()


clean_pbp <- play_df %>%
    dplyr::mutate(down = as.numeric(.data$down)) %>%
    dplyr::filter(.data$down > 0) %>%
    dplyr::filter(.data$period <= 4)

## 1) pred_df and pred_df_after selection and prediction ----
weights <- c(7, -7, 3, -3, 2, -2, 0)

classes = c(
    "Touchdown", "Opp_Touchdown", "Field_Goal", "Opp_Field_Goal",
    "Safety", "Opp_Safety", "No_Score"
)
# get before play expected points model variables
pred_df <- clean_pbp %>%
    dplyr::mutate(
        down1 = dplyr::if_else(down == 1, 1, 0),
        down2 = dplyr::if_else(down == 2, 1, 0),
        down3 = dplyr::if_else(down == 3, 1, 0),
        down4 = dplyr::if_else(down == 4, 1, 0),
        is_home = (pos_team == home)
    ) %>%
    dplyr::select(
        "id_play",
        "drive_id",
        "game_id",
        "play_type",

        # "TimeSecsRem",
        # "down",
        # "distance",
        # "yards_to_goal",
        # "log_ydstogo",
        # "Under_two",
        # "Goal_To_Go",
        # "pos_score_diff_start"
        #
        half_seconds_remaining = TimeSecsRem,

        down1,
        down2,
        down3,
        down4,
        distance,
        yards_to_goal,

        is_home,
        pos_team_timeouts = pos_team_timeouts_rem_before,
        def_pos_team_timeouts = def_pos_team_timeouts_rem_before,
    )

# get after play expected points model variables
pred_df_after <- clean_pbp %>%
    dplyr::select(
        "id_play",
        "drive_id",
        "game_id",
        "play_type",
        "turnover",

        new_half_seconds_remaining,
        new_down1,
        new_down2,
        new_down3,
        new_down4,
        new_distance,
        new_yardline,

        new_is_home,
        new_pos_team_timeouts,
        new_def_pos_team_timeouts
    )

make_ep_probs = function(df) {
    pred_vars_df = df %>%
        dplyr::select(
            half_seconds_remaining,

            down1, down2, down3, down4,
            distance,
            yards_to_goal,

            is_home,
            pos_team_timeouts,
            def_pos_team_timeouts,
        )
    pred_vars = xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = pred_vars_df))
    stopifnot(nrow(pred_vars) == nrow(pred_vars_df))
    ep <- as.data.frame(matrix(predict(ep_model, pred_vars), ncol = 7, byrow = TRUE))
    colnames(ep) = classes
    return(ep)
}

ep_start <- make_ep_probs(pred_df)
colnames(ep_start)[1:7] <- paste0(colnames(ep_start)[1:7], "_before")
pred_df <- cbind(pred_df, ep_start)
pred_df$ep_before <- NA_real_
# ep_before - calculate pre-play expected points
pred_df$ep_before <- apply(ep_start[1:7], 1, function(row) {
    sum(row * weights)
})


ep_end <- pred_df_after %>%
    dplyr::rename(
        new_yards_to_goal = new_yardline
    ) %>%
    dplyr::rename_with(
        .fn = function(x) stringr::str_replace(x, "new_", "")
    ) %>%
    make_ep_probs()
# append `_after` to next score type probability columns
colnames(ep_end) <- paste0(colnames(ep_end), "_after")
pred_df_after <- cbind(pred_df_after, ep_end)
pred_df_after$ep_after <- NA_real_
# ep_after - calculate post-play expected points
pred_df_after$ep_after <- apply(ep_end, 1, function(row) {
    sum(row * weights)
})


# constant vectors to be used again
turnover_play_type <- c(
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    #"Blocked Punt",
    "Blocked Punt Touchdown",
    "Punt",
    "Punt Touchdown",
    "Punt Return Touchdown",
    "Sack Touchdown",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Interception Return",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Uncategorized Touchdown"
)
normalplay <- c(
    "Rush",
    "Pass",
    "Pass Reception",
    "Pass Incompletion",
    "Pass Completion",
    "Sack",
    "Fumble Recovery (Own)"
)
off_TD <- c(
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown",
    "Kickoff Return Touchdown",
    "Punt Touchdown",
    "Punt Team Fumble Recovery Touchdown"
)
def_TD <- c(
    "Defensive 2pt Conversion",
    "Safety",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown",
    "Fumble Return Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Sack Touchdown",
    "Blocked Punt Touchdown",
    "Punt Return Touchdown",
    "Kickoff Touchdown",
    "Kickoff Team Fumble Recovery Touchdown",
    "Interception Return Touchdown",
    "Pass Interception Return Touchdown",
    "Uncategorized Touchdown"
)
punt <- c(
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Blocked Punt (Safety)",
    "Punt (Safety)",
    "Punt",
    "Punt Touchdown",
    "Punt Team Fumble Recovery",
    "Punt Team Fumble Recovery Touchdown",
    "Punt Return Touchdown"
)
kickoff <- c(
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown",
    "Kickoff Touchdown",
    "Kickoff Team Fumble Recovery",
    "Kickoff Team Fumble Recovery Touchdown",
    "Kickoff (Safety)",
    "Penalty (Kickoff)"
)
field_goal <- c(
    "Field Goal Good",
    "Blocked Field Goal",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown"
)

## 5) Kickoff plays -----
## Calculate EP before at kickoff as what happens if it was a touchback
## 25 yard line in 2012 and onwards
## Question for the class: where is the EPA on touchbacks being set to 0?
kickoff_ind <- (pred_df$play_type %in% kickoff)
if (any(kickoff_ind)) {
    new_kick <- pred_df[kickoff_ind, ]
    new_kick["down1"] <- 1
    new_kick["down2"] <- 0
    new_kick["down3"] <- 0
    new_kick["down4"] <- 0
    new_kick["distance"] <- 10
    new_kick["yards_to_goal"] <- 75
    ep_kickoffs <- make_ep_probs(new_kick)
    if (table(kickoff_ind)[2] > 1) {
        pred_df[kickoff_ind, "ep_before"] <- apply(ep_kickoffs, 1, function(row) {
            sum(row * weights)
        })
    }
    else {
        pred_df[kickoff_ind, "ep_before"] <- sum(ep_kickoffs * weights)
    }
}

kickoff_ind2 <- (pred_df_after$play_type %in% kickoff)
# **Due to ESPN data quality issues, some drives end on 3rd down that are listed as turnovers
# For turnover and punt plays make sure the ep_after is negative
turnover_plays <- which(pred_df_after$turnover == 1 & !kickoff_ind2 & (pred_df_after$play_type %in% turnover_play_type))
pred_df_after[turnover_plays, "ep_after"] <- -1 * pred_df_after[turnover_plays, "ep_after"]
kickoff_turnovers <- which(pred_df_after$play_type %in% c("Kickoff Team Fumble Recovery", "Kickoff Team Fumble Recovery Touchdown"))
pred_df_after[kickoff_turnovers, "ep_after"] <- -1 * pred_df_after[kickoff_turnovers, "ep_after"]
punt_turnovers <- which(pred_df_after$play_type %in% punt)
pred_df_after[punt_turnovers, "ep_after"] <- -1 * pred_df_after[punt_turnovers, "ep_after"]

# Game end EP is 0
pred_df[pred_df$end_of_half == 1, "ep_after"] <- 0

## Scoring plays from here on out
pred_df_after[(pred_df_after$play_type %in% off_TD), "ep_after"] <- 7
pred_df_after[(pred_df_after$play_type %in% def_TD), "ep_after"] <- -7
pred_df_after[pred_df_after$play_type == "Defensive 2pt Conversion", "ep_after"] <- -2
pred_df_after[pred_df_after$play_type == "Safety", "ep_after"] <- -2
pred_df_after[pred_df_after$play_type == "Blocked Punt (Safety)", "ep_after"] <- -2
pred_df_after[pred_df_after$play_type == "Punt (Safety)", "ep_after"] <- -2
pred_df_after[pred_df_after$play_type == "Penalty (Safety)", "ep_after"] <- -2
pred_df_after[pred_df_after$play_type == "Kickoff (Safety)", "ep_after"] <- -2
pred_df_after[pred_df_after$play_type == "Field Goal Good", "ep_after"] <- 3

pred_df[pred_df$play_type == "Defensive 2pt Conversion", "ep_before"] <- 0

# insert before lags here
pred_df <- pred_df %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::arrange(.data$id_play, .by_group = TRUE) %>%
    dplyr::mutate(
        lag_ep_before3 = dplyr::lag(.data$ep_before, 3),
        lag_ep_before2 = dplyr::lag(.data$ep_before, 2),
        lag_ep_before = dplyr::lag(.data$ep_before, 1),
        lead_ep_before = dplyr::lead(.data$ep_before, 1),
        lead_ep_before2 = dplyr::lead(.data$ep_before, 2)
    ) %>%
    dplyr::ungroup()

# insert after lags here
pred_df_after <- pred_df_after %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::arrange(.data$id_play, .by_group = TRUE) %>%
    dplyr::mutate(
        lag_ep_after = dplyr::lag(.data$ep_after, 1),
        lag_ep_after2 = dplyr::lag(.data$ep_after, 2),
        lag_ep_after3 = dplyr::lag(.data$ep_after, 3),
        lead_ep_after = dplyr::lead(.data$ep_after, 1),
        lead_ep_after2 = dplyr::lead(.data$ep_after, 2)
    ) %>%
    dplyr::ungroup()


pred_df <- play_df %>%
    dplyr::left_join(
        pred_df_after %>%
            dplyr::select(-"play_type", -"turnover"),
        by = c("game_id", "drive_id", "id_play")
    ) %>%
    dplyr::left_join(
        pred_df %>% select(-"play_type") %>%
            dplyr::select(
                "id_play",
                "drive_id",
                "game_id",
                "No_Score_before",
                "Field_Goal_before",
                "Opp_Field_Goal_before",
                "Opp_Safety_before",
                "Opp_Touchdown_before",
                "Safety_before",
                "Touchdown_before",
                "ep_before",
                # "fg_make_prob",
                "lag_ep_before3",
                "lag_ep_before2",
                "lag_ep_before",
                "lead_ep_before",
                "lead_ep_before2"
            ),
        by = c("game_id", "drive_id", "id_play")
    )

pred_df <- pred_df %>%
    dplyr::arrange(.data$game_id, .data$id_play) %>%
    dplyr::mutate(
        ep_after = ifelse(.data$downs_turnover == 1, -1 * .data$lead_ep_before, .data$ep_after),
        ep_after = ifelse(stringr::str_detect(.data$play_text, regex("safety", ignore_case = TRUE)) &
                              .data$play_type %in% c("Blocked Punt (Safety)", "Punt (Safety)", "Penalty (Safety)"),
                          -2, .data$ep_after
        ),
        ep_after = ifelse(.data$kickoff_safety == 1, -2, .data$ep_after),
        ep_after = ifelse(.data$turnover_vec == 1 & is.na(.data$ep_after),
                          -1 * .data$lead_ep_before,
                          .data$ep_after
        ),
        ep_before = ifelse(.data$play_type == "Timeout" & !is.na(.data$lag_ep_after) &
                               is.na(.data$ep_before), .data$lag_ep_after, .data$ep_before),
        ep_before = ifelse(.data$play_type == "Timeout" & is.na(.data$ep_before), .data$lag_ep_after2, .data$ep_before)
    )


pred_df <- pred_df %>%
    dplyr::mutate(
        adj_TimeSecsRem = ifelse(.data$half == 1, 1800 + .data$TimeSecsRem, .data$TimeSecsRem),
        turnover_vec_lag = dplyr::lag(.data$turnover_vec, 1),
        lag_defense_score_play = dplyr::lag(.data$defense_score_play, 1),
        play_after_turnover = ifelse(.data$turnover_vec_lag == 1 & .data$lag_defense_score_play != 1, 1, 0),
        EPA = NA_real_,
        def_EPA = NA_real_,
        home_EPA = NA_real_,
        away_EPA = NA_real_,
        EPA = ifelse(.data$scoring_play == 0 & .data$end_of_half == 1, -1 * .data$ep_before, .data$ep_after - .data$ep_before),
        def_EPA = -1 * .data$EPA,
        home_EPA = ifelse(.data$pos_team == .data$home, .data$EPA, -1 * .data$EPA),
        away_EPA = -1 * .data$home_EPA,
        home_EPA_rush = ifelse(.data$pos_team == .data$home & .data$rush == 1, .data$EPA, NA_real_),
        away_EPA_rush = ifelse(.data$pos_team == .data$away & .data$rush == 1, .data$EPA, NA_real_),
        home_EPA_pass = ifelse(.data$pos_team == .data$home & .data$pass == 1, .data$EPA, NA_real_),
        away_EPA_pass = ifelse(.data$pos_team == .data$away & .data$pass == 1, .data$EPA, NA_real_),
        total_home_EPA = cumsum(ifelse(is.na(.data$home_EPA), 0, .data$home_EPA)),
        total_away_EPA = cumsum(ifelse(is.na(.data$away_EPA), 0, .data$away_EPA)),
        total_home_EPA_rush = cumsum(ifelse(is.na(.data$home_EPA_rush), 0, .data$home_EPA_rush)),
        total_away_EPA_rush = cumsum(ifelse(is.na(.data$away_EPA_rush), 0, .data$away_EPA_rush)),
        total_home_EPA_pass = cumsum(ifelse(is.na(.data$home_EPA_pass), 0, .data$home_EPA_pass)),
        total_away_EPA_pass = cumsum(ifelse(is.na(.data$away_EPA_pass), 0, .data$away_EPA_pass)),
        net_home_EPA = cumsum(ifelse(is.na(.data$home_EPA), 0, .data$home_EPA)) -
            cumsum(ifelse(is.na(.data$away_EPA), 0, .data$away_EPA)),
        net_away_EPA = cumsum(ifelse(is.na(.data$away_EPA), 0, .data$away_EPA)) -
            cumsum(ifelse(is.na(.data$home_EPA), 0, .data$home_EPA)),
        net_home_EPA_rush = cumsum(ifelse(is.na(.data$home_EPA_rush), 0, .data$home_EPA_rush)) -
            cumsum(ifelse(is.na(.data$away_EPA_rush), 0, .data$away_EPA_rush)),
        net_home_EPA_pass = cumsum(ifelse(is.na(.data$home_EPA_pass), 0, .data$home_EPA_pass)) -
            cumsum(ifelse(is.na(.data$away_EPA_pass), 0, .data$away_EPA_pass)),
        net_away_EPA_rush = cumsum(ifelse(is.na(.data$away_EPA_rush), 0, .data$away_EPA_rush)) -
            cumsum(ifelse(is.na(.data$home_EPA_rush), 0, .data$home_EPA_rush)),
        net_away_EPA_pass = cumsum(ifelse(is.na(.data$away_EPA_pass), 0, .data$away_EPA_pass)) -
            cumsum(ifelse(is.na(.data$home_EPA_pass), 0, .data$home_EPA_pass)),
        ExpScoreDiff = .data$pos_score_diff_start + .data$ep_before,
        half = as.factor(.data$half),
        ExpScoreDiff_Time_Ratio = .data$ExpScoreDiff / (.data$adj_TimeSecsRem + 1)
    ) %>%
    dplyr::select(
        -dplyr::starts_with("new_")
    ) %>%
    dplyr::select(
        "game_id",
        "id_play",
        "drive_id",
        "home",
        "away",
        "drive_number",
        "drive_play_number",
        "game_play_number",
        "pos_team",
        "def_pos_team",
        "clock.minutes",
        "clock.seconds",
        "half",
        "period",
        "TimeSecsRem",
        "play_type",
        "play_text",
        "down",
        # "down1",
        # "down2",
        # "down3",
        # "down4",
        "distance",
        "yards_to_goal",
        "yards_gained",
        # "Goal_To_Go",
        # "Under_two",
        "pos_score_diff",
        "pos_score_diff_start",
        "pos_team_score",
        "def_pos_team_score",
        "ep_before",
        "ep_after",
        "EPA",
        "def_EPA",
        "pos_team_timeouts_rem_before",
        "def_pos_team_timeouts_rem_before",
        "offense_timeouts",
        "defense_timeouts",
        # "down_end",
        # "down1_end",
        # "down2_end",
        # "down3_end",
        # "down4_end",
        # "distance_end",
        # "yards_to_goal_end",
        # "TimeSecsRem_end",
        "pos_team_timeouts",
        "def_pos_team_timeouts",
        # "log_ydstogo_end",
        # "Goal_To_Go_end",
        # "Under_two_end",
        "score_diff",
        "score_diff_start",
        "ppa",
        "drive_start_yards_to_goal",
        "drive_end_yards_to_goal",
        "drive_yards",
        "drive_scoring",
        "drive_result_detailed",
        # "new_drive_pts",
        "offense_play",
        "defense_play",
        "offense_score",
        "defense_score",
        dplyr::everything()
    ) %>%
    dplyr::mutate(
        middle_8 = ifelse(.data$adj_TimeSecsRem >= 1560 & .data$adj_TimeSecsRem <= 2040, TRUE, FALSE),
        rz_play = ifelse((.data$yards_to_goal <= 20), 1, 0),
        scoring_opp = ifelse((.data$yards_to_goal <= 40), 1, 0),
        stuffed_run = ifelse((.data$rush == 1 & .data$yards_gained <= 0), 1, 0),
        success =
            ifelse(.data$yards_gained >= .5 * .data$distance & .data$down == 1, 1,
                   ifelse(.data$yards_gained >= .7 * .data$distance & .data$down == 2, 1,
                          ifelse(.data$yards_gained >= .data$distance & .data$down == 3, 1,
                                 ifelse(.data$yards_gained >= .data$distance & .data$down == 4, 1, 0)
                          )
                   )
            ),
        success = ifelse(.data$play_type %in% turnover_play_type, 0, .data$success),
        epa_success = ifelse(.data$EPA > 0, 1, 0)
    )

## comparisons
pred_df %>%
    dplyr::filter(pass == 1 | rush == 1) %>%
    dplyr::group_by(pos_team) %>%
    dplyr::arrange(.data$id_play, .by_group = TRUE) %>%
    dplyr::mutate(
        team_index = dplyr::row_number(),
        cum_EPA = cumsum(EPA)
    ) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = team_index, y = cum_EPA, color = pos_team)) +
        ggplot2::geom_line() +
        ggplot2::scale_color_manual(values = c("Georgia" = "brown", "Georgia Tech" = "goldenrod"))

pred_df %>%
    dplyr::filter(pass == 1 | rush == 1) %>%
    dplyr::group_by(pos_team) %>%
    dplyr::arrange(.data$id_play, .by_group = TRUE) %>%
    dplyr::mutate(
        team_index = dplyr::row_number(),
        cum_PPA = cumsum(ppa)
    ) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = team_index, y = cum_PPA, color = pos_team)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = c("Georgia" = "brown", "Georgia Tech" = "goldenrod"))


pred_df %>%
    dplyr::filter(pass == 1 | rush == 1) %>%
    dplyr::group_by(pos_team) %>%
    dplyr::summarise(plays = dplyr::n(), TEPA = sum(EPA, na.rm = T), avg_EPA = mean(EPA, na.rm = T), epa_success = mean(epa_success, na.rm = T), success = mean(success, na.rm = T))
