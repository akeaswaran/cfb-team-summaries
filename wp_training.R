library(cfbfastR)
library(dplyr)
library(glue)
library(stringr)
library(ggplot2)
library(purrr)
library(tidyverse)

max_season <- cfbfastR:::most_recent_cfb_season()
seasons <- 2014:max_season
valid_fbs_teams <- cfbfastR::load_cfb_teams() %>%
    filter(classification == 'fbs') %>%
    select(
        team_id,
        school,
        abbreviation
    )

# lines <- readRDS("~/Desktop/14_24_lines.RDS") %>%
#     dplyr::filter(home_team %in% valid_fbs_teams$school & away_team %in% valid_fbs_teams$school) %>%
#     dplyr::mutate(
#         provider_value = dplyr::case_when(
#             provider %in% c("ESPN Bet", "consensus") ~ 0,
#             TRUE ~ 1
#         ),
#         winner = dplyr::case_when(
#             home_score > away_score ~ home_team,
#             home_score < away_score ~ away_team,
#         ),
#     ) %>%
#     dplyr::filter(
#         !is.na(spread) & !is.na(over_under)
#     ) %>%
#     dplyr::group_by(game_id) %>%
#     dplyr::slice_min(order_by = provider_value, n = 1, with_ties = F) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(
#         -provider_value
#     ) %>%
#     dplyr::mutate(
#         # spread is always in context of home team
#         home_team_spread = spread,
#         away_team_spread = -1 * spread
#     )
#
#
# # # check scrimmage EPA trend season over season: https://discord.com/channels/798201448843837500/854585280560627743/1254784405260468265
# plays = cfbfastR::load_cfb_pbp(seasons)
#
# fbs_plays = plays %>%
#     dplyr::left_join(lines %>% dplyr::select(game_id, winner, home_team, away_team, home_team_spread, away_team_spread), by = "game_id") %>%
#     dplyr::mutate(
#         label = dplyr::if_else(pos_team == winner, 1, 0),
#         pos_team_spread = dplyr::case_when(
#             home_team == pos_team ~ home_team_spread,
#             away_team == pos_team ~ away_team_spread,
#         ),
#         is_home = pos_team == home_team
#     ) %>%
#     dplyr::group_by(game_id) %>%
#     dplyr::mutate(
#         receive_2h_ko = dplyr::if_else(period <= 2 & pos_team == dplyr::first(stats::na.omit(def_pos_team)), 1, 0)
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(
#         half_seconds_remaining = dplyr::case_when(
#             period > 4 ~ 0,
#             period %in% c(1, 3) ~ (15 * 60) + ((clock.minutes * 60) + clock.seconds),
#             .default = ((clock.minutes * 60) + clock.seconds)
#         ),
#         game_seconds_remaining = dplyr::case_when(
#             period > 4 ~ 0,
#             period %in% c(1, 2) ~ (2 * 15 * 60) + half_seconds_remaining,
#             .default = half_seconds_remaining
#         ),
#         # posteam_spread = dplyr::if_else(.data$home == 1, .data$spread_line, -1 * .data$spread_line),
#         elapsed_share = (3600 - game_seconds_remaining) / 3600,
#         spread_time = pos_team_spread * exp(-4 * elapsed_share),
#         Diff_Time_Ratio = pos_score_diff / (exp(-4 * elapsed_share))
#     ) %>%
#     dplyr::filter(
#         !is.na(pos_team_timeouts)
#         & !is.na(def_pos_team_timeouts)
#         & !is.na(yards_to_goal)
#         & !is.na(season)
#         & !is.na(label)
#         & !is.na(pos_team_spread)
#         & !is.na(ydstogo)
#         & !is.na(yardline_100)
#     ) %>%
#     select(
#         label,
#
#         receive_2h_ko,
#         spread_time,
#         home = is_home,
#         half_seconds_remaining,
#         game_seconds_remaining,
#         Diff_Time_Ratio,
#         score_differential = pos_score_diff,
#         down,
#         ydstogo = distance,
#         yardline_100 = yards_to_goal,
#         posteam_timeouts_remaining = pos_team_timeouts,
#         defteam_timeouts_remaining = def_pos_team_timeouts,
#         season,
#         # only needed for the plots here, not used in model
#         qtr = period,
#         game_id
#     )
# saveRDS(fbs_plays, paste0("./wp_training_data_",Sys.Date(),".RDS"))
# remove(fbs_plays, plays)

model_data = readRDS("./wp_training_data_2025-08-04.RDS") %>%
    dplyr::filter(
        !is.na(ydstogo)
        & !is.na(yardline_100)
        & qtr <= 4
    )

nrounds <- 534
params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.05,
    gamma = .79012017,
    subsample = 0.9224245,
    colsample_bytree = 5 / 12,
    max_depth = 5,
    min_child_weight = 7,
    monotone_constraints = "(0, 0, 0, 0, 0, 1, 1, -1, -1, -1, 1, -1)"
)

cv_results_raw <- purrr::map(seasons, function(x) {
    test_data <- model_data %>%
        dplyr::filter(season == x) %>%
        dplyr::select(-season)
    train_data <- model_data %>%
        dplyr::filter(season != x) %>%
        dplyr::select(-season)

    full_train <- xgboost::xgb.DMatrix(
        model.matrix(~ . + 0, data = train_data %>% dplyr::select(-label, -game_id, -qtr)),
        label = train_data$label
    )
    wp_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2, print_every_n = 10)

    full_test <- xgboost::xgb.DMatrix(
        model.matrix(~ . + 0, data = test_data %>% dplyr::select(-label, -game_id, -qtr))
    )

    preds <- as.data.frame(
        matrix(predict(wp_model, full_test))
    ) %>%
        dplyr::rename(wp = V1)

    cv_data <- bind_cols(test_data, preds) %>% mutate(season = x)
    return(cv_data)
}, .progress = T)


# TIME FOR BINNING
wp_cv_loso_calibration_results <- purrr::list_rbind(cv_results) %>%
    # Create BINS for wp:
    mutate(bin_pred_prob = round(wp / 0.05) * .05) %>%
    # Group by both the qtr and bin_pred_prob:
    group_by(qtr, bin_pred_prob) %>%
    # Calculate the calibration results:
    summarize(
        n_plays = n(),
        n_wins = length(which(label == 1)),
        bin_actual_prob = n_wins / n_plays
    )

wp_cv_cal_error <- wp_cv_loso_calibration_results %>%
    ungroup() %>%
    mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
    group_by(qtr) %>%
    summarize(
        weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
        n_wins = sum(n_wins, na.rm = TRUE)
    )

glue::glue(
    "--CALIBRATION ERROR--

nflfastR:
{round(with(wp_cv_cal_error, weighted.mean(weight_cal_error, n_wins)), 4)}"
)


# Create a label data frame for the chart:
ann_text <- data.frame(
    x = c(.25, 0.75), y = c(0.75, 0.25),
    lab = c("More times\nthan expected", "Fewer times\nthan expected"),
    qtr = factor("1st Quarter")
)

# Create the calibration chart:
wp_cv_loso_calibration_results %>%
    ungroup() %>%
    mutate(qtr = fct_recode(factor(qtr),
                            "1st Quarter" = "1", "2nd Quarter" = "2",
                            "3rd Quarter" = "3", "4th Quarter" = "4"
    )) %>%
    ggplot() +
    geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
    geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
    geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
    coord_equal() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
        size = "Number of plays",
        x = "Estimated win probability",
        y = "Observed win probability"
    ) +
    geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 2) +
    theme_bw() +
    theme(
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 90),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "bottom"
    ) +
    facet_wrap(~qtr, ncol = 4)
