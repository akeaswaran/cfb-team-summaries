library(cfbfastR)
library(dplyr)
library(glue)
library(stringr)
library(glmnet)
library(janitor)

max_season <- cfbfastR:::most_recent_cfb_season()
seasons <- c(max_season) #2014:max_season

write_team_csvs <- function (data, team, yr, type) {
    print(glue("Creating folder /data/{yr}/{team} if necessary"))
    dir.create(file.path(glue('./data/{yr}'), team), showWarnings = FALSE)

    write.csv(data, glue("./data/{yr}/{team}/{type}.csv"), row.names = FALSE)
}

summarize_passer_df <- function(x) {
    tmp <- x %>%
        # filter(
        #     grepl('incomplete', passer_player_name) == FALSE,
        #     grepl('TEAM', passer_player_name) == FALSE
        # ) %>%
        summarize(
            # player_id = dplyr::first(na.omit(passer_player_id)),
            player_name = dplyr::first(passer_player_name),
            plays = n(),
            games = length(unique(game_id)),
            team_games = dplyr::last(team_games),
            playsgame = plays / games,

            # EPA + EPA/play
            TEPA = sum(EPA),
            EPAplay = mean(EPA),
            EPAgame = TEPA / games,

            # yards
            yards = sum(yds_receiving, na.rm = TRUE),
            yardsplay = yards / plays,
            yardsgame = yards / games,

            # SR
            success = mean(success),
            comp = sum(completion),
            att = sum(pass_attempt),
            comppct = mean(completion),

            passing_td = sum(pass_td),
            sacked = sum(sack_vec, na.rm = TRUE),
            sack_yds = sum(yds_sacked, na.rm = TRUE),

            pass_int = sum(int),
            detmer = (yards / (400 * games)) * ((passing_td + pass_int) / (1 + abs(passing_td - pass_int))),
            detmergame = (yardsgame / 400) * (((passing_td / games) + (pass_int / games)) / (1 + abs((passing_td / games) - (pass_int / games)))),
            dropbacks = att + sacked,
            sack_adj_yards = yards - abs(sack_yds),
            yardsdropback = sack_adj_yards / dropbacks
        ) %>%
        ungroup()
    return(tmp)
}

summarize_receiver_df <- function(x) {
    tmp <- x %>%
        # filter(
        #     grepl('incomplete', receiver_player_name) == FALSE,
        #     grepl('TEAM', receiver_player_name) == FALSE
        # ) %>%
        summarize(
            receiver_player_name = dplyr::first(receiver_player_name),
            # player_id = dplyr::first(na.omit(target_player_id)),
            plays = n(),
            games = length(unique(game_id)),
            team_games = dplyr::last(team_games),
            playsgame = plays / games,

            # EPA + EPA/play
            TEPA = sum(EPA),
            EPAplay = mean(EPA),
            EPAgame = TEPA / games,

            # yards
            yards = sum(yds_receiving, na.rm = TRUE),
            yardsplay = yards / plays,
            yardsgame = yards / games,

            # SR
            success = mean(epa_success),
            comp = sum(!is.na(reception_player_id)),
            targets = sum(!is.na(target_player_id)),
            catchpct = comp / targets,

            passing_td = sum(pass_td),
            fumbles = sum(fumble_vec),
        ) %>%
        ungroup()

    return(tmp)
}

summarize_rusher_df <- function(x) {
    tmp <- x %>%
        # filter(
        #     grepl('TEAM', rusher_player_name) == FALSE
        # ) %>%
        summarize(
            # player_id = dplyr::first(na.omit(rush_player_id)),
            rush_player_name = dplyr::first(rusher_player_name),
            plays = n(),
            games = length(unique(game_id)),
            team_games = dplyr::last(team_games),
            playsgame = plays / games,

            # EPA + EPA/play
            TEPA = sum(EPA),
            EPAplay = mean(EPA),
            EPAgame = TEPA / games,

            # yards
            yards = sum(yds_rushed, na.rm = TRUE),
            yardsplay = yards / plays,
            yardsgame = yards / games,

            # SR
            success = mean(epa_success),

            rushing_td = sum(rush_td),
            fumbles = sum(fumble_vec),
        ) %>%
        ungroup()

    return(tmp)
}

summarize_team_df <- function(x, ascending=FALSE, remove_cols = c()) {
    tmp <- x %>%
        summarize(
                plays = n(),
                playsgame = plays / length(unique(game_id)),

                # playcalling
                passrate = mean(pass),
                rushrate = mean(rush),

                havoc = mean(havoc),
                explosive = mean(explosive),

                # edrushrate = sum(early_downs_rush) / sum(early_down),
                # edpassrate = sum(early_downs_pass) / sum(early_down),

                # EPA + EPA/play
                TEPA = sum(EPA),
                EPAplay = mean(EPA),
                EPAdrive = TEPA / length(unique(drive_id)),
                EPAgame = TEPA / length(unique(game_id)),

                # yards
                yards = sum(yards_gained),
                yardsplay = mean(yards_gained),
                yardsgame = yards / length(unique(game_id)),
                play_stuffed = mean(play_stuffed),

                # drives
                drives = length(unique(drive_id)),
                drivesgame = drives / length(unique(game_id)),
                yardsdrive = yards / drives,
                playsdrive = plays / drives,

                # SR
                success = mean(epa_success),
                red_zone_success = mean(red_zone_success, na.rm = TRUE),
                third_down_success = mean(third_down_success, na.rm = TRUE),
                third_down_distance = mean(third_down_distance, na.rm = TRUE),
                late_down_success = mean(late_down_success, na.rm = TRUE),
                early_down_EPA = mean(early_down_EPA, na.rm = T),

                # Field Position
                start_position = mean(drive_start_yards_to_goal),

                # Profile stuff
                nonExplosiveEpaPerPlay = mean(nonExplosiveEpa, na.rm = T),
                line_yards = mean(line_yards, na.rm = T),
                opportunity_rate = mean(opportunity_run, na.rm = T),

                # Available Yards Pct
                # available_yards_pct = mean(available_yards_pct, na.rm = T)
            )
    if (ascending) {
        tmp <- tmp %>%
            mutate(
                # rank ascending cause defense
                playsgame_rank = rank(playsgame),
                TEPA_rank = rank(TEPA),
                EPAgame_rank = rank(EPAgame),
                EPAplay_rank = rank(EPAplay),
                EPAdrive_rank = rank(EPAdrive),
                early_down_EPA_rank = rank(early_down_EPA),
                success_rank = rank(success),

                yards_rank = rank(yards),
                yardsplay_rank = rank(yardsplay),
                yardsgame_rank = rank(yardsgame),

                drivesgame_rank = rank(drivesgame),
                yardsdrive_rank = rank(yardsdrive),
                playsdrive_rank = rank(playsdrive),

                play_stuffed_rank = rank(-play_stuffed), # want high stuff rate on defense
                red_zone_success_rank = rank(red_zone_success),
                third_down_success_rank = rank(third_down_success),
                late_down_success_rank = rank(late_down_success),
                # available_yards_pct = rank(available_yards_pct),

                # except start position
                third_down_distance_rank = rank(-third_down_distance), # long 3rd downs are good for defense
                start_position_rank = rank(-start_position),
                havoc_rank = rank(-havoc),
                explosive_rank = rank(explosive),
                passrate_rank = rank(-passrate),
                rushrate_rank = rank(-rushrate),

                # Profile stuff
                nonExplosiveEpaPerPlay_rank = rank(nonExplosiveEpaPerPlay),
                line_yards_rank = rank(line_yards),
                opportunity_rate_rank = rank(opportunity_rate),
            )
    } else {
        tmp <- tmp %>%
            mutate(
                # rank descending cause offense
                playsgame_rank = rank(-playsgame),
                TEPA_rank = rank(-TEPA),
                EPAgame_rank = rank(-EPAgame),
                EPAplay_rank = rank(-EPAplay),
                EPAdrive_rank = rank(-EPAdrive),
                early_down_EPA_rank = rank(-early_down_EPA),
                success_rank = rank(-success),

                yards_rank = rank(-yards),
                yardsplay_rank = rank(-yardsplay),
                yardsgame_rank = rank(-yardsgame),

                drivesgame = rank(-drivesgame),
                yardsdrive_rank = rank(-yardsdrive),
                playsdrive_rank = rank(-playsdrive),

                play_stuffed_rank = rank(play_stuffed), # want low stuff rate on offense
                red_zone_success_rank = rank(-red_zone_success),
                third_down_success_rank = rank(-third_down_success),
                late_down_success_rank = rank(-late_down_success),
                # available_yards_pct = rank(-available_yards_pct),

                third_down_distance_rank = rank(third_down_distance),
                start_position_rank = rank(start_position),
                havoc_rank = rank(havoc),
                explosive_rank = rank(-explosive),
                passrate_rank = rank(-passrate),
                rushrate_rank = rank(-rushrate),

                # Profile stuff
                nonExplosiveEpaPerPlay_rank = rank(-nonExplosiveEpaPerPlay),
                line_yards_rank = rank(-line_yards),
                opportunity_rate_rank = rank(-opportunity_rate),
                # edrushrate_rank = rank(-edrushrate),
                # edpassrate_rank = rank(-edpassrate)
            )
    }

    return(tmp %>% select(!matches(paste(remove_cols,sep="|"))))
}

mutate_summary_df <- function(x) {
    tmp <- x %>%
        mutate(
            TEPA_margin = TEPA_off - TEPA_def,
            EPAplay_margin = EPAplay_off - EPAplay_def,
            EPAdrive_margin = EPAdrive_off - EPAdrive_def,
            EPAgame_margin = EPAgame_off - EPAgame_def,
            success_margin = success_off - success_def,

            yardsplay_margin = yardsplay_off - yardsplay_def,


            TEPA_margin_rank = rank(-TEPA_margin),
            EPAgame_margin_rank = rank(-EPAgame_margin),
            EPAdrive_margin_rank = rank(-EPAdrive_margin),
            EPAplay_margin_rank = rank(-EPAplay_margin),
            success_margin_rank = rank(-success_margin),

            yardsplay_margin_rank = rank(-yardsplay_margin),

        )
    if (("start_position_off" %in% colnames(x))) {
        tmp <- tmp %>% mutate(
            start_position_margin = (100 - start_position_off) - (100 - start_position_def),
            start_position_margin_rank = rank(-start_position_margin)
        )
    }

    return (tmp)
}

clean_columns <- function (x) {
    old_columns <- colnames(x)
    old_columns <- old_columns[grepl("_rank", old_columns)]
    new_columns <- str_replace(old_columns, "_rank","")
    new_columns <- paste(new_columns,"rank",sep="_")

    return(x %>% rename_at(all_of(old_columns), ~ new_columns))
}

prepare_for_write <- function(x, yr, schools) {
    # browser()
    tmp <- x %>%
        clean_columns() %>%
        dplyr::mutate(
            season = yr
        ) %>%
        dplyr::left_join(schools, by = "pos_team_id") %>%
        select(
            team_id = pos_team_id,
            pos_team,
            conference = pos_team_conference,
            season,
            dplyr::everything()
        ) %>%
        dplyr::mutate(
            fbs_class = dplyr::case_when(
                season >= 2024 & !is.na(conference) & (conference %in% c("SEC", "Big 12", "ACC", "Big Ten") | pos_team == "Notre Dame") ~ "P4",
                season >= 2024 & (!is.na(conference) | (team_id %in% c("41", "113"))) ~ "G6", # UCONN and UMASS
                season <= 2023 & !is.na(conference) & (conference %in% c("SEC", "Big 12", "ACC", "Big Ten", "Pac-12") | pos_team == "Notre Dame") ~ "P5",
                season <= 2023 & (!is.na(conference) | (team_id %in% c("349", "41", "113"))) ~ "G5",
            )
        )
    return(tmp)
}

prepare_percentiles <- function(x) {
    tmp <- x %>%
        summarize(
            GEI = sum(abs(wpa), na.rm = T),
            GEI = GEI * (179.01777401608126 / dplyr::n()),
            EPAplay = mean(EPA, na.rm = TRUE),
            success = mean(epa_success, na.rm = TRUE),
            yardsplay = mean(yards_gained, na.rm = TRUE),
            dropbacks = (sum(pass, na.rm = TRUE)),
            rushes = (sum(rush, na.rm = TRUE)),
            EPAdropback = dplyr::case_when(
                dropbacks == 0 ~ 0,
                TRUE ~ (sum(pos_EPA_pass, na.rm = TRUE) / dropbacks)
            ),
            EPArush = case_when(
                rushes == 0 ~ 0,
                TRUE ~ (sum(pos_EPA_rush, na.rm = TRUE) / rushes)
            ),
            yardsdropback = case_when(
                dropbacks == 0 ~ 0,
                TRUE ~ ((sum(yds_receiving, na.rm = TRUE) + sum(yds_sacked, na.rm = TRUE)) / dropbacks)
            ),
            explosive = mean(explosive, na.rm = TRUE),
            third_down_success = mean(third_down_success, na.rm = TRUE),
            red_zone_success = mean(red_zone_success, na.rm = TRUE),
            play_stuffed = mean(play_stuffed, na.rm = TRUE),
            havoc = mean(havoc, na.rm = TRUE)
        ) %>% ungroup()

    summ_tmp <- tmp %>% summarize(
        pctile = seq(.01, .99, by = .01),
        GEI = quantile(GEI, probs = pctile, na.rm = TRUE),
        EPAplay = quantile(EPAplay, probs = pctile, na.rm = TRUE),
        success = quantile(success, probs = pctile, na.rm = TRUE),
        yardsplay = quantile(yardsplay, probs = pctile, na.rm = TRUE),
        EPAdropback = quantile(EPAdropback, probs = pctile, na.rm = TRUE),
        EPArush = quantile(EPArush, probs = pctile, na.rm = TRUE),
        yardsdropback = quantile(yardsdropback, probs = pctile, na.rm = TRUE),

        explosive = quantile(explosive, probs = pctile, na.rm = TRUE),
        third_down_success = quantile(third_down_success, probs = pctile, na.rm = TRUE),
        red_zone_success = quantile(red_zone_success, probs = pctile, na.rm = TRUE),

        play_stuffed = quantile(play_stuffed, probs = pctile, na.rm = TRUE),
        havoc = quantile(havoc, probs = pctile, na.rm = TRUE),
    )
    return(summ_tmp)
}

adjust_epa = function(plays) {
    # https://makennnahack.github.io/makenna-hack.github.io/publications/opp_adj_rank_project/
    pbp.base = plays %>%
        dplyr::filter(
            !is.na(EPA)
            & ((pass == 1) | (rush == 1))
        ) %>%
        dplyr::mutate(
            dplyr::across(c(pos_team_id, def_pos_team_id), ~ as.character(.x))
        )

    pbp.clean <- pbp.base |>
        filter(
            (wp_before >= 0.1) & (wp_before <= 0.9) # remove garbage time, no vegas spread included in this WP Model
        ) |>
        group_by(game_id) |>
        mutate(hfa = dplyr::case_when(
            neutral_site ~ 0,
            pos_team == home ~ 1,
            .default = -1
        )) %>%
        ungroup() %>%
        select(pos_team_id, pos_team, def_pos_team_id, def_pos_team, game = game_id, EPA, hfa)

    EPA.data <- pbp.clean |>
        select(EPA)

    dummies <- model.matrix(~hfa+pos_team_id+def_pos_team_id, data = pbp.clean)

    data.dummies <- as.data.frame(dummies)

    data.dummies <- cbind(data.dummies, EPA.data) |> select(-`(Intercept)`)

    x <- as.matrix(data.dummies[, -ncol(data.dummies)])
    y <- data.dummies$EPA

    cv = glmnet::cv.glmnet(x, y, lambda = c(75,100,125,150,175,200,225,250,275,300,325))
    target_lambda = cv$lambda[[1]]

    ridge_model <- glmnet::glmnet(x, y, alpha = 0, lambda = target_lambda, intercept = T)
    ridge.coeff <- coef(ridge_model, s = target_lambda)

    intercept <- ridge.coeff[1]

    pos_team_coeffs <- ridge.coeff[grep("^pos_team_id", rownames(ridge.coeff)), , drop = FALSE] + intercept
    def_pos_team_coeffs <- ridge.coeff[grep("^def_pos_team_id", rownames(ridge.coeff)), , drop = FALSE] + intercept

    offense <- data.frame(
        team_id = rownames(pos_team_coeffs),
        adjmodelOff = pos_team_coeffs[, 1]
    )
    rownames(offense) <- NULL

    offense$team_id <- gsub("^pos_team_id", "", offense$team_id)
    # offense$team_id = factor(offense$team_id, levels = levels(pbp.base$pos_team_id))

    defense <- data.frame(
        team_id = rownames(def_pos_team_coeffs),
        adjmodelDef = def_pos_team_coeffs[, 1]
    )
    rownames(defense) <- NULL

    defense$team_id <- gsub("^def_pos_team_id", "", defense$team_id)
    # defense$team_id = factor(defense$team_id, levels = levels(pbp.base$pos_team_id))

    off_epa_game <- pbp.base |>
        group_by(game_id, pos_team_id, def_pos_team_id) |>
        summarise(
            count = dplyr::n(),
            pos_team = dplyr::last(pos_team),
            def_pos_team = dplyr::last(def_pos_team),
            rawOffEPA = mean(EPA, na.rm = T)
        ) |>
        left_join(defense, by = c("def_pos_team_id" = "team_id")) |>
        mutate(adjOffEPA = rawOffEPA - adjmodelDef) |>
        select(game_id, pos_team_id, pos_team, off_count = count, def_strength_faced = adjmodelDef, rawOffEPA, adjOffEPA) |>
        ungroup()

    def_epa_game <- pbp.base |>
        group_by(game_id, def_pos_team_id, pos_team_id) |>
        summarise(
            count = dplyr::n(),
            pos_team = dplyr::last(pos_team),
            def_pos_team = dplyr::last(def_pos_team),
            rawDefEPA = mean(EPA, na.rm = T)
        ) |>
        left_join(offense, by = c("pos_team_id" = "team_id")) |>
        mutate(adjDefEPA = rawDefEPA - adjmodelOff) |>
        select(game_id, def_pos_team_id, def_pos_team, def_count = count, off_strength_faced = adjmodelOff, rawDefEPA, adjDefEPA) |>
        ungroup()

    opp.adj <- off_epa_game |>
        left_join(def_epa_game, by = c("game_id", "pos_team_id" = "def_pos_team_id"))

    team.adj = opp.adj %>%
        dplyr::group_by(pos_team_id) %>%
        dplyr::summarise(
            pos_team = dplyr::last(pos_team),
            team_games = dplyr::n(),
            valid_games = sum(!is.na(adjOffEPA) & !is.na(adjDefEPA), na.rm = T),
            dplyr::across(dplyr::contains("EPA") | dplyr::contains("strength"), ~ mean(.x, na.rm = T))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(dplyr::where(is.numeric), ~ dplyr::if_else(is.nan(.x), NA_real_, .x))
        ) %>%
        dplyr::filter(
            !is.na(adjOffEPA)
            & !is.na(adjDefEPA)
            & valid_games >= 2
        )

    team.adj %>%
        dplyr::select(
            -dplyr::starts_with("raw"),
            -team_games,
        ) %>%
        dplyr::mutate(
            netAdjEPA = adjOffEPA - adjDefEPA,

            adjOffEPA_rank = rank(-adjOffEPA),
            adjDefEPA_rank = rank(adjDefEPA),
            netAdjEPA_rank = rank(-netAdjEPA),
        ) %>%
        janitor::clean_names() %>%
        dplyr::rename(
            team_id = pos_team_id
        )
}

for (yr in seasons) {
    print(glue("Starting processing for {yr} season..."))
    print(glue("Downloading plays..."))
    plays <- cfbfastR::load_cfb_pbp(seasons = c(yr))
    print(glue("Downloading schedule..."))
    schedules <- cfbfastR::load_cfb_schedules(seasons = c(yr))

    # print(glue("Downloading teams..."))

    print(glue("Found {nrow(plays)} total plays, filtering to FBS/FBS"))

    plays <- plays %>%
        dplyr::left_join(schedules %>% dplyr::select(game_id, neutral_site, home_id, home_division, away_id, away_division), by = "game_id") %>%
        filter(
            ((pass == 1) | (rush == 1))
            & !(game_id %in% c(401635537))
            & (home_division == "fbs" & away_division == "fbs")
            # ((wp_before >= 0.1) & (wp_before <= 0.9))
        ) %>%
        mutate(
            pos_team_id = dplyr::case_when(
                pos_team == home ~ home_id,
                pos_team == away ~ away_id,
            ),
            pos_team_id = as.character(pos_team_id),
            def_pos_team_id = dplyr::case_when(
                pos_team == home ~ away_id,
                pos_team == away ~ home_id,
            ),
            def_pos_team_id = as.character(def_pos_team_id),
            pos_EPA_pass = case_when(
                pos_team == home ~ home_EPA_pass,
                pos_team == away ~ away_EPA_pass,
                TRUE ~ NA_real_
            ),
            pos_EPA_rush = case_when(
                pos_team == home ~ home_EPA_rush,
                pos_team == away ~ away_EPA_rush,
                TRUE ~ NA_real_
            ),
            game_id = as.character(game_id),
            play_stuffed = (yards_gained <= 0),
            red_zone = (yards_to_goal <= 20),
            epa_success = as.double(epa_success),

            red_zone_success = case_when(
                (as.numeric(red_zone) == 1) ~ epa_success,
                TRUE ~ NA_real_
            ),
            third_down_success = case_when(
                (as.numeric(down) == 3) ~ epa_success,
                TRUE ~ NA_real_
            ),
            late_down_success = case_when(
                (as.numeric(down) >= 3) ~ epa_success,
                TRUE ~ NA_real_
            ),
            third_down_distance = case_when(
                (as.numeric(down) == 3) ~ distance,
                TRUE ~ NA_real_
            ),
            early_down_EPA = case_when(
                (as.numeric(down) <= 2) ~ EPA,
                TRUE ~ NA_real_
            ),
            havoc = (sack_vec | int | fumble_vec | !is.na(pass_breakup_player_name) | (yards_gained < 0)),
            explosive = case_when(
                (pass == 1) ~ (EPA >= 2.4),
                (rush == 1) ~ (EPA >= 1.8),
                TRUE ~ FALSE
            ),
            opportunity_run = ((rush == 1) & (yds_rushed >= 4)),
            adj_rush_yardage = dplyr::case_when(
                (rush == 1) & (yds_rushed > 10) ~ 10,
                (rush == 1) & (yds_rushed <= 10) ~ yds_rushed,
                .default = NA_real_
            ),
            line_yards = dplyr::case_when(
                (rush == 1) & (yds_rushed < 0) ~ (1.2 * adj_rush_yardage),
                (rush == 1) & (yds_rushed >= 0) & (yds_rushed <= 4) ~ adj_rush_yardage,
                (rush == 1) & (yds_rushed >= 5) & (yds_rushed <= 10) ~ (0.5 * adj_rush_yardage),
                (rush == 1) & (yds_rushed >= 11) ~ 0,
                .default = NA_real_
            ),
            second_level_yards = dplyr::case_when(
                (rush == 1) & (yds_rushed >= 5) ~ (0.5 * (adj_rush_yardage - 5)),
                (rush == 1) ~ 0,
                .default = NA_real_
            ),
            open_field_yards = dplyr::case_when(
                (rush == 1) & (yds_rushed > 10) ~ (yds_rushed - adj_rush_yardage),
                (rush == 1) ~ 0,
                .default = NA_real_
            ),
            highlight_yards = second_level_yards + open_field_yards,
            opp_highlight_yards = dplyr::case_when(
                (opportunity_run == T) ~ highlight_yards,
                (opportunity_run == F) & (rush == 1) ~ 0,
                .default = NA_real_
            ),
            nonExplosiveEpa = dplyr::case_when(
                !is.na(EPA) & (explosive == F) ~ EPA,
                .default = NA_real_
            ),
        ) %>%
        dplyr::arrange(game_id, game_play_number)

    if ("neutral_site.x" %in% colnames(plays)) {
        plays <- plays %>%
            dplyr::select(
                -neutral_site.y
            ) %>%
            dplyr::rename(
                neutral_site = neutral_site.x
            )
    }

    print(glue("Found {nrow(plays)} total FBS/FBS non-garbage-time plays, summarizing offensive data"))

    team_off_plays <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & !is.na(epa_success))

    team_off_pctls <- team_off_plays %>%
        group_by(game_id, pos_team) %>%
        prepare_percentiles()

    team_off_data <- team_off_plays %>%
        group_by(pos_team_id) %>%
        summarize_team_df(ascending = FALSE)

    team_off_drives_data <- plays %>%
        filter(!is.na(drive_id)) %>%
        group_by(pos_team_id, drive_id) %>%
        summarize(
            total_available_yards = first(drive_start_yards_to_goal),
            total_gained_yards = last(drive_yards),
        ) %>%
        ungroup() %>%
        group_by(pos_team_id) %>%
        summarize(
            total_available_yards = sum(total_available_yards),
            total_gained_yards = sum(total_gained_yards),
            available_yards_pct = total_gained_yards / total_available_yards
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            available_yards_pct_rank = rank(-available_yards_pct)
        )

    team_qb_data <- plays %>%
        dplyr::mutate(
            passer_player_id = dplyr::case_when(
                !is.na(completion_player_id) ~ completion_player_id,
                .default = incompletion_player_id
            ),
            passer_player_name = dplyr::case_when(
                !is.na(completion_player_id) ~ completion_player,
                .default = incompletion_player
            )
        ) %>%
        filter((pass == 1) & !is.na(EPA) & !is.na(success) & !is.na(epa_success) & !is.na(passer_player_id)) %>%
        group_by(pos_team_id) %>%
        dplyr::mutate(
            team_games = length(unique(game_id))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(pos_team_id, passer_player_id) %>%
        summarize_passer_df() %>%
        dplyr::ungroup()

    team_qb_ranks <- team_qb_data %>%
        filter(
            dropbacks >= (14 * team_games) # leaderboard minimums - based on profootballreference - https://www.pro-football-reference.com/about/minimums.htm
        ) %>%
        mutate(
            TEPA_rank = rank(-TEPA),
            EPAgame_rank = rank(-EPAgame),
            EPAplay_rank = rank(-EPAplay),
            success_rank = rank(-success),
            comppct_rank = rank(-comppct),

            yards_rank = rank(-yards),
            yardsplay_rank = rank(-yardsplay),
            yardsgame_rank = rank(-yardsgame),
            sack_adj_yards_rank = rank(-sack_adj_yards),
            yardsdropback_rank = rank(-yardsdropback),

            detmer_rank = rank(-detmer),
            detmergame_rank = rank(-detmergame)
        ) %>%
        dplyr::select(
            pos_team_id,
            passer_player_id,
            TEPA_rank,
            EPAgame_rank,
            EPAplay_rank,
            success_rank,
            comppct_rank,

            yards_rank,
            yardsplay_rank,
            yardsgame_rank,
            sack_adj_yards_rank,
            yardsdropback_rank,

            detmer_rank,
            detmergame_rank,
        )

    team_qb_data = team_qb_data %>%
        dplyr::left_join(team_qb_ranks, by = c("pos_team_id", "passer_player_id"))


    team_rb_data <- plays %>%
        dplyr::filter((rush == 1) & !is.na(EPA) & !is.na(success) & !is.na(epa_success) & !is.na(rush_player_id)) %>%
        dplyr::group_by(pos_team_id) %>%
        dplyr::mutate(
            team_games = length(unique(game_id))
        ) %>%
        dplyr::ungroup() %>%
        group_by(pos_team_id, rush_player_id) %>%
        summarize_rusher_df() %>%
        ungroup()

    team_rb_ranks = team_rb_data %>%
        dplyr::filter(
            plays >= (6.25 * team_games) # leaderboard minimums - based on profootballreference - https://www.pro-football-reference.com/about/minimums.htm
        ) %>%
        dplyr::mutate(
            TEPA_rank = rank(-TEPA),
            EPAgame_rank = rank(-EPAgame),
            EPAplay_rank = rank(-EPAplay),
            success_rank = rank(-success),

            yards_rank = rank(-yards),
            yardsplay_rank = rank(-yardsplay),
            yardsgame_rank = rank(-yardsgame),
        ) %>%
        dplyr::select(
            pos_team_id,
            rush_player_id,
            TEPA_rank,
            EPAgame_rank,
            EPAplay_rank,
            success_rank,

            yards_rank,
            yardsplay_rank,
            yardsgame_rank,
        )

    team_rb_data = team_rb_data %>%
        dplyr::left_join(team_rb_ranks, by = c("pos_team_id", "rush_player_id"))

    team_wr_data <- plays %>%
        dplyr::mutate(
            receiver_player_id = dplyr::case_when(
                !is.na(reception_player_id) ~ reception_player_id,
                !is.na(target_player_id) ~ target_player_id,
            ),
            receiver_player_name = dplyr::case_when(
                !is.na(reception_player_id) ~ reception_player,
                !is.na(target_player_id) ~ target_player,
            )
        ) %>%
        filter((pass == 1) & !is.na(EPA) & !is.na(success) & !is.na(epa_success) & !is.na(receiver_player_id)) %>%
        group_by(pos_team_id) %>%
        dplyr::mutate(
            team_games = length(unique(game_id))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(pos_team_id, receiver_player_id) %>%
        summarize_receiver_df() %>%
        dplyr::ungroup()

    team_wr_ranks = team_wr_data %>%
        filter(
            plays >= (1.875 * team_games) # leaderboard minimums - based on profootballreference - https://www.pro-football-reference.com/about/minimums.htm
        ) %>%
        mutate(
            TEPA_rank = rank(-TEPA),
            EPAgame_rank = rank(-EPAgame),
            EPAplay_rank = rank(-EPAplay),
            success_rank = rank(-success),
            catchpct_rank = rank(-catchpct),

            yards_rank = rank(-yards),
            yardsplay_rank = rank(-yardsplay),
            yardsgame_rank = rank(-yardsgame)
        ) %>%
        dplyr::select(
            pos_team_id,
            receiver_player_id,
            TEPA_rank,
            EPAgame_rank,
            EPAplay_rank,
            success_rank,
            catchpct_rank,

            yards_rank,
            yardsplay_rank,
            yardsgame_rank,
        )

    team_wr_data = team_wr_data %>%
        dplyr::left_join(team_wr_ranks, by = c("pos_team_id", "receiver_player_id"))


    team_off_pass_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & !is.na(epa_success) & (pass == 1)) %>%
        group_by(pos_team_id) %>%
        summarize_team_df(remove_cols = c(
            'start_position', 'start_position_rank'
        ))

    team_off_rush_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & !is.na(epa_success) & (rush == 1)) %>%
        group_by(pos_team_id) %>%
        summarize_team_df(remove_cols = c(
            'start_position', 'start_position_rank'
        ))

    print(glue("Summarizing defensive data"))

    team_def_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & !is.na(epa_success)) %>%
        group_by(def_pos_team_id) %>%
        summarize_team_df(ascending = TRUE)

    team_def_drives_data <- plays %>%
        filter(!is.na(drive_id)) %>%
        group_by(def_pos_team_id, drive_id) %>%
        summarize(
            total_available_yards = first(drive_start_yards_to_goal),
            total_gained_yards = last(drive_yards),
        ) %>%
        ungroup() %>%
        group_by(def_pos_team_id) %>%
        summarize(
            total_available_yards = sum(total_available_yards),
            total_gained_yards = sum(total_gained_yards),
            available_yards_pct = total_gained_yards / total_available_yards
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            available_yards_pct_rank = rank(available_yards_pct)
        )

    team_def_pass_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & !is.na(epa_success) & (pass == 1)) %>%
        group_by(def_pos_team_id) %>%
        summarize_team_df(ascending = TRUE, remove_cols = c(
            'start_position', 'start_position_rank'
        ))

    team_def_rush_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & !is.na(epa_success) & (rush == 1)) %>%
        group_by(def_pos_team_id) %>%
        summarize_team_df(ascending = TRUE, remove_cols = c(
            'start_position', 'start_position_rank'
        ))

    print(glue("Merging offensive and defensive data, calculating full season ranks"))

    team_overall_data <- left_join(team_off_data, team_def_data, by = c("pos_team_id" = "def_pos_team_id"), suffix = c("_off","_def"))
    team_drives_data <- left_join(team_off_drives_data, team_def_drives_data, by = c("pos_team_id" = "def_pos_team_id"), suffix = c("_off","_def"))
    team_pass_data <- left_join(team_off_pass_data, team_def_pass_data, by = c("pos_team_id" = "def_pos_team_id"), suffix = c("_off","_def"))
    team_rush_data <- left_join(team_off_rush_data, team_def_rush_data, by = c("pos_team_id" = "def_pos_team_id"), suffix = c("_off","_def"))

    team_overall_data <- team_overall_data %>%
        mutate_summary_df()
    team_drives_data <- team_drives_data %>%
        mutate(
            total_available_yards_margin = total_available_yards_off - total_available_yards_def,
            total_gained_yards_margin = total_gained_yards_off - total_gained_yards_def,
            available_yards_pct_margin = available_yards_pct_off - available_yards_pct_def,

            total_available_yards_margin_rank = rank(-total_available_yards_margin),
            total_gained_yards_margin_rank = rank(-total_gained_yards_margin),
            available_yards_pct_margin_rank = rank(-available_yards_pct_margin)
        )
    team_pass_data <- team_pass_data %>%
        mutate_summary_df()
    team_rush_data <- team_rush_data %>%
        mutate_summary_df()

    team_data <- left_join(team_overall_data, team_drives_data, by = c("pos_team_id" = "pos_team_id"), suffix = c("","_drive"))
    team_data <- left_join(team_data, team_pass_data, by = c("pos_team_id" = "pos_team_id"), suffix = c("","_pass"))
    team_data <- left_join(team_data, team_rush_data, by = c("pos_team_id" = "pos_team_id"), suffix = c("","_rush"))

    print(glue::glue("running ridge regression adjustments...."))
    adj_EPA_df = adjust_epa(plays)

    print(glue("Generating year and team CSVs..."))

    schools = plays %>%
        dplyr::mutate(
            pos_team_division = dplyr::if_else(home_team_id == pos_team_id, home_team_division, away_team_division),
            pos_team_conference = dplyr::if_else(home_team_id == pos_team_id, home_team_conference, away_team_conference)
        ) %>%
        dplyr::distinct(pos_team_id, .keep_all = T) %>%
        dplyr::select(pos_team_id, pos_team, pos_team_division, pos_team_conference) %>%
        dplyr::arrange(pos_team_id)

    team_data <- team_data %>%
        prepare_for_write(yr, schools) %>%
        dplyr::left_join(adj_EPA_df %>% dplyr::select(-pos_team), by = c("team_id"))

    team_qb_data <- team_qb_data %>%
        prepare_for_write(yr, schools)

    team_rb_data <- team_rb_data %>%
        prepare_for_write(yr, schools)

    team_wr_data <- team_wr_data %>%
        prepare_for_write(yr, schools)

    print(glue("Creating folder /data/ if necessary"))
    dir.create(file.path('./data', ""), showWarnings = FALSE)

    print(glue("Creating folder /data/{yr} if necessary"))
    dir.create(file.path('./data', glue("{yr}")), showWarnings = FALSE)

    print(glue("Writing year CSVs to folder /data/{yr}"))
    write.csv(team_off_pctls, glue("./data/{yr}/percentiles.csv"), row.names = FALSE)
    write.csv(team_data, glue("./data/{yr}/overall.csv"), row.names = FALSE)
    write.csv(team_qb_data, glue("./data/{yr}/passing.csv"), row.names = FALSE)
    write.csv(team_rb_data, glue("./data/{yr}/rushing.csv"), row.names = FALSE)
    write.csv(team_wr_data, glue("./data/{yr}/receiving.csv"), row.names = FALSE)

    print(glue("Writing team CSVs to folder /data/{yr}"))
    team_data %>%
        group_by(team_id) %>%
        group_walk(~ write_team_csvs(.x, .y$team_id, yr, 'overall'), .keep = T)

    print(glue("Writing player CSVs to folder /data/{yr}"))
    team_qb_data %>%
        group_by(team_id) %>%
        group_walk(~ write_team_csvs(.x, .y$team_id, yr, 'passing'), .keep = T)

    team_rb_data %>%
        group_by(team_id) %>%
        group_walk(~ write_team_csvs(.x, .y$team_id, yr, 'rushing'), .keep = T)

    team_wr_data %>%
        group_by(team_id) %>%
        group_walk(~ write_team_csvs(.x, .y$team_id, yr, 'receiving'), .keep = T)
}


