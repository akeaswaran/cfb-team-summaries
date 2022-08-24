library(cfbfastR)
library(dplyr)
library(glue)
library(stringr)

max_season <- cfbfastR:::most_recent_cfb_season()
seasons <- 2014:max_season
valid_fbs_teams <- cfbfastR::load_cfb_teams() %>%
    select(
        team_id,
        school,
        abbreviation
    )

write_team_csvs <- function (data, team, yr, type) {
    print(glue("Creating folder /data/{yr}/{team} if necessary"))
    dir.create(file.path(glue('./data/{yr}'), team), showWarnings = FALSE)

    write.csv(data, glue("./data/{yr}/{team}/{type}.csv"), row.names = FALSE)
}

summarize_passer_df <- function(x) {
    tmp <- x %>%
        filter(
            grepl('incomplete', passer_player_name) == FALSE,
            grepl('TEAM', passer_player_name) == FALSE
        ) %>%
        summarize(
            plays = n(),
            games = length(unique(game_id)),
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
            sacked = sum(sack_vec),
            sack_yds = sum(yds_sacked),

            pass_int = sum(int),
            detmer = (yards / (400 * games)) * ((passing_td + pass_int) / (1 + abs(passing_td - pass_int))),
            detmergame = (yardsgame / 400) * (((passing_td / games) + (pass_int / games)) / (1 + abs((passing_td / games) - (pass_int / games))))
        )

    tmp <- tmp %>%
        ungroup() %>%
        mutate(
            TEPA_rank = rank(-TEPA),
            EPAgame_rank = rank(-EPAgame),
            EPAplay_rank = rank(-EPAplay),
            success_rank = rank(-success),
            comppct_rank = rank(-comppct),

            yards_rank = rank(-yards),
            yardsplay_rank = rank(-yardsplay),
            yardsgame_rank = rank(-yardsgame),

            detmer_rank = rank(-detmer),
            detmergame_rank = rank(-detmergame)
        )

    return(tmp)
}

summarize_receiver_df <- function(x) {
    tmp <- x %>%
        filter(
            grepl('incomplete', passer_player_name) == FALSE,
            grepl('TEAM', passer_player_name) == FALSE
        ) %>%
        summarize(
            plays = n(),
            games = length(unique(game_id)),
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
            targets = sum(target),
            catchpct = comp / targets,

            passing_td = sum(pass_td),
            fumbles = sum(fumble_vec),
        )

    tmp <- tmp %>%
        ungroup() %>%
        mutate(
            TEPA_rank = rank(-TEPA),
            EPAgame_rank = rank(-EPAgame),
            EPAplay_rank = rank(-EPAplay),
            success_rank = rank(-success),
            catchpct_rank = rank(-catchpct),

            yards_rank = rank(-yards),
            yardsplay_rank = rank(-yardsplay),
            yardsgame_rank = rank(-yardsgame)
        )

    return(tmp)
}

summarize_rusher_df <- function(x) {
    tmp <- x %>%
        filter(
            grepl('TEAM', passer_player_name) == FALSE
        ) %>%
        summarize(
            plays = n(),
            games = length(unique(game_id)),
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
            success = mean(success),

            rushing_td = sum(rush_td),
            fumbles = sum(fumble_vec),
        )

    tmp <- tmp %>%
        ungroup() %>%
        mutate(
            TEPA_rank = rank(-TEPA),
            EPAgame_rank = rank(-EPAgame),
            EPAplay_rank = rank(-EPAplay),
            success_rank = rank(-success),

            yards_rank = rank(-yards),
            yardsplay_rank = rank(-yardsplay),
            yardsgame_rank = rank(-yardsgame),
        )

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

                # edrushrate = sum(early_downs_rush) / sum(early_down),
                # edpassrate = sum(early_downs_pass) / sum(early_down),

                # EPA + EPA/play
                TEPA = sum(EPA),
                EPAplay = mean(EPA),
                EPAgame = TEPA / length(unique(game_id)),

                # yards
                yards = sum(yards_gained),
                yardsplay = mean(yards_gained),
                yardsgame = yards / length(unique(game_id)),

                # drives
                drives = length(unique(drive_id)),
                drivesgame = drives / length(unique(game_id)),
                yardsdrive = yards / drives,
                playsdrive = plays / drives,

                # SR
                success = mean(success),

                # Field Position
                start_position = mean(drive_start_yards_to_goal),
            )
    if (ascending) {
        tmp <- tmp %>%
            mutate(
                # rank ascending cause defense
                playsgame_rank = rank(playsgame),
                TEPA_rank = rank(TEPA),
                EPAgame_rank = rank(EPAgame),
                EPAplay_rank = rank(EPAplay),
                success_rank = rank(success),

                yards_rank = rank(yards),
                yardsplay_rank = rank(yardsplay),
                yardsgame_rank = rank(yardsgame),

                drivesgame_rank = rank(drivesgame),
                yardsdrive_rank = rank(yardsdrive),
                playsdrive_rank = rank(playsdrive),

                # except start position
                start_position_rank = rank(-start_position),
                passrate_rank = rank(-passrate),
                rushrate_rank = rank(-rushrate),
                # edrushrate_rank = rank(-edrushrate),
                # edpassrate_rank = rank(-edpassrate)
            )
    } else {
        tmp <- tmp %>%
            mutate(
                # rank descending cause offense
                playsgame_rank = rank(-playsgame),
                TEPA_rank = rank(-TEPA),
                EPAgame_rank = rank(-EPAgame),
                EPAplay_rank = rank(-EPAplay),
                success_rank = rank(-success),

                yards_rank = rank(-yards),
                yardsplay_rank = rank(-yardsplay),
                yardsgame_rank = rank(-yardsgame),

                drivesgame = rank(-drivesgame),
                yardsdrive_rank = rank(-yardsdrive),
                playsdrive_rank = rank(-playsdrive),

                start_position_rank = rank(-start_position),
                passrate_rank = rank(-passrate),
                rushrate_rank = rank(-rushrate),
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
            EPAgame_margin = EPAgame_off - EPAgame_def,
            success_margin = success_off - success_def,

            yardsplay_margin = yardsplay_off - yardsplay_def,


            TEPA_margin_rank = rank(-TEPA_margin),
            EPAgame_margin_rank = rank(-EPAgame_margin),
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

prepare_for_write <- function(x) {
    tmp <- x %>%
        clean_columns() %>%
        mutate(
            season = yr
        ) %>%
        left_join(valid_fbs_teams, by = c('pos_team' = 'school')) %>%
        select(
            team_id,
            pos_team,
            abbreviation,
            season,
            everything()
        )
    return(tmp)
}

for (yr in seasons) {
    print(glue("Starting processing for {yr} season..."))
    plays <- cfbfastR::load_cfb_pbp(seasons = c(yr))

    print(glue("Found {nrow(plays)} total plays, filtering to FBS/FBS non-garbage-time"))
    plays <- plays %>%
        filter(
            pos_team %in% valid_fbs_teams$school,
            def_pos_team %in% valid_fbs_teams$school,
            (pass == 1) | (rush == 1),
            # ((wp_before >= 0.1) & (wp_before <= 0.9))
        ) %>%
        mutate(
            game_id = as.character(game_id),
            # early_down = (down < 3),
            # early_downs_rush = (rush & early_down),
            # early_downs_pass = (pass & early_down),
        )

    print(glue("Found {nrow(plays)} total FBS/FBS non-garbage-time plays, summarizing offensive data"))

    team_off_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success)) %>%
        group_by(pos_team) %>%
        summarize_team_df(ascending = FALSE)

    team_qb_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & !is.na(passer_player_name)) %>%
        group_by(pos_team, passer_player_name) %>%
        summarize_passer_df()

    team_rb_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & !is.na(rusher_player_name)) %>%
        group_by(pos_team, rusher_player_name) %>%
        summarize_rusher_df()

    team_wr_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & !is.na(receiver_player_name)) %>%
        group_by(pos_team, receiver_player_name) %>%
        summarize_receiver_df()

    team_off_pass_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & (pass == 1)) %>%
        group_by(pos_team) %>%
        summarize_team_df(remove_cols = c(
            'start_position', 'start_position_rank'
        ))

    team_off_rush_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & (rush == 1)) %>%
        group_by(pos_team) %>%
        summarize_team_df(remove_cols = c(
            'start_position', 'start_position_rank'
        ))

    print(glue("Summarizing defensive data"))

    team_def_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success)) %>%
        group_by(def_pos_team) %>%
        summarize_team_df(ascending = TRUE)

    team_def_pass_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & (pass == 1)) %>%
        group_by(def_pos_team) %>%
        summarize_team_df(ascending = TRUE, remove_cols = c(
            'start_position', 'start_position_rank'
        ))

    team_def_rush_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & (rush == 1)) %>%
        group_by(def_pos_team) %>%
        summarize_team_df(ascending = TRUE, remove_cols = c(
            'start_position', 'start_position_rank'
        ))

    print(glue("Merging offensive and defensive data, calculating full season ranks"))

    team_overall_data <- left_join(team_off_data, team_def_data, by = c("pos_team" = "def_pos_team"), suffix = c("_off","_def"))
    team_pass_data <- left_join(team_off_pass_data, team_def_pass_data, by = c("pos_team" = "def_pos_team"), suffix = c("_off","_def"))
    team_rush_data <- left_join(team_off_rush_data, team_def_rush_data, by = c("pos_team" = "def_pos_team"), suffix = c("_off","_def"))

    team_overall_data <- team_overall_data %>%
        mutate_summary_df()
    team_pass_data <- team_pass_data %>%
        mutate_summary_df()
    team_rush_data <- team_rush_data %>%
        mutate_summary_df()

    team_data <- left_join(team_overall_data, team_pass_data, by = c("pos_team" = "pos_team"), suffix = c("","_pass"))
    team_data <- left_join(team_data, team_rush_data, by = c("pos_team" = "pos_team"), suffix = c("","_rush"))

    print(glue("Generating year and team CSVs..."))

    team_data <- team_data %>%
        prepare_for_write()

    team_qb_data <- team_qb_data %>%
        prepare_for_write()

    team_rb_data <- team_rb_data %>%
        prepare_for_write()

    team_wr_data <- team_wr_data %>%
        prepare_for_write()

    print(glue("Creating folder /data/ if necessary"))
    dir.create(file.path('./data', ""), showWarnings = FALSE)

    print(glue("Creating folder /data/{yr} if necessary"))
    dir.create(file.path('./data', glue("{yr}")), showWarnings = FALSE)

    print(glue("Writing year CSVs to folder /data/{yr}"))
    write.csv(team_data, glue("./data/{yr}/overall.csv"), row.names = FALSE)
    write.csv(team_qb_data, glue("./data/{yr}/passing.csv"), row.names = FALSE)
    write.csv(team_rb_data, glue("./data/{yr}/rushing.csv"), row.names = FALSE)
    write.csv(team_wr_data, glue("./data/{yr}/receiving.csv"), row.names = FALSE)

    print(glue("Writing team CSVs to folder /data/{yr}"))
    team_data %>%
        group_by(abbreviation) %>%
        group_walk(~ write_team_csvs(.x, .y$abbreviation, yr, 'overall'))

    print(glue("Writing player CSVs to folder /data/{yr}"))
    team_qb_data %>%
        group_by(abbreviation) %>%
        group_walk(~ write_team_csvs(.x, .y$abbreviation, yr, 'passing'))

    team_rb_data %>%
        group_by(abbreviation) %>%
        group_walk(~ write_team_csvs(.x, .y$abbreviation, yr, 'rushing'))

    team_wr_data %>%
        group_by(abbreviation) %>%
        group_walk(~ write_team_csvs(.x, .y$abbreviation, yr, 'receiving'))
}


