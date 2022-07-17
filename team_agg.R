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

write_team_csvs <- function (data, team, yr) {
    print(glue("Creating folder /data/{yr}/{team} if necessary"))
    dir.create(file.path(glue('./data/{yr}'), team), showWarnings = FALSE)

    write.csv(data, glue("./data/{yr}/{team}/overall.csv"), row.names = FALSE)
}

summarize_df <- function(x, ascending=FALSE, remove_cols = c()) {
    tmp <- x %>%
        summarize(
                plays = n(),
                playsgame = plays / length(unique(game_id)),

                # EPA + EPA/play
                TEPA = sum(EPA),
                EPAplay = mean(EPA),
                EPAgame = TEPA / length(unique(game_id)),

                # yards
                yards = sum(EPA),
                yardsplay = mean(EPA),
                yardsgame = TEPA / length(unique(game_id)),

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

                drivesgame = rank(drivesgame),
                yardsdrive_rank = rank(yardsdrive),
                playsdrive_rank = rank(playsdrive),

                # except start position
                start_position_rank = rank(-start_position)
            )
    } else {
        tmp <- tmp %>%
            mutate(
                # rank ascending cause defense
                playsgame_rank = rank(-playsgame),
                TEPA_rank = rank(-TEPA),
                EPAgame_rank = rank(-EPAgame),
                EPAplay_rank = rank(-EPAplay),
                success_rank = rank(-success),

                drivesgame = rank(-drivesgame),
                yardsdrive_rank = rank(-yardsdrive),
                playsdrive_rank = rank(-playsdrive),

                # except start position
                start_position_rank = rank(-start_position)
            )
    }

    return(tmp %>% select(!matches(paste(remove_cols,sep="|"))))
}

mutate_summary_df <- function(x) {
    # base_check =
    print(names(x))
    tmp <- x %>%
        mutate(
            TEPA_margin = TEPA_off - TEPA_def,
            EPAplay_margin = EPAplay_off - EPAplay_def,
            EPAgame_margin = EPAgame_off - EPAgame_def,
            success_margin = success_off - success_def,


            TEPA_margin_rank = rank(-TEPA_margin),
            EPAgame_margin_rank = rank(-EPAgame_margin),
            EPAplay_margin_rank = rank(-EPAplay_margin),
            success_margin_rank = rank(-success_margin),

        )
    if (("start_position_off" %in% colnames(x))) {
        tmp <- tmp %>% mutate(
            start_position_margin = (100 - start_position_off) - (100 - start_position_def),
            start_position_margin_rank = rank(-start_position_margin)
        )
    }

    return (tmp)
}

# for (yr in seasons) {
    print(glue("Starting processing for {yr} season..."))
    plays <- cfbfastR::load_cfb_pbp(seasons = c(yr))

    print(glue("Found {nrow(plays)} total plays, filtering to FBS/FBS"))
    plays <- plays %>%
        filter(
            pos_team %in% valid_fbs_teams$school,
            def_pos_team %in% valid_fbs_teams$school,
            (pass == 1) | (rush == 1)
        ) %>%
        mutate(
            game_id = as.character(game_id)
        )

    print(glue("Found {nrow(plays)} total FBS/FBS plays, summarizing offensive data"))

    team_off_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success)) %>%
        group_by(pos_team) %>%
        summarize_df()

    team_off_pass_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & (pass == 1)) %>%
        group_by(pos_team) %>%
        summarize_df(remove_cols = c(
            'start_position', 'start_position_rank'
        ))

    team_off_rush_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & (rush == 1)) %>%
        group_by(pos_team) %>%
        summarize_df(remove_cols = c(
            'start_position', 'start_position_rank'
        ))

    print(glue("Summarizing defensive data"))

    team_def_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success)) %>%
        group_by(def_pos_team) %>%
        summarize_df(ascending = TRUE)

    team_def_pass_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & (pass == 1)) %>%
        group_by(def_pos_team) %>%
        summarize_df(ascending = TRUE, remove_cols = c(
            'start_position', 'start_position_rank'
        ))

    team_def_rush_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success) & (rush == 1)) %>%
        group_by(def_pos_team) %>%
        summarize_df(ascending = TRUE, remove_cols = c(
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

    team_data <- team_data %>%
        mutate(
            season = yr
        )

    print(glue("Generating year and team CSVs..."))
    old_columns <- colnames(team_data)
    old_columns <- old_columns[grepl("_rank", old_columns)]
    new_columns <- str_replace(old_columns, "_rank","")
    new_columns <- paste(new_columns,"rank",sep="_")

    team_data <- team_data %>%
        rename_at(all_of(old_columns), ~ new_columns) %>%
        left_join(valid_fbs_teams, by = c('pos_team' = 'school')) %>%
        select(
            team_id,
            pos_team,
            abbreviation,
            season,
            everything()
        )

    print(glue("Creating folder /data/ if necessary"))
    dir.create(file.path('./data', ""), showWarnings = FALSE)

    print(glue("Creating folder /data/{yr} if necessary"))
    dir.create(file.path('./data', glue("{yr}")), showWarnings = FALSE)

    print(glue("Writing year CSV to folder /data/{yr}"))
    write.csv(team_data, glue("./data/{yr}/overall.csv"), row.names = FALSE)

    print(glue("Writing team CSVs to folder /data/{yr}"))
    team_data %>%
        group_by(abbreviation) %>%
        group_walk(~ write_team_csvs(.x, .y$abbreviation, yr))
# }


