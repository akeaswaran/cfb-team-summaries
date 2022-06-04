library(cfbfastR)
library(dplyr)
library(glue)
library(stringr)

seasons <- 2014:2021
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

for (yr in seasons) {
    print(glue("Starting processing for {yr} season..."))
    plays <- cfbfastR::load_cfb_pbp(seasons = c(yr))

    print(glue("Found {nrow(plays)} total plays, filtering to FBS/FBS"))
    plays <- plays %>%
        filter(
            pos_team %in% valid_fbs_teams$school,
            def_pos_team %in% valid_fbs_teams$school
        ) %>%
        mutate(
            game_id = as.character(game_id)
        )

    print(glue("Found {nrow(plays)} total FBS/FBS plays, summarizing offensive data"))

    team_off_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success)) %>%
        group_by(pos_team) %>%
        summarize(
            plays = n(),
            playsgame = plays / length(unique(game_id)),

            # EPA + EPA/play
            TEPA = sum(EPA),
            EPAplay = mean(EPA),
            EPAgame = TEPA / length(unique(game_id)),

            # SR
            success = mean(success),

            # Field Position
            start_position = mean(drive_start_yards_to_goal),
        ) %>%
        mutate(
            # rank descending cause offense
            playsgame_rank = rank(-playsgame),
            TEPA_rank = rank(-TEPA),
            EPAgame_rank = rank(-EPAgame),
            EPAplay_rank = rank(-EPAplay),
            success_rank = rank(-success),

            # except start position
            start_position_rank = rank(start_position)
        )

    print(glue("Summarizing defensive data"))

    team_def_data <- plays %>%
        filter(!is.na(EPA) & !is.na(success)) %>%
        group_by(def_pos_team) %>%
        summarize(
            plays = n(),
            playsgame = plays / length(unique(game_id)),

            # EPA + EPA/play
            TEPA = sum(EPA),
            EPAplay = mean(EPA),
            EPAgame = TEPA / length(unique(game_id)),

            # SR
            success = mean(success),

            # Field Position
            start_position = mean(drive_start_yards_to_goal),
        ) %>%
        mutate(
            # rank ascending cause defense
            playsgame_rank = rank(playsgame),
            TEPA_rank = rank(TEPA),
            EPAgame_rank = rank(EPAgame),
            EPAplay_rank = rank(EPAplay),
            success_rank = rank(success),

            # except start position
            start_position_rank = rank(-start_position)
        )

    print(glue("Merging offensive and defensive data, calculating full season ranks"))

    team_data <- left_join(team_off_data, team_def_data, by = c("pos_team" = "def_pos_team"), suffix = c("_off","_def"))
    team_data <- team_data %>%
        mutate(
            TEPA_margin = TEPA_off - TEPA_def,
            EPAplay_margin = EPAplay_off - EPAplay_def,
            EPAgame_margin = EPAgame_off - EPAgame_def,
            success_margin = success_off - success_def,
            start_position_margin = (100 - start_position_off) - (100 - start_position_def),

            TEPA_margin_rank = rank(-TEPA_margin),
            EPAgame_margin_rank = rank(-EPAgame_margin),
            EPAplay_margin_rank = rank(-EPAplay_margin),
            success_margin_rank = rank(-success_margin),
            start_position_margin_rank = rank(-start_position_margin),

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

    print(glue("Creating folder /data/{yr} if necessary"))
    dir.create(file.path('./data', glue("{yr}")), showWarnings = FALSE)

    print(glue("Writing year CSV to folder /data/{yr}"))
    write.csv(team_data, glue("./data/{yr}/overall.csv"), row.names = FALSE)

    print(glue("Writing team CSVs to folder /data/{yr}"))
    team_data %>%
        group_by(abbreviation) %>%
        group_walk(~ write_team_csvs(.x, .y$abbreviation, yr))
}


