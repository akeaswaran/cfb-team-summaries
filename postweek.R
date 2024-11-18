# run code
library(tidyverse)
library(dplyr)
library(cbbdata)
library(cbbplotR)
library(cfbplotR)
library(bskyr)

current_year = lubridate::year(Sys.Date())
data_path = paste0("./data/", current_year, "/overall.csv")
file_date = file.info(data_path)$ctime
formatted_file_date = strptime(file_date, format = "%F", tz = "America/New_York")
team.adj = read.csv(data_path) %>%
    dplyr::mutate(
        pos_team = dplyr::case_when(
            pos_team == "App State" ~ "Appalachian State",
            .default = pos_team
        )
    ) %>%
    dplyr::rename(
        adjOffEPA = adj_off_epa,
        adjDefEPA = adj_def_epa
    )

current_schedule = cfbfastR::espn_cfb_calendar(groups = "FBS")
current_week = current_schedule %>%
    dplyr::mutate(
        end_date = lubridate::as_datetime(end_date, format = "%FT%H:%SZ"),
        start_date = lubridate::as_datetime(start_date, format = "%FT%H:%SZ"),
    ) %>%
    dplyr::filter(
        Sys.Date() >= end_date
        # & Sys.Date() >= start_date
    ) %>%
    dplyr::last() %>%
    dplyr::select(
        season,
        season_type,
        week,
        week_start_date = start_date,
        week_end_date = end_date
    ) %>%
    dplyr::mutate(
        week = as.character(week),
        season = as.character(season),
        season_type_key = dplyr::case_when(
            season_type == "Regular Season" ~ "regular",
            season_type == "Postseason" ~ "postseason",
            season_type == "Off Season" ~ "offseason"
        )
    )

generate_adj_epa_comp = function(classif) {
    base = team.adj %>%
        dplyr::filter(fbs_class == classif & !is.na(adjOffEPA) & !is.na(adjDefEPA))

    x_margin = sum(abs(range(base$adjOffEPA, na.rm = T))) * 0.10
    y_margin = sum(abs(range(base$adjDefEPA, na.rm = T))) * 0.05

    base %>%
        ggplot2::ggplot(ggplot2::aes(x = adjOffEPA, y = adjDefEPA, team = pos_team))  +
        ggplot2::annotate(
            "text",
            x = max(base$adjOffEPA, na.rm = T) - x_margin,
            y = max(base$adjDefEPA, na.rm = T) - y_margin,
            size = 4,
            label = "Good Offense, Bad Defense",
            color = "gray50"
        ) +
        ggplot2::annotate(
            "text",
            x = min(base$adjOffEPA, na.rm = T) + x_margin,
            y = min(base$adjDefEPA, na.rm = T) + y_margin,
            size = 4,
            label = "Bad Offense, Good Defense",
            color = "gray50"
        ) +
        ggplot2::annotate(
            "text",
            x = max(base$adjOffEPA, na.rm = T) - x_margin,
            y = min(base$adjDefEPA, na.rm = T) + y_margin,
            size = 4,
            label = "Good Offense, Good Defense",
            color = "gray50"
        ) +
        ggplot2::annotate(
            "text",
            x = min(base$adjOffEPA, na.rm = T) + x_margin,
            y = max(base$adjDefEPA, na.rm = T) - y_margin,
            size = 4,
            label = "Bad Offense, Bad Defense",
            color = "gray50"
        ) +
        ggplot2::geom_vline(xintercept = mean(base$adjOffEPA, na.rm = T), linetype = "dashed", color = "red") +
        ggplot2::geom_hline(yintercept = mean(base$adjDefEPA, na.rm = T), linetype = "dashed", color = "red") +
        cfbplotR::geom_cfb_logos(width = 0.025) +
        # ggplot2::geom_point() +
        ggplot2::scale_y_reverse() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = paste0("Opponent-Adjusted EPA/Play (",classif,") - ", formatted_file_date),
            subtitle = "FBS vs FBS games only, garbage time excluded. Adjusted for home-field advantage and opponent quality.",
            caption = "Data via ESPN, collegefootballdata.com, cfbfastR, and @gameonpaper.com.\nMethodology from @makennahack. Idea from @jbuddavis.\nVisit https://gameonpaper.com for more CFB advanced stats.",
            x = "Offense Adjusted EPA/Play",
            y = "Defense Adjusted EPA/Play"
        ) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            # plot.caption = ggtext::element_markdown()
        )
}

purrr::map(unique(team.adj$fbs_class), function(classif) {
    ggsave(
        filename = paste0(formatted_file_date, "-", classif, ".png"),
        path = "./figures",
        plot = generate_adj_epa_comp(classif),
        device = "png",
        dpi = 320,
        bg = "white",
        width = 1200 * 3.2,
        height = 800 * 3.2,
        units = "px",
        create.dir = T
    )
}, .progress = T)

## skeet the adj epa charts

save_crop_gt <- function(gt_obj, file, whitespace = 50) {
    if (!dir.exists(file.path("./figures"))) {
        dir.create(file.path("./figures"))
    }

    gtExtras::gtsave_extra(gt_obj, paste0("./figures/", file), zoom = 2)

    magick::image_read(paste0("./figures/", file)) |>
        magick::image_trim() |>
        magick::image_border("white", glue::glue('{whitespace}x{whitespace}')) |>
        magick::image_write(paste0("./figures/", file))
}


net_stats_graphic = team.adj %>%
    dplyr::arrange(dplyr::desc(net_adj_epa)) %>%
    head(10) %>%
    dplyr::mutate(
        `#` = dplyr::row_number()
    ) %>%
    dplyr::select(
        `#`,
        pos_team,
        net_adj_epa,
        EPAplay_margin,
        yardsplay_margin,
        success_margin
    ) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team) %>%
    dplyr::mutate(
        pos_team = dplyr::case_when(
            stringr::str_detect(pos_team, "http://a.espncdn.com/i/teamlogos/ncaa/500/61.png") ~ "<img src='https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png' style='width: auto; vertical-align: -30%;'> georgia",
            .default = pos_team
        ),
    ) %>%
    gt::gt() %>%
    gt::fmt_percent(
        columns = c(success_margin),
        decimals = 1,
        force_sign = T,
    ) %>%
    gt::fmt_number(
        columns = c(net_adj_epa, EPAplay_margin, yardsplay_margin),
        decimals = 2,
        force_sign = T,
    ) %>%
    gtExtras::gt_theme_538() %>%
    gt::cols_align(
        align = "left",
        columns = c(pos_team)
    ) %>%
    gt::cols_align(
        align = "right",
        columns = c(`#`)
    ) %>%
    gt::cols_align(
        align = "center",
        columns = -c(`#`, pos_team)
    ) %>%
    gt::fmt_markdown(c(pos_team)) %>%
    gtExtras::gt_hulk_col_numeric(net_adj_epa, domain = team.adj$net_adj_epa) %>%
    gtExtras::gt_hulk_col_numeric(EPAplay_margin, domain = team.adj$EPAplay_margin) %>%
    gtExtras::gt_hulk_col_numeric(success_margin, domain = team.adj$success_margin) %>%
    gtExtras::gt_hulk_col_numeric(yardsplay_margin, domain = team.adj$yardsplay_margin) %>%
    gt::cols_label(
        "pos_team" = "Team",
        "net_adj_epa" = "Adj EPA/Play",
        "yardsplay_margin" = "Yds/Play",
        "success_margin" = "Success %",
        "EPAplay_margin" = "EPA/Play",
        "#" = "",
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = "#")
    ) %>%
    gt::tab_header(
        title = paste0(current_week$season, " Net Statistics Leaders"),
        subtitle = paste0("Updated: ", formatted_file_date)
    ) %>%
    gt::tab_footnote(gt::html("Data via ESPN, collegefootballdata.com, cfbfastR, and @gameonpaper.com. Table created with help from cbbdata by Andrew Weatherman (@aweatherman.com).<br/>Visit https://gameonpaper.com for more CFB advanced stats."))

off_stats_graphic = team.adj %>%
    dplyr::arrange(dplyr::desc(adjOffEPA)) %>%
    head(10) %>%
    dplyr::mutate(
        `#` = dplyr::row_number()
    ) %>%
    dplyr::select(
        `#`,
        pos_team,
        adjOffEPA,
        EPAplay_off,
        yardsplay_off,
        success_off
    ) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team) %>%
    dplyr::mutate(
        pos_team = dplyr::case_when(
            stringr::str_detect(pos_team, "http://a.espncdn.com/i/teamlogos/ncaa/500/61.png") ~ "<img src='https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png' style='width: auto; vertical-align: -30%;'> georgia",
            .default = pos_team
        ),
    ) %>%
    gt::gt() %>%
    gt::fmt_percent(
        columns = c(success_off),
        decimals = 1,
        # force_sign = T,
    ) %>%
    gt::fmt_number(
        columns = c(adjOffEPA, EPAplay_off, yardsplay_off),
        decimals = 2,
        # force_sign = T,
    ) %>%
    gtExtras::gt_theme_538() %>%
    gt::cols_align(
        align = "left",
        columns = c(pos_team)
    ) %>%
    gt::cols_align(
        align = "right",
        columns = c(`#`)
    ) %>%
    gt::cols_align(
        align = "center",
        columns = -c(`#`, pos_team)
    ) %>%
    gt::fmt_markdown(c(pos_team)) %>%
    gtExtras::gt_hulk_col_numeric(adjOffEPA, domain = team.adj$adjOffEPA) %>%
    gtExtras::gt_hulk_col_numeric(EPAplay_off, domain = team.adj$EPAplay_off) %>%
    gtExtras::gt_hulk_col_numeric(success_off, domain = team.adj$success_off) %>%
    gtExtras::gt_hulk_col_numeric(yardsplay_off, domain = team.adj$yardsplay_off) %>%
    gt::cols_label(
        "pos_team" = "Team",
        "adjOffEPA" = "Adj EPA/Play",
        "yardsplay_off" = "Yds/Play",
        "success_off" = "Success %",
        "EPAplay_off" = "EPA/Play",
        "#" = "",
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = "#")
    ) %>%
    gt::tab_header(
        title = paste0(current_week$season, " Offensive Statistics Leaders"),
        subtitle = paste0("Updated: ", formatted_file_date)
    ) %>%
    gt::tab_footnote(gt::html("Data via ESPN, collegefootballdata.com, cfbfastR, and @gameonpaper.com. Table created with help from cbbdata by Andrew Weatherman (@aweatherman.com).<br/>Visit https://gameonpaper.com for more CFB advanced stats."))

def_stats_graphic = team.adj %>%
    dplyr::arrange(adjDefEPA) %>%
    head(10) %>%
    dplyr::mutate(
        `#` = dplyr::row_number()
    ) %>%
    dplyr::select(
        `#`,
        pos_team,
        adjDefEPA,
        EPAplay_def,
        yardsplay_def,
        success_def
    ) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team) %>%
    dplyr::mutate(
        pos_team = dplyr::case_when(
            stringr::str_detect(pos_team, "http://a.espncdn.com/i/teamlogos/ncaa/500/61.png") ~ "<img src='https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png' style='width: auto; vertical-align: -30%;'> georgia",
            .default = pos_team
        ),
    ) %>%
    gt::gt() %>%
    gt::fmt_percent(
        columns = c(success_def),
        decimals = 1,
        # force_sign = T,
    ) %>%
    gt::fmt_number(
        columns = c(adjDefEPA, EPAplay_def, yardsplay_def),
        decimals = 2,
        # force_sign = T,
    ) %>%
    gtExtras::gt_theme_538() %>%
    gt::cols_align(
        align = "left",
        columns = c(pos_team)
    ) %>%
    gt::cols_align(
        align = "right",
        columns = c(`#`)
    ) %>%
    gt::cols_align(
        align = "center",
        columns = -c(`#`, pos_team)
    ) %>%
    gt::fmt_markdown(c(pos_team)) %>%
    gtExtras::gt_hulk_col_numeric(adjDefEPA, domain = team.adj$adjDefEPA, reverse = T) %>%
    gtExtras::gt_hulk_col_numeric(EPAplay_def, domain = team.adj$EPAplay_def, reverse = T) %>%
    gtExtras::gt_hulk_col_numeric(success_def, domain = team.adj$success_def, reverse = T) %>%
    gtExtras::gt_hulk_col_numeric(yardsplay_def, domain = team.adj$yardsplay_def, reverse = T) %>%
    gt::cols_label(
        "pos_team" = "Team",
        "adjDefEPA" = "Adj EPA/Play",
        "yardsplay_def" = "Yds/Play",
        "success_def" = "Success %",
        "EPAplay_def" = "EPA/Play",
        "#" = "",
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = "#")
    ) %>%
    gt::tab_header(
        title = paste0(current_week$season, " Defensive Statistics Leaders"),
        subtitle = paste0("Updated: ", formatted_file_date)
    ) %>%
    gt::tab_footnote(gt::html("Data via ESPN, collegefootballdata.com, cfbfastR, and @gameonpaper.com. Table created with help from cbbdata by Andrew Weatherman (@aweatherman.com).<br/>Visit https://gameonpaper.com for more CFB advanced stats."))


## skeet the net, off, def tables

pbp = cfbfastR::load_cfb_pbp(seasons = as.numeric(current_week$season))
games_raw = cfbfastR::load_cfb_schedules()

valid_fbs_teams <- cfbfastR::cfbd_team_info(only_fbs = T)
valid_fbs_teams = valid_fbs_teams %>%
    dplyr::mutate(
        team_id = as.character(team_id)
    )

games = games_raw %>%
    dplyr::mutate(
        game_id = as.character(game_id),
        week = as.character(week),
        season = as.character(season),
        season_type = stringr::str_trim(season_type)
    ) %>%
    dplyr::left_join(current_week, by = c("season", "season_type" = "season_type_key", "week")) %>%
    dplyr::filter(
        !is.na(week_start_date)
        & !is.na(week_end_date)
        & as.character(home_id) %in% valid_fbs_teams$team_id
        & as.character(away_id) %in% valid_fbs_teams$team_id
    )

week_pbp = pbp %>%
    dplyr::filter(
        as.character(game_id) %in% games$game_id
        & ((pass == 1) | (rush == 1))
        & !grepl("kneel", play_text)
    )

passer = week_pbp %>%
    dplyr::filter(
        pass == 1
        & passer_player_name != "TEAM"
        & passer_player_name != ""
        & !is.na(passer_player_name)
    ) %>%
    dplyr::group_by(passer_player_name) %>%
    dplyr::summarize(
        team = dplyr::last(pos_team),
        opponent = dplyr::last(def_pos_team),
        dropbacks = dplyr::n(),
        attempts = sum(pass_attempt, na.rm = T),
        completions = sum(completion, na.rm = T),
        comp_pct = completions / attempts,
        yards = sum(yds_receiving, na.rm = T),
        Pass_TD = sum(pass_td, na.rm = T),
        Pass_INT = sum(int, na.rm = T),

        sacks = sum(sack_vec, na.rm = T),
        sack_adj_yards = yards - sum(yds_sacked, na.rm = T),
        yards_db = sack_adj_yards / dropbacks,

        EPA_db = mean(EPA, na.rm = T),
        SR = mean(EPA > 0, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
        dropbacks >= 10
    )

pass_epa_graphic = passer %>%
    dplyr::arrange(dplyr::desc(EPA_db)) %>%
    head(10) %>%
    dplyr::mutate(
        `#` = dplyr::row_number()
    ) %>%
    dplyr::select(
        `#`,
        pos_team = team,
        def_pos_team_name = opponent,
        passer_player_name,
        # games,
        dropbacks,
        comp = completions,
        att = attempts,
        comppct = comp_pct,
        passing_td = Pass_TD,
        pass_int = Pass_INT,
        sacked = sacks,
        sack_adj_yards,
        yardsdropback = yards_db,
        success = SR,
        EPAplay = EPA_db,
    ) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team, logo_height = 15) %>%
    cbbplotR::gt_cbb_teams(def_pos_team_name, def_pos_team_name) %>%
    dplyr::mutate(
        pos_team = dplyr::case_when(
            stringr::str_detect(pos_team, "http://a.espncdn.com/i/teamlogos/ncaa/500/61.png") ~ "<img src='https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png' style='height: 15px; width: auto; vertical-align: -30%;'> georgia",
            .default = pos_team
        ),
        passer_player_name = dplyr::case_when(
            grepl("ennui-uga", pos_team) ~ tolower(passer_player_name),
            .default = passer_player_name
        )
    ) %>%
    gt::gt() %>%
    gt::fmt_percent(
        columns = c(comppct, success),
        decimals = 0
    ) %>%
    gt::fmt_number(
        columns = c(yardsdropback),
        decimals = 1
    ) %>%
    gt::fmt_number(
        columns = c(EPAplay),
        decimals = 2
    ) %>%
    gtExtras::gt_theme_538() %>%
    gt::cols_align(
        align = "left",
        columns = c(pos_team, passer_player_name, def_pos_team_name)
    ) %>%
    gt::cols_align(
        align = "right",
        columns = c(`#`)
    ) %>%
    gt::cols_align(
        align = "center",
        columns = -c(`#`, pos_team, passer_player_name, def_pos_team_name)
    ) %>%
    gt::fmt_markdown(c(pos_team, def_pos_team_name)) %>%
    gtExtras::gt_merge_stack(passer_player_name, pos_team, small_cap = F, font_size = c("18px", "12px"), font_weight = c("bold", "normal"), palette = c("black", "black")) %>%
    gtExtras::gt_hulk_col_numeric(EPAplay, domain = passer$EPA_db) %>%
    gtExtras::gt_hulk_col_numeric(success, domain = passer$SR) %>%
    gtExtras::gt_hulk_col_numeric(yardsdropback, domain = passer$yards_db) %>%
    gtExtras::gt_hulk_col_numeric(sack_adj_yards, domain = passer$sack_adj_yards) %>%
    gt::cols_label(
        "passer_player_name" = "Player",
        "def_pos_team_name" = "Opponent",
        "dropbacks" = "Dropbacks",
        "comp" = "CMP",
        "att" = "ATT",
        "comppct" = "CMP%",
        "sacked" = "Sck",
        "sack_adj_yards" = "Adj Yds",
        "yardsdropback" = "Yds/DB",
        "success" = "Success %",
        "EPAplay" = "EPA/Play",
        "#" = "",
        "passing_td" = "TD",
        "pass_int" = "INT"
    ) %>%
    gt::cols_move(
        columns = def_pos_team_name,
        after = passer_player_name
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = "#")
    ) %>%
    gt::tab_header(
        title = paste0(current_week$season, " ", current_week$season_type, " - Week ", current_week$week, " Passing EPA Leaders"),
        subtitle = "Min. 10 dropbacks. Drives with kneels stripped out."
    ) %>%
    gt::tab_footnote(gt::html("Data via ESPN, collegefootballdata.com, cfbfastR, and @gameonpaper.com. Table created with help from cbbdata by Andrew Weatherman (@aweatherman.com).<br/>Visit https://gameonpaper.com for more CFB advanced stats."))


rusher = week_pbp %>%
    dplyr::filter(
        rush == 1
        & rusher_player_name != "TEAM"
        & rusher_player_name != ""
        & !is.na(rusher_player_name)
    ) %>%
    dplyr::group_by(rusher_player_name) %>%
    dplyr::summarize(
        pos_team = dplyr::last(pos_team),
        def_pos_team_name = dplyr::last(def_pos_team),
        carries = dplyr::n(),
        # attempts = sum(rush, na.rm = T),
        yards = sum(yds_rushed, na.rm = T),

        rushing_td = sum(rush_td, na.rm = T),
        fumbles = sum(fumble_vec, na.rm = T),

        yards_per_carry = mean(yds_rushed, na.rm = T),

        EPAplay = mean(EPA, na.rm = T),
        success = mean(EPA > 0, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
        carries >= 10
    )

rushing_epa_graphic = rusher %>%
    dplyr::arrange(dplyr::desc(EPAplay)) %>%
    head(10) %>%
    dplyr::mutate(
        `#` = dplyr::row_number()
    ) %>%
    dplyr::select(
        `#`,
        pos_team,
        def_pos_team_name,
        rusher_player_name,
        carries,
        yards,
        yards_per_carry,
        rushing_td,
        fumbles,# = ('fumble_vec', sum),
        # fum_lost = Fum_Lost,# = ('fumble_lost', sum)
        success,
        EPAplay
    ) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team, logo_height = 15) %>%
    cbbplotR::gt_cbb_teams(def_pos_team_name, def_pos_team_name) %>%
    dplyr::mutate(
        pos_team = dplyr::case_when(
            stringr::str_detect(pos_team, "http://a.espncdn.com/i/teamlogos/ncaa/500/61.png") ~ "<img src='https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png' style='height: 15px; width: auto; vertical-align: -30%;'> georgia",
            .default = pos_team
        ),
        rusher_player_name = dplyr::case_when(
            grepl("ennui-uga", pos_team) ~ tolower(rusher_player_name),
            .default = rusher_player_name
        )
    ) %>%
    gt::gt() %>%
    gt::fmt_percent(
        columns = c(success),
        decimals = 0
    ) %>%
    gt::fmt_number(
        columns = c(yards_per_carry),
        decimals = 1
    ) %>%
    gt::fmt_number(
        columns = c(EPAplay),
        decimals = 2
    ) %>%
    gtExtras::gt_theme_538() %>%
    gt::cols_align(
        align = "left",
        columns = c(pos_team, rusher_player_name, def_pos_team_name)
    ) %>%
    gt::cols_align(
        align = "right",
        columns = c(`#`)
    ) %>%
    gt::cols_align(
        align = "center",
        columns = -c(`#`, pos_team, rusher_player_name, def_pos_team_name)
    ) %>%
    gt::fmt_markdown(c(pos_team, def_pos_team_name)) %>%
    gtExtras::gt_merge_stack(rusher_player_name, pos_team, small_cap = F, font_size = c("18px", "12px"), font_weight = c("bold", "normal"), palette = c("black", "black")) %>%
    gtExtras::gt_hulk_col_numeric(EPAplay, domain = rusher$EPAplay) %>%
    gtExtras::gt_hulk_col_numeric(success, domain = rusher$success) %>%
    gtExtras::gt_hulk_col_numeric(yards_per_carry, domain = rusher$yards_per_carry) %>%
    gtExtras::gt_hulk_col_numeric(yards, domain = rusher$yards) %>%
    gt::cols_label(
        "rusher_player_name" = "Player",
        "def_pos_team_name" = "Opponent",
        "rushing_td" = "TD",
        "yards_per_carry" = "Yds/Car",
        # "fum_lost" = "Lost",
        "success" = "Success %",
        "EPAplay" = "EPA/Play",
        "#" = "",
    ) %>%
    gt::cols_move(
        columns = def_pos_team_name,
        after = rusher_player_name
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = "#")
    ) %>%
    gt::tab_header(
        title = paste0(current_week$season, " ", current_week$season_type, " - Week ", current_week$week, " Rushing EPA Leaders"),
        subtitle = "Min. 10 carries. Drives with kneels stripped out."
    ) %>%
    gt::tab_footnote(gt::html("Data via ESPN, collegefootballdata.com, cfbfastR, and @gameonpaper.com. Table created with help from cbbdata by Andrew Weatherman (@aweatherman.com).<br/>Visit https://gameonpaper.com for more CFB advanced stats."))



receiver = week_pbp %>%
    dplyr::filter(
        pass == 1
        & receiver_player_name != "TEAM"
        & receiver_player_name != ""
        & !is.na(receiver_player_name)
    ) %>%
    dplyr::group_by(receiver_player_name) %>%
    dplyr::summarize(
        pos_team = dplyr::last(pos_team),
        def_pos_team_name = dplyr::last(def_pos_team),

        targets = dplyr::n(),
        # attempts = sum(pass_attempt, na.rm = T),
        receptions = sum(completion, na.rm = T),
        comp_pct = receptions / targets,

        yards = sum(yds_receiving, na.rm = T),
        yards_per_target = mean(yds_receiving, na.rm = T),
        yards_per_rec = dplyr::if_else(receptions == 0, 0, yards / receptions),
        rec_td = sum(pass_td, na.rm = T),
        fumbles = sum(fumble_vec, na.rm = T),

        EPAplay = mean(EPA, na.rm = T),
        success = mean(EPA > 0, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
        targets >= 5
    )


rec_epa_graphic = receiver %>%
    dplyr::arrange(dplyr::desc(EPAplay)) %>%
    head(10) %>%
    dplyr::mutate(
        `#` = dplyr::row_number()
    ) %>%
    dplyr::select(
        `#`,
        pos_team,
        def_pos_team_name,
        receiver_player_name,
        receptions,
        # targets, # = Tar,
        yards,
        yards_per_rec,# yards_per_target,
        rec_td,
        fumbles,# = ('fumble_vec', sum),
        # fum_lost = Fum_Lost,# = ('fumble_lost', sum)
        success,
        EPAplay,
    ) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team, logo_height = 15) %>%
    cbbplotR::gt_cbb_teams(def_pos_team_name, def_pos_team_name) %>%
    dplyr::mutate(
        pos_team = dplyr::case_when(
            stringr::str_detect(pos_team, "http://a.espncdn.com/i/teamlogos/ncaa/500/61.png") ~ "<img src='https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png' style='height: 15px; width: auto; vertical-align: -30%;'> georgia",
            .default = pos_team
        ),
        receiver_player_name = dplyr::case_when(
            grepl("ennui-uga", pos_team) ~ tolower(receiver_player_name),
            .default = receiver_player_name
        )
    ) %>%
    gt::gt() %>%
    gt::fmt_percent(
        columns = c(success),
        decimals = 0
    ) %>%
    gt::fmt_number(
        columns = c(yards_per_rec),
        decimals = 1
    ) %>%
    gt::fmt_number(
        columns = c(EPAplay),
        decimals = 2
    ) %>%
    gtExtras::gt_theme_538() %>%
    gt::cols_align(
        align = "left",
        columns = c(pos_team, receiver_player_name, def_pos_team_name)
    ) %>%
    gt::cols_align(
        align = "right",
        columns = c(`#`)
    ) %>%
    gt::cols_align(
        align = "center",
        columns = -c(`#`, pos_team, receiver_player_name, def_pos_team_name)
    ) %>%
    gt::fmt_markdown(c(pos_team, def_pos_team_name)) %>%
    gtExtras::gt_merge_stack(receiver_player_name, pos_team, small_cap = F, font_size = c("18px", "12px"), font_weight = c("bold", "normal"), palette = c("black", "black")) %>%
    gtExtras::gt_hulk_col_numeric(EPAplay, domain = receiver$EPAplay) %>%
    gtExtras::gt_hulk_col_numeric(success, domain = receiver$success) %>%
    gtExtras::gt_hulk_col_numeric(yards_per_rec, domain = receiver$yards_per_rec) %>%
    gtExtras::gt_hulk_col_numeric(yards, domain = receiver$yards) %>%
    gt::cols_label(
        "receiver_player_name" = "Player",
        "def_pos_team_name" = "Opponent",
        "rec_td" = "TD",
        "yards_per_rec" = "Yds/Rec",
        # "fum_lost" = "Lost",
        "success" = "Success %",
        "EPAplay" = "EPA/Rec",
        "#" = "",
    ) %>%
    gt::cols_move(
        columns = def_pos_team_name,
        after = receiver_player_name
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = "#")
    ) %>%
    gt::tab_header(
        title = paste0(current_week$season, " ", current_week$season_type, " - Week ", current_week$week, " Receiving EPA Leaders"),
        subtitle = "Min. 5 targets."
    ) %>%
    gt::tab_footnote(gt::html("Data via ESPN, collegefootballdata.com, cfbfastR, and @gameonpaper.com. Table created with help from cbbdata by Andrew Weatherman (@aweatherman.com).<br/>Visit https://gameonpaper.com for more CFB advanced stats."))

## generate graphics screenshots
graphics = list(
    "net_stat.png" = net_stats_graphic,
    "off_stat.png" = off_stats_graphic,
    "def_stat.png" = def_stats_graphic,
    "passing.png" = pass_epa_graphic,
    "rushing.png" = rushing_epa_graphic,
    "receiving.png" = rec_epa_graphic
)

purrr::walk(names(graphics), function(key) {
    tb = graphics[[key]]
    save_crop_gt(tb, paste0(formatted_file_date, "-", key))
}, .progress = T)

#### SKEETING #####

retrieve_skeet_id = function(resp) {
    resp_id = resp$uri[[1]]
    resp_id = stringr::str_extract(resp_id, "post/(\\w+)$", group = 1)
    return(resp_id)
}

# start skeet thread
thread_content = list(
    list(
        "text" = glue::glue('🤖 Stand-by for an automated thread of updates:\n\nSeason: {current_week$season}\nWeek: {current_week$week}\nType: {current_week$season_type}\n\nReach out to @akeaswaran.me or @saiemgilani.bsky.social with any concerns!')#,
    ),
    list(
        "text" = glue::glue("Adjusted EPA through {formatted_file_date}, P4\n\nLink: https://gameonpaper.com/cfb/year/2024/charts/team/epa"),
        "image" = glue::glue("./figures/{formatted_file_date}-P4.png"),
        "alt_text" = paste0("Shows team logos plotted by offensive adjusted EPA/play on the X-axis and defensive adjusted EPA/play on the Y-axis for the Power 4 teams as of ", formatted_file_date)#,
    ),
    list(
        "text" = glue::glue("Adjusted EPA through {formatted_file_date}, G6\n\nLink: https://gameonpaper.com/cfb/year/2024/charts/team/epa"),
        "image" = glue::glue("./figures/{formatted_file_date}-G6.png"),
        "alt_text" = paste0("Shows team logos plotted by offensive adjusted EPA/play on the X-axis and defensive adjusted EPA/play on the Y-axis for the Group of 6 teams as of ", formatted_file_date)#,
    ),
    list(
        "text" = glue::glue("Net Statistics Leaders through {formatted_file_date}\n\nFull list: https://gameonpaper.com/cfb/year/2024/teams/differential"),
        "image" = glue::glue("./figures/{formatted_file_date}-net_stat.png")#,
        # "alt_text" = ""
    ),
    list(
        "text" = glue::glue("Offensive Statistics Leaders through {formatted_file_date}\n\nFull list: https://gameonpaper.com/cfb/year/2024/teams/offensive"),
        "image" = glue::glue("./figures/{formatted_file_date}-off_stat.png")#,
        # "alt_text" = ""
    ),
    list(
        "text" = glue::glue("Defensive Statistics Leaders through {formatted_file_date}\n\nFull list: https://gameonpaper.com/cfb/year/2024/teams/defensive"),
        "image" = glue::glue("./figures/{formatted_file_date}-def_stat.png")#,
        # "alt_text" = ""
    ),
    list(
        "text" = glue::glue("Weekly Passing Leaders - {formatted_file_date}\n\nFull list: https://gameonpaper.com/cfb/year/2024/players/passing"),
        "image" = glue::glue("./figures/{formatted_file_date}-passing.png")#,
        # "alt_text" = ""
    ),
    list(
        "text" = glue::glue("Weekly Rushing Leaders - {formatted_file_date}\n\nFull list: https://gameonpaper.com/cfb/year/2024/players/rushing"),
        "image" = glue::glue("./figures/{formatted_file_date}-rushing.png")#,
        # "alt_text" = ""
    ),
    list(
        "text" = glue::glue("Weekly Receiving Leaders - {formatted_file_date}\n\nFull list: https://gameonpaper.com/cfb/year/2024/players/receiving"),
        "image" = glue::glue("./figures/{formatted_file_date}-receiving.png")#,
        # "alt_text" = ""
    )
)

fire_skeet = function(content, reply = NULL, live_run = FALSE) {
    # alt_text = ifelse(!is.null(content[["alt_text"]]), content[["alt_text"]], content[["text"]])
    if (!is.null(content[["alt_text"]]) && !is.null(content[["image"]])) {
        alt_text <- content[["alt_text"]]
    } else if (!is.null(content[["image"]])) {
        alt_text <- content[["text"]]
    } else {
        alt_text <- NULL
    }
    printing_verb = dplyr::if_else(live_run, "Sending", "Previewing")
    print(
        paste0(
            c(
                paste0(printing_verb, " skeet:"),
                content[["text"]],
                paste0("With image at filepath: ", content[["image"]]),
                "Alt Text:",
                alt_text
            ),
            collapse = "\n"
        )
    )
    if (live_run) {
        if (is.null(reply)) {
            return(
                bskyr::bs_post(
                    content[["text"]],
                    images = content[["image"]],
                    images_alt = alt_text
                )
            )
        } else {
            return(
                bskyr::bs_post(
                    content[["text"]],
                    images = content[["image"]],
                    images_alt = alt_text,
                    reply = paste0("https://bsky.app/profile/gameonpaper.com/post/", retrieve_skeet_id(reply))
                )
            )
        }
    }
}

is_live_run = Sys.getenv("SKEET_ENVIRONMENT") == "prod"
reply = NULL
delay = dplyr::if_else(is_live_run, 60 * 2.5, 5)
for (i in 1:(length(thread_content))) {
    g = thread_content[[i]]
    reply <- fire_skeet(g, reply, is_live_run)
    print(paste0("Sleeping for ", delay, " seconds before skeeting again..."))
    Sys.sleep(delay)
}
