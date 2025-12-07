# run code
library(tidyverse)
library(dplyr)
library(cbbdata)
library(cbbplotR)
library(cfbplotR)
library(gt)

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

# current_schedule = cfbfastR::espn_cfb_calendar(groups = "FBS")
# current_week = current_schedule %>%
#     dplyr::mutate(
#         end_date = lubridate::as_datetime(end_date, format = "%FT%H:%SZ"),
#         end_date = lubridate::as_date(end_date),
#         start_date = lubridate::as_datetime(start_date, format = "%FT%H:%SZ"),
#     ) %>%
#     dplyr::filter(
#         Sys.Date() >= end_date
#         # & Sys.Date() >= start_date
#     ) %>%
#     dplyr::last() %>%
#     dplyr::select(
#         season,
#         season_type,
#         week,
#         week_start_date = start_date,
#         week_end_date = end_date
#     ) %>%
#     dplyr::mutate(
#         week = as.character(week),
#         season = as.character(season),
#         season_type_key = dplyr::case_when(
#             season_type == "Regular Season" ~ "regular",
#             season_type == "Postseason" ~ "postseason",
#             season_type == "Off Season" ~ "offseason"
#         )
#     )

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

seeds = c("Indiana", "Georgia", "Texas Tech", "Ohio State", "Oregon", "Ole Miss", "Texas A&M", "Oklahoma", "Alabama", "Miami", "Tulane", "James Madison")

seeds = data.frame(seeds) %>%
    dplyr::rename(pos_team = seeds)

seeds$seed = as.character(1:12)
seeds$rank = c(1,2,3,4,5,6,7,8,9,10,20,24)

bubble_teams = c("Alabama", "Miami", "Notre Dame", "BYU", "Duke", "Virginia", "North Texas")
bubble_teams = data.frame(bubble_teams) %>%
    dplyr::rename(pos_team = bubble_teams)

bubble_teams$seed = c(9, 10, rep(NA_integer_, 5))
bubble_teams$rank = c(9, 10, 11, 12, NA_integer_, 19, 25)

net_stats_graphic = team.adj %>%
    dplyr::left_join(seeds, by = "pos_team") %>%
    dplyr::filter(!is.na(seed)) %>%
    dplyr::arrange(dplyr::desc(net_adj_epa)) %>%
    dplyr::mutate(
        `#` = dplyr::row_number(),
        conference = dplyr::case_when(
            conference == "American Athletic" ~ "American Athletic Conference",
            .default = conference
        ),
        is_conf_champ = pos_team %in% c("Tulane", "James Madison", "Indiana", "Georgia", "Texas Tech", "Duke")
    ) %>%
    dplyr::select(
        `#`,
        pos_team,
        is_conf_champ,
        conference,
        seed,
        rank,
        net_adj_epa,
        EPAplay_margin,
        yardsplay_margin,
        success_margin
    ) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team) %>%
    cbbplotR::gt_cbb_conferences(conference, conference, include_name = F, vertical_align = "-15%") %>%
    dplyr::mutate(
        pos_team = dplyr::case_when(
            stringr::str_detect(pos_team, "http://a.espncdn.com/i/teamlogos/ncaa/500/61.png") ~ "<img src='https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png' style='width: auto; vertical-align: -30%; height:25px'> georgia",
            .default = pos_team
        ),
        pos_team = dplyr::case_when(
            is_conf_champ ~ paste(pos_team, "🏆"),
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
        columns = c(pos_team, conference)
    ) %>%
    gt::cols_align(
        align = "right",
        columns = c(`#`)
    ) %>%
    gt::cols_align(
        align = "center",
        columns = -c(`#`, pos_team, conference)
    ) %>%
    gt::fmt_markdown(c(pos_team, conference)) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(rows = as.integer(seed) <= 4, columns = c(seed, pos_team))
    ) %>%
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
        "seed" = gt::md("<img src='https://playoffpremium.com/wp-content/themes/responsive/images/events/playoff-premium-hard-rock-stadium-107459_600.png' style='height: 15px;vertical-align: -5%;'> CFP Seed"),
        "rank" = gt::md("<img src='https://playoffpremium.com/wp-content/themes/responsive/images/events/playoff-premium-hard-rock-stadium-107459_600.png' style='height: 15px;vertical-align: -5%;'> CFP Rank"),
    ) %>%
    gt::cols_hide(columns = c(is_conf_champ)) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = "#")
    ) %>%
    gt::tab_header(
        title = gt::md("<img src='https://playoffpremium.com/wp-content/themes/responsive/images/events/playoff-premium-hard-rock-stadium-107459_600.png' style='height: 30px; vertical-align: -30%;'> 2025-26 College Football Playoff <img src='https://github.com/saiemgilani/game-on-paper-app/blob/main/frontend/public/assets/img/web-app-manifest-512x512.png?raw=true' style='height: 25px; float: right; vertical-align: middle;'/>"),
        subtitle = "Net Statistics Breakdown - FBS vs FBS Games"
    ) %>%
    gt::tab_footnote(gt::html("🏆 - Conference champion, <b>Bold</b> - First-round bye<br/>Data via ESPN, collegefootballdata.com, cfbfastR, and @gameonpaper.com. Table created with help from cbbdata by Andrew Weatherman (@aweatherman.com).<br/>Visit https://gameonpaper.com for more CFB advanced stats."))


save_crop_gt(net_stats_graphic, "25playoff.png")


bubble_graphic = team.adj %>%
    dplyr::left_join(seeds, by = "pos_team") %>%
    dplyr::arrange(dplyr::desc(net_adj_epa)) %>%
    dplyr::mutate(
        `#` = dplyr::row_number(),
        conference = dplyr::case_when(
            conference == "American Athletic" ~ "American Athletic Conference",
            conference == "FBS Independents" ~ "Independent",
            .default = conference
        ),
        is_conf_champ = pos_team %in% c("Tulane", "James Madison", "Indiana", "Georgia", "Texas Tech", "Duke")
    ) %>%
    dplyr::filter(
        pos_team %in% c("Alabama", "Notre Dame", "BYU", "Duke", "Virginia", "Miami", "North Texas")
    ) %>%
    dplyr::select(
        `#`,
        pos_team,
        is_conf_champ,
        conference,
        seed,
        rank,
        net_adj_epa,
        EPAplay_margin,
        yardsplay_margin,
        success_margin
    ) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team) %>%
    cbbplotR::gt_cbb_conferences(conference, conference, include_name = F, vertical_align = "-15%") %>%
    dplyr::mutate(
        pos_team = dplyr::case_when(
            stringr::str_detect(pos_team, "http://a.espncdn.com/i/teamlogos/ncaa/500/61.png") ~ "<img src='https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png' style='width: auto; vertical-align: -30%; height:25px'> georgia",
            .default = pos_team
        ),
        pos_team = dplyr::case_when(
            is_conf_champ ~ paste(pos_team, "🏆"),
            .default = pos_team
        ),
        seed = dplyr::case_when(
            is.na(seed) ~ "-",
            .default = as.character(seed)
        ),
        rank = dplyr::case_when(
            is.na(rank) ~ "-",
            .default = as.character(rank)
        )
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
        columns = c(pos_team, conference)
    ) %>%
    gt::cols_align(
        align = "right",
        columns = c(`#`)
    ) %>%
    gt::cols_align(
        align = "center",
        columns = -c(`#`, pos_team, conference)
    ) %>%
    gt::fmt_markdown(c(pos_team, conference)) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(rows = as.integer(seed) <= 4, columns = c(seed, pos_team))
    ) %>%
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
        "seed" = gt::md("<img src='https://playoffpremium.com/wp-content/themes/responsive/images/events/playoff-premium-hard-rock-stadium-107459_600.png' style='height: 15px;vertical-align: -5%;'> CFP Seed"),
        "rank" = gt::md("<img src='https://playoffpremium.com/wp-content/themes/responsive/images/events/playoff-premium-hard-rock-stadium-107459_600.png' style='height: 15px;vertical-align: -5%;'> CFP Rank"),
    ) %>%
    gt::cols_hide(columns = c(is_conf_champ)) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = "#")
    ) %>%
    gt::tab_header(
        title = gt::md("<img src='https://playoffpremium.com/wp-content/themes/responsive/images/events/playoff-premium-hard-rock-stadium-107459_600.png' style='height: 30px; vertical-align: -30%;'> 2025-26 College Football Playoff - Bubble Teams <img src='https://github.com/saiemgilani/game-on-paper-app/blob/main/frontend/public/assets/img/web-app-manifest-512x512.png?raw=true' style='height: 25px; float: right; vertical-align: middle;'/>"),
        subtitle = "Net Statistics Breakdown - FBS vs FBS Games"
    ) %>%
    gt::tab_footnote(gt::html("🏆 - Conference champion<br/>Data via ESPN, collegefootballdata.com, cfbfastR, and @gameonpaper.com. Table created with help from cbbdata by Andrew Weatherman (@aweatherman.com).<br/>Visit https://gameonpaper.com for more CFB advanced stats."))


save_crop_gt(net_stats_graphic, "25playoff.png")
