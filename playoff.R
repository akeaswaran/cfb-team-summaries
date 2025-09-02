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
        end_date = lubridate::as_date(end_date),
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

# rankings_raw = cfbfastR::cfbd_rankings(
#     year = cfbfastR:::most_recent_cfb_season(),
#     week = current_week$week,
#     season_type = current_week$season_type_key
# )
# teams = cfbfastR::cfbd_team_info(
#     year = cfbfastR:::most_recent_cfb_season(),
#     only_fbs = T
# )
# selected_poll = ifelse("Playoff Committee Rankings" %in% rankings_raw$poll, "Playoff Committee Rankings", "AP Top 25")
rankings = data.frame(
    "seed" = seq(1, 12),
    "rank" = c(1,2,9,12,3,4,5,6,7,8,10,16),
    "team_name" = c("Oregon", "Georgia", "Boise State", "Arizona State", "Texas", "Penn State", "Notre Dame", "Ohio State", "Tennessee", "Indiana", "SMU", "Clemson"),
    "conf" = c("B10", "SEC", "MWC", "B12", "SEC", "B10", "ind", "B10", "SEC", "B10", "ACC", "ACC")
)

seed_table = team.adj %>%
    dplyr::left_join(
        rankings, by = c("pos_team" = "team_name")
    ) %>%
    dplyr::filter(!is.na(seed)) %>%
    dplyr::arrange(seed) %>%
    # dplyr::arrange(dplyr::desc(net_adj_epa)) %>%
    dplyr::rename(
        `#` = seed,#dplyr::row_number()
    ) %>%
    dplyr::select(
        `#`,
        pos_team,
        conference = conf,
        rank,
        net_adj_epa,
        net_adj_epa_rank,
        EPAplay_margin,
        EPAplay_margin_rank,
        yardsplay_margin,
        yardsplay_margin_rank,
        success_margin,
        success_margin_rank,
    ) %>%
    cbbplotR::gt_cbb_conferences(conference, conference, vertical_align = "-20%") %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team, vertical_align = "-20%") %>%
    dplyr::mutate(
        # pos_team = dplyr::case_when(
        #     stringr::str_detect(pos_team, "http://a.espncdn.com/i/teamlogos/ncaa/500/61.png") ~ "<img src='https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png' style='height: 25px; vertical-align: -20%;'> georgia",
        #     .default = pos_team
        # ),
        pos_team = dplyr::case_when(
            stringr::str_detect(pos_team, "http://a.espncdn.com/i/teamlogos/ncaa/500/61.png") ~ "<img src='http://a.espncdn.com/i/teamlogos/ncaa/500/61.png' style='height: 25px; vertical-align: -20%;'> georgia",
            .default = pos_team
        ),
        # conference = dplyr::case_when(
        #     stringr::str_detect(pos_team, "Notre Dame") ~ "<img src='https://a.espncdn.com/i/teamlogos/ncaa_conf/500/18.png' style='height: 25px; vertical-align: -20%;'>",
        #     .default = conference
        # )
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
    gtExtras::gt_hulk_col_numeric(net_adj_epa, domain = team.adj$net_adj_epa) %>%
    gtExtras::gt_hulk_col_numeric(EPAplay_margin, domain = team.adj$EPAplay_margin) %>%
    gtExtras::gt_hulk_col_numeric(success_margin, domain = team.adj$success_margin) %>%
    gtExtras::gt_hulk_col_numeric(yardsplay_margin, domain = team.adj$yardsplay_margin) %>%
    gt::cols_label(
        "pos_team" = "Team",
        "conference" = "Conference",
        "net_adj_epa" = "Adj EPA/Play",
        "yardsplay_margin" = "Yds/Play",
        "success_margin" = "Success %",
        "EPAplay_margin" = "EPA/Play",
        "#" = "",
        "rank" = "CFP Rk"
    ) %>%
    gt::cols_label_with(
        columns = dplyr::ends_with("_rank"),
        fn = ~ "Rk"
    ) %>%
    gt::fmt_number(
        columns = dplyr::ends_with("rank"),
        pattern = "#{x}",
        decimals = 0
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = c("#", "pos_team"))
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
            columns = EPAplay_margin_rank,
            rows = EPAplay_margin_rank <= 10
        )
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
            columns = net_adj_epa_rank,
            rows = net_adj_epa_rank <= 10
        )
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
            columns = yardsplay_margin_rank,
            rows = yardsplay_margin_rank <= 10
        )
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
            columns = success_margin_rank,
            rows = success_margin_rank <= 10
        )
    ) %>%
    gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
            columns = rank,
            rows = rank <= 10
        )
    ) %>%
    gt::tab_header(
        title = gt::md(paste0("<img src='https://playoffpremium.com/wp-content/themes/responsive/images/events/private-luxury-nrg-stadium-80833_600.png' style='height: 25px; vertical-align: -20%;'> ", current_week$season, " College Football Playoff - Advanced Stat Profiles")),
        subtitle = gt::html(paste0("Taking a look at the numbers for all 12 seeds. All stats are net margins (Net = Offense - Defense). <b>Bold</b>: top 10 in stat."))
    ) %>%
    gt::tab_footnote(gt::html("Data via ESPN, collegefootballdata.com, cfbfastR, and @gameonpaper.com. Table created with help from cbbdata by Andrew Weatherman (@aweatherman.com).<br/>Visit https://gameonpaper.com for more CFB advanced stats."))

seed_table

seed_table %>%
    save_crop_gt("cfp24_table.png")

