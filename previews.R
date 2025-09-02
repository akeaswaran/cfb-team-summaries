library(tidyverse)
library(dplyr)
library(cbbdata)
library(cbbplotR)
library(cfbplotR)
library(bskyr)
library(webshot2)
library(gt)
library(gtExtras)
# devtools::install_github("sportsdataverse/cfbfastR")
library(cfbfastR)

# all games involving ranked teams + GT + FSU
current_schedule = cfbfastR::espn_cfb_calendar(groups = "FBS")
current_week = current_schedule %>%
    dplyr::mutate(
        end_date = lubridate::as_datetime(end_date, format = "%FT%H:%SZ"),
        end_date = as.Date(end_date),
        start_date = lubridate::as_datetime(start_date, format = "%FT%H:%SZ"),
        start_date = as.Date(start_date),
    ) %>%
    dplyr::filter(
        Sys.Date() <= end_date
        & Sys.Date() >= start_date
    ) %>%
    # dplyr::last() %>%
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

games = purrr::map(1:nrow(current_week), function(i) {
    cur = current_week[i, ]
    cfbfastR::espn_cfb_schedule(
        year = as.integer(cur$season),
        # week = cur$week,
        season_type = cur$season_type_key
    )
}) %>% purrr::list_rbind()

if (!("notes" %in% colnames(games))) {
    games_cfbd = purrr::map(1:nrow(current_week), function(i) {
        cur = current_week[i, ]
        cfbfastR::cfbd_game_info(
            year = as.integer(cur$season),
            # week = cur$week,
            season_type = cur$season_type_key
        )
    }) %>%
        purrr::list_rbind() %>%
        dplyr::mutate(game_id = as.character(game_id))

    games = games %>%
        dplyr::left_join(games_cfbd %>% dplyr::select(game_id, notes), by = "game_id")
}

rankings_raw = cfbfastR::cfbd_rankings(
    year = as.integer(dplyr::first(current_week)$season),
    # week = dplyr::first(current_week)$week,
    # season_type = dplyr::first(current_week)$season_type_key
)

teams = cfbfastR::cfbd_team_info(
    year = 2025,
    only_fbs = T
)
selected_poll = ifelse("Playoff Committee Rankings" %in% rankings_raw$poll, "Playoff Committee Rankings", "AP Top 25")

rankings = rankings_raw %>%
    dplyr::filter(
        poll == selected_poll
        & week == max(week)
    ) %>%
    dplyr::left_join(teams %>% dplyr::select(team_id, school), by = c("school")) %>%
    dplyr::mutate(
        team_id = as.character(team_id)
    )

target_teams = c(rankings$team_id, 59, 52)



selected_games = games %>%
    dplyr::filter(
        # game is today
        lubridate::as_date(game_date_time) == Sys.Date()
        # teams are FBS
        & (as.character(home_team_id) %in% as.character(teams$team_id))
        & (as.character(away_team_id) %in% as.character(teams$team_id))
        & (
            (
                # if saturday in Aug-Dec, only post ranked games
                base::weekdays(Sys.Date()) == "Saturday"
                & (lubridate::month(Sys.Date()) >= 8 & lubridate::month(Sys.Date()) < 12)
                &  (as.character(home_team_id) %in% as.character(target_teams)
                    | as.character(away_team_id) %in% as.character(target_teams))
            )
            | (
                # if any non-saturday, post any games
                (base::weekdays(Sys.Date()) != "Saturday")
                | (base::weekdays(Sys.Date()) == "Saturday"
                    & (lubridate::month(Sys.Date()) == 12 | lubridate::month(Sys.Date()) == 1))
            )
        )
    ) %>%
    dplyr::left_join(rankings %>% dplyr::select(home_team_rank = rank, team_id), by = c("home_team_id" = "team_id")) %>%
    dplyr::left_join(rankings %>% dplyr::select(away_team_rank = rank, team_id), by = c("away_team_id" = "team_id")) %>%
    dplyr::group_by(game_id) %>%
    dplyr::slice_min(order_by = dplyr::row_number(), n = 1, with_ties = F) %>%
    dplyr::ungroup()
# recreate Sumer matchup table from GoP page with CSV data, include team hashtags, link to matchup page
clean_team_data = function(df) {
    df %>%
        dplyr::mutate(
            dplyr::across(dplyr::where(is.numeric) & !dplyr::ends_with("_id"), ~ as.double(.x))
        ) %>%
        tidyr::pivot_longer(cols = -c(
            dplyr::where(is.character),
            season,
            dplyr::contains("alt_"),
            division,
            fbs_class,
            venue_id,
            zip,
            latitude,
            longitude,
            grass,
            dome,
            elevation,
            capacity,
            year_constructed
        )) %>%
        dplyr::mutate(
            logo = dplyr::if_else(pos_team == "Georgia", "https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png", logo),
            pos_team = dplyr::if_else(pos_team == "Georgia", "georgia", pos_team),
            side = dplyr::case_when(
                grepl("_off", name) | grepl("off_", name) ~ "off",
                grepl("_def", name) | grepl("def_", name) ~ "def",
                grepl("_margin", name) | grepl("net_", name) ~ "margin",
            ),
            is_rank = grepl("_rank", name),
            name = stringr::str_replace(name, "_rank", ""),
            name = stringr::str_replace(name, "_off", ""),
            name = stringr::str_replace(name, "_def", ""),
            name = stringr::str_replace(name, "_margin", ""),
        )
}

generate_matchup_df = function(home_id, away_id) {
    home_data = read.csv(paste0("./data/", 2024, "/", home_id, "/overall.csv")) %>%
        clean_team_data()

    away_data = read.csv(paste0("./data/", 2024, "/", away_id, "/overall.csv")) %>%
        clean_team_data()

    away_off_home_df = away_data %>%
        dplyr::filter(side == "off") %>%
        dplyr::select(
            pos_team,
            logo,
            name,
            value,
            is_rank,
        ) %>%
        dplyr::left_join(
            home_data %>%
                dplyr::filter(side == "def") %>%
                dplyr::select(
                    pos_team,
                    logo,
                    name,
                    value,
                    is_rank,
                ),
            by = c("name", "is_rank")
        ) %>%
        dplyr::rename(
            "away_team" = "pos_team.x",
            "away_logo_url" = "logo.x",
            "away_off_value" = "value.x",
            "home_team" = "pos_team.y",
            "home_logo_url" = "logo.y",
            "home_def_value" = "value.y",
        )

    away_def_home_off = away_data %>%
        dplyr::filter(side == "def") %>%
        dplyr::select(
            pos_team,
            logo,
            name,
            value,
            is_rank,
        ) %>%
        dplyr::left_join(
            home_data %>%
                dplyr::filter(side == "off") %>%
                dplyr::select(
                    pos_team,
                    logo,
                    name,
                    value,
                    is_rank,
                ),
            by = c("name", "is_rank")
        ) %>%
        dplyr::rename(
            "away_team" = "pos_team.x",
            "away_logo_url" = "logo.x",
            "away_def_value" = "value.x",
            "home_team" = "pos_team.y",
            "home_logo_url" = "logo.y",
            "home_off_value" = "value.y",
        )

    matchup_df = away_off_home_df %>%
        dplyr::left_join(away_def_home_off, by = c("away_team", "away_logo_url", "home_team", "home_logo_url", "name", "is_rank"))

    values = matchup_df %>%
        dplyr::filter(!is_rank) %>%
        dplyr::select(-is_rank)

    ranks = matchup_df %>%
        dplyr::filter(is_rank) %>%
        dplyr::select(-is_rank) %>%
        dplyr::rename_with(
            .cols = dplyr::ends_with("_value"),
            .fn = function(x) stringr::str_replace(x, "_value", "_rank")
        )

    final = values %>%
        dplyr::left_join(ranks, by = c("name", "away_team", "away_logo_url", "home_team", "home_logo_url")) %>%
        dplyr::relocate(dplyr::starts_with("away_"))

    return(final)
}

gt_hulk_col_numeric_target <- function(gt_object, columns = NULL, target_columns = NULL, domain = NULL, ..., trim = FALSE) {
    stopifnot("Input must be a gt table" = "gt_tbl" %in% class(gt_object))

    pal_hex <- c(
        "#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
        "#d9f0d3", "#7fbf7b", "#1b7837"
    )

    if (isTRUE(trim)) pal_hex <- pal_hex[2:6]

    hulk_pal <- function(x) {
        scales::col_numeric(
            pal_hex,
            domain = domain,
            ...
        )(x)
    }

    gt::data_color(
        gt_object,
        columns = {{ columns }},
        target_columns = {{ target_columns }},
        fn = hulk_pal
    )
}

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

retrieve_skeet_id = function(resp) {
    resp_id = resp$uri[[1]]
    resp_id = stringr::str_extract(resp_id, "post/(\\w+)$", group = 1)
    return(resp_id)
}

generate_matchup_image = function(game_id, home_id, away_id) {
    matchup_df = generate_matchup_df(home_id, away_id) %>%
        dplyr::filter(
            name %in% c(
                # "adj_epa",
                "EPAplay_pass",
                "EPAplay_rush",
                "available_yards_pct",
                "start_position",
                "early_down_EPA",
                "late_down_success",
                "third_down_distance"
            )
        ) %>%
        dplyr::mutate(
            dplyr::across(dplyr::ends_with("_rank"), ~ dplyr::if_else(grepl("\\.5", .x), paste0("T-#", floor(.x)), paste0("#", .x)), .names = "{.col}_text"),
            away_off_value_text = dplyr::case_when(
                name == "start_position" & away_off_value < 50 ~ paste0("Opp ", round(away_off_value)),
                name == "start_position" & away_off_value == 50 ~ "50",
                name == "start_position" & away_off_value > 50 ~ paste0("Own ", round(100 - away_off_value)),

                name %in% c("available_yards_pct", "late_down_success") ~ paste0(round(100 * away_off_value, 1), "%"),

                .default = paste0(round(away_off_value, 2))
            ),

            away_def_value_text = dplyr::case_when(
                name == "start_position" & away_def_value < 50 ~ paste0("Opp ", round(away_def_value)),
                name == "start_position" & away_def_value == 50 ~ "50",
                name == "start_position" & away_def_value > 50 ~ paste0("Own ", round(100 - away_def_value)),

                name %in% c("available_yards_pct", "late_down_success") ~ paste0(round(100 * away_def_value, 1), "%"),

                .default = paste0(round(away_def_value, 2))
            ),

            home_def_value_text = dplyr::case_when(
                name == "start_position" & home_def_value < 50 ~ paste0("Opp ", round(home_def_value)),
                name == "start_position" & home_def_value == 50 ~ "50",
                name == "start_position" & home_def_value > 50 ~ paste0("Own ", round(100 - home_def_value)),

                name %in% c("available_yards_pct", "late_down_success") ~ paste0(round(100 * home_def_value, 1), "%"),

                .default = paste0(round(home_def_value, 2))
            ),

            home_off_value_text = dplyr::case_when(
                name == "start_position" & home_off_value < 50 ~ paste0("Opp ", round(home_off_value)),
                name == "start_position" & home_off_value == 50 ~ "50",
                name == "start_position" & home_off_value > 50 ~ paste0("Own ", round(100 - home_off_value)),

                name %in% c("available_yards_pct", "late_down_success") ~ paste0(round(100 * home_off_value, 1), "%"),

                .default = paste0(round(home_off_value, 2))
            ),
            # dplyr::across(dplyr::contains("_logo_") ~ dplyr::if_else(stringr::str_detect(.x, "61.png"), "https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png", .x),
            dplyr::across(dplyr::contains("_logo_"), ~ paste0('<img style="margin-bottom: 0; vertical-align: middle;" width="35px" src="',.x,'"/>'))
        ) %>%
        dplyr::relocate(dplyr::starts_with("away_")) %>%
        dplyr::mutate(
            title = dplyr::case_when(
                name == "EPAplay_pass" ~ "EPA/Pass",
                name == "EPAplay_rush" ~ "EPA/Rush",
                name == "available_yards_pct" ~ "Available Yards %",
                name == "start_position" ~ "Starting Field Position",
                name == "early_down_EPA" ~ "Early Downs EPA/Play",
                name == "late_down_success" ~ "3rd/4th Down Success",
                name == "third_down_distance" ~ "Avg 3rd Down Distance",
            ),
            title = paste0("   ", title, "   "),
            name = factor(name, levels = c(
                "EPAplay_pass",
                "EPAplay_rush",
                "available_yards_pct",
                "start_position",
                "early_down_EPA",
                "late_down_success",
                "third_down_distance"
            ))
        )

    matchup_df %>%
        dplyr::arrange(name) %>%
        dplyr::select(
            away_off_rank_text,
            away_off_rank,
            away_off_value_text,
            title,
            home_def_value_text,
            home_def_rank,
            home_def_rank_text,
        ) %>%
        gt::gt(id = "tbl1") %>%
        gtExtras::gt_theme_538() %>%
        gt::cols_align(align = "center") %>%
        gt_hulk_col_numeric_target(
            columns = home_def_rank,
            target_columns = home_def_rank_text,
            domain = 1:134,
            reverse = T
        ) %>%
        gt_hulk_col_numeric_target(
            columns = away_off_rank,
            target_columns = away_off_rank_text,
            domain = 1:134,
            reverse = T
        ) %>%
        gt::tab_style(
            style = gt::cell_text(weight = 600),
            locations = gt::cells_body(
                columns = dplyr::ends_with("_rank_text")
            )
        ) %>%
        gt::cols_hide(
            columns = dplyr::ends_with("_rank")
        ) %>%
        gt::cols_label_with(
            fn = function(x) " "
        ) %>%
        gt::tab_header(
            title = gt::html(paste0('<h3 style="margin-bottom: 0; vertical-align: middle;">', matchup_df$away_logo_url[[1]], " Offense vs ", matchup_df$home_logo_url[[1]], " Defense", "</h3>"))
        ) %>%
        gt::opt_align_table_header(align = "center") %>%
        gt::tab_source_note(gt::md("Table created with help from cbbdata by Andrew Weatherman (@aweatherman.com).")) %>%
        gt::opt_css(
        "
        #tbl1 .gt_sourcenote {
            line-height: 1.25em;
            opacity: 0;
        }
        #tbl1 {
            margin-bottom: 0;
        }
        "
        ) %>%
        save_crop_gt(paste0(game_id[1], "-1.png"))

    matchup_df %>%
        dplyr::arrange(name) %>%
        dplyr::select(
            away_def_rank_text,
            away_def_rank,
            away_def_value_text,
            title,
            home_off_value_text,
            home_off_rank,
            home_off_rank_text,
        ) %>%
        gt::gt() %>%
        gtExtras::gt_theme_538() %>%
        gt::cols_align(align = "center") %>%
        gt_hulk_col_numeric_target(
            columns = home_off_rank,
            target_columns = home_off_rank_text,
            domain = 1:134,
            reverse = T
        ) %>%
        gt_hulk_col_numeric_target(
            columns = away_def_rank,
            target_columns = away_def_rank_text,
            domain = 1:134,
            reverse = T
        ) %>%
        gt::tab_style(
            style = gt::cell_text(weight = 600),
            locations = gt::cells_body(
                columns = dplyr::ends_with("_rank_text")
            )
        ) %>%
        gt::cols_hide(
            columns = dplyr::ends_with("_rank")
        ) %>%
        gt::cols_label_with(
            fn = function(x) " "
        ) %>%
        gt::tab_header(
            title = gt::html(paste0('<h3 style="margin-bottom: 0; vertical-align: middle;">', matchup_df$away_logo_url[[1]], " Defense vs ", matchup_df$home_logo_url[[1]], " Offense", "</h3>"))
        ) %>%
        gt::opt_align_table_header(align = "center") %>%
        gt::tab_source_note(gt::md("Data via ESPN, collegefootballdata.com, cfbfastR, and @gameonpaper.com.<br/>Table created with help from cbbdata by Andrew Weatherman (@aweatherman.com).<br/>Visit https://gameonpaper.com for more CFB advanced stats.")) %>%
        gt::opt_css(
        "
        .gt_sourcenote {
            line-height: 1.25em;
        }
        "
        ) %>%
        save_crop_gt(paste0(game_id[1], "-2.png"))

    file_path = paste0("./figures/", game_id[1], ".png")
    images = c(
        paste0("./figures/", game_id[1], "-1.png"),
        paste0("./figures/", game_id[1], "-2.png")
    )
    magick::image_read(images) %>%
    magick::image_append(stack = T) %>%
    magick::image_write(file_path)

    file.remove(images)


    alt_text = matchup_df %>%
        dplyr::arrange(name) %>%
        dplyr::select(
            away_off_rank_text,
            # away_off_rank,
            away_off_value_text,
            title,
            home_def_value_text,
            # home_def_rank,
            home_def_rank_text,
        ) %>%
        dplyr::mutate(
            home_text = paste0(home_def_value_text, " (", home_def_rank_text,")"),
            away_text = paste0(away_off_value_text, " (", away_off_rank_text,")"),
            title = stringr::str_trim(title)
        ) %>%
        dplyr::select(
            away_text,
            metric = title,
            home_text
        ) %>%
        dplyr::add_row(
            data.frame(
                "away_text" = "-----",
                "metric" = "-----",
                "home_text" = "-----"
            ),
            .before = 0
        ) %>%
        dplyr::rename_with(~matchup_df$away_team[[1]], away_text) %>%
        dplyr::rename_with(~matchup_df$home_team[[1]], home_text) %>%
        readr::format_tsv()

    return(
        list(
            "file_path" = file_path,
            "alt_text" = alt_text
        )
    )
}

fire_skeet = function(row, reply = NULL, live_run = FALSE) {
    print(paste0("generating skeet for game ", row$game_id, " - reply? ", !is.null(reply), ", live_run: ", live_run))

    print(paste0("generating skeet image data..."))
    img_data = generate_matchup_image(row$game_id[1], row$home_team_id[1], row$away_team_id[1])

    print(paste0("generating skeet content..."))
    home_team_title = dplyr::if_else(is.na(row$home_team_rank), row$home_team_location, paste0("#", row$home_team_rank, " ", row$home_team_location))
    away_team_title = dplyr::if_else(is.na(row$away_team_rank), row$away_team_location, paste0("#", row$away_team_rank, " ", row$away_team_location))
    game_title = paste0(away_team_title, " vs ", home_team_title)
    if (!is.na(row$notes)) {
        game_title = paste0(row$notes,": ", game_title)
    }

    time_string = format(row$game_date_time, "%I:%M %p %Z")
    network = dplyr::if_else(is.na(row$broadcast), "", row$broadcast)

    skeet_title_parts = c(
        game_title,
        time_string,
        network
    )
    skeet_title_parts = skeet_title_parts[nzchar(skeet_title_parts)]
    skeet_title = paste0(skeet_title_parts, collapse = ", ")

    skeet_content_parts = c(
        paste0("🤖🏈: ", skeet_title, "\n"),
        paste0("Game Preview: https://gameonpaper.com/cfb/game/", row$game_id),
        paste0(away_team_title, ": ", "https://gameonpaper.com/cfb/team/", row$away_team_id),
        paste0(home_team_title, ": ", "https://gameonpaper.com/cfb/team/", row$home_team_id)
    )

    skeet_content = paste0(skeet_content_parts, collapse = "\n")
    printing_verb = dplyr::if_else(live_run, "Sending", "Previewing")
    print(
        paste0(
            c(
                paste0(printing_verb, " skeet:"),
                skeet_content,
                paste0("With image at filepath: ", img_data$file_path),
                "Alt Text:",
                img_data$alt_text
            ),
            collapse = "\n"
        )
    )

    args = list(
        "text" = skeet_content,
        "images" = c(img_data$file_path),
        "images_alt" = c(img_data$alt_text),
        "max_tries" = 1
    )

    if (!is.null(reply)) {
        args[["reply"]] = paste0("https://bsky.app/profile/gameonpaper.com/post/", retrieve_skeet_id(reply))
    }

    if (live_run) {
        return(do.call(bskyr::bs_post, args))
    } else {
        return(data.frame(
            uri = c("post/test")
        ))
    }
}

# fire off at 0.5 min intervals
is_live_run = F #Sys.getenv("SKEET_ENVIRONMENT") == "prod"
reply = NULL
delay = 5 #dplyr::if_else(is_live_run, 30, 5)
if (nrow(selected_games) > 0) {
    print(paste0("Skeeting for relevant FBS games: ", nrow(selected_games), " - live_run: ", is_live_run))
    for (i in 1:(nrow(selected_games))) {
        g = selected_games[i, ]
        reply <- fire_skeet(g, NULL, is_live_run) # disable threading
        print(paste0("Waiting ", delay, " seconds for skeeting again..."))
        Sys.sleep(delay)
    }
} else {
    print("No relevant FBS games, bailing out")
}
