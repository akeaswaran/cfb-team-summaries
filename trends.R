library(tidyverse)
sysfonts::font_add_google("Chivo")
showtext::showtext_auto()
seasons <- 2014:2025

percentile_list = purrr::map(seasons, ~ read.csv(paste0("./data/", .x, "/percentiles.csv")) %>% dplyr::mutate(season = .x))
percentiles = purrr::list_rbind(percentile_list)


percentiles %>%
    dplyr::filter(pctile %in% c(0.25, 0.50, 0.75)) %>%
    dplyr::select(-yardsdropback, -GEI) %>%
    tidyr::pivot_longer(cols = colnames(.)[which(!grepl("season|pctile", colnames(.)))]) %>%
    dplyr::mutate(
        value = dplyr::case_when(
            name %in% c("success", "explosive", "third_down_success", "red_zone_success", "havoc", "play_stuffed") ~ (100 * value),
            .default = value
        )
    ) %>%
    tidyr::pivot_wider(id_cols = c(season, name), names_from = c(pctile), values_from = c(value)) %>%
    #View()
    dplyr::mutate(
        name = dplyr::case_when(
            name == "EPAdropback" ~ "EPA/dropback",
            name == "EPAplay" ~ "EPA/play",
            name == "EPArush" ~ "EPA/rush",
            name == "explosive" ~ "Explosive %",
            name == "havoc" ~ "Havoc %",
            name == "play_stuffed" ~ "Stuff %",
            name == "red_zone_success" ~ "RZ Success %",
            name == "success" ~ "Success %",
            name == "third_down_success" ~ "3rd Down Success %",
            name == "yardsplay" ~ "Yards/play"
        ),
        name = factor(
            name,
            levels = c(
                "Yards/play", "EPA/play", "EPA/dropback", "EPA/rush",
                "Success %", "3rd Down Success %", "RZ Success %", "Explosive %",
                "Havoc %", "Stuff %"
            )
        )
    ) %>%
    ggplot2::ggplot() +

    ggplot2::geom_point(ggplot2::aes(x = season, y = `0.25`, color = "25th")) +
    ggplot2::geom_line(ggplot2::aes(x = season, y = `0.25`, color = "25th")) +

    ggplot2::geom_point(ggplot2::aes(x = season, y = `0.5`, color = "50th")) +
    ggplot2::geom_line(ggplot2::aes(x = season, y = `0.5`, color = "50th")) +


    ggplot2::geom_point(ggplot2::aes(x = season, y = `0.75`, color = "75th")) +
    ggplot2::geom_line(ggplot2::aes(x = season, y = `0.75`, color = "75th")) +

    ggplot2::facet_wrap(~ name, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(family = "Chivo"),
        strip.text = ggplot2::element_text(family = "Chivo", face = "bold"),
        legend.title = ggplot2::element_text(family = "Chivo", face = "bold"),
        legend.text = ggplot2::element_text(family = "Chivo"),
        legend.position = "inside",
        legend.position.inside = c(0.55, 0.15),

        plot.title = ggplot2::element_text(family = "Chivo", size = 15, face = "bold"),
        plot.subtitle = ggplot2::element_text(family = "Chivo", size = 12),
        plot.caption = ggplot2::element_text(family = "Chivo"),
    ) +
    ggplot2::scale_color_manual(
        name='Percentile',
        breaks=c("25th", "50th", "75th"),
        values=c("25th"='purple', "50th"='black', "75th"='forestgreen')
    ) +
    ggplot2::labs(
        title = "The Binion Box Score: 2014-2025",
        subtitle = "How Binion Box Score metric IQRs have evolved through the CollegeFootballData era.",
        caption = "Data from collegefootballdata.com, ESPN, and cfbfastR. Chart created by @gameonpaper.com."
    ) +
    ggplot2::guides(
        legend = ggplot2::guide_legend(position = "bottom")
    )
# 1200x800
