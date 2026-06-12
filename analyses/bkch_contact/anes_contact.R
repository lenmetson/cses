# Script to extend Table 1 in Panagopoulos (2016) looking at rates of contact by parties over time 

library(tidyverse)
library(here)
library(ggtext)

pth <- list(data = "data/anes/cdf")

df <- read_csv(here(pth$data, "anes_timeseries_cdf_csv_20260205.csv"))

df <- df |>
    mutate(
        contact = case_when(VCF9030a == 1 ~ TRUE, VCF9030a == 2 ~ FALSE, .default = NA),
        pid_group = case_when(
            VCF0301 == 4           ~ "Pure Independents",
            VCF0301 %in% c(3, 5)  ~ "Leaning Partisans",
            VCF0301 %in% c(2, 6)  ~ "Weak Partisans",
            VCF0301 %in% c(1, 7)  ~ "Strong Partisans",
            .default = NA
        ) |> factor(levels = c("Pure Independents", "Leaning Partisans", "Weak Partisans", "Strong Partisans"))
    )

presidential_years <- c(1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 
                         1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)

table1 <- df |>
    group_by(Year = VCF0004) |>
    summarise(
        All                = mean(contact, na.rm = TRUE),
        `Pure Independents` = mean(contact[pid_group == "Pure Independents"], na.rm = TRUE),
        `Leaning Partisans` = mean(contact[pid_group == "Leaning Partisans"], na.rm = TRUE),
        `Weak Partisans`    = mean(contact[pid_group == "Weak Partisans"],    na.rm = TRUE),
        `Strong Partisans`  = mean(contact[pid_group == "Strong Partisans"],  na.rm = TRUE),
        .groups = "drop"
    ) |>
    filter(!is.na(All)) |> 
    mutate(`Election Type` = ifelse(Year %in% presidential_years, "Presidental", "Midterm"))

se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

table1_long <- df |>
    filter(!is.na(contact)) |>
    mutate(group = "All") |>
    bind_rows(df |> filter(!is.na(contact), !is.na(pid_group)) |> mutate(group = as.character(pid_group))) |>
    group_by(Year = VCF0004, group) |>
    summarise(
        pct_contacted = mean(contact, na.rm = TRUE),
        se            = se(contact),
        .groups = "drop"
    ) |>
    filter(!is.na(pct_contacted)) |>
    mutate(`Election Type` = ifelse(Year %in% presidential_years, "Presidental", "Midterm"))

p_contact <- table1_long |>
    filter(`Election Type` == "Presidental", group == "All") |>
    ggplot(aes(x = Year, y = pct_contacted)) +
    # geom_smooth(
    #     method = "loess", se = TRUE, span = 0.5,
    #     color = "#2C3E50", fill = "#2C3E50", alpha = 0.08, linewidth = 0.8
    # ) +
    geom_line() +
    geom_vline(xintercept = 2012, alpha = 0.3, color = "#acacac", linewidth = 0.4) +
    geom_point(color = "#2C3E50", size = 1.8) +
    # geom_errorbar(
    #     aes(ymin = pct_contacted - 1.96 * se, ymax = pct_contacted + 1.96 * se),
    #     width = 0.2, color = "#2C3E50", linewidth = 0.4
    # )  +
    geom_ribbon(aes(ymin = pct_contacted - 1.96 * se, ymax = pct_contacted + 1.96 * se),
            alpha = 0.12, fill = "#2C3E50", colour = NA) +
    scale_y_continuous(
        limits = c(0, 0.5),
        breaks = seq(0, 0.5, 0.1),
        labels = scales::percent_format(accuracy = 1)
    ) +
    scale_x_continuous(
        breaks = seq(1956, 2020, 8),
        minor_breaks = NULL
    ) +
    labs(
        title    = "**Party Contact in US Presidential Elections**",
        subtitle = "Share of respondents contacted by a major party, 1956–2020",
        x = NULL,
        y = "Share Contacted",
        caption  = "**Data:** ANES Cumulative Data File 1956-2020"
    ) +
    theme_minimal() +
    theme(
        plot.background    = element_rect(fill = "white", color = NA),
        panel.background   = element_rect(fill = "white", color = NA),
        panel.grid.major.y = element_line(color = "#F5F5F5", linewidth = 0.3),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title         = element_markdown(size = 14, hjust = 0, margin = margin(b = 5)),
        plot.subtitle      = element_text(size = 10, hjust = 0, color = "#606060", margin = margin(b = 15)),
        plot.caption       = element_markdown(size = 8, hjust = 0, color = "#808080", margin = margin(t = 15)),
        axis.title.y       = element_text(size = 10, color = "#404040", margin = margin(r = 5)),
        axis.text          = element_text(size = 9, color = "#606060"),
        axis.ticks         = element_blank()
    ) 

ggsave("analyses/bkch_contact/contact-trends.png", p_contact, height = 4, width = 5)


