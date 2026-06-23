# Script to extend Table 1 in Panagopoulos (2016) looking at rates of contact by parties over time 

library(tidyverse)
library(here)
library(ggtext)
library(ggrepel)

pth <- list(
    data = "data/anes/cdf", 
    d24 = "data/anes/2024")

df <- read_csv(here(pth$data, "anes_timeseries_cdf_csv_20260205.csv"))
df24 <- read_csv(here(pth$d24, "anes_timeseries_2024_csv_20260519.csv"))


presidential_years <- c(1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 
                         1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020, 2024)





#------------------------------------------
# Overall


df <- df |>
    mutate(
        contact = case_when(VCF9030a == 1 ~ TRUE, VCF9030a == 2 ~ FALSE, .default = NA),
        weight  = VCF0010z
    )



df24 <- df24 |>
    mutate(
        contact = case_when(V242004 == 1 ~ TRUE, V242004 == 2 ~ FALSE, .default = NA),
        weight  = V240107a
    )

table1 <- df |>
    group_by(Year = VCF0004) |>
    summarise(
        pct_contacted = weighted.mean(contact, weight, na.rm = TRUE),
        se = sqrt(weighted.mean((contact - weighted.mean(contact, weight, na.rm = TRUE))^2, weight, na.rm = TRUE) / sum(!is.na(contact))),
        .groups = "drop"
    ) |>
    filter(!is.na(pct_contacted)) |>
    rbind(tibble(
        Year = 2024,
        pct_contacted = weighted.mean(df24$contact,df24$weight, na.rm = TRUE), 
        se = sqrt(weighted.mean((df24$contact - weighted.mean(df24$contact, df24$weight, na.rm = TRUE))^2, df24$weight, na.rm = TRUE) / sum(!is.na(df24$contact)))
    )) |>
    mutate(`Election Type` = ifelse(Year %in% presidential_years, "Presidental", "Midterm"))



p_contact <- table1 |>
    filter(`Election Type` == "Presidental") |>
    ggplot(aes(x = Year, y = pct_contacted)) +
    geom_line() +
    geom_vline(xintercept = 2012, alpha = 0.3, color = "#acacac", linewidth = 0.4) +
    geom_point(color = "#2C3E50", size = 1.8) +
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

p_contact

ggsave("analyses/bkch_contact/contact-trends.png", p_contact, height = 4, width = 5)


#------------------------------------------
# By party 

df <- df |>
    mutate(
        contact    = case_when(VCF9030a == 1 ~ TRUE, VCF9030a == 2 ~ FALSE, .default = NA),
        contact_dem = case_when(VCF9030b == 1 ~ TRUE, VCF9030b == 2 ~ FALSE, .default = NA),
        contact_rep = case_when(VCF9030c == 1 ~ TRUE, VCF9030c == 2 ~ FALSE, .default = NA),
        weight = VCF0010z
    )

df24 <- df24 |>
    mutate(
        contact     = case_when(V242004 == 1 ~ TRUE, V242004 == 2 ~ FALSE, .default = NA),
        contact_dem = case_when(V242007x %in% c(1, 3) ~ TRUE, V242007x %in% c(2, 4, 5) ~ FALSE, .default = NA),
        contact_rep = case_when(V242007x %in% c(2, 3) ~ TRUE, V242007x %in% c(1, 4, 5) ~ FALSE, .default = NA),
        weight = V240107a
    )

table2 <- df |>
    group_by(Year = VCF0004) |>
    summarise(
        Democrat   = weighted.mean(contact_dem, weight, na.rm = TRUE),
        Republican = weighted.mean(contact_rep, weight, na.rm = TRUE),
        .groups = "drop"
    ) |>
    filter(!is.na(Democrat) | !is.na(Republican)) |>
    rbind(tibble(
        Year       = 2024,
        Democrat   = weighted.mean(df24$contact_dem, df24$weight, na.rm = TRUE),
        Republican = weighted.mean(df24$contact_rep, df24$weight, na.rm = TRUE)
    )) |>
    filter(Year %in% presidential_years) |>
    pivot_longer(c(Democrat, Republican), names_to = "Party", values_to = "pct_contacted")

p_contact_party <- table2 |>
    ggplot(aes(x = Year, y = pct_contacted, color = Party)) +
    geom_line() +
    geom_point(size = 1.8) +
    scale_color_manual(values = c(Democrat = "#2166ac", Republican = "#d6604d")) +
    scale_y_continuous(
        # limits = c(0, 0.4),
        # breaks = seq(0, 0.4, 0.1),
        labels = scales::percent_format(accuracy = 1)
    ) +
    scale_x_continuous(breaks = seq(1956, 2024, 8), minor_breaks = NULL) +
    labs(
        title    = "**Party Contact in US Presidential Elections, by Party**",
        subtitle = "Share of respondents contacted by each party, 1956–2024",
        x = NULL, y = "Share Contacted", color = NULL,
        caption  = "**Data:** ANES Cumulative Data File 1956–2020; ANES 2024 Time Series"
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
        axis.ticks         = element_blank(),
        legend.position    = "top",
        legend.text        = element_text(size = 9, color = "#404040")
    )

p_contact_party


#----------------------------------
# Stacked 


df <- df |>
    mutate(
        contact_cat = case_when(
            VCF9030 == 1 ~ "Democrat only",
            VCF9030 == 2 ~ "Republican only",
            VCF9030 == 3 ~ "Both",
            VCF9030 == 7 ~ "Neither",
            .default = NA
        ),
        weight = VCF0010z
    )

df24 <- df24 |>
    mutate(
        contact_cat = case_when(
            V242007x == 1 ~ "Democrat only",
            V242007x == 2 ~ "Republican only",
            V242007x == 3 ~ "Both",
            V242007x == 5 ~ "Neither",
            .default = NA_character_
        ),
        weight = V240107a
    )
cat_levels <- c("Republican only", "Both", "Democrat only")
table3 <- df |>
    group_by(Year = VCF0004) |>
    summarise(
        `Democrat only`   = weighted.mean(contact_cat == "Democrat only",   weight, na.rm = TRUE),
        `Both`            = weighted.mean(contact_cat == "Both",            weight, na.rm = TRUE),
        `Republican only` = weighted.mean(contact_cat == "Republican only", weight, na.rm = TRUE),
        .groups = "drop"
    ) |>
    filter(Year %in% presidential_years, Year < 2024) |>
    rbind(
        df24 |>
            summarise(
                `Democrat only`   = weighted.mean(contact_cat == "Democrat only",   weight, na.rm = TRUE),
                `Both`            = weighted.mean(contact_cat == "Both",            weight, na.rm = TRUE),
                `Republican only` = weighted.mean(contact_cat == "Republican only", weight, na.rm = TRUE)
            ) |>
            mutate(Year = 2024)
    ) |>
    pivot_longer(-Year, names_to = "Category", values_to = "pct") |>
    mutate(Category = factor(Category, levels = cat_levels))

table3_totals <- table3 |>
    group_by(Year) |>
    summarise(total = sum(pct), .groups = "drop")



p_stacked <- table3 |>
    ggplot(aes(x = Year, y = pct, fill = Category)) +
    geom_area(colour = "white", linewidth = 0.5, alpha = 0.85) +

    geom_line(
        data = table3_totals,
        aes(x = Year, y = total),
        inherit.aes = FALSE, 
        colour = "#404040", linewidth = 0.5, alpha = 0.85
    ) +

    geom_point(
        shape = 21,
        data = table3_totals, size = 3,
        aes(x = Year, y = total),
        inherit.aes = FALSE, colour = "#404040", fill = "#404040"
    ) +
    geom_text(
        data = table3_totals,
        aes(x = Year, y = total, label = round(total*100, 0)),
        inherit.aes = FALSE,
        size = 1.5, color = "white",
    ) +

    geom_text(
        data = tibble(
            Category = c("Republican only", "Both", "Democrat only"), 
            label = c("R Only", "  Both", "D Only"), 
            y = c(0.21, 0.15, 0.05)),
        aes(x = 2024.5, y = y, label = label, color = Category),
        inherit.aes = FALSE,
        hjust = -0.5, size = 2.5
    ) +
    scale_fill_manual(values = c(
        "Democrat only"   = "#2166ac",
        "Both"            = "#7B3F9E",
        "Republican only" = "#d6604d"
    )) +
    scale_color_manual(values = c(
        "Democrat only"   = "#2166ac",
        "Both"            = "#7B3F9E",
        "Republican only" = "#d6604d"
    )) +
    scale_y_continuous(
        limits = c(0, 0.55),
        labels = scales::percent_format(accuracy = 1)
    ) +
    scale_x_continuous(
        breaks = seq(1956, 2024, 8),
        minor_breaks = NULL,
        expand = expansion(mult = c(0.02, 0.25))
    ) +
    labs(
        title    = "**Party Contact in US Presidential Elections**",
        subtitle = "Share contacted by each party, 1956–2024",
        x = NULL, y = "Share Contacted",
        caption  = "**Data:** ANES Cumulative Data File 1956–2020; ANES 2024 Time Series"
    ) +
    guides(fill = "none", color = "none") +
    theme_minimal() +
    coord_cartesian(clip = "off") + 
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

p_stacked
ggsave("analyses/bkch_contact/contact-trends_p_stacked.png", p_stacked, height = 5, width = 7)
