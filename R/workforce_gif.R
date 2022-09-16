library(tidyverse)
library(lubridate)
library(ggthemes)
library(cowplot)
library(scales)
library(visitalaneysluverds)
library(feather)
library(gganimate)
library(pxweb)
library(mgcv)
library(emmeans)
library(broom)

title <- "#08306b"
subtitle <- "#4A4C45"
caption <- "#023047"

axis_text <- "#4A4C45"
axis_title <- "black"

strip_background <- "#e0e0e0"
background <- "#ffffff"




main_font <- "Lato"
axis_title_font <- NULL
axis_line_col <- "#403d39"



base_size <- 14

theme_set(
    theme(
        text = element_text(family = main_font, size = base_size),
        plot.title = element_text(
            face = "bold",
            colour = title,
            size = base_size * 1.4,
            hjust = 0,
            margin = margin(t = 0, r = 0, b = 5, l = 0)
        ),
        plot.subtitle = element_text(
            colour = subtitle,
            size = base_size * 1,
            hjust = 0,
            margin = margin(t = 0, r = 0, b = 8, l = 5)
        ),
        plot.caption = element_text(
            colour = caption,
            hjust = 1,
            size = 0.7 * base_size
        ),
        plot.caption.position = "panel",
        panel.background = element_rect(fill = background, colour = NA),
        plot.background = element_rect(fill = background, colour = NA),
        panel.grid = element_blank(),
        axis.title = element_text(
            size = base_size ,
            family = axis_title_font,
            color = "black",
            vjust = 1,
            margin = margin(t = 0, r = 0, b = 0, l = 0)
        ),
        axis.text = element_text(
            size = base_size * 0.7,
            colour = axis_text
        ),
        # axis.line = element_line(
        #     colour = "black"
        # ),
        axis.line = element_blank(),
        axis.ticks = element_line(
            size = 0.6,
            colour = axis_line_col
        ),
        strip.background = element_rect(
            fill = strip_background,
            colour = NA,
        ),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    )
)


url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Samfelag/vinnumarkadur/vinnuaflskraargogn/VIN10052.px"

px_vars <- pxweb_get(url)

query_list <- list(  
    "Mánuður" = c("*"),  
    "Aldur" = c("0"),  
    "Rekstrarform" = c("*"),  
    "Kyn" = c("0"),  
    "Bakgrunnur" = c("*"),  
    "Lögheimili" = c("0")
    
)

d <- pxweb_get(url, query = pxweb_query(query_list), verbose = FALSE) |>
    as.data.frame() |>
    as_tibble() |>
    janitor::clean_names() |>
    separate(manudur, into = c("ar", "manudur"), sep = "M") |>
    mutate(dags = str_c(ar, "-", manudur, "-01") |> ymd()) |>
    select(dags, kyn, rekstrarform, starfandi, bakgrunnur) |>
    drop_na() |>
    mutate(tegund = case_when(str_detect(rekstrarform, "^B2") ~ "Opinbert",
                              str_detect(rekstrarform, "^K1") ~ "Opinbert",
                              str_detect(rekstrarform, "^K2") ~ "Opinbert",
                              TRUE ~ "Annad")) |>
    select(-kyn)

model_dat <- d |>  
    filter(rekstrarform != "Alls starfandi", bakgrunnur == "Alls") |>  
    count(dags, tegund, wt = starfandi) |>  
    pivot_wider(names_from = tegund, values_from = n) |>  
    mutate(Heild = Opinbert + Annad) |> 
    filter(year(dags) >= 2008) |>  
    mutate(manudur = month(dags),
           timi = as.numeric(as.factor(dags)),
           hlutf = Opinbert / (Annad + Opinbert)) |> 
    ungroup()

m <- gam(Opinbert ~ s(manudur, bs = "cc") + s(timi), 
         data = model_dat, offset = log(Heild), family = nb(), method = "REML")

plot_dat <- emmeans(m, ~ timi + manudur, at = list(timi = model_dat$timi,
                                                   manudur = 1:12)) |> 
    tidy(type = "response") |> 
    group_by(timi) |> 
    mutate(wt = 1 / std.error,
           wt = wt / sum(wt)) |> 
    summarise(response = sum(response * wt)) |> 
    inner_join(
        model_dat,
        by = "timi"
    )


opinbert_dat <- gam(Opinbert ~ s(manudur, bs = "cc") + s(timi, bs = "ad"), data = model_dat,
                    family = nb(), method = "REML") |> 
    emmeans(~ timi, at = list(timi = unique(model_dat$timi))) |> 
    tidy() |> 
    inner_join(
        model_dat |> 
            distinct(timi, dags)
    ) |> 
    mutate(change = c(0, diff(estimate)),
           perc_change = exp(change))


alm_dat <- gam(Annad ~ s(manudur, bs = "cc") + s(timi, bs = "ad"), data = model_dat,
               family = nb(), method = "REML") |> 
    emmeans(~ timi, at = list(timi = unique(model_dat$timi))) |> 
    tidy() |> 
    inner_join(
        model_dat |> 
            distinct(timi, dags)
    ) |> 
    mutate(change = c(0, diff(estimate)),
           perc_change = exp(change))


heild_dat <- gam(Heild ~ s(manudur, bs = "cc") + s(timi, bs = "ad"), data = model_dat,
                 family = nb(), method = "REML") |> 
    emmeans(~ timi, at = list(timi = unique(model_dat$timi))) |> 
    tidy() |> 
    inner_join(
        model_dat |> 
            distinct(timi, dags)
    ) |> 
    mutate(change = c(0, diff(estimate)),
           perc_change = exp(change))

plot_dat2 <- heild_dat |> 
    mutate(tegund = "Heild") |> 
    bind_rows(
        opinbert_dat |> 
            mutate(tegund = "Opinber")
    )


dates <- c("2009-08-01", "2018-01-01", "2021-01-01")

p1 <- plot_dat |> 
    ggplot(aes(dags, hlutf)) +  
    geom_vline(xintercept = ymd(dates), lty = 2, alpha = 0.5) +
    geom_line(aes(y = hlutf), alpha = 0.1) +
    geom_line(aes(y = response)) +  
    geom_rangeframe(col = "black", size  = 1, sides = "l") +  
    scale_x_date(breaks = seq.Date(from = min(plot_dat$dags), to = ymd("2022-01-01"), by = "year"), 
                 date_labels = "%Y",  
                 guide = guide_axis(n.dodge = 1),  
                 expand = expansion(),
                 limits = c(min(plot_dat$dags), max(plot_dat$dags))) +
    scale_y_continuous(breaks = c(range(plot_dat$hlutf),
                                  0.3, 0.325),
                       labels = label_percent(accuracy = 0.01),
                       limits = c(0.25, 0.37)) +  
    labs(x = NULL, y = NULL,  
         title = "Government staff as a % of total workforce",
         subtitle = "Percentage official staff of total workforce (2008 - 2022)") +  
    theme(plot.margin = margin(t = 5, r = 70, b = 5, l = 5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank())

p2 <- plot_dat2 |> 
    ggplot(aes(dags, perc_change)) +
    geom_vline(xintercept = ymd(dates), lty = 2, alpha = 0.5) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_line(aes(col = tegund)) +
    geom_text(
        data = plot_dat2 |> 
            filter(dags == max(dags)) |> 
            mutate(
                y = case_when(
                    tegund == "Opinber" ~ perc_change + 0.00048,
                    tegund == "Heild" ~ perc_change - 0.00048,
                ),
                label = ifelse(tegund == "Opinber", "Government", "Total")
            ),
        aes(y = y, label = label, col = tegund),
        hjust = 0, nudge_x = 20
    ) +
    geom_rangeframe() +
    scale_x_date(date_breaks = "year", date_labels = "%Y", 
                 expand = expansion()) +
    scale_y_log10(labels = function(x) percent(x - 1),
                  breaks = c(1, range(plot_dat2$perc_change), 1.005, 0.995)) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(t = 10, r = 67, b = 5, l = 6),
          legend.position = "none") +
    labs(x = NULL,
         y = NULL,
         subtitle = "Monthly percentage change in workforce size")


p <- plot_grid(p1, p2, ncol = 1)




ggsave(plot = p, filename = "images/workforce.png",
       width = 8, height = 0.621 * 8, scale = 1.3, bg = "white")
