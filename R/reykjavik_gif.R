library(tidyverse)
library(lubridate)
library(ggthemes)
library(cowplot)
library(scales)
library(visitalaneysluverds)
library(feather)
library(gganimate)

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
            margin = margin(t = 0, r = 0, b = 5, l = 5)
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
        axis.line = element_line(
            colour = "black"
            ),
        # axis.line = element_blank(),
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

d <- read_feather("files/arsreikningagogn.feather") |> 
    filter(sveitarfelag == "Reykjavíkurborg",
           hluti == "A og B-hluti") |> 
    select(ar, heildarskuldir, skuldir_hlutf_tekjur, skuldir_per_ibui) |> 
    inner_join(
        vnv() |> 
            group_by(ar = year(date)) |> 
            summarise(cpi = mean(cpi)) |> 
            mutate(cpi = cpi / cpi[ar == max(ar)]),
        by = "ar"
    ) |> 
    pivot_longer(c(2:4)) |> 
    mutate(value_cpi = ifelse(name == "skuldir_hlutf_tekjur", value, value / cpi)) |> 
    select(-cpi) |> 
    pivot_longer(c(value, value_cpi), names_to = "state", values_to = "value") |> 
    mutate(
        state = factor(state,
                       levels = c("value", "value_cpi"),
                       labels = c(
                           "Yearly numbers",
                           "Adjusted for CPI"
                       )
        ),
        name = fct_recode(
            name,
            "Total debts" = "heildarskuldir",
            "Debts / Income" = "skuldir_hlutf_tekjur",
            "Debts / Population" = "skuldir_per_ibui"
        )
    )



p <- d |> 
    ggplot(aes(ar, value)) +
    geom_line() +
    scale_x_continuous(breaks = c(2002, 2005, 2010, 2015, 2021)) +
    scale_y_continuous(expand = expansion()) +
    facet_grid(name ~ ., scales = "free_y") +
    coord_cartesian(ylim = c(0, NA)) +
    theme(panel.spacing.y = unit(1, "cm")) +
    labs(x = NULL, y = NULL,
         title = "How have Reykjavík's debts evolved over time?",
         subtitle = "{closest_state}") 


# p

p <- p +
    transition_states(state, wrap = TRUE) +
    ease_aes("cubic-in-out")

# p

p_vid <- animate(p, width = 12, height = 0.621 * 12, unit = "in", res = 150, fps = 25, duration = 5, end_pause = 0)


# p_vid

anim_save(filename = "images/reykjavik.gif", animation = p_vid)
