library(cowplot)
library(tidyverse)
library(scales)
library(pxweb)
library(ggthemes)
library(kableExtra)
library(gganimate)
library(lubridate)
library(plotly)
Sys.setlocale(locale = "is_IS.UTF-8")

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

d <- pxweb_get(
    url ="https://px.hagstofa.is:443/pxis/api/v1/is/Efnahagur/visitolur/1_vnv/2_undirvisitolur/VIS01301.px", 
    query = list(
        "Mánuður" = c("*"),
        "Liður"  = c("effect", "change_M", "breakdown"),
        "Undirvísitala" = c("*")
    ),
    verbose = FALSE
) |> 
    as.data.frame() |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    separate(manudur, into = c("ar", "manudur"), sep = "M", convert = T) |> 
    mutate(manudur = str_pad(manudur, width = 2, side = "left", pad = "0"),
           date = str_c(ar, "-", manudur, "-01") |> ymd(),
           visitala_neysluverds = visitala_neysluverds / 100) |> 
    select(date, undirvisitala, lidur, value = visitala_neysluverds) |> 
    filter(str_detect(undirvisitala, "^[0-9]{2} |Vísitala neysluverðs")) |>
    arrange(date) |> 
    mutate(undirvisitala = str_replace(undirvisitala, "^[0-9]{2} ", ""),
           lidur = fct_recode(lidur,
                              "vaegi" = "Vægi, %",
                              "breyting" = "Mánaðarbreyting, %",
                              "ahrif" = "Áhrif á vísitölu, %")) |> 
    pivot_wider(names_from = lidur, values_from = value)


plot_dat1 <- d |> 
    filter(undirvisitala != "Vísitala neysluverðs",
           date %in% tail(unique(date), 12)) |> 
    group_by(undirvisitala) |> 
    summarise(vaegi = sum(vaegi),
              breyting = exp(sum(log(breyting + 1))) - 1,
              ahrif = exp(sum(log(ahrif + 1))) - 1,
              .groups = "drop") |> 
    mutate(vaegi = vaegi / sum(vaegi)) |> 
    mutate(fill = fct_reorder(undirvisitala, ahrif),
           undirvisitala = str_wrap(undirvisitala, width = 20),
           undirvisitala = fct_reorder(undirvisitala, breyting)) |> 
    arrange(desc(undirvisitala)) |> 
    mutate(vaegi = cumsum(vaegi),
           x_min = lag(vaegi, default = 0),
           x_max = vaegi,
           y_min = 0,
           y_max = breyting,
           area = (y_max - y_min) * (x_max - x_min))

plot_dat1 |> 
    ggplot(aes(xmin = x_min, xmax = x_max,
               ymin = y_min, ymax = y_max)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_rect(aes(fill = fill)) +
    geom_rangeframe(aes(y = y_max)) +
    scale_x_continuous(labels = label_percent(),
                       expand = expansion()) +
    scale_y_continuous(labels = label_percent(),
                       expand = expansion()) +
    scale_fill_brewer(type = "qual", palette = "Paired") +
    theme_tufte() +
    theme(legend.position = "none") +
    labs(x = NULL,
         y = NULL)


plot_dat2 <- plot_dat1 |> 
    mutate(vaegi = 1) |> 
    mutate(vaegi = cumsum(vaegi),
           x_min = lag(vaegi, default = 0),
           x_max = vaegi,
           y_min = 0,
           y_max = ahrif,
           area = (y_max - y_min) * (x_max - x_min))

plot_dat2 |> 
    ggplot(aes(xmin = x_min, xmax = x_max,
               ymin = y_min, ymax = y_max)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_rect(aes(fill = fill)) +
    geom_rangeframe(aes(y = y_max)) +
    scale_x_continuous(labels = label_percent(),
                       expand = expansion()) +
    scale_y_continuous(labels = label_percent(),
                       expand = expansion()) +
    scale_fill_brewer(type = "qual", palette = "Paired") +
    theme_tufte() +
    theme(legend.position = "none") +
    labs(x = NULL,
         y = NULL)



plot_dat3 <- plot_dat2 |> 
    mutate(vaegi = 1/n()) |> 
    mutate(ahrif = c(cumsum(ahrif[undirvisitala != "Póstur og sími"]), 0) + c(rep(0, n() - 1), ahrif[undirvisitala == "Póstur og sími"]),
           x_min = 0,
           x_max = 1,
           y_min = lag(ahrif, default = 0),
           y_max = ahrif,
           y_min = ifelse(undirvisitala == "Póstur og sími", y_max, y_min),
           y_max = ifelse(undirvisitala == "Póstur og sími", 0, y_max),
           area = (y_max - y_min) * (x_max - x_min))


plot_dat3 |> 
    ggplot(aes(xmin = x_min, xmax = x_max,
               ymin = y_min, ymax = y_max)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_rect(aes(fill = fill)) +
    geom_rangeframe(aes(y = y_max)) +
    scale_x_continuous(labels = label_percent(),
                       expand = expansion()) +
    scale_y_continuous(labels = label_percent(),
                       expand = expansion()) +
    scale_fill_brewer(type = "qual", palette = "Paired") +
    theme_tufte() +
    theme(legend.position = "none") +
    labs(x = NULL,
         y = NULL)

text <- tibble(
    state = 1:3,
    undirvisitala = "Póstur og sími",
    text = c(
        "Height is % price increase and width is % of expenses for average person\n(Sum of areas is change (%) in CPI)",
        "Stretch out all rectangles while keeping area fixed\n(Now rectangle width = 1, so area = height)",
        "Stack the rectangles vertically\n(There's our inflation!)"
    )
)

plot_dat <- plot_dat1 |> 
    mutate(state = 1) |> 
    bind_rows(
        plot_dat2 |> 
            mutate(state = 2)
    ) |> 
    bind_rows(
        plot_dat3 |> 
            mutate(state = 3)
    ) |> 
    left_join(
        text,
        by = c("state", "undirvisitala")
    ) |> 
    mutate(
        state = factor(
            state, 
            labels = c(
                "Yearly % price increase by % of average person's expenses",
                "Effect on yearly inflation (equal weights)",
                "Total yearly inflation"
            )
        )
    )

p <- plot_dat |> 
    ggplot(aes(xmin = x_min, xmax = x_max,
               ymin = y_min, ymax = y_max,
               group = undirvisitala)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_rect(aes(fill = fill)) +
    geom_text(data = plot_dat |> drop_na(),
              aes(x = 0.05, y = -0.02, label = text), hjust = 0) +
    scale_x_continuous(labels = label_percent(),
                       expand = expansion()) +
    scale_y_continuous(labels = label_percent(),
                       expand = expansion()) +
    scale_fill_brewer(type = "qual", palette = "Paired") +
    # theme_half_open() +
    theme(legend.position = "none",
          plot.margin = margin(t = 5, r = 20, b = 5, l = 5)) +
    labs(x = "Weight (%)",
         y = "Price Increase / Effect on CPI (%)",
         title = "Inflation as an area",
         subtitle = "{closest_state}") +
    transition_states(state, wrap = T) +
    view_follow(fixed_y = T) +
    ease_aes("cubic-in-out")


# p


p_vid <- animate(p, width = 8, height = 0.621 * 8, unit = "in", res = 150, fps = 25, duration = 12, end_pause = 10)


# p_vid

anim_save(filename = "images/cpi.gif", animation = p_vid)
