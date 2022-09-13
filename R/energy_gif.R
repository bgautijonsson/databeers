library(cowplot)
library(tidyverse)
library(scales)
library(pxweb)
library(ggthemes)
library(kableExtra)
library(gganimate)
library(lubridate)
library(geomtextpath)
library(ggtext)
library(here)
library(readxl)
library(janitor)


d <- read_csv("https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv") |> 
    select(country, year, 
           contains("gdp"), contains("pop"), 
           fossil_share_energy, fossil_share_elec, fossil_energy_per_capita,
           fossil_fuel_consumption,
           renewables_share_elec, renewables_share_energy, 
           energy_per_capita, per_capita_electricity,
           oil_prod_per_capita,
           greenhouse_gas_emissions,
           carbon_intensity_elec,
           low_carbon_energy_per_capita) |> 
    mutate(gdp_per_capita = gdp / population,
           greenhouse_per_capita = greenhouse_gas_emissions / population * 1e6 * 1e5,
           gdp_per_greenhouse = gdp / greenhouse_gas_emissions,
           high_carbon_energy_per_capita = energy_per_capita - low_carbon_energy_per_capita,
           fossil_fuel_consumption = fossil_fuel_consumption * 1e9)


start <- d |> filter(year >= 1980, energy_per_capita > 0) |> pull(energy_per_capita) |> min(na.rm = T)
end <- d |> filter(year >= 1980, energy_per_capita > 0) |> pull(energy_per_capita) |> max(na.rm = T)

min_gdp <- d |> filter(year >= 1980) |> pull(gdp_per_capita) |> min(na.rm = T)
max_gdp <- d |> filter(year >= 1980) |> pull(gdp_per_capita) |> max(na.rm = T)

plot_dat <- crossing(energy_per_capita = seq(log(start), log(end), length = 100) |> exp(),
                     ratio = c(0.25, 0.5, 1, 2, 4)) |> 
    mutate(gdp_per_capita = ratio * energy_per_capita) |> 
    filter(gdp_per_capita < max_gdp * 1.1,
           gdp_per_capita > min_gdp)

add_preds <- function(data, ...) {
    m <- lm(log(gdp_per_capita) ~ log(energy_per_capita), data = data)
    data$pred <- exp(predict(m))
    
    data
}


p <- d |> 
    select(gdp_per_capita, energy_per_capita, country, year) |> 
    drop_na() |> 
    filter(year >= 1980, country != "World") |> 
    group_by(country) |>
    ungroup() |> 
    group_by(year) |> 
    group_modify(add_preds) |> 
    ungroup() |> 
    ggplot(aes(energy_per_capita, gdp_per_capita,
               colour = (country == "Iceland"),
               size = (country == "Iceland"),
               alpha = (country == "Iceland"))) +
    geom_line(
        data = plot_dat,
        aes(energy_per_capita, gdp_per_capita, group = ratio),
        inherit.aes = FALSE
    ) +
    geom_text(data = plot_dat |> 
                  group_by(ratio) |> 
                  filter(energy_per_capita == max(energy_per_capita)) |> 
                  ungroup(),
              aes(x = energy_per_capita,
                  y = gdp_per_capita,
                  label = 1 / ratio),
              inherit.aes = FALSE, hjust = 0, vjust = 0, 
              nudge_x = 0.03,
              nudge_y = 0.03) +
    geom_line(aes(y = pred), lty = 2, col = "black", size = 1) +
    geom_point(aes(group = country)) +
    # geom_rangeframe() +
    scale_x_log10(labels = label_number(suffix = " kWh", big.mark = ".", decimal.mark = ",")) +
    scale_y_log10(labels = label_number(suffix = "$", big.mark = ".", decimal.mark = ",")) +
    scale_colour_manual(values = c("#969696", "#4292c6")) +
    scale_size_manual(values = c(2.5, 4)) +
    scale_alpha_manual(values = c(0.7, 1)) +
    theme_half_open(font_size = 12) +
    theme(legend.position = "none",
          plot.title = element_markdown(face = "plain")) +
    labs(x = "Energy production per capita",
         y = "GDP per capita",
         title = "Evolution of energy and GDP in <b style='color:#4292c6'>Iceland</b> compared to <b style='color:#969696'>the world</b>",
         subtitle = str_c("Whole lines show different ratios of energy production / GDP",
                          "\n",
                          "Broken line shows trend worldwide",
                          "\n",
                          "Year: {closest_state}")) +
    transition_states(as.integer(year), state_length = 0, transition_length = 10, wrap = TRUE) +
    ease_aes("cubic-in-out")

p_vid <- animate(p, width = 12, height = 0.5 * 12, unit = "in", res = 150, fps = 50, duration = 10)

anim_save(filename = "images/energy_production.gif", animation = p_vid)