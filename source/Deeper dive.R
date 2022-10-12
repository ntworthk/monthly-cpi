#--- Script details ------------------------------------------------------------
# Creation date: 26 October 2022
# Client:        Public
# Project:       monthly-cpi
# Description:   Analysis of first monthly CPI full release
# Author:        Nick Twort

library(tidyverse)
library(readabs)
library(glue)
library(directlabels)

#--- Import data ---------------------------------------------------------------

# monthly_cpi <- read_abs_local(filenames = "648401 (2).xlsx", path = "C:/Users/nick.twort/Downloads/")
monthly_cpi <- read_abs(cat_no = "6484.0", tables = "1")
quarter_cpi <- read_abs_series("A2325846C") |> 
  mutate(series = "Index Numbers ;  All groups CPI (quarterly) ;  Australia ;")

monthly_cpi <- monthly_cpi |>
  bind_rows(quarter_cpi) |> 
  separate_series() |> 
  group_by(series) |> 
  mutate(value_ch = case_when(
    series_id == "A2325846C" ~ value / lag(value, 4) - 1,
    series_1 == "Index Numbers" ~ value / lag(value, 12) - 1,
    TRUE ~ value
  )) |> 
  ungroup()

monthly_cpi |>
  filter(series_id %in% c(
    "A2325846C", # quarterly all groups, original
    "A128478317T", # all groups CPI, original
    "A128473239F", # all groups CPI excluding volatile items, original
    "A128481587A", # all groups CPI, seasonally adjusted
    "A128481593W", # trimmed mean, seasonally adjusted
    "A128485125F"  # weighted median, seasonally adjusted
  )) |> 
  filter(!is.na(value_ch), date >= "2018-09-01") |> 
  mutate(series_2 = str_replace_all(series_2, c("CPI " = "CPI\n", "CPI, " = "CPI,\n"))) |> 
  ggplot(
    aes(x = date, y = value_ch, colour = series_2)
  ) +
  geom_line(size = 1) +
  geom_text(
    data = \(x) filter(group_by(x, series_2), date == max(date)),
    aes(label = series_2),
    hjust = -0.1
    ) +
  labs(
    x = NULL,
    y = "Annual CPI change (%)",
    # colour = NULL,
    # title = "Reaching an inflation peak?",
    # subtitle = "ABS estimate 6.8% inflation in year to August, slightly less than the 7.0% to July",
    caption = "Source: ABS, chart by Nick Twort"
  ) +
  scale_x_date(expand = expansion(mult = c(0.05, 0.15))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = hktools::hk_colours, guide = guide_none()) +
  theme_light(base_family = "Lato", base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_blank(),
    legend.key=element_blank(),
    legend.position = c(0.85, 0.2),
    strip.background = element_rect(fill = "#E6E7E8"),
    plot.subtitle = element_text(size = 8)
  )

individual_chart <- function(series_id) {
  
  chart_data <- monthly_cpi |> 
    filter(series_id == {{series_id}})
  
  series_name <- chart_data$series_2[1]
  
  monthly_cpi |> 
    filter(series_id == {{series_id}}) |> 
    ggplot(
      aes(x = date, y = value_ch)
    ) +
    geom_line(colour = "#008698", size = 1) +
    geom_text(
      data = \(x) filter(group_by(x, series_2), date == max(date)),
      aes(label = scales::percent(value_ch, accuracy = 0.1)),
      hjust = -0.1
    ) +
    labs(
      x = NULL,
      y = "Annual CPI change (%)",
      title = series_name,
      subtitle = glue("ABS estimate {scales::percent(last(chart_data$value_ch), accuracy = 0.1)}"),
      caption = "Source: ABS, chart by Nick Twort"
    ) +
    scale_x_date(expand = expansion(mult = c(0.05, 0.15))) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_colour_manual(values = hktools::hk_colours, guide = guide_none()) +
    theme_light(base_family = "Lato", base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.background = element_blank(),
      legend.key=element_blank(),
      legend.position = c(0.85, 0.2),
      strip.background = element_rect(fill = "#E6E7E8"),
      plot.subtitle = element_text(size = 8)
    )
  
}


# Automotive fuel
individual_chart("A128484685K")

# Food & non-alch
individual_chart("A128480733V")

# Fruit & veg
individual_chart("A128480215V")

# Rent
individual_chart("A128473449C")

# Housing
individual_chart("A128485865L")

