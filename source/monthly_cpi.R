library(tidyverse)
library(readabs)

# read_abs_series("A128478317T")

cpi_data <- read_csv("data/raw/old_data.csv")

g <- cpi_data |> 
  rename(date = 1, Monthly = 2, Quarterly = 3) |> 
  pivot_longer(cols = -date) |> 
  mutate(date = parse_date(paste0("01-", date), "%d-%b-%y")) |> 
  ggplot(aes(x = date, y = value, colour = name)) +
  geom_line(data = \(x) filter(x, name == "Monthly"), size = 0.7) +
  geom_point(data = \(x) filter(x, name == "Quarterly")) +
  labs(
    x = NULL,
    y = "Annual CPI change (%)",
    colour = NULL,
    title = "Reaching an inflation peak?",
    subtitle = "ABS estimate 6.8% inflation in year to August, slightly less than the 7.0% to July",
    caption = "Source: ABS, chart by Nick Twort"
    ) +
  scale_colour_manual(values = hktools::hk_colours) +
  theme_light(base_family = "Lato", base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.background = element_blank(),
    legend.key=element_blank(),
    legend.position = c(0.85, 0.2),    strip.background = element_rect(fill = "#E6E7E8"), plot.subtitle = element_text(size = 8)
  ) +
  guides(colour = guide_legend(override.aes = list(linetype = c(1, 0), shape = c(NA, 16))))


ggsave(filename = "Figures/g.svg",
       plot = g,
       width = 8 * 1.5,
       height = 8,
       units = "cm"
)
ggsave(filename = "Figures/png/g.png",
       plot = g,
       width = 8 * 1.5,
       height = 8,
       units = "cm"
)

