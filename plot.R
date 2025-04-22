
# Packages
library(tidyverse)   # ggplot2, dplyr, readr, ...
library(ggrepel)
library(lubridate)

movies <-
  read_csv("./data/dis3.csv", show_col_types = FALSE) %>% 
  mutate(
    vote_weight  = scales::rescale(`Num Votes`, to = c(0, 1)),
    release_year = year(`Release Date`),
    label        = if_else(rank(-`Num Votes`) <= 10, Title, NA_character_)
  )

ggplot(movies, aes(x = release_year, y = `IMDb Rating`)) +
  geom_point(aes(alpha = vote_weight), stroke = 0.2) +
  geom_smooth(aes(weight = `Num Votes`), method = "loess", se = FALSE) +
  geom_text_repel(
    aes(label = label),
    size          = 3,
    box.padding   = 0.5,
    max.overlaps  = Inf,
    segment.color = "grey70"
  ) +
  scale_alpha_continuous(range = c(0.1, 1), guide = "none") +
  scale_x_continuous(breaks = seq(1930, 2025, 5)) +
  labs(
    title    = "Disney Movies on IMDb (1937 – 2024)",
    subtitle = "Point transparency reflects number of audience votes",
    x        = "Release Year",
    y        = "IMDb Rating"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

