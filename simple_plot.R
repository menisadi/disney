library(tidyverse)
library(ggrepel)

selected_titles_list <- read_lines("./data/selected_titles.txt")

movies <-
  read_csv('./data/dis3.csv', show_col_types = FALSE) %>% 
  mutate(
    label = if_else(Title %in% selected_titles_list, Title, NA_character_)
  )

ggplot(movies, aes(x = `Release Date`, y = `IMDb Rating`)) +
  geom_point(aes(alpha = `Num Votes`), stroke = 0.1) +
  geom_smooth(aes(weight = `Num Votes`), method = 'loess', se = FALSE) +
  geom_text_repel(
    aes(label = label),
    size = 2,
    segment.color = 'grey70'
  ) +
  scale_alpha_continuous(range = c(0.3, 1), guide = 'none') +
  labs(
    title = 'Disney Movies on IMDb',
    subtitle = 'Point transparency reflects number of audience votes',
    x = 'Release Date',
    y = 'IMDb Rating'
  )