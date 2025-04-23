#' ---
#' title: "Disney Movies Analysis"
#' author: "Meni"
#' ---

# @knitr setup
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

library(tidyverse)
library(ggrepel)
library(stringr)

#' ## Basic Stats

selected_titles_list <- read_lines("./data/selected_titles.txt")

movies <-
  read_csv('./data/dis3.csv', show_col_types = FALSE)

summ <- movies %>%
  summarise(
    avg_rating = mean(`IMDb Rating`, na.rm = TRUE),
    sd_rating = sd(`IMDb Rating`, na.rm = TRUE),
    avg_runtime = mean(`Runtime (mins)`, na.rm = TRUE),
    sd_runtime = sd(`Runtime (mins)`, na.rm = TRUE),
    avg_votes = mean(`Num Votes`, na.rm = TRUE),
    sd_votes = sd(`Num Votes`, na.rm = TRUE)
  )
print(summ)

#' ## Plots

ggplot(movies, aes(x = `IMDb Rating`)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of IMDb Ratings")

ggplot(movies, aes(x = `Runtime (mins)`)) +
  geom_histogram(bins=20) +
  labs(title = "Distribution of IMDB Runtimes")

ggplot(movies, aes(x = Year, y = `IMDb Rating`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'loess', se = FALSE) +
  labs(title = "IMDb Rating Trends Over Time")

ggplot(movies, aes(x = `Runtime (mins)`, y = `IMDb Rating`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = TRUE) +
  labs(title = "Correlation: Runtime vs IMDb Rating")

movies <- movies %>%
  mutate(is_comedy = str_detect(Genres, "Comedy"))

movies <- movies %>%
  mutate(is_musical = str_detect(Genres, "Musical"))

ggplot(movies, aes(x = is_musical, y = `IMDb Rating`)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Non-Musical", "Musical")) +
  labs(
    title = "IMDb Ratings: Musicals vs Non-Musicals",
    x = "",
    y = "IMDb Rating"
  ) +
  theme(legend.position = "none")

#' ## More Advanced Stats

print(cor.test(movies$`IMDb Rating`, movies$`Runtime (mins)`))
print(t.test(`IMDb Rating` ~ is_comedy, data = movies))
print(t.test(`IMDb Rating` ~ is_musical, data = movies))

movies <- movies %>%
  mutate(
    genre_group = case_when(
      str_detect(Genres, "Action") ~ "Action",
      str_detect(Genres, "Drama") ~ "Drama",
      str_detect(Genres, "Comedy") ~ "Comedy",
      TRUE ~ "Other"
    )
  )

print(count(movies, genre_group))

aov_result <- aov(`IMDb Rating` ~ genre_group, data = movies)
print(summary(aov_result))
