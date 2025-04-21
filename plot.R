library(ggrepel)  # install.packages("ggrepel")
library(ggplot2)
library(readr)
library(dplyr)
library(scales)

# Load your data
df <- read_csv("data/dis3.csv")

# Preprocess and clean
df <- df %>%
  mutate(
    `Release Date` = as.Date(`Release Date`),
    `IMDb Rating` = as.numeric(`IMDb Rating`),
    `Num Votes` = as.numeric(`Num Votes`)
  ) %>%
  filter(!is.na(`Release Date`) & !is.na(`IMDb Rating`))

# Compute visual weight for point size/opacity
df <- df %>%
  mutate(vote_weight = sqrt(`Num Votes`))

# Tag the top 10 most voted movies for labeling
top_movies <- df %>%
  arrange(desc(`Num Votes`)) %>%
  slice_head(n = 10) %>%
  pull(Title)

df <- df %>%
  mutate(label = ifelse(Title %in% top_movies, Title, NA))

# Plot

ggplot(df, aes(x = `Release Date`, y = `IMDb Rating`)) +
  geom_point(aes(alpha = vote_weight), color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", size = 1.2) +
  geom_text_repel(
    aes(label = label),
    size = 3.5,
    box.padding = 0.5,
    max.overlaps = Inf,
    fontface = "bold",
    segment.color = "gray70"
  ) +
  scale_size_continuous(range = c(1, 10), guide = "none") +
  scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  labs(
    title = "IMDb Ratings Timeline of Disney Movies",
    subtitle = "Top 10 most popular titles are labeled",
    x = "Release Year",
    y = "IMDb Rating"
  ) +
  theme_minimal(base_size = 13)
