library(tidyverse)


movies <- read_csv('./data/dis3.csv')

summeries = movies %>% summarise(
  mean_rating = mean(`IMDb Rating`),
  sd_rating = sd(`IMDb Rating`),
  mean_time = mean(`Runtime (mins)`),
  sd_time = sd(`Runtime (mins)`)
  )
print(summeries)

ggplot(movies, aes(x = `IMDb Rating`)) +
  geom_histogram()