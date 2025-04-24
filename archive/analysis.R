library(tidyverse)
library(rvest)

wiki_url <- "https://en.wikipedia.org/wiki/List_of_Disney_theatrical_animated_feature_films"
wiki_page <- read_html(wiki_url)
tables <- wiki_page %>% html_table(fill = TRUE)
disney_wiki <- tables[[2]]
names(disney_wiki) <- make.names(names(disney_wiki), unique = TRUE)
disney_wiki_clean <- disney_wiki %>% mutate(Title = str_remove_all(Title, "\\[[^\\]]*\\]"))

df1 <- read_csv("data/dis1.csv")
df2 <- read_csv("data/dis2.csv")
df3 <- read_csv("data/dis3.csv")

all_movies_list = union(
  disney_wiki_clean$Title, union(
    df1$Title, union(
      df2$Title, df3$Title
      )
    )
  )

all_movies <- tibble(Title = all_movies_list) %>%
  mutate(
    in_1 = Title %in% df1$Title, 
    in_2 = Title %in% df2$Title, 
    in_3 = Title %in% df3$Title, 
    in_wiki_clean = Title %in% disney_wiki_clean$Title
  )

normalized_movies <- all_movies %>%
  mutate(
    title_clean = Title %>%
      str_to_lower() %>%
      str_replace_all("&", " and ") %>%
      str_replace_all("\\biii\\b", " 3 ") %>%
      str_replace_all("\\bii\\b",  " 2 ") %>%
      str_replace_all("[^a-z0-9 ]", "") %>%
      str_replace_all("\\b(the|a|an)\\b", "") %>%
      str_squish()
  )

canonical <- normalized_movies %>% 
  arrange(!in_wiki_clean,  # rows found on Wikipedia first
          str_length(Title)) %>%      # …otherwise choose the shortest string
  group_by(title_clean) %>% 
  summarise(canonical_title = first(Title), .groups = "drop")

movies_fixed <- normalized_movies %>% 
  left_join(canonical,  by = "title_clean") %>% 
  select(-title_clean) %>% 
  relocate(canonical_title, .after = Title) %>%
  mutate(in_wiki_by_canonical = canonical_title %in% disney_wiki_clean$Title)

final_movies <- movies_fixed