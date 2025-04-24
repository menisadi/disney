# ────────────────────────────────────────────────────────────────────────────
# reconcile_disney.R
# Scrape Wikipedia, line up df1/df2/df3 titles, and output harmonised data
# ────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(rvest)
library(stringdist)   # distance metrics
library(fuzzyjoin)    # stringdist_*_join helpers

#............................................................................
# 1. Pull the authoritative list from Wikipedia
#............................................................................
wiki_url  <- "https://en.wikipedia.org/wiki/List_of_Disney_theatrical_animated_feature_films"

disney_wiki_clean <- wiki_url |> 
  read_html() |> 
  html_table(fill = TRUE) |>            # second table is the film list
  magrittr::extract2(2) |> 
  set_names(make.names) |> 
  mutate(
    Title = str_remove_all(Title, "\\[[^]]*\\]"),   # drop footnote markers
    Title = str_squish(Title)
  )

#............................................................................
# 2. Load your three source files
#............................................................................
df1 <- read_csv("data/dis1.csv")     # ← adjust paths as needed
df2 <- read_csv("data/dis2.csv")
df3 <- read_csv("data/dis3.csv")

#............................................................................
# 3. Minimal normalisation helper for fuzzy matching
#............................................................................
normalise <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("&", "and") |>             # ‘&’ ↔ ‘and’
    str_replace_all("ii+", " 2 ") |>           # Roman II/III… → Arabic
    str_replace_all("[[:punct:][:space:]]+", "")  # strip punct/space
}

wiki_n <- disney_wiki_clean |> mutate(norm = normalise(Title))
df1_n  <- df1 |> mutate(norm = normalise(Title))
df2_n  <- df2 |> mutate(norm = normalise(Title))
df3_n  <- df3 |> mutate(norm = normalise(Title))

#............................................................................
# 4. Auto‑match df1 ⇢ wiki with a forgiving join (Jaro–Winkler)
#............................................................................
auto_df1 <- stringdist_left_join(
  df1_n, wiki_n,
  by       = "norm",
  method   = "jw",
  max_dist = .08        # ≈ 92 % similarity – tweak if too loose/tight
) |>
  filter(!is.na(Title.y)) |>
  transmute(df1_title = Title.x, wiki_title = Title.y) |>
  distinct()

#............................................................................
# 5. Create / update manual lookup for titles still unmatched
#............................................................................
lookup_path <- "data/df1_to_wiki_lookup.csv"

if (!file.exists(lookup_path)) {
  df1_n |>
    anti_join(auto_df1, by = c(Title = "df1_title")) |>
    transmute(df1_title = Title, wiki_title = NA_character_) |>
    write_csv(lookup_path)
  
  stop(
    glue::glue(
      "\nLookup template written to {lookup_path}.\n",
      "Open it in your editor, fill in wiki_title for each row (leave NA if ",
      "the film truly is NOT on the Wikipedia list, e.g. direct‑to‑video), ",
      "and run this script again."
    )
  )
}

manual_df1 <- read_csv(lookup_path,
                       show_col_types = FALSE) |>
  mutate(wiki_title = if_else(is.na(wiki_title), df1_title, wiki_title))

#............................................................................
# 6. Combine auto + manual, then standardise the Title column
#............................................................................
lookup_df1 <- bind_rows(auto_df1, manual_df1) |> distinct()

df1_fixed <- df1 |>
  left_join(lookup_df1, by = c(Title = "df1_title")) |>
  mutate(Title = coalesce(wiki_title, Title)) |>
  select(-wiki_title)

#............................................................................
# 7. Repeat (short‑cut) for df2 & df3 – they usually share the same issues
#    so we can just re‑use the manual lookup you filled for df1
#............................................................................
fix_with_lookup <- function(df, lookup) {
  df |>
    left_join(lookup, by = c(Title = "df1_title")) |>
    mutate(Title = coalesce(wiki_title, Title)) |>
    select(-wiki_title)
}

df2_fixed <- fix_with_lookup(df2, lookup_df1)
df3_fixed <- fix_with_lookup(df3, lookup_df1)

#............................................................................
# 8. Optional: save the harmonised versions for downstream work
#............................................................................
write_csv(df1_fixed, "output/df1_fixed.csv")
write_csv(df2_fixed, "output/df2_fixed.csv")
write_csv(df3_fixed, "output/df3_fixed.csv")

message("🎉  Titles harmonised. Files written to output/.")
