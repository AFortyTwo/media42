library(jsonlite)
library(tidyverse)
library(rvest)
devtools::load_all()


# Comments ----------------------------------------------------------------

# Resortliste aus krone.at/ URL
r <- tibble(resort = c("bundeslaender", "wirtschaft",  "politik", "welt", "sport", "oesterreich",
                        "nachrichten", "wissen", "viral", "fussball", "motorsport", "wintersport",
                        "tennis", "stars-society", "lifestyle", "kino", "musik", "medien", "digital",
                        "freizeit", "auto", "web", "elektronik", "spiele", "digitale-trends",
                        "gesund-fit", "life", "tierecke", "reisen-urlaub", "bauen-wohnen", "lieblingsrezept"
                        ))

# Download links zu den Artikel für jede Resortseite und bereinigen
links <- r %>% mutate(links = map(resort, get_links)) %>% unnest()
links <- links %>%
  mutate(n = TRUE, resort = str_replace(resort, "-", "_")) %>%
  spread(resort, n, fill = FALSE)

# Alle Kommentare inklusive Diggs runterladen (Dauer ca. 30 min) und bereinigen
tictoc::tic()
pb <- dplyr::progress_estimated(length(links$id_article))
df <- links %>% mutate(comments = map(id_article, get_comments, pb = pb))
tictoc::toc()
df <- df %>%
  filter(comments != "no comments") %>%
  unnest() %>%
  mutate(retrieved = Sys.Date())

# Speichern unter heutigem Datum
save(df, file = paste0("export/krone_scrape_", Sys.Date(), ".RData"))
saveRDS(df, file = paste0("export/krone_scrape_", Sys.Date(), ".rds"))
df %>% write_delim(paste0("export/krone_scrape_", Sys.Date(), ".csv"), delim = ",")


# Articles ----------------------------------------------------------------

# files <- list.files(path='export/', pattern="*.csv", full.names=T, recursive=FALSE)
links <- links %>%
  select(id_article, url) %>%
  distinct()

pb <- dplyr::progress_estimated(length(links$id_article))
df <- links %>% mutate(article = map(id_article, get_article, pb = pb))

out <- df %>%
  filter(article != "down") %>%
  unnest()

# Fix authors
out <- out %>%
  mutate(
    author1 = stringr::str_remove(author1, ", Kronen Zeitung"),
    author1 = stringr::str_remove(author1, ", wohnkrone.at"),
    author1 = case_when(
      author1 == "Kronen Zeitung" ~ NA_character_,
      stringr::str_length(author1) <= 6 | stringr::str_length(author1) > 60 ~ NA_character_,
      stringr::str_detect(author1, "[:!]") ~ NA_character_,
      TRUE ~ author1
    ),
    author2 = case_when(
      author2 == "krone.at" ~ NA_character_,
      author2 == "krone Sport" ~ NA_character_,
      TRUE ~ author2
    ),
    author = case_when(
      !is.na(author2) ~ author2,
      !is.na(author1) ~ author1,
      TRUE ~ NA_character_
    ),
    retrieved = Sys.Date(),
    modified = lubridate::ymd_hms(meta_modified)
  ) %>% rename(para = data)

# Speichern unter heutigem Datum
saveRDS(out, file = paste0("export/articles/krone_articles_", Sys.Date(), ".rds"))
# out %>% write_delim(paste0("export/articles/krone_articles_", Sys.Date(), ".csv"), delim = ",")



# Merge -------------------------------------------------------------------

# Load files
f_articles <- list.files(path = 'export/articles', pattern = "*.rds", full.names = TRUE, recursive = FALSE)
df_articles <- map_df(f_articles, readRDS)
df_articles <- df_articles %>%
  select(-retrieved) %>%
  group_by(id_article) %>%
  arrange(desc(modified)) %>%
  filter(row_number() == 1) %>%
  ungroup()


resort_list <- read_csv('data/krone_resorts.csv')

f_comments <- list.files(path='export/', pattern="*.csv", full.names=T, recursive=FALSE)
df <- map_df(f_comments, readr::read_csv) %>% distinct() # possible to re-write this as RDS; but error with datetime variable

# Drop unchanged duplicates
df <- df %>% select(-retrieved) %>% distinct() %>% mutate(id_article = as.character(id_article))

# Only keep the most recent observation of a comment (what about changing resorts?)
df <- df %>%
  group_by(id) %>%
  mutate(diggs = positive + negative) %>%
  arrange(desc(diggs)) %>%
  filter(row_number() == 1) %>%
  ungroup()

df <-  df %>% left_join(df_articles, by = "id_article", suffix = c("_comment", "_article"))
df <- df %>% left_join(resort_list, by = "meta_ressort_slug")

df <- df %>%
  select(
    id_article, id_comment = id, id_user = user.id, date_comment,
    positive, negative, diggs,
    user = user.username, title, content, cat, resort,
    url = url_comment,
    everything(),
    -auto:-wissen,
    auto:wissen
  ) %>%
  select(
    -status:-url_article
  ) %>%
  mutate(
    id_comment = as.character(id_comment),
    id_user = as.character(id_user)
  )

saveRDS(df, file = "export/other/merged.rds")
df %>% select(-para) %>% write_csv("export/other/merged.csv")


# Export for Dashboard ----------------------------------------------------
df %>%
  filter(date_article > max(date_article, na.rm = T) - lubridate::weeks(8)) %>%
  filter(date_comment > max(date_comment, na.rm = T) - lubridate::weeks(8)) %>%
  select(-meta_description:-meta_modified, -para, -text) %>%
  saveRDS(file = "../krone-dashboard/data/comments.RDS")

df %>%
  filter(date_article > max(date_article, na.rm = T) - lubridate::weeks(8)) %>%
  select(id_article, para, text) %>%
  group_by(id_article) %>% filter(row_number() == 1) %>%
  saveRDS(file = "../krone-dashboard/data/articles.RDS")


