library(jsonlite)
library(tidyverse)
library(rvest)
devtools::load_all()

# Resortliste aus krone.at/ URL
df <- tibble(resort = c("bundeslaender", "wirtschaft",  "politik", "welt", "sport", "oesterreich",
                        "nachrichten", "wissen", "viral", "fussball", "motorsport", "wintersport",
                        "tennis", "stars-society", "lifestyle", "kino", "musik", "medien", "digital",
                        "freizeit", "auto", "web", "elektronik", "spiele", "digitale-trends",
                        "gesund-fit", "life", "tierecke", "reisen-urlaub", "bauen-wohnen", "lieblingsrezept"
                        ))

# Download links zu den Artikel für jede Resortseite und bereinigen
df <- df %>% mutate(links = map(resort, get_links)) %>% unnest()
df <- df %>%
  mutate(n = TRUE, resort = str_replace(resort, "-", "_")) %>%
  spread(resort, n, fill = FALSE)

# Alle Kommentare inklusive Diggs runterladen (Dauer ca. 30 min) und bereinigen
tictoc::tic()
df_comments <- df %>% mutate(comments = map(id_article, get_comments))
tictoc::toc()
df_comments <- df_comments %>%
  filter(comments != "no comments") %>%
  unnest() %>%
  mutate(retrieved = Sys.Date())

# Speichern unter heutigem Datum
save(df_comments, file = paste0("export/krone_scrape_", Sys.Date(), ".RData"))
df_comments %>% write_delim(paste0("export/krone_scrape_", Sys.Date(), ".csv"), delim = ",")






