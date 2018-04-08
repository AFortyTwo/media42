library(jsonlite)
library(tidyverse)
library(rvest)

df <- tibble(resort = c("bundeslaender", "wirtschaft",  "politik", "welt", "sport", "oesterreich",
                        "nachrichten", "wissen", "viral", "fussball", "motorsport", "wintersport",
                        "tennis", "stars-society", "lifestyle", "kino", "musik", "medien", "digital",
                        "freizeit", "auto", "web", "elektronik", "spiele", "digitale-trends",
                        "gesund-fit", "life", "tierecke", "reisen-urlaub", "bauen-wohnen", "lieblingsrezept"
                        ))
df <- df %>% mutate(links = map(resort, get_links)) %>% unnest()
df <- df %>%
  mutate(n = TRUE, resort = str_replace(resort, "-", "_")) %>%
  spread(resort, n, fill = FALSE)
df_comments <- df %>% mutate(comments = map(id_article, get_comments))
df_comments <- df_comments %>% filter(comments != "no comments") %>% unnest()

save(df_comments, file = "scrape.RData")
df_comments %>% write_csv(paste0("krone_scrape_", Sys.Date(),".csv"))





