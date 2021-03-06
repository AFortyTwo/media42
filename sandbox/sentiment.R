library(tidyverse)
library(tidytext)

# Sentiment analysis ------------------------------------------------------
files <- list.files(path='export/', pattern="*.csv", full.names=T, recursive=FALSE)
df <- map_df(files, readr::read_csv) %>% distinct()
df <- df %>%
  group_by(id) %>%
  mutate(diggs = positive + negative) %>%
  arrange(desc(diggs)) %>%
  filter(row_number() == 1) %>%
  ungroup()


# Lexika und Stopwords vorbereiten
lexicon <-
  rbind(
    read_delim("data/SentiWS_v1.8c_Negative.txt", delim = "\t", col_names = c("word", "s", "synonym")),
    read_delim("data/SentiWS_v1.8c_Positive.txt", delim = "\t", col_names = c("word", "s", "synonym"))
  )

lexicon <- lexicon %>%
  mutate(word = gsub("\\|.*", "", word) %>% tolower) %>%
  # manche Wörter kommen doppelt vor, hier nehmen wir den mittleren Wert
  group_by(word) %>% summarise(s = mean(s)) %>% ungroup %>% select(word, s) %>%
  mutate(sentiment = case_when(
    s < 0 ~ "negative",
    TRUE ~ "positive"
    )
  )

stopwords <- tokenizers::stopwords("de") %>% append(
  c("dass", "ja", "mehr", "mal", "schon", "immer", "gibt", "wäre", "wer", "gut", "geht", "warum", "viele")
)

# Wörter extrahieren
words <- df %>%
  unnest_tokens(word, content, token = "words") %>%  #  "regex", pattern = reg) %>%
  filter(!word %in% stopwords,
         str_detect(word, "[a-z]"))

# Häufigsten Wörter
words %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()

words %>%
  count(word, sort = TRUE) %>%
  head(5000) %>%
  write_csv('export/woerter.csv')

# Relative häufigkeit
ratios <- words %>%
  count(word, id_article) %>%
  filter(sum(n) >= 5) %>%
  spread(id_article, n, fill = 0) %>%
  ungroup() %>%
  mutate_at(vars(-word), funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log2(`1675973`/ `1676695`)) %>%
  arrange(desc(logratio))

 ratios %>%
  group_by(logratio > 0) %>%
  top_n(15, abs(logratio)) %>% # Genius
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Lauda/Kurden ratio") +
  scale_fill_manual(name = "", labels = c("Lauda", "Kurden"),
                    values = c("red", "lightblue"))

# Verbindung mit sentiment score
articles <- words %>%
  group_by(id_article) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id_krone, id_article, total_words)

by_article_sentiment <- words %>%
  inner_join(lexicon, by = "word") %>%
  count(sentiment, id_krone) %>%
  ungroup() %>%
  complete(sentiment, id_krone, fill = list(n = 0)) %>%
  inner_join(articles, by = "id_krone") %>%
  group_by(id_article, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup() %>%
  mutate(prop = words / total_words)

head(by_source_sentiment)

# Sentiment plot
ratios %>%
  inner_join(lexicon, by = "word") %>%
  # filter(!sentiment %in% c("positive", "negative")) %>%
  mutate(sentiment = reorder(sentiment, -logratio),
         word = reorder(word, -logratio)) %>%
  group_by(sentiment) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  facet_wrap(~ sentiment, scales = "free", nrow = 2) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Lauda / Kurden log ratio") +
  scale_fill_manual(name = "", labels = c("Lauda", "Kurden"),
                    values = c("red", "lightblue"))

# Ein bisschen Statistik, aber nicht sehr ergibig
library(broom)

sentiment_differences <- by_article_sentiment %>%
  group_by(sentiment) %>%
  do(tidy(poisson.test(.$words, .$total_words)))

sentiment_differences

library(scales)

sentiment_differences %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, estimate)) %>%
  mutate_at(vars(estimate, conf.low, conf.high), funs(. - 1)) %>%
  ggplot(aes(estimate, sentiment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  scale_x_continuous(labels = percent_format()) +
  labs(x = "% increase in Lauda relative to Kurden",
       y = "Sentiment")





# Clean up ----------------------------------------------------------------

remDr$close(); remDr$closeServer()
