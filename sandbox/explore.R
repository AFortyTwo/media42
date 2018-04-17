library(tidyverse)
library(plotly)

# Resortliste aus krone.at/ URL
resort_list <- read_csv('data/krone_resorts.csv')

files <- list.files(path='export/', pattern="*.csv", full.names=T, recursive=FALSE)
df <- map_df(files, readr::read_csv) %>% distinct() %>%
  select(id_article, id, user.id, user.username, date, positive, negative, content, everything())
# Drop unchanged duplicates
df <- df %>% select(-retrieved) %>% distinct()

# Only keep the most recent observation of a comment (what about changing resorts?)
df <- df %>%
  group_by(id) %>%
  mutate(diggs = positive + negative) %>%
  arrange(desc(diggs)) %>%
  filter(row_number() == 1) %>%
  ungroup()


# Resort analyse ----------------------------------------------------------

flip <- df %>% gather(resort, value, auto:wissen) %>% filter(value == T) %>% select(-value)
flip <- flip %>% left_join(resort_list, by = "resort")

comment_count <- flip %>%  count(resort) %>% mutate(prop = n/sum(n))
comment_count %>% ggplot(aes(x = reorder(resort, n), y = prop)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Proportion of Comments in Resort") +
    xlab("Resort")

article_count <- flip %>% select(id_article, resort) %>% distinct() %>%
  count(resort) %>% mutate(prop = n/sum(n))
article_count %>% ggplot(aes(x = reorder(resort, n), y = prop)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Proportion of Articles in Resort") +
  xlab("Resort")

article_count %>% left_join(comment_count, by = "resort", suffix = c("_article", "_comment")) %>%
  ggplot(aes(x = reorder(resort, n_article), group = 1)) +
    geom_bar(aes(y = prop_article), stat = "identity") +
    geom_line(aes(y = prop_comment)) +
    coord_flip() +
    labs(y = "Proportion of Articles/Comments", x = "Resort", title = "Artikel vs Kommentare", subtitle = "Double counting if article appears in multiple resorts")

# Combos
flip %>% select(id_article, resort = cat) %>% distinct() %>%
  group_by(id_article) %>%
  summarise(r = paste(resort, collapse = ","), n = n()) %>%
  ungroup() %>%
  count(r) %>%  # %>% mutate(prop = n/sum(n))
  #filter(nn > 1) %>%
  ggplot(aes(x = reorder(r, nn), y = nn)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Proportion of Articles in Resort") +
  xlab("Resort")

# User analyse ------------------------------------------------------------

users <- df %>% group_by(user.username) %>%
  summarise(
    n_comments = n(),
    n_positive = sum(positive, na.rm = TRUE),
    n_negative = sum(negative, na.rm = TRUE),
    prop = n_positive / n_negative) %>%
  arrange(desc(n_comments))

flip %>% group_by(user.id) %>% mutate(n = n()) %>% arrange(desc(n)) %>% filter(n > 100) %>% ungroup() %>%
  ggplot() +
    geom_bar(aes(x = reorder(user.username, n), fill = cat), position = position_stack()) +
    coord_flip()

p <- users %>%
  # ggplot(aes(x = log(n_positive), y = log(n_negative), text = user.username)) +
  ggplot(aes(x = n_positive, y = n_negative, text = user.username)) +
  geom_point()

ggplotly(p)

df %>% select(date)
