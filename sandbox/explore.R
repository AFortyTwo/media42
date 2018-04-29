library(tidyverse)
library(plotly)
library(lubridate)

# Resortliste aus krone.at/ URL
resort_list <- read_csv('data/krone_resorts.csv')
df <- readRDS("export/other/merged.rds") %>% tibble()

# Resort analyse ----------------------------------------------------------

flip <- df %>% gather(resort, value, auto:wissen) %>% filter(value == T) %>% select(-value)
flip <- flip %>% left_join(resort_list, by = "resort")

comment_count <- flip %>%  count(cat) %>% mutate(prop = n/sum(n))
comment_count %>% ggplot(aes(x = reorder(cat, n), y = prop)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Proportion of Comments in Ressort") +
    xlab("Ressort")

article_count <- flip %>% select(id_article, cat) %>% distinct() %>%
  count(cat) %>% mutate(prop = n/sum(n))
article_count %>% ggplot(aes(x = reorder(cat, n), y = prop)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Proportion of Articles in Resort") +
  xlab("Resort")

article_count %>% left_join(comment_count, by = "cat", suffix = c("_article", "_comment")) %>%
  ggplot(aes(x = reorder(cat, n_article), group = 1)) +
    geom_bar(aes(y = prop_article), stat = "identity") +
    geom_point(aes(y = prop_comment)) +
    coord_flip() +
    labs(y = "Proportion of Articles/Comments", x = "Resort", title = "Artikel vs Kommentare") # , subtitle = "Double counting if article appears in multiple resorts"

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


# diggs -------------------------------------------------------------------

p <- df %>% arrange(desc(diggs)) %>% filter(row_number() < 1000) %>%
  ggplot(aes(x = positive, y = negative,
             text = paste0(user.username, ": ", content),
             colour = politik)) +
    geom_point()

ggplotly(p, tooltip = "text")

# Time series -------------------------------------------------------------

df %>% group_by(period = floor_date(date, "5 day")) %>%
  summarise(n = n())  %>%
  ggplot(aes(x = period, y = n)) +
    geom_line()


# Individual article ------------------------------------------------------

lol %>% ggplot(aes(x = user.username)) +
  geom_bar() +
  coord_flip()

lol %>% group_by(user.id) %>% mutate(n = n()) %>%
  arrange(desc(n)) %>% select(user.username, diggs, content, positive, negative, date)

lol %>% saveRDS("falter_golan.rds")


# user over time ----------------------------------------------------------


