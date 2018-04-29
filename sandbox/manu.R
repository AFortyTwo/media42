library(tidyverse)
library(plotly)
library(lubridate)

# Resortliste aus krone.at/ URL
df <- readRDS("export/other/merged.rds")


# Zeitreihe ---------------------------------------------------------------

df <- df %>% filter(date_comment > max(date_comment, na.rm = T) - weeks(2))


df %>%
  group_by(period = floor_date(date_comment, "day")) %>%
  summarise(n = n())  %>%
  ggplot(aes(x = period, y = n)) +
  geom_line()

df %>% count(id_article) %>% arrange(desc(n)) %>%
  ggplot() +
  geom_histogram(aes(x = n))



# manu --------------------------------------------------------------------


df %>%  filter(
  user %in% c("Mensch", "jedeMeinungZaehlt", "einsam", "echterRealist")
) %>% View()

df %>%
  group_by(id_article) %>% mutate(n_comments = n()) %>%
  ungroup() %>%
  filter(
    user %in% c("Mensch", "jedeMeinungZaehlt", "einsam", "echterRealist")
  ) %>% ggplot() +
  geom_point(aes(x = n_comments, y = diggs))


# timing ------------------------------------------------------------------
df %>% group_by(id_article) %>%
  mutate(n = n()) %>% filter(n > 330) %>%
  arrange(date_comment) %>% mutate(rank = row_number()) %>%
  ggplot(aes(x = rank, y = diggs, colour = id_article)) +
    geom_point() +
    # geom_smooth() +
    ylim(0, 3000)


# Polarisierend --------------------------------------------------------------------

# by user
df %>%
  mutate(pol = (positive - negative)/diggs) %>%
  ggplot() +
    geom_point(aes(x = diggs, y = pol, text = paste("positive", positive, "negative", negative))) +
    xlim(50, 2500)

p <- df %>%
  filter(diggs > 300) %>%
  mutate(
    diff = positive - negative,
    pol = case_when(
      positive > negative ~ negative/positive,
      TRUE ~ positive/negative
    )
  ) %>%
  ggplot() +
  geom_point(aes(x = diggs, y = pol, text = paste0(user.username, ": ", content, "<br>", url), colour = diff)) +
  xlim(NA, 2500) +
  scale_colour_continuous(type = "viridis")
ggplotly(p)

# by user
df %>% group_by(user.username) %>%
  summarise(
    positive = sum(positive, na.rm = T),
    negative = sum(negative, na.rm = T),
    diggs = sum(diggs, na.rm = T)
  ) %>%
  # filter(diggs > 250) %>%
  mutate(
    diff = positive - negative,
    pol = case_when(
      positive > negative ~ negative/positive,
      TRUE ~ positive/negative
    )
  ) %>%
  ggplot() +
  geom_point(aes(x = diggs, y = pol, text = user.username, colour = diff)) +
  xlim(200, NA) +
  scale_colour_continuous(type = "viridis") -> p
ggplotly(p)



# Uhrzeit -----------------------------------------------------------------

df %>% group_by(user.id) %>%
  mutate(n_comments = n()) %>% ungroup() %>%
  # group_by(hour = floor_date(date, "hour")) %>%
  mutate(user_group = case_when(
    n_comments > 100 ~ "Fan",
    TRUE ~ "Amateur"
  )) %>%
  group_by(hour = hour(date), user_group) %>%
  count() %>% ungroup() %>%
  ggplot(aes(x = hour, y = n, colour = user_group)) +
    geom_line() +
    facet_wrap(~ user_group, nrow = 2, scales = "free_y")

df %>% group_by(user.id) %>%
  mutate(n_comments = n()) %>% ungroup() %>%
  filter(n_comments > 110 | user.username == "Mensch") %>%
  group_by(hour = hour(date), user.username) %>%
  count() %>% ungroup() %>%
  ggplot(aes(x = hour, y = n, colour = user.username)) +
  geom_line() -> p

ggplotly(p)


# user --------------------------------------------------------------------

df %>% group_by(user.id, id_article) %>%
  mutate(n()) %>% group_by(user.id)




