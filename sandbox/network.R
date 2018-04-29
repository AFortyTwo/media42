library(ggraph)
library(tidygraph)
library(tidyverse)
library(plotly)
library(igraph)

# Network
df <- readRDS("export/other/merged.rds")

df %>% count(user) %>% arrange(desc(n)) %>%
  ggplot(aes(x = n)) + geom_histogram()
df <- df %>% group_by(user) %>%  mutate(n = n()) %>%
  filter(n>20) # %>% distinct(user)

r_count <- df %>% count(id_user) %>% na.omit()
r_corr <- df %>%
  # filter(!is.na(cat)) %>%
  semi_join(r_count) %>%
  group_by(id_article) %>%
  mutate(n = n_distinct(id_user)) %>% ungroup() %>%
  filter(n > 50) %>%
  widyr::pairwise_cor(id_user, id_article)

hist(r_corr$correlation)

# First indication of multiple usernames:
r_corr %>% arrange(desc(correlation))
# Todo relative frequency same article
# Dashboard Find similar

# Network
set.seed(2018)

# we set an arbitrary threshold of connectivity
r_corr %>%
  filter(correlation > .1) %>%
  graph_from_data_frame(vertices = r_count) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation)) +
  geom_node_point(aes(size = n), color = "lightblue") +
  theme_void() +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme(legend.position = "none")


df <- df %>%
  group_by(id_user) %>% mutate(d = n_distinct(id_article)) %>%
  filter(d > 50) %>%
  select(id_article, id_user) # %>%
  # filter(row_number() < 5000)

m <- df %>% count(id_article, id_user) %>% spread(id_article,n, fill = 0)
# g <- igraph::graph_from_data_frame(df %>% count(id_article, id_user))
g <- as_tbl_graph(m)

gd <- igraph::simplify(igraph::graph.data.frame(df, directed=FALSE))
igraph::vcount(g)
igraph::ecount(g)
edges <- df %>% select(source = )
