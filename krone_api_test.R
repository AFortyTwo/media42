library(jsonlite)
library(tidyverse)
library(rvest)

c = NULL
for (r in c("politik", "welt", "sport")) {
  print(r)
  krone <- read_html(paste0("http://www.krone.at/", r))
  articles <- krone %>% html_nodes(".imgteaser")

  # articles %>%
  #   map(html_nodes, ".l_title") %>%
  #   map(html_text, trim = TRUE)

  links <- tibble(
    # title = articles %>% html_nodes(".l_title") %>% html_text(trim = TRUE),
    title = articles %>% html_nodes("a") %>% html_text(trim = TRUE),
    url = articles %>% html_nodes("a") %>% html_attr("href"),
    id_article = url %>% str_extract("[:digit:]+")
  )
  df <- map_df(links$id_article, parse_article) %>% mutate(
    r = r
  )
  c <- rbind(c, df)
}

save(c, file = "scrape.RData")

parse_article <- function(id_article) {
  j = 2
  i = 1
  df <- NULL
  while (i <= j) {
    print(paste(i,j))
    url <- paste0(
      "http://api.krone.at/v1/comments/posts/",
      id_article,
      "/page/",
      i,
      "?limit=10&cors=true&domain=www.krone.at"
    )
    comments_json <- fromJSON(url, flatten = TRUE)
    comments <- comments_json$comments %>% as.tibble()
    df <- rbind(df, comments)

    j <- comments_json$maxPages
    i <- i + 1
  }
  df %>%
    mutate(
      content = gsub("<[^>]+>", "", .$content),
      id_article = id_article
    )
}




comments_json <- fromJSON("http://api.krone.at/v1/comments/posts/1687051/page/1?limit=10&cors=true&domain=www.krone.at",
                          flatten = TRUE)
j = 2
i = 1
df <- NULL
while (i <= j) {
  print(paste(i,j))
  comments <- comments_json$comments %>% as.tibble()
  df <- rbind(df, comments)

  j <- comments_json$maxPages
  i <- i + 1
}
df <- df %>%
  mutate(content = gsub("<[^>]+>", "", .$content))

