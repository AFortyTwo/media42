library(tidyverse)
library(rvest)
# library(RSelenium)


#' Resort Links extrahieren
#'
#' Extrahiert eine Liste von Artikeln für ein Resort der Krone.
#'
#' @param resort Website extension für Krone resort (z.B. "oesterreich", "welt", "sport")
#'
#' @return Linkliste
#' @export
#'
#' @examples
get_links <- function(resort) {
  print(resort)
  krone <- read_html(paste0("http://www.krone.at/", resort))
  articles <- krone %>% html_nodes(".imgteaser")

  return(
    tibble(
      url = articles %>% html_nodes("a") %>% html_attr("href"),
      id_article = url %>% str_extract("[:digit:]+")
    ) %>% distinct()
  )
  Sys.sleep(sample(seq(0,1,0.25), 1))

  # tibble(
  #   # title = articles %>% html_nodes(".l_title") %>% html_text(trim = TRUE),
  #   pretitle = articles %>% map(html_nodes, ".imgteaser__pretitle")  %>% map(html_text ,trim = TRUE),
  #   title = articles %>% map(html_nodes, ".imgteaser__title")  %>% map(html_text ,trim = TRUE),
  #   url = articles %>% html_nodes("a") %>% html_attr("href"),
  #   id_article = url %>% str_extract("[:digit:]+")
  # ) %>% unnest() %>% distinct()
}

#' Kommentare aus einem Artikel extrahieren
#'
#' Ruft die Kronenzeitung API auf und läd alle Kommentare eines Artikels inklusive Diggs
#'
#' @param id_article Artikel ID aus der URL
#'
#' @return Dataframe mit Kommentaren und Diggs
#' @export
#'
#' @examples
get_comments <- function(id_article) {
  url <- paste0(
    "http://api.krone.at/v1/comments/posts/", id_article,
    "/page/1?limit=10&cors=true&domain=www.krone.at"
  )
  comments_json <- fromJSON(url, flatten = TRUE)
  total <- comments_json$total
  if (total > 0) {
    print(paste("Article:", id_article, ", Comments", total))
    df <- NULL
    for (i in 1:comments_json$maxPages) {
      url <- paste0(
        "http://api.krone.at/v1/comments/posts/",
        id_article,
        "/page/",
        i,
        "?limit=10&cors=true&domain=www.krone.at"
      )
      comments_json <- fromJSON(url, flatten = TRUE)
      comments <- comments_json$comments %>%
        as.tibble() %>%
        mutate(date = lubridate::ymd_hms(date.dateTime.date))
      # Retrieve diggs
      query = paste0(
        "http://api.krone.at/v1/comments/diggs?query=[",
        paste(comments$id, collapse = ","),
        "]&cors=true&domain=www.krone.at"
      )
      digs <- fromJSON(query, flatten = TRUE)
      if (length(digs$error) > 0) {
        print(digs$message)
        comments <- comments %>% mutate(positive = NA, negative = NA)
      } else {
        comments <- comments %>% left_join(
          digs, by = c("id" = "commentId")
        )
      }
      df <- rbind(df, comments)
      Sys.sleep(sample(seq(0,1,0.25), 1))
    }
    df %>% mutate(content = gsub("<[^>]+>", "", .$content))
  } else {
    print(paste("Artikel", id_article, ": No comments"))
    return("no comments")
  }
}

