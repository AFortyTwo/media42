library(tidyverse)
library(rvest)
library(RSelenium)

#' Krone kommentare extrahieren
#'
#' Parsed aus einer Seite der Krone alle Kommentare. Seite muss vorher mit
#' remDr$navigate(url) angesteuert werden. Danach steuert die Funktion durch
#' alle Seiten der Kommentare durch und ruft `parse_comments()`, um die Kommentare
#' einzulesen.
#'
#' @examples
#' rd <- rsDriver(browser = "firefox")
#' remDr <- rd$client
#' remDr$open(silent = TRUE)
#'
#' url <- "http://www.krone.at/1676695"
#' remDr$navigate(url)
#' extract_comments()
extract_comments <- function(article){
  page <- remDr$getPageSource()[[1]] %>% read_html()
  # remDr$screenshot(display = TRUE)
  n_comments <- page %>% html_node(".c_count") %>% html_text(trim = TRUE) %>% parse_integer()
  next_button <- remDr$findElement(using = "css selector", value = ".next")
  df <- NULL
  for (i in 1:floor(n_comments / 10)) {
    page <- remDr$getPageSource()[[1]] %>% read_html()
    comments <- page %>%
      html_nodes(".c_comment")
    df <- rbind(df, map_df(comments, parse_comments))  # Maybe it is better to keep the comments in lists
    next_button$clickElement()
    Sys.sleep(sample(seq(0,1,0.25), 1))
    print(i)
  }
  df$article = remDr$getTitle()[[1]]
  df$id_article = article
  return(df)
}

#' Kommentare auslesen
#'
#' Liest eine Seite der Kommentare aus

#' @param comments
#'
#' @return
#' @export
#'
#' @examples
parse_comments <- function(comments){
  list(
    name = comments %>% html_node(".c_name") %>% html_text(),
    date = comments %>% html_node(".c_datetime") %>% html_text(trim = TRUE) %>%
      str_split(",") %>% map_chr(2) %>%
      parse_datetime(format = "%d. %B %Y %H:%M", locale = locale("de")),
    id_krone = comments %>% html_node(".c_id") %>% html_text(),
    text = comments %>% html_nodes("p") %>% html_text() %>% paste(collapse = " "),
    up = comments %>% html_node(".c_up") %>% html_node(".c_count") %>%  html_text(trim = TRUE) %>% parse_integer(),
    down = comments %>% html_node(".c_down") %>% html_node(".c_count") %>%  html_text(trim = TRUE) %>% parse_integer()
  )
}

