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
get_comments <- function(id_article, pb = NULL) {
  # progress bar magic!
  if (!is.null(pb)) pb$tick()$print()

  url <- paste0(
    "http://api.krone.at/v1/comments/posts/", id_article,
    "/page/1?limit=10&cors=true&domain=www.krone.at"
  )
  comments_json <- fromJSON(url, flatten = TRUE)
  total <- comments_json$total
  if (total > 0) {
    # print(paste("Article:", id_article, ", Comments", total))
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
    df %>% mutate(
      content = gsub("<[^>]+>", "", .$content),
      content = map_chr(content, unescape_html)
    )
  } else {
    # print(paste("Artikel", id_article, ": No comments"))
    return("no comments")
  }
}

#' Artikelinhalt extrahieren
#'
#' Ruft einen Artikel auf und speichert Inhalt und Metadaten
#'
#' @param id_article Artikel ID aus der URL
#'
#' @return Dataframe
#' @export
#'
#' @examples
get_article <- function(id_article, pb = NULL) {
  # progress bar magic!
  if (!is.null(pb)) pb$tick()$print()

  # Scrape
  url <- paste0("http://www.krone.at/", id_article)
  site <- s_read_html(url)
  if (!is.null(site$result)) {
    a <- site$result
    df <- list(
      date = a %>% html_nodes(".c_pretitle .c_time") %>%
        html_text() %>% lubridate::dmy_hm(tz = "CET"),
      author1 = a %>% html_node("em span") %>% html_text(trim = TRUE),
      author2 = a %>% html_node(".authorline__name") %>% html_text(trim = TRUE),
      title = a %>% html_nodes("h1") %>% html_text(),
      subtitle = a %>% html_nodes(".c_pretitle h2") %>% html_text(),
      lead = a %>% html_nodes(".c_lead") %>% html_text(),
      meta_description = a %>% html_node(xpath = '//meta[@name="description"]') %>% html_attr('content'),
      meta_ressort_id = a %>% html_node(xpath = '//meta[@name="krn-ressort-id"]') %>% html_attr('content'),
      meta_ressort_slug = a %>% html_node(xpath = '//meta[@name="krn-ressort-slug"]') %>% html_attr('content'),
      meta_template_id = a %>% html_node(xpath = '//meta[@name="krn-template-id"]') %>% html_attr('content'),
      meta_modified = a %>% html_node(xpath = '//meta[@name="krn-article-modified"]') %>% html_attr('content'),
      image_link = a %>% html_node(xpath = '//meta[@property="og:image"]') %>% html_attr('content'),
      text = a %>% html_nodes(".c_content") %>% html_text(trim = TRUE),
      para = a %>% html_nodes(".c_content") %>% html_nodes("p") %>% html_text()
      # page = as.character(a)
    )
    df[lengths(df) == 0] <- NA
    df <- df %>% as_tibble() %>% group_by_at(vars(-para)) %>% nest()
    # print(paste("Article:", id_article, ", Title: ", df$title))
    return(df)
    Sys.sleep(sample(seq(0,1,0.25), 1))
  } else {
    closeAllConnections()
    # print(paste("Artikel", id_article, ": Not found"))
    return("down")
  }
}


unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

s_read_html <- safely(read_html)

