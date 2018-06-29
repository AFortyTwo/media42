library(RSelenium)
library(httr)
rd <- rsDriver()

remDr <- rd$client
remDr$open(silent = TRUE)
# url <- "https://derstandard.at/2000081710162/Oesterreich-verlangt-in-Spionageaffaere-volle-Aufklaerung-von-Deutschland"
url <- "https://derstandard.at/"
# remDr$addCookie(name = DSGVO_ZUSAGE_V1, value = "true")
remDr$navigate(url)
webElem <- remDr$findElement(using = 'css selector',"body > main > section > div > div.privacywall-text-container.privacywall-info > button")
webElem$clickElement()
remDr$navigate("https://derstandard.at/2000081710162/Oesterreich-verlangt-in-Spionageaffaere-volle-Aufklaerung-von-Deutschland")

page <- remDr$getPageSource()[[1]] %>% read_html()

posts <- page %>% html_nodes("#postinglist") %>% html_children()
get_comments <- function(posts){
  tibble(
    user = posts[1] %>% html_nodes(".upost-communityname") %>% html_text(),
    title = posts[1] %>% html_nodes(".upost-title") %>% html_text(),
    content = posts[1] %>% html_nodes(".upost-text") %>% html_text()
  )
}


