library(quanteda)
x <- corpus(df, text_field = "content_comment")
summary(x)
kwic(x, "kern")

stopwords <- tokenizers::stopwords("de") %>% append(
  c("dass", "ja", "mehr", "mal")
)
model <- dfm(x, tolower = TRUE, stem = FALSE, remove_punct = TRUE, remove = stopwords)
textstat_frequency(model)

