# ---------------------------
# 0) Paket
# ---------------------------
# install.packages(c("dplyr","readr","stringr","lubridate","tidyr","purrr","reticulate"))
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)
library(reticulate)

# ---------------------------
# 1) Läs in och bygg full_text
# ---------------------------
df <- read_csv("Alla_artiklar.csv", show_col_types = FALSE)

news <- df %>%
  mutate(
    full_text = str_squish(paste(Headlines, Description, sep = ". "))
  ) %>%
  select(-Headlines, -Description)

# ---------------------------
# 2) Extrahera datum ur Time och städa
#    Exempel: "9:45 AM ET Sat, 16 May 2020" -> "16 May 2020" -> Date
# ---------------------------
news_clean <- news %>%
  mutate(
    date_text = str_extract(Time, "\\d{1,2} \\w+ \\d{4}"),
    date      = dmy(date_text)
  ) %>%
  select(-Time, -date_text) %>%
  tidyr::drop_na(date, full_text)

# ---------------------------
# 3) Slå ihop alla texter per dag (om du vill FinBERT:a på dagsnivå)
# ---------------------------
news_daily <- news_clean %>%
  group_by(date) %>%
  summarise(
    full_text = str_squish(paste(full_text, collapse = " ")),
    n_articles = n(),
    .groups = "drop"
  )

# ---------------------------
# 4) Dela långa texter i chunkar (ord-baserat, max 512 ord per chunk)
#    (Enkelt och robust – räcker fint för FinBERT)
# ---------------------------
split_text <- function(text, max_words = 512) {
  w <- unlist(str_split(text, "\\s+"))
  if (length(w) <= max_words) return(list(paste(w, collapse = " ")))
  split_id <- ceiling(seq_along(w) / max_words)
  as.list(tapply(w, split_id, paste, collapse = " "))
}

news_split <- news_daily %>%
  mutate(chunks = map(full_text, split_text)) %>%
  select(date, chunks) %>%
  unnest(chunks) %>%
  rename(full_text = chunks)

# (valfritt) snabb kontroll: max ord i någon chunk
# news_split %>% mutate(n_words = str_count(full_text, "\\S+")) %>% summarise(max(n_words))

# ---------------------------
# 5) FinBERT via reticulate (Transformers)
# ---------------------------
# Rekommenderat: skapa separat python-miljö (kör EN gång)
# virtualenv_create("r-finbert")
# virtualenv_install("r-finbert", c("transformers","torch","sentencepiece","accelerate"))

use_virtualenv("r-finbert", required = FALSE)  # sätt TRUE om du skapat env ovan

tf <- import("transformers")
tokenizer <- tf$AutoTokenizer$from_pretrained("yiyanghkust/finbert-tone")
model     <- tf$AutoModelForSequenceClassification$from_pretrained("yiyanghkust/finbert-tone")
pipe      <- tf$TextClassificationPipeline(
  model = model, tokenizer = tokenizer,
  return_all_scores = TRUE
)

# ---------------------------
# 6) Kör FinBERT på alla chunkar
# ---------------------------
preds <- pipe(
  news_split$full_text,
  truncation = TRUE,
  max_length = as.integer(512),
  padding = "longest"
)

# ---------------------------
# 7) Mappa output -> data.frame med sannolikheter
#    (Positive / Neutral / Negative), avrunda, och räkna sentiment-score
# ---------------------------
sentiment_df <- map_dfr(preds, function(x) {
  # x är listan med tre element (label/score)
  labs   <- tolower(vapply(x, function(y) y$label, character(1)))
  scores <-        vapply(x, function(y) y$score, numeric(1))
  tibble(
    positive = scores[match("positive", labs)],
    neutral  = scores[match("neutral",  labs)],
    negative = scores[match("negative", labs)]
  )
}) %>%
  mutate(
    positive = round(positive, 3),
    neutral  = round(neutral,  3),
    negative = round(negative, 3),
    sentiment = round(positive - negative, 3)
  )

news_scored_chunks <- bind_cols(news_split, sentiment_df)

# ---------------------------
# (Valfritt) Kategorilabel + fördelning
# ---------------------------
news_scored_chunks <- news_scored_chunks %>%
  mutate(
    sentiment_label = case_when(
      sentiment >=  0.5 ~ "Positive",
      sentiment <= -0.5 ~ "Negative",
      TRUE              ~ "Neutral"
    )
  )

# Fördelning (antal och %)
# table(news_scored_chunks$sentiment_label)
# prop.table(table(news_scored_chunks$sentiment_label)) * 100

# ---------------------------
# 9) Aggregera tillbaka till daglig nivå (medel & andelar)
# ---------------------------
daily_sentiment <- news_scored_chunks %>%
  group_by(date) %>%
  summarise(
    sent_mean    = mean(sentiment, na.rm = TRUE),
    prop_pos_ext = mean(sentiment >=  0.5, na.rm = TRUE),
    prop_neg_ext = mean(sentiment <= -0.5, na.rm = TRUE),
    n_chunks     = n(),
    .groups = "drop"
  )

# Klart: 'news_scored_chunks' har chunk-vis FinBERT, 'daily_sentiment' har dagsnivå