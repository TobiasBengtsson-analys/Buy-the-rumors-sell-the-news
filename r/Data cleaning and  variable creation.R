
library(tidyquant)
library(dplyr)
library(lubridate)
library(car)
library(interactions)
library(sjPlot)
library(ggplot2)
library(emmeans)
library(rsq)
library(tibble)
library(lmtest)
library(sandwich)





### Läs in data ###

alla_artiklar <- read.csv("AllaArtiklar.csv")

alla_artiklar_filtrerade <- alla_artiklar[grepl("AAPL", alla_artiklar$text), ]

df <- read.csv("apple_news_data.csv")

# write.csv(alla_artiklar, file = "AllaArtiklar.csv", row.names = FALSE, quote = TRUE)
# write.csv(alla_artiklar_filtrerade, file = "AllaArtiklarFiltrerade.csv", row.names = FALSE, quote = TRUE)

### Få in datum ###




alla_artiklar_filtrerade <- alla_artiklar_filtrerade |>
  dplyr::left_join(df |>
                     dplyr::select(link, date),
                   by = c("url" = "link"))


### AAPL data ###

AAPL <- tidyquant::tq_get("AAPL")



### Få in sentiment i AAPL ###

df_sent <- read.csv("df_sent.csv")


df_sent$date <- as.Date(df_sent$date)
df_sentAAPL <- df_sent %>%
  left_join(AAPL, by = "date")

df_sentAAPL$week <- strftime(df_sentAAPL$date, format = "%V")


### Ta bort dubletter ###



df_articles <- df_sentAAPL %>% 
  filter(!is.na(title)) %>%
  distinct(title, .keep_all = TRUE)


#### Lägg till unika veckor ####

AAPL_weekly <- df_articles %>%
  mutate( 
    week_start = date - (wday(date, week_start = 6) - 1)
  )

  
#### Är detta rätt? ####

AAPL_weekly_summary <- AAPL_weekly %>%
  group_by(week_start) %>%
  summarise(
    open = {
      x <- open[!is.na(open)]
      if (length(x) == 0)  NA_real_ else x[1]
    }, 
    close = {
      x <- close[!is.na(close)]
      if (length(x) == 0) NA_real_ else x[length(x)]
    }, 
    high = {
      x <- high[!is.na(high)]
      if (length(x) == 0) NA_real_ else max(x)
    }, 
    low = {
      x <- low[!is.na(low)]
      if (length(x) == 0) NA_real_ else min(x)
    }, 
    volume = {
      x <- volume[!is.na(volume)]
    if (length(x) == 0) NA_real_ else sum(x)
    }, 
    sentiment_sum = sum(sentiment_score, na.rm = TRUE), 
    .groups = "drop"
  )



######## Volatilitetsmåyy ########

AAPL_daily_weekly <- AAPL %>%
  mutate(
    week_start = date - (wday(date, week_start = 6) - 1)
  ) %>%
  arrange(date)


### Yang-Zhang ###

AAPL_daily_weekly <- AAPL_daily_weekly %>%
  mutate(
    prev_close = lag(close), 
    o = log(open/prev_close), 
    u = log(high/open), 
    d = log(low/open), 
    c = log(close/open)
  )
AAPL_YZ <- AAPL_daily_weekly %>%
  filter(!is.na(prev_close))

AAPL_YZ <- AAPL_YZ %>%
  mutate(
    RogersSatchell = log(high/open)*(log(high/open)-log(close/open)) + log(low/open)*(log(low/open) - log(close/open))
  )

YZ_weekly <- AAPL_YZ %>% 
  group_by(week_start) %>%
  summarise(
    n = n(), 
    V_O = if(n>1) var(o, na.rm = TRUE) else NA, 
    V_C = if(n>1) var(c, na.rm = TRUE) else NA, 
    V_RS = mean(RogersSatchell, na.rm = TRUE), 
    k = (0.34 / (1.34 + ((n + 1)/(n-1)))), 
    V = V_O + k*V_C + (1 - k)*V_RS
  ) %>%
  mutate(YangZhang = sqrt(V)) %>%
  ungroup()



### Lägg till YZ_vol till AAPL_weekly_summary ###

AAPL_weekly_summary <- AAPL_weekly_summary %>%
  inner_join(
    YZ_weekly %>% select(week_start, YangZhang), 
    by = "week_start"
  )



### Rogers-Satchell ###


RS_weekly <- AAPL_YZ %>%
  group_by(week_start) %>%
  summarise(
    n = n(),
    RogersSatchell = mean((log(high/open)*(log(high/open)-log(close/open)) + log(low/open)*(log(low/open) - log(close/open)))), 
  ) %>%
  mutate(RogersSatchell = sqrt(RogersSatchell)) %>%
  select(week_start, RogersSatchell)
  ungroup()

### Lägg till RogersSatchell i AAPL_weekly_summary ###

AAPL_weekly_summary <- AAPL_weekly_summary %>%
  inner_join(RS_weekly, by = "week_start")



### Garman Klass ###

GK_weekly <- AAPL_YZ %>%
  group_by(week_start) %>%
  summarise(
    n = n(), 
    GarmanKlass = mean(0.511*(log(high/low))^2 - 0.386*(log(close/open)^2))
  ) %>% 
  mutate(GarmanKlass = sqrt(GarmanKlass)) %>%
  select(week_start, GarmanKlass)


### Lägg till GarmanKlass i AAPL_weekly_summary ###

AAPL_weekly_summary <- AAPL_weekly_summary %>%
  inner_join(GK_weekly, by = "week_start")


### Parkinson (och ParkinsonAdj) ###

PK_weekly <- AAPL_YZ %>%
  group_by(week_start) %>%
  summarise(
    n = n(),
    Parkinson = mean((1/(4*log(2)))*(log(high/low))^2),
    ParkinsonAdj = mean(((1/(4*log(2)))*(log(high/low))^2) + (log(open/prev_close))^2)
  ) %>%
  mutate(Parkinson = sqrt(Parkinson), 
         ParkinsonAdj = sqrt(ParkinsonAdj)) %>%
  select(week_start, Parkinson, ParkinsonAdj)


### Lägg till Parkinson och ParkinsonAdj i AAPL_weekly_summary ###

AAPL_s_test <- AAPL_s_test %>%
  inner_join(PK_weekly, by = "week_start")

### Rensa bort sista datan ###

AAPL_weekly_summary <- AAPL_weekly_summary[-(1:43), ]








### Skapa fler förklarande variabler ####

### Lägg till news count

NC <- AAPL_weekly %>% 
  group_by(week_start) %>%
  summarise( 
    news_count = n() 
    ) %>% 
  ungroup()

NC <- NC[-(1:43), ]

AAPL_s <- AAPL_weekly_summary

AAPL_s <- AAPL_s %>%
  inner_join(NC, by = "week_start")



### Lägg till laggad YZ ###

AAPL_s <- AAPL_s %>%
  mutate(
    YZ_lag = lag(YangZhang, n = 1)
  )


### Lägg till S&P ###



SP500_daily <- tq_get("^GSPC", 
                      from = "2016-01-01", 
                      to = Sys.Date(), 
                      get = "stock.prices")

SP500_daily <- SP500_daily %>%
  mutate( 
    week_start = date - (wday(date, week_start = 6) - 1)
  ) %>%
  arrange(date)

SP500_daily <- SP500_daily %>% 
  mutate( 
    RogersSatchell = log(high/open)*(log(high/open)-log(close/open)) + log(low/open)*(log(low/open) - log(close/open))
    )

SP500_daily <- SP500_daily %>%
  mutate(
    prev_close = lag(close), 
    o = log(open/prev_close), 
    u = log(high/open), 
    d = log(low/open), 
    c = log(close/open)
  )


SP500_daily <- SP500_daily %>%
  group_by(week_start) %>%
  summarise(
    n = n(), 
    V_O = if(n>1) var(o, na.rm = TRUE) else NA, 
    V_C = if(n>1) var(c, na.rm = TRUE) else NA, 
    V_RS = mean(RogersSatchell, na.rm = TRUE), 
    k = (0.34 / (1.34 + ((n + 1)/(n-1)))), 
    V = V_O + k*V_C + (1 - k)*V_RS
  ) %>% 
  mutate(YZ_500 = sqrt(V))


SP500_daily <- SP500_daily[-(1:261), ]

SP500_daily <- SP500_daily[-(206:255), ]
SP500_daily <- SP500_daily[-(205), ]


AAPL_s_test <- AAPL_s_test %>%
  left_join(SP500_daily %>% select(week_start, YZ_500),
            by = "week_start")


## Park S&P500 ###

SP500_daily2 <- tq_get("^GSPC", 
                      from = "2016-01-01", 
                      to = Sys.Date(), 
                      get = "stock.prices")

SP500_daily2 <- SP500_daily2 %>%
  mutate(
    prev_close = lag(close),
    week_start = date - (wday(date, week_start = 6) - 1)
  ) %>%
  arrange(date)

PK_weekly2 <- SP500_daily2 %>%
  group_by(week_start) %>%
  summarise(
    n = n(),
    Parkinson_500 = mean((1/(4*log(2)))*(log(high/low))^2),
    ParkinsonAdj_500 = mean(((1/(4*log(2)))*(log(high/low))^2) + (log(open/prev_close))^2)
  ) %>%
  mutate(Parkinson_500 = sqrt(Parkinson_500), 
         ParkinsonAdj_500 = sqrt(ParkinsonAdj_500)) %>%
  select(week_start, Parkinson_500, ParkinsonAdj_500)

AAPL_s_test <- AAPL_s

AAPL_s_test <- AAPL_s_test %>%
  left_join(PK_weekly2 %>% select(week_start, Parkinson_500, ParkinsonAdj_500),
            by = "week_start")

### RS S&P500 ###


SP500_daily3 <- tq_get("^GSPC", 
                       from = "2016-01-01", 
                       to = Sys.Date(), 
                       get = "stock.prices")

SP500_daily3 <- SP500_daily2 %>%
  mutate(
    prev_close = lag(close),
    week_start = date - (wday(date, week_start = 6) - 1)
  ) %>%
  arrange(date)

RS_weekly2 <- SP500_daily3 %>%
  group_by(week_start) %>%
  summarise(
    n = n(),
    RogersSatchell_500 = mean((log(high/open)*(log(high/open)-log(close/open)) + log(low/open)*(log(low/open) - log(close/open)))), 
  ) %>%
  mutate(RogersSatchell_500 = sqrt(RogersSatchell_500)) %>%
  select(week_start, RogersSatchell_500)

AAPL_s_test <- AAPL_s_test %>%
  left_join(RS_weekly2 %>% select(week_start, RogersSatchell_500),
            by = "week_start")


### GK S&P 500 ###


GK_weekly2 <- SP500_daily3 %>%
  group_by(week_start) %>%
  summarise(
    n = n(), 
    GarmanKlass_500 = mean(0.511*(log(high/low))^2 - 0.386*(log(close/open)^2))
  ) %>% 
  mutate(GarmanKlass_500 = sqrt(GarmanKlass_500)) %>%
  select(week_start, GarmanKlass_500)

AAPL_s_test <- AAPL_s_test %>%
  left_join(GK_weekly2 %>% select(week_start, GarmanKlass_500),
            by = "week_start")






### Centrera sentiment_sum #######
AAPL_s_test <- AAPL_s_test %>%
  mutate(
    sentiment_c = sentiment_sum - mean(sentiment_sum, na.rm = TRUE),
    news_c = news_count - mean(news_count, na.rm = TRUE)
  )


# AAPL Earnings #
date <- c("2024-10-31",
          "2024-08-01",
          "2024-05-02",
          "2024-02-01",
          "2023-11-02",
          "2023-08-03",
          "2023-05-04",
          "2023-02-02",
          "2022-10-27",
          "2022-07-28",
          "2022-04-28",
          "2022-01-27",
          "2021-10-28",
          "2021-07-27",
          "2021-04-28",
          "2021-01-27")

Earnings <- as.data.frame(date)
Earnings$date <- as.Date(Earnings$date)


Earnings_week <- Earnings %>%
  mutate( 
    week_start = date - (wday(date, week_start = 6) - 1)
  ) %>%
  arrange(date)

Earnings_week$report <- 1

Earnings_week <- Earnings_week %>%
  mutate(report = 1)

# Left join with AAPL_s_test on week_start
AAPL_s_test <- AAPL_s_test %>%
  left_join(Earnings_week %>% select(week_start, report),
            by = "week_start") %>%
  mutate(report = if_else(is.na(report), 0, report))



