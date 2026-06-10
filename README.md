# Buy-the-rumors-sell-the-news
![Python](https://img.shields.io/badge/Python-3.10-blue)
![R](https://img.shields.io/badge/R-4.3-blue)
![NLP](https://img.shields.io/badge/NLP-FinBERT-orange)
![Machine Learning](https://img.shields.io/badge/ML-Econometrics-red)
![Status](https://img.shields.io/badge/Project-Academic%20Case%20Study-blue)

## Project Overview

This project demonstrates applied skills in data analysis, econometrics, and financial machine learning using real-world market data. It combines NLP, econometrics, and financial time series modeling in a single end-to-end pipeline.

Using Apple Inc. (AAPL) as a real-world dataset, the project combines:

- Natural Language Processing (NLP) on financial news
- Time series financial data
- Econometric regression analysis
- Market volatility modeling

The goal is to understand how textual information translates into measurable financial market behavior.

## Tech Stack


### Python
- pandas (data processing)
- newspaper3k (web scraping & article extraction)
- os (file handling)

### R
- tidyquant (financial data)
- dplyr, tidyr, tibble (data manipulation)
- lubridate (time series handling)
- ggplot2 (visualization)
- car, lmtest, sandwich (econometric testing & robust SE)
- emmeans, interactions, sjPlot (interaction analysis & visualization)
- rsq (model evaluation)
- purrr, stringr, readr (data wrangling)
- reticulate (Python–R integration)

## Environment

- Python 3.10
- R 4.3
- Key packages listed in Tech Stack section

## Project Structure

Python scripts are used for data extraction (news scraping).

R scripts are used for:
- data cleaning
- feature engineering
- econometric modeling

The project is fully reproducible. Python is used for data acquisition, while R handles preprocessing, modeling and evaluation.

## Pipeline Overview

- News scraping (Python)  
- FinBERT sentiment analysis  
- Feature engineering (R)  
- Volatility construction  
- OLS regression + diagnostics

## How to Run

1. Run Python script to scrape financial news articles
2. Run R scripts for data cleaning and feature engineering
3. Run regression models in R
4. Results (tables and figures) are generated in /results


## Objective

This analysis aims to answer:

- Does news sentiment explain changes in stock volatility?
- How does news volume influence market reactions?
- Are effects linear or driven by extreme events?
- How important are broader market conditions compared to firm-specific news?

## Data Sources

The analysis combines:

- Financial news articles related to Apple (2016–2024)
- Daily OHLC stock data for Apple Inc. (AAPL)
- S&P 500 index data as market benchmark

Due to missing timestamps, news data is aggregated to weekly level.

## Methods

### Sentiment Analysis

- FinBERT (Transformer-based NLP model)
- Weekly sentiment scores derived from financial news text
- Aggregation of news volume per week

### Volatility Measures

Four different range-based volatility estimators are used:

- Parkinson volatility
- Garman-Klass volatility
- Rogers-Satchell volatility
- Yang-Zhang volatility


## Statistical Modeling

The relationship is estimated using OLS regression models, including:

- Lagged volatility (time dependence)
- News sentiment
- News volume
- Interaction effects (Sentiment × News Volume)
- Non-linear (quadratic) effects
- Market volatility (S&P 500)

## Results

These results are robust across four different volatility estimators.

- S&P 500 volatility is consistently the strongest predictor across all models
- Adjusted R² ranges between ~0.69 and ~0.73
- News volume shows a statistically significant U-shaped relationship with volatility
- Sentiment alone is not significant in linear form but becomes significant in interaction terms
- Interaction (Sentiment × News Volume) is significant in 3 out of 4 models
- Effects are conditional, not direct

## Key Insights
- Market-wide volatility (S&P 500) is the dominant and most consistent driver of Apple’s stock volatility across all model specifications.
- News volume exhibits a non-linear (U-shaped) relationship with volatility, indicating that both low and high information intensity are associated with higher market turbulence.
- News sentiment has limited standalone predictive power but becomes statistically significant in interaction with news intensity, suggesting that market reactions are context-dependent rather than sentiment-driven in isolation.
- Overall, financial markets respond to information in a conditional and non-linear manner rather than through simple linear effects.

## Practical implications

- Sentiment-based trading strategies are only meaningful when conditioned on news intensity and volatility regimes.
- Ignoring market context can lead to misleading signals in text-based financial models.
- Interaction effects are essential when designing NLP-driven trading or risk models.

## Model Diagnostics & Robustness

Across all four volatility models, the results are consistent and statistically robust.

### Model Fit
- Adjusted R² ranges between 0.69 and 0.74, indicating strong explanatory power across all specifications.
- The Yang-Zhang model achieves the highest explanatory power (Adj. R² ≈ 0.735).

### Functional Form (RESET Test)
- No strong evidence of functional misspecification in any model.
- All specifications are considered adequately specified after polynomial adjustments.

### Heteroskedasticity (Breusch-Pagan)
- Most models do not show strong evidence of heteroskedasticity.
- Minor issues appear in isolated specifications but do not affect inference due to robust standard errors.

### Normality of Residuals (Shapiro-Wilk)
- Residual normality is violated in some models (especially Parkinson and Garman-Klass).
- This is expected in financial return data and does not bias OLS estimates.

### Multicollinearity (VIF)
- All VIF values are below critical threshold (5), indicating no severe multicollinearity.
- Interaction terms increase VIF but remain within acceptable econometric bounds.


## Limitations

- Weekly aggregation due to missing timestamps
- No causal inference (associative model only)
- Potential endogeneity between sentiment, news, and volatility
- FinBERT is a black-box model with limited interpretability
- Results may be affected by market-wide shocks not fully controlled for

### Main takeaway

- The impact of sentiment on volatility accelerates for extreme levels of news intensity.
- Results suggest that volatility does not respond proportionally to neither sentiment or news volume, but rather increases at an increasing rate when either variable is extreme.

## What This Project Demonstrates

- End-to-end data pipeline design (data collection → modeling → evaluation)
- Integration of NLP and econometrics in financial applications
- Time series modeling with real-world market data
- Applied use of interaction effects and non-linear econometric models

## Skills Demonstrated

**Data Engineering**
- Data scraping (newspaper3k)
- Data cleaning & aggregation
- Time series structuring

**Machine Learning / NLP**
- FinBERT sentiment analysis
- Feature engineering from text data

**Econometrics**
- OLS regression modeling
- Interaction effects
- Non-linear modeling (polynomials)
- Robust standard errors (HAC)

**Financial Analytics**
- Volatility modeling
- Market data analysis (OHLC, S&P 500)
  
