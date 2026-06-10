



model_RS <- lm(log(RogersSatchell) ~ lag(log(RogersSatchell), 1) 
               + news_c*sentiment_c
               + I(news_c^2) 
               + I(sentiment_c^2) 
               + I(log(volume)^2) 
               + log(RogersSatchell_500) 
               + report
               + lag(log(RogersSatchell_500), 1), 
               data = AAPL_s_test)
summary(model_RS)
rsq.partial(model_RS)

nonpoly_RS <- lm(log(RogersSatchell) ~ lag(log(RogersSatchell), 1) 
                 + sentiment_c*news_c
                 + I(news_c^2) 
                 + I(sentiment_c^2) 
                 + log(volume) 
                 + log(RogersSatchell_500) 
                 + report, 
                 data = AAPL_s_test)

AIC(model_RS, nonpoly_RS)
BIC(model_RS, nonpoly_RS)

### Få ut siffror till table ###

round(model_RS$coefficients, digits = 4)

coef_table <- summary(model_RS)$coefficients
std_errors <- coef_table[, "Std. Error"]
std_errors_rounded <- round(std_errors, 4)
std_errors_rounded

p_values <- summary(model_RS)$coefficients[, "Pr(>|t|)"]
p_values_rounded <- round(p_values, 4)

p_values_rounded

partial <- rsq.partial(model_RS)
round(partial$partial.rsq, digits = 4)
partial




### Assumptions ###

# Linearity

resettest(model_RS, type = "fitted") # Vill ha p > 0.05

df_plot <- data.frame(
  fitted = fitted(model_RS),
  residuals = residuals(model_RS)
)

ggplot(df_plot, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted Values",
    y = "Residuals"
  )

# Independence of errors


plot(residuals(model_RS), type = "l",
     main = "Residuals vs Observation Index",
     ylab = "Residuals", xlab = "Observation")
abline(h = 0, col = "red")

Box.test(
  residuals(model_RS),
  lag = 3,
  type = "Ljung-Box"
)

# Homoscedasticity

bptest(model_RS) # Vill ha p > 0.05

# No multicollinearity

vif(model_RS) # 5 < är ok

# Normality of errors

shapiro.test(residuals(model_RS)) # Vill ha p > 0.05

par(mfrow = c(1, 2))
res <- residuals(model_RS)
hist(res, breaks = 50, freq = FALSE, col = "steelblue", xlab = "Residuals", main = "Residuals Histogram")
curve(dnorm(x, mean = mean(res), sd = sd(res)), add = TRUE, col = "red", lwd = 2)

qqnorm(residuals(model_RS))
qqline(residuals(model_RS), col = "red", lwd = 2)


## Plots ###

emmip(model_RS, news_c ~ sentiment_c, at = mylist, CIs = TRUE, type = "response")

johnson_neyman(model_RS, pred = "sentiment_c", modx = "news_c", 
               control.fdr = TRUE,  # optional, keeps it simple
               plot = TRUE)



par(mfrow = c(4, 4))






### PRE DICK SHIN ####

AAPL_s_test$lag_RogersSatchell <- dplyr::lag(log(AAPL_s_test$RogersSatchell), 1)
AAPL_s_test$lag_RogersSatchell_500 <- dplyr::lag(log(AAPL_s_test$RogersSatchell_500), 1)




window_size <- 100
n <- nrow(AAPL_s_test)
preds <- numeric(n - window_size)

coef_names <- c("sentiment_c", "news_c", "sentiment_c:news_c")
coef_matrix <- matrix(NA, nrow = n - window_size, ncol = length(coef_names))
colnames(coef_matrix) <- coef_names


for(i in 1:(n - window_size)) { 
  train_data <- AAPL_s_test[i:(i + window_size -1), ]
  test_data <- AAPL_s_test[i + window_size, , drop = FALSE]
  
  model <- lm(log(RogersSatchell) ~ lag_RogersSatchell
              + sentiment_c*news_c
              + I(log(volume)^2) 
              + log(RogersSatchell_500) 
              + report, 
     data = train_data)
  
  preds[i] <- predict(model, newdata = test_data)
  
  for(j in seq_along(coef_names)) {
    coef_matrix[i, j] <- coef(model)[coef_names[j]]
  }
}

predict_YZ <- exp(preds)


actual <- AAPL_s_test$YangZhang[(window_size + 1):n]

mse <- mean((predict_YZ - actual)^2)
rmse <- sqrt(mse)
mae <- mean(abs(predict_YZ - actual))
r2 <- 1 - sum((predict_YZ - actual)^2) / sum((actual - mean(actual))^2)

mse
rmse
mae
r2


library(reshape2)

par(mfrow = c(1, 1))

coef_df <- data.frame(
  index = 1:(n - window_size),
  coef_matrix
)

coef_long <- melt(coef_df, id.vars = "index")

ggplot(coef_long, aes(x = index, y = value, color = variable)) +
  geom_line(size = 1) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(
    title = "Rolling Coefficients (Window = 100)",
    x = "Window End Index",
    y = "Coefficient Value"
  )



# HAC #

round(coeftest(model_RS, vcov = vcovHAC(model_GK, type = "HAC3")), digits = 4)
