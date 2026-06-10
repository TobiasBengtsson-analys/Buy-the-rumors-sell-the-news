
round(coeftest(model_YZ, vcov = vcovHAC(model_YZ, type = "HAC3")), digits = 4)

model_YZ <- lm(log(YangZhang) ~ lag(log(YangZhang), 1) 
               + news_c*sentiment_c
               + I(news_c^2)
               + I(sentiment_c^2)
               + I(log(volume)^2) 
               + log(YZ_500)
               + lag(log(YZ_500), 1)
               + report, 
               data = AAPL_s_test)
summary(model_YZ)
rsq.partial(model_YZ)

### AIC/BIC ###
summary(poly_YZ)
poly_YZ <- lm(log(YangZhang) ~ lag(log(YangZhang), 1)
              + news_c
              + sentiment_c
              + I(sentiment_c^2)
              + I(news_c^2)
              + I(log(volume)^2) 
              + log(YZ_500.x) 
              + lag(log(YZ_500.x), 1) 
              + report, 
              data = AAPL_s_test)

AIC(model_YZ, poly_YZ)
BIC(model_YZ, poly_YZ)


linearHypothesis(
  model_YZ,
  c(
    "sentiment_c = 0",
    "I(sentiment_c^2) = 0",
    "news_c:sentiment_c = 0", 
    "I(news_c^2) = 0", 
    "news_c = 0"
  ),
  vcov = vcovHC(model_YZ, type = "HC1")
)


coeftest(model_YZ, vcov = vcovHC(model_YZ, type = "HC1"))


### Få ut siffror till table ###

round(model_YZ$coefficients, digits = 4)

coef_table <- summary(model_YZ)$coefficients
std_errors <- coef_table[, "Std. Error"]
std_errors_rounded <- round(std_errors, 4)
std_errors_rounded

p_values <- summary(model_YZ)$coefficients[, "Pr(>|t|)"]
p_values_rounded <- round(p_values, 4)

p_values_rounded

partial <- rsq.partial(model_YZ)
round(partial$partial.rsq, digits = 4)
partial



### Assumptions ###

# Linearity

resettest(model_YZ, type = "fitted") # Vill ha p > 0.05

df_plot <- data.frame(
  fitted = fitted(model_YZ),
  residuals = residuals(model_YZ)
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


plot(residuals(model_YZ), type = "l",
     main = "Residuals vs Observation Index",
     ylab = "Residuals", xlab = "Observation")
abline(h = 0, col = "red")

Box.test(
  residuals(model_YZ),
  lag = 3,
  type = "Ljung-Box"
)

# Homoscedasticity

bptest(model_YZ) # Vill ha p > 0.05

# No multicollinearity

vif(model_YZ) # 5 < är ok

# Normality of errors

shapiro.test(residuals(model_YZ)) # Vill ha p > 0.05

par(mfrow = c(1, 2))
res <- residuals(model_YZ)
hist(res, breaks = 50, freq = FALSE, col = "steelblue", xlab = "Residuals", main = "Residuals Histogram")
curve(dnorm(x, mean = mean(res), sd = sd(res)), add = TRUE, col = "red", lwd = 2)

qqnorm(residuals(model_YZ))
qqline(residuals(model_YZ), col = "red", lwd = 2)


### Plots ###
par(mfrow = c(1, 1))

emmip(model_YZ, news_c ~ sentiment_c, at = mylist, CIs = TRUE, type = "response")

johnson_neyman(model_YZ, pred = "sentiment_c", modx = "news_c", 
               control.fdr = TRUE,  # optional, keeps it simple
               plot = TRUE)







 