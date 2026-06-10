



model_GK <- lm(log(GarmanKlass) ~ lag(log(GarmanKlass), 1) 
               + news_c*sentiment_c 
               + I(news_c^2) 
               + I(sentiment_c^2) 
               + I(log(volume)^2) 
               + log(GarmanKlass_500) 
               + report
               + lag(log(GarmanKlass_500), 1), 
                    data = AAPL_s_test)
summary(model_GK)
rsq.partial(model_GK)

### Få ut siffror till table ###

round(model_GK$coefficients, digits = 4)

coef_table <- summary(model_GK)$coefficients
std_errors <- coef_table[, "Std. Error"]
std_errors_rounded <- round(std_errors, 4)
std_errors_rounded

p_values <- summary(model_GK)$coefficients[, "Pr(>|t|)"]
p_values_rounded <- round(p_values, 4)

p_values_rounded

partial <- rsq.partial(model_GK)
round(partial$partial.rsq, digits = 4)
partial



### Assumptions ###

# Linearity

resettest(model_GK, type = "fitted") # Vill ha p > 0.05

df_plot <- data.frame(
  fitted = fitted(model_GK),
  residuals = residuals(model_GK)
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

plot(residuals(model_GK), type = "l",
     main = "Residuals vs Observation Index",
     ylab = "Residuals", xlab = "Observation")
abline(h = 0, col = "red")

Box.test(
  residuals(model_GK),
  lag = 3,
  type = "Ljung-Box"
)

# Homoscedasticity

bptest(model_GK) # Vill ha p > 0.05

# No multicollinearity

vif(model_GK) # 5 < är ok

# Normality of errors

shapiro.test(residuals(model_GK)) # Vill ha p > 0.05


par(mfrow = c(1, 2))
res <- residuals(model_GK)
hist(res, breaks = 50, freq = FALSE, col = "steelblue", xlab = "Residuals", main = "Residuals Histogram")
curve(dnorm(x, mean = mean(res), sd = sd(res)), add = TRUE, col = "red", lwd = 2)

qqnorm(residuals(model_GK))
qqline(residuals(model_GK), col = "red", lwd = 2)

# HAC # 

round(coeftest(model_GK, vcov = vcovHAC(model_GK, type = "HAC3")), digits = 4)



## Plots ###

par(mfrow = c(1, 1))

emmip(model_GK, news_c ~ sentiment_c, at = mylist, CIs = TRUE, type = "response")

johnson_neyman(model_GK, pred = "sentiment_c", modx = "news_c", 
               control.fdr = TRUE,  # optional, keeps it simple
               plot = TRUE)


robust_vcov


robust_vcov <- vcovHC(model_GK, type = "HC3")
em_robust <- emmeans(model_GK,
                     specs = ~ news_c | sentiment_c,
                     at = mylist,
                     vcov. = robust_vcov,
                     type = "response")
emmip(em_robust, news_c ~ sentiment_c, CIs = TRUE)

