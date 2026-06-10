### Model evaluation ###
library(lmtest)
library(broom)

model_park <- lm(log(Parkinson) ~ lag(log(Parkinson), 1)
                 + sentiment_c*news_c
                 + I(news_c)^2
                 + I(sentiment_c)^2
                 + I(log(volume)^2) 
                 + log(Parkinson_500)
                 + lag(log(Parkinson_500), 1)
                 + report, 
             data = AAPL_s_test)
summary(model_park)
vif(model_park)
rsq.partial(model_park)



### Assumptions ###

# Linearity

par(mfrow = c(1, 1))

plot(fitted(model_park), residuals(model_park),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")



plot(model_park, 1)
plot(model_park, 2)
plot(model_park, 3)
plot(model_park, 4)
# Linearity, numerically



resettest(model_park, type = "fitted")
help(resettest)

# Independence of errors

Box.test(
  residuals(model_park),
  lag = 3,
  type = "Ljung-Box"
)

plot(AAPL_s_test$week_start, residuals(model_park),
     type="l", xlab="Time (week)", ylab="Residuals",
     main="Residuals over Time")
abline(h=0, col="red")

# Homoscedasticity

bptest(model_park)

# No multicollinearity

vif(model_park)

cor(AAPL_s_test[, c("sentiment_c", "news_c", "volume", "Parkinson_500")])


