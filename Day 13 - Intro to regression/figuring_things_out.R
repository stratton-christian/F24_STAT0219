# using GLS
library(nlme)
gls_fit <- gls(log_ap ~ t + t2 + month, ap_sub, correlation = corARMA(p = 1), method = "ML")
acf(resid(gls_fit, type = "normalized"))

summary(fit_log)
summary(gls_fit)
predict(
  gls_fit, newdata = ap_pred_tbl, se.fit = TRUE
)

#using arima
log_ap_ts <- ts(
  ap_sub$log_ap,
  start = c(1,1),
  freq = 12
)

ap_tbl2 <- ap_tbl %>%
  mutate(t2 = t^2) %>%
  mutate(t = c(scale(t)), t2 = c(scale(t2)))

arima_fit <- arima(
  log_ap_ts,
  order = c(1, 0, 0),
  include.mean = FALSE,
  xreg = model.matrix(~ t + t2 + month, ap_tbl2)[1:132,]
)

lm_fit <- lm(log(val) ~ t + t2 + month, ap_tbl2[1:132,])
summary(lm_fit)



fitted <- predict(arima_fit, newxreg = model.matrix(~ t + t2 + month, ap_tbl2)[1:132,])
fitted_ts <- ts(c(fitted$pred), start = c(1,1), freq = 12)
plot(log_ap_ts)
lines(fitted_ts, lty = 2, col = "red")


pred <- predict(
  arima_fit, 
  n.ahead = 12, 
  newxreg = model.matrix(~ t + t2 + month, ap_tbl2)[133:144,]
)
plot(pred$pred)



