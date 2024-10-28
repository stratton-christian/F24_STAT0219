









```{r, indent = "    "}
tmp <- predict(
  ss_fit, n.ahead = nrow(forecast_df),
  newxreg = model.matrix(~ t + sint + sin2t + sin3t + cost + cos2t + cos3t, forecast_df)
)

cbind(
  c(tmp$pred) - qt(.975, 130)*c(tmp$se),
  c(tmp$pred) + qt(.975, 130)*c(tmp$se)
)

# a few things
sim_ts <- ts(
  obs_df$y,
  freq = 7
)
X <- model.matrix(~ t + sint + sin2t + sin3t + cost + cos2t + cos3t, obs_df)

ss_fit <- arima(
  sim_ts,
  order = c(0, 0, 0),
  xreg = X,
  include.mean = F
)
ss_fit

tmp <- predict(
  ss_fit, 
  n.ahead = nrow(forecast_df),
  newxreg = model.matrix(~ t + sint + sin2t + sin3t + cost + cos2t + cos3t, forecast_df)
)
tmp$se


predict(ols_fit, newdata = forecast_df, se.fit = T)
```














```{r, eval = F}
pred_sefit <- predict(ols_fit, newdata = forecast_df, se.fit = T)

# matches pred exactly
cbind(
  pred_sefit$fit - qt(0.975, 132)*pred_sefit$se.fit,
  pred_sefit$fit + qt(0.975, 132)*pred_sefit$se.fit
)


# now prediction
pred3 <- predict(ols_fit, newdata = forecast_df, interval = "prediction")
pred3

# matches pred3 
cbind(
  pred_sefit$fit - qt(0.975, 132)*sqrt(pred_sefit$residual.scale^2 + pred_sefit$se.fit^2),
  pred_sefit$fit + qt(0.975, 132)*sqrt(pred_sefit$residual.scale^2 + pred_sefit$se.fit^2)
)

```

