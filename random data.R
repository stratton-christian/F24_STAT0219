library(MASS)
data(SP500)
plot(SP500, type = "l")
acf(SP500)

web <- read_csv("daily-website-visitors.csv")

web_clean <- web %>%
  mutate(dt = mdy(Date))

web2017 <- web_clean %>%
  filter(year(dt) == 2017)

web_ts <- ts(
  web2017$Unique.Visits,
  start = c(1,1),
  freq = 7
)
plot(web_ts)
plot(decompose(web_ts))


hw <- HoltWinters(web_ts)
plot(hw)

acf(resid(hw))

arima_model <- arima(
  web_ts, 
  order = c(1, 1, 1),
  seasonal = list(order = c(1, 1, 1))
)
acf(resid(arima_model))


www <- "https://raw.githubusercontent.com/prabeshdhakal/Introductory-Time-Series-with-R-Datasets/refs/heads/master/cbe.dat"
aus_dat <- read.table(www, header = T)

elect_ts <- ts(
  aus_dat[,3],
  start = c(1958, 1),
  freq = 12
)
plot(elect_ts)
hw <- HoltWinters(elect_ts, seasonal = "mult")
acf(resid(hw))

ar_fit <- ar(resid(hw))

acf(na.omit(ar_fit$resid))
