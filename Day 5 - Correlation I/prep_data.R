library(readr)
mt_aqi <- read_csv("Montana_Air_Quality_Monitoring_Data.csv")
# from: https://discover-mtdeq.hub.arcgis.com/datasets/7ff8441bb7014aa1b8a1da58f51c16db_0/explore?filters=eyJzaXRlbmFtZSI6WyJCb3plbWFuIl0sImRhdGV0aW1lIjpbMTUxNDUwNTEyOTUxNy40NywxNzI2OTg0ODAwMDAwXX0%3D&location=46.335398%2C-109.995100%2C7.00&showTable=true

# prepare data
mt_aqi_pm25_sept2020 <- mt_aqi %>%
  filter(parameter == "PM25") %>%
  mutate(dt = ymd_hms(datetime)) %>%
  arrange(dt) %>%
  filter(
    dt >= "2020-09-01 00:00:00",
    dt <= "2020-09-30 00:00:00"
  ) %>%
  dplyr::select(-dt)

# impute some missing values
mt_pm_clean <- mt_aqi_pm25_sept2020 %>%
  mutate(dt = ymd_hms(datetime)) %>%
  dplyr::select(dt, rawvalue, everything()) %>%
  arrange(dt)

# create ts
boz_pm_ts <- ts(
  mt_pm_clean$rawvalue,
  start = c(1, 5),
  end = c(30, 5),
  freq = 24
)

# impute missing values
library(imputeTS)
tmp <- imputeTS::na_kalman(boz_pm_ts, model = "auto.arima")
mt_aqi_pm25_sept2020$rawvalue <- c(tmp)


saveRDS(mt_aqi_pm25_sept2020, file = "mt_pm25_sept2020.rds")
