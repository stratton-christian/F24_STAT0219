library(tidyverse)
dat <- read_csv("56-32 10sec data 27029986.csv")
utah_forge_56_32_2021_02_08_03_04 <- dat %>%
  filter(`YYYY/MM/DD` == "2021-02-08") %>%
  filter(hour(`HH:MM:SS`) < 4 & hour(`HH:MM:SS`) >= 3)

write_csv(utah_forge_56_32_2021_02_08_03_04, file = "utah_forge_56_32_2021_02_08_03_04.csv")
