library(lubridate)

evi <- head(EVI_Export3, 50)
evi <- EVI_Export3 %>%
  select(date, `Site_ID/SSS`, mean) %>%
  pivot_wider(names_from = `Site_ID/SSS`, values_from = mean)

evi_monthly <- evi %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

start_year <- year(min(evi_monthly$month))
start_month <- month(min(evi_monthly$month))
frequency <- 12

evi_ts <- ts(evi_monthly[,-1], start = c(start_year, start_month), frequency = frequency)

fit <- bfast01(evi_ts[,1500])
plot(fit)
```