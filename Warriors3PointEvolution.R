# title: Warriors 3 Point Evolution
# author: Abdullah Nasir
# date: 15/03/2026
# ---

# Load library --------------------------------------------------

library(prophet)

# Load dataset --------------------------------------------------

team_stats <- read.csv("data/TeamStatistics.csv")

# Prepare time series -------------------------------------------

warriors_stats <- subset(team_stats, teamName == "Warriors")
warriors_stats$gameDateTimeEst <- as.Date(warriors_stats$gameDateTimeEst)

warriors_ts <- data.frame(ds = warriors_stats$gameDateTimeEst, y = warriors_stats$threePointersAttempted)
warriors_ts <- subset(warriors_ts, ds >= as.Date("1979-01-01"))

warriors_ts <- na.omit(warriors_ts)
warriors_ts <- warriors_ts[order(warriors_ts$ds), ]

# Full time series plot -----------------------------------------

plot(warriors_ts$ds, warriors_ts$y, type = "l", xlab = "Date", ylab = "Three-Point Attempts", main = "Golden State Warriors Three-Point Attempts Per Game (1979-Present)")

# Modern era plot -----------------------------------------------

modern_era <- subset(warriors_ts, ds >= as.Date("2014-01-01"))

plot(modern_era$ds, modern_era$y, type = "l", xlab = "Date", ylab = "Three-Point Attempts", main = "Warriors Three-Point Attempts Per Game in the Modern Era")

# Season in focus -----------------------------------------------

season_2019 <- subset(warriors_ts, ds >= as.Date("2019-10-01") & ds <= as.Date("2020-04-01"))

plot(season_2019$ds, season_2019$y, type = "l", xlab = "Date", ylab = "Three-Point Attempts", main = "Warriors Three-Point Attempts During the 2019-2020 Season")

# Forecasting model ---------------------------------------------

warriors_model <- prophet(warriors_ts)

future <- make_future_dataframe(warriors_model, periods = 365)

forecast <- predict(warriors_model, future)

# Forecast plot -------------------------------------------------

plot(warriors_model, forecast)

