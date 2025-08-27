data <- read.csv("~/Desktop/stat_fin_2015_2025_unemployment.csv", header = TRUE)
names(data) <- c("Kuukausi", "Työttömyysaste")

install.packages("tidyr")
install.packages("dplyr")

library(tidyr)
library(dplyr)

install.packages("lubridate")
library(lubridate)

data$pvm <- as.Date(paste0(substr(data$Kuukausi, 1, 4), "-", substr(data$Kuukausi, 6, 7), "-01"))
data$kuukausi_muotoiltu <- format(data$pvm, "%m.%Y")

head(data)
data$Kuukausi <- NULL

names(data)[names(data) == "kuukausi_muotoiltu"] <- "kuukausi"
data$pvm <- NULL

install.packages("zoo")   # Asenna paketti

library(zoo)

# Muunna kuukausi sarake Date-tyyppiseksi
data$kuukausi <- as.yearmon(data$kuukausi, format = "%m.%Y")

# Järjestä data aikajärjestykseen
data <- data[order(data$kuukausi), ]

# Muunna aikasarjaksi
ts_data <- ts(data$Työttömyysaste, start = c(2015, 6), frequency = 12)
data$kuukausi <- as.yearmon(data$kuukausi, format = "%m.%Y")
str(data$kuukausi)
str(data$Työttömyysaste)
names(data)
head(data)
names(data) <- c("Työttömyysaste", "Kuukausi") 
library(zoo)

# Muunna Kuukausi yearmon-tyyppiseksi
data$Kuukausi <- as.yearmon(data$Kuukausi, format = "%b %Y")

# Järjestä data aikajärjestykseen
data <- data[order(data$Kuukausi), ]

ts_data <- ts(data$Työttömyysaste, start = c(2015, 6), frequency = 12)

install.packages("forecast")
library(forecast)

model <- auto.arima(ts_data)
ennuste <- forecast(model, h = 6)


ennustetut_arvot <- data.frame(
  kuukausi = as.yearmon(time(ennuste$mean)),
  tyottomyysaste = round(ennuste$mean, 2)
)

ennustetut_arvot$kuukausi <- format(ennustetut_arvot$kuukausi, "%b %Y")
print(ennustetut_arvot)
summary(model)
plot(ennuste, main = "Työttömyysasteen ennuste (ARIMA)")





