library(data.table)
library(lubridate)
library(tidyverse)
dt <- fread("Indian_Airlines.csv", na.strings = c("", "NA", "null"))
print(dim(dt))
#View(dt)
#50 days of data were gathered, from February 11 to March 31 of 2022

#hypothesis, price is a function of days before departure, stops, duration
model <- lm(price ~ days_left + stops + duration, data = dt)
summary(model)
model <- lm(log(price) ~ days_left + stops + duration, data = dt)
summary(model)
names(dt)


#plotting line of best price v departure fit no scatter
ggplot(dt, aes(x = days_left, y = price)) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Prices increase as departure approaches",
       x = "Days Before Departure",
       y = "Price")

#plotting line of best fit price duration no scatter
ggplot(dt, aes(x = duration, y = price)) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "duration",
       x = "Duration of flight",
       y = "Price")


#plotting price vs days before departure
 ggplot(dt, aes(x = stops, y = price)) +
   geom_point(alpha = 0.05) +
   geom_smooth(method = "lm", color = "blue", se = FALSE) +
   labs(title = "Price vs Days Before Departure",
        x = "Days Before Departure",
        y = "Price") +
   theme_minimal()

#box plot of price distribution of stops 
ggplot(dt, aes(x = stops, y = price)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 3) +
  labs(title = "Price Distribution by Stops",
       x = "Stops",
       y = "Price") +
  theme_minimal()


#plotting price vs days left
ggplot(dt, aes(x = days_left, y = price)) +
  geom_jitter(alpha = 0.05, width = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Prices increase as departure approaches")
  ggsave("price_vs_days.png")

#checking for indigo
unique(dt$airline)
dt_indigo <- dt[airline == "Indigo"]
library(ggplot2)
 {
 ggplot(dt_indigo, aes(x = days_left, y = price)) +
   geom_point(alpha = 0.05) +
   geom_smooth(method = "lm", color = "blue", se = FALSE) +
   labs(title = "IndiGo: Prices increase as departure approaches",
        x = "Days Before Departure",
        y = "Price")
 
 model <- lm(price ~ days_left + stops + duration, data = dt_indigo)
 summary(model)
 model <- lm(log(price) ~ days_left + stops + duration, data = dt_indigo)
 summary(model)
 }