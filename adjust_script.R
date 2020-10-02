# Required Packages 
library(readr)
library(dplyr)
library(ggplot2)

# Load Data
bike_rides <- read_csv("./data/SeoulBikeData.csv")

# Clean
bike_rides <- bike_rides %>% 
  mutate(date = as.POSIXct(date, format = "%d/%m/%Y"), 
         season = as.factor(season))

# Aggregate to daily intervals 
daily_df <- bike_rides %>% 
  filter(functioning_day == "Yes") %>% 
  group_by(date) %>% 
  summarise(count = sum(rented_bike_count), 
            temp = mean(temperature_C), 
            rainfall = sum(rainfall_mm), 
            season = last(season)) %>% 
  ungroup()


##### Simple Linear Regression #####

# Plot ride counts vs temperature 
daily_df %>% 
  ggplot(aes(x = temp, y = count, colour = "steelblue")) + 
  geom_point(colour = "steelblue") + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "black") + 
  geom_hline(yintercept = mean(daily_df$count), linetype = "dashed") + 
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Seoul Bike Rentals", 
       x = "Temperature (C)", 
       y = "No. of Rides") 

# Fit model
lm.simple <- lm(count ~ temp, data = daily_df)
summary(lm.simple)


##### Compare geographies ##### 

# Seoul rainfall per month 
seoul_rain <- daily_df %>% 
  mutate(month = format(date, "%m")) %>% 
  group_by(month) %>% 
  summarise(rainfall = sum(rainfall), season = last(season)) %>% 
  mutate(location = "Seoul") %>% 
  ungroup() 

# Compare with San Francisco 
# Load data
sf_data <- read_csv("./data/USW00023272.csv")

# Average monthly rainfall 
sf_rain <- sf_data %>% 
  select(DATE, PRCP, TAVG) %>% 
  filter(DATE >= "2008-12", DATE <= "2018-11") %>% 
  mutate(month = substr(DATE, 6, 8)) %>% 
  group_by(month) %>% 
  summarise(rainfall = mean(PRCP)) %>% 
  ungroup()

sf_rain$season <- seoul_rain$season 
sf_rain$location <- "San Francisco"

# Merge with Seoul data
rain_df <- rbind(seoul_rain, sf_rain)
rain_df$location <- factor(rain_df$location, levels = c("Seoul", "San Francisco"))

# Plot monthly rainfall of two cities 
rain_df %>% 
  ggplot(aes(x = month, y = rainfall, fill = season)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("lightblue", "tan1", "indianred2", "steelblue2")) + 
  labs(title = "Comparative Rainfall per Month", 
       x = "Month", 
       y = "Total Rainfall (mm)", 
       fill = "Season") + 
  theme() + 
  facet_wrap(~location, ncol = 1, scales = "free")


##### Mutliple Regression #####

# Flag rainy days (min. 2mm rainfall)
daily_df <- daily_df %>% 
  mutate(rain = ifelse(rainfall > 2, "Y", "N"))

# Group mean for plot
group_mean <- daily_df %>% 
  group_by(rain) %>% 
  summarise(mean = mean(count)) %>% 
  ungroup()

# Redo plot with rainy days identified 
daily_df %>% 
  ggplot(aes(x = temp, y = count)) + 
  geom_point(aes(colour = rain)) + 
  geom_smooth(aes(colour = rain), method = "lm", formula = y ~ x, se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "black") +
  scale_color_manual(values = c("tan1", "steelblue2")) + 
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept = unlist(group_mean[,2]), 
             colour = c("tan1", "steelblue2"), 
             linetype = "dashed") + 
  labs(title = "Seoul Bike Rentals", 
       x = "Temperature (C)", 
       y = "No. of Rides", 
       colour = "Rain") +
  theme(legend.position = c(0.925, 0.15)) 


# Fit model 
lm.multi <- lm(count ~ temp + rain, data = daily_df)
summary(lm.multi)  

