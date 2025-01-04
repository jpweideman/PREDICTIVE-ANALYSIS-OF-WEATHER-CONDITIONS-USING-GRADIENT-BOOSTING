# Load necessary libraries
library(ggplot2)
library(dplyr)
library(GGally)
library(gridExtra)

# Import the dataset
data <- read.csv("Data/Szeged_cleaned.csv", header = TRUE, sep = ",")
data <- data %>% mutate_if(is.character, as.factor)
head(data)
summary(data)
str(data)

# Correlation matrix
numeric_data <- data %>%
  select(where(is.numeric)) %>%
  select(-X) # Remove irrelevant columns
cor_matrix <- cor(numeric_data, use = "complete.obs")
ggcorrplot::ggcorrplot(cor_matrix, lab = TRUE, title = "Correlation Matrix")
# Save the plot 
ggsave("Plots/Correlation_matrix.png", plot = last_plot(), device = "png", bg = "white")

# Plot temperature and apparent temperature as a function of time
ggplot(aggregated_data, aes(x = Date)) +
  geom_line(aes(y = Mean_Temperature, color = "Temperature"), size = 1) +
  geom_line(aes(y = Mean_Apparent_Temperature, color = "Apparent Temperature"), size = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Smoothed Temperature and Apparent Temperature Trends",
       x = "Date", y = "Temperature (°C)") +
  theme_minimal() +
  scale_color_manual(name = "Legend", values = c("Temperature" = "blue", "Apparent Temperature" = "red"))
# Save the plot
ggsave("Plots/Temperature_time.png", plot = last_plot(), device = "png", bg = "white")

# Plot the correlation between temperature and apparent temperature as a function of various weather features
# Temperature vs Humidity
plot1 <- ggplot(data, aes(x = Humidity, y = Temperature..C.)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(title = "Temperature vs Humidity",
       x = "Humidity", y = "Temperature (C)")
# Temperature vs Visibility
plot2 <- ggplot(data, aes(x = Visibility..km., y = Temperature..C.)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(title = "Temperature vs Visibility",
       x = "Visibility (km)", y = "Temperature (C)")
# Temperature vs Pressure
plot3 <- ggplot(data, aes(x = Pressure..millibars., y = Temperature..C.)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(title = "Temperature vs Pressure",
       x = "Pressure (millibars)", y = "Temperature (C)")
# Temperature vs Wind Speed
plot4 <- ggplot(data, aes(x = Wind.Speed..km.h., y = Temperature..C.)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(title = "Temperature vs Wind Speed",
       x = "Wind Speed (km/h)", y = "Temperature (C)")
# Arrange the plots into a single image
combined_plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
# Save the combined plot
ggsave("Plots/Combined_Temperature_Plots.png", plot = combined_plot, width = 10, height = 12, bg = "white")

# Apparent Temperature vs Humidity
plot1_apparent <- ggplot(data, aes(x = Humidity, y = Apparent.Temperature..C.)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(title = "Apparent Temperature vs Humidity",
       x = "Humidity", y = "Apparent Temperature (C)")
# Apparent Temperature vs Visibility
plot2_apparent <- ggplot(data, aes(x = Visibility..km., y = Apparent.Temperature..C.)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(title = "Apparent Temperature vs Visibility",
       x = "Visibility (km)", y = "Apparent Temperature (C)")
# Apparent Temperature vs Pressure
plot3_apparent <- ggplot(data, aes(x = Pressure..millibars., y = Apparent.Temperature..C.)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(title = "Apparent Temperature vs Pressure",
       x = "Pressure (millibars)", y = "Apparent Temperature (C)")
# Apparent Temperature vs Wind Speed
plot4_apparent <- ggplot(data, aes(x = Wind.Speed..km.h., y = Apparent.Temperature..C.)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(title = "Apparent Temperature vs Wind Speed",
       x = "Wind Speed (km/h)", y = "Apparent Temperature (C)")
# Arrange the plots into a single image
combined_plot_apparent <- grid.arrange(plot1_apparent, plot2_apparent, plot3_apparent, plot4_apparent, ncol = 2)
# Save the combined plot
ggsave("Plots/Combined_Apparent_Temperature_Plots.png", plot = combined_plot_apparent, width = 10, height = 12, bg = "white")




