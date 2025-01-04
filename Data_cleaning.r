# Load necessary library
library(readr)
library(ggplot2)

# Import the dataset
data <- read.csv("Data/Szeged.csv", header = TRUE, sep = ";")
head(data)

# Summary statistics
summary(data)

# Check structure and data types
str(data)
#we see that data is not in the corect format (Numerical values are characters etc.), we need to convert it to the correct format
#delete the Temperature column and the Apparent Temperature column


#1) Convert the data types

data$Formatted.Date <- as.Date(data$Formatted.Date, format = "%Y-%m-%d")
data$Summary <- as.factor(data$Summary)
data$Precip.Type <- as.factor(data$Precip.Type)

data$Temperature..C. <- gsub(",", ".", data$Temperature..C.)
data$Temperature..C. <- as.numeric(data$Temperature..C.)

data$Apparent.Temperature..C. <- gsub(",", ".", data$Apparent.Temperature..C.)
data$Apparent.Temperature..C. <- as.numeric(data$Apparent.Temperature..C.)

data$Humidity <- gsub(",", ".", data$Humidity)
data$Humidity <- as.numeric(data$Humidity)

data$Wind.Speed..km.h. <- gsub(",", ".", data$Wind.Speed..km.h.)
data$Wind.Speed..km.h. <- as.numeric(data$Wind.Speed..km.h.)

data$Visibility..km. <- gsub(",", ".", data$Visibility..km.)
data$Visibility..km. <- as.numeric(data$Visibility..km.)

data$Pressure..millibars. <- gsub(",", ".", data$Pressure..millibars.)
data$Pressure..millibars. <- as.numeric(data$Pressure..millibars.)

# make data a data frame
data <- as.data.frame(data)
head(data)



#2) clean each column separately

## Summary column

table(data$Summary)
summary_df <- as.data.frame(table(data$Summary))

# make a bar plot of the summary column
ggplot(summary_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Summary", y = "Frequency", title = "Bar Plot of Summary Column") +
  scale_fill_discrete(name = "Summary") +
  theme(legend.position = "none")
# Save the plot to the Plots folder
ggsave(filename = "Plots/summary_bar_plot.png", plot = last_plot(), width = 10, height = 6)

# Aggregate the summary column to reduce the number of levels to the most common ones (as seen in the bar plot)
library(dplyr)
# Create a named vector for the mappings
summary_mapping <- c(
  "Breezy" = "Clear",
  "Breezy and Dry" = "Clear",
  "Breezy and Foggy" = "Foggy",
  "Breezy and Mostly Cloudy" = "Mostly Cloudy",
  "Breezy and Overcast" = "Overcast",
  "Breezy and Partly Cloudy" = "Partly Cloudy",
  "Dangerously Windy and Partly Cloudy" = "Partly Cloudy",
  "Drizzle" = "Overcast",
  "Dry" = "Clear",
  "Dry and Mostly Cloudy" = "Mostly Cloudy",
  "Dry and Partly Cloudy" = "Partly Cloudy",
  "Humid and Mostly Cloudy" = "Mostly Cloudy",
  "Humid and Overcast" = "Overcast",
  "Humid and Partly Cloudy" = "Partly Cloudy",
  "Light Rain" = "Overcast",
  "Rain" = "Overcast",
  "Windy" = "Clear",
  "Windy and Dry" = "Clear",
  "Windy and Foggy" = "Foggy",
  "Windy and Mostly Cloudy" = "Mostly Cloudy",
  "Windy and Overcast" = "Overcast",
  "Windy and Partly Cloudy" = "Partly Cloudy"
)

# Recode the Summary column
data$Summary <- recode(data$Summary, !!!summary_mapping)
# Remove levels with frequency 0
data$Summary <- droplevels(data$Summary)

# Create a data frame for the Summary column after aggregation
summary_df1 <- as.data.frame(table(data$Summary))
summary_df1

# make a bar plot of the summary column after aggregation
colors <- ggplot_build(plot1)$data[[1]]$fill
names(colors) <- summary_df$Var1  # Map colors to categories
ggplot(summary_df1, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Summary", y = "Frequency", title = "Bar Plot of Summary Column") +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none")
# Save the plot to the Plots folder
ggsave(filename = "Plots/summary_bar_plot_aggregated.png", plot = last_plot(), width = 10, height = 6)

# Checking missing values in Summary column
missing_summary <- sum(is.na(data$Summary))
missing_summary # 0 missing values


## Precip.Type column

table(data$Precip.Type)
# Visualize the Precip.Type column as a pie chart
precip_df <- as.data.frame(table(data$Precip.Type))
colnames(precip_df) <- c("Precip.Type", "Count")
precip_df
ggplot(precip_df, aes(x = "", y = Count, fill = Precip.Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Precip.Type Column") +
  theme_void() +
  theme(legend.title = element_blank())
# Save the pie chart to the Plots folder
ggsave(filename = "Plots/precip_type_pie_chart.png", plot = last_plot(), width = 8, height = 6, bg = "white")


# calculate null as a percentage of the total entries
null_count <- sum(data$Precip.Type == "null", na.rm = TRUE)
total_count <- nrow(data)
null_percentage <- (null_count / total_count) * 100
null_percentage # 0.5360124%

# box plot of Temperature vs Precip.Type
ggplot(data, aes(x = Precip.Type, y = Temperature..C., fill = Precip.Type)) +
  geom_boxplot() +
  labs(title = "Temperature vs Precipitation Type", x = "Precipitation Type", y = "Temperature") +
  theme_minimal()
# Save the box plot to the Plots folder
ggsave(filename = "Plots/temperature_vs_precip_type_box_plot.png", plot = last_plot(), width = 6, height = 6, bg = "white")

# add null values to the rain category and remove the null category
data$Precip.Type <- recode(data$Precip.Type, "null" = "rain")
data$Precip.Type <- droplevels(data$Precip.Type)

# check for missing values in Precip.Type column
missing_precip <- sum(is.na(data$Precip.Type))
missing_precip # 0 missing values


## Temperature..C. column

# Summary statistics of the Temperature column
summary(data$Temperature)

# Plot 2: Boxplot of Temperature (to highlight outliers)
ggplot(data, aes(x = "", y = Temperature..C.)) +
  geom_boxplot(fill = "orange", color = "black", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Temperature", x = "", y = "Temperature") +
  theme_minimal()
# Save the box plot to the Plots folder
ggsave(filename = "Plots/temperature_box_plot.png", plot = last_plot(), width = 4, height = 6, bg = "white")

# Check for missing values in the Temperature column
missing_values <- sum(is.na(data$Temperature))
missing_values # 0 missing values


## Apparent.Temperature..C. column

# Summary statistics of the Apparent Temperature column
summary(data$Apparent.Temperature..C.)

# Plot 3: Boxplot of Apparent Temperature (to highlight outliers)
ggplot(data, aes(x = "", y = Apparent.Temperature..C.)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Apparent Temperature", x = "", y = "Apparent Temperature") +
  theme_minimal()
# Save the box plot to the Plots folder
ggsave(filename = "Plots/apparent_temperature_box_plot.png", plot = last_plot(), width = 4, height = 6, bg = "white")

# Check for missing values in the Apparent Temperature column
missing_values <- sum(is.na(data$Apparent.Temperature))
missing_values # 0 missing values


## Humidity column

# Summary statistics of the Humidity column
summary(data$Humidity)

# scatter plot humididty
ggplot(data, aes(x = Temperature..C., y = Humidity)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Temperature vs Humidity", x = "Temperature", y = "Humidity") +
  theme_minimal()
# Save the scatter plot to the Plots folder
ggsave(filename = "Plots/temperature_vs_humidity_scatter_plot.png", plot = last_plot(), width = 6, height = 6, bg = "white")

# Check for missing values in the Humidity column
missing_values_humidity <- sum(is.na(data$Humidity))
missing_values_humidity # 0 missing values

# Since some values for humidity are 0 (which we treat as missing values since 0 values are not possible in the real world), 
# we will use the method of Multiple Imputation by Chained Equation to impute the missing values. This works because the 0 values are Missing at random (MAR)
# We use this method instead of the EM-algorithm because the em.norm() function assumes the data follows a multivariate normal distribution. 
# Humidity, however, is constrained to the range [0, 1], making it unsuitable for direct imputation using a normal distribution.
library(mice)
# Replace 0 with NA
data$Humidity[data$Humidity == 0] <- NA
# Impute using mice
imputed_data <- mice(data, method = "pmm", m = 1, maxit = 10, seed = 10101)
# Extract the completed dataset
data1 <- complete(imputed_data)
# Replace the Humidity column in data with the Humidity column from data1
data$Humidity <- data1$Humidity
# Summary
summary(data$Humidity)

# Scatter plot of Humidity
ggplot(data, aes(x = Temperature..C., y = Humidity)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Temperature vs Humidity", x = "Temperature (°C)", y = "Humidity") +
  theme_minimal()
# Save the scatter plot to the Plots folder
ggsave(filename = "Plots/temperature_vs_humidity_scatter_plot_after_imputation.png", plot = last_plot(), width = 6, height = 6, bg = "white")


## Wind.Speed..km.h. column

# Summary statistics of the Wind Speed column
summary(data$Wind.Speed..km.h.)

# Check for missing values in the Wind Speed column
missing_values_wind <- sum(is.na(data$Wind.Speed..km.h.))
missing_values_wind # 0 missing values

# Histogram of Wind Speed
ggplot(data, aes(x = Wind.Speed..km.h.)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 30) +
  labs(title = "Histogram of Wind Speed", x = "Wind Speed (km/h)", y = "Frequency") +
  theme_minimal()
# Save the histogram to the Plots folder
ggsave(filename = "Plots/wind_speed_histogram.png", plot = last_plot(), width = 6, height = 6, bg = "white")


## Wind.Bearing..degrees. column

# Summary statistics of the Wind Bearing column
summary(data$Wind.Bearing..degrees.)

# Check for missing values in the Wind Bearing column
missing_values_wind_bearing <- sum(is.na(data$Wind.Bearing..degrees.))
missing_values_wind_bearing # 0 missing values

# Histogram of Wind Bearing
ggplot(data, aes(x = Wind.Bearing..degrees.)) +
  geom_histogram(fill = "lightcoral", color = "black", bins = 30) +
  labs(title = "Histogram of Wind Bearing", x = "Wind Bearing (degrees)", y = "Frequency") +
  theme_minimal()
# Save the histogram to the Plots folder
ggsave(filename = "Plots/wind_bearing_histogram.png", plot = last_plot(), width = 6, height = 6, bg = "white")


## Visibility..km. column

# Summary statistics of the Visibility column
summary(data$Visibility..km.)

# Check for missing values in the Visibility column
missing_values_visibility <- sum(is.na(data$Visibility..km.))
missing_values_visibility # 0 missing values

# Histogram of Visibility
ggplot(data, aes(x = Visibility..km.)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Histogram of Visibility", x = "Visibility (km)", y = "Frequency") +
  theme_minimal()
# Save the histogram to the Plots folder
ggsave(filename = "Plots/visibility_histogram.png", plot = last_plot(), width = 6, height = 6, bg = "white")


## Loud Cover column

# Summary statistics of the Loud Cover column
summary(data$Loud.Cover)
# check the unique values in the Loud Cover column
unique(data$Loud.Cover)
# the Loud Cover column has only one unique value, which is 0. We can drop this column as it does not provide any useful information.
data$Loud.Cover <- NULL


## Pressure..millibars. column

# Summary statistics of the Pressure column
summary(data$Pressure..millibars.)
# count number of 0 values in the Pressure column
zero_values_pressure <- sum(data$Pressure..millibars. == 0)
zero_values_pressure # 0 zero values
# Check for missing values in the Pressure column
missing_values_pressure <- sum(is.na(data$Pressure..millibars.))
missing_values_pressure # 0 missing values

# Replace 0 values with NA in the Pressure column
data$Pressure..millibars.[data$Pressure..millibars. == 0] <- NA
# Summary statistics of the Pressure column
summary(data$Pressure..millibars.)

# density plot of Pressure 
ggplot(data, aes(x = Pressure..millibars.)) +
  geom_density(fill = "lightcoral", color = "black") +
  labs(title = "Density Plot of Pressure", x = "Pressure (millibars)", y = "Density") +
  theme_minimal()
# Save the density plot to the Plots folder
ggsave(filename = "Plots/pressure_density_plot.png", plot = last_plot(), width = 6, height = 6, bg = "white")
# We can use the EM-algorithm for imputing missing values, as the Pressure column is assumed to follow a normal distribution

# Impute missing values in the Pressure column using the EM-algortithm from the norm package
library(norm)
# IMPUTATION ON MISSING AT RANDOM DATA
#do preliminary manipulations
MAR <- prelim.norm(as.matrix(data))   
MAR
#find the mle
thetahat1 <- em.norm(MAR1) 
thetahat1
#  put the parameters to the original scale 
getparam.norm(MAR1,thetahat1, corr=T) 
#set random number generator seed
rngseed(10101)  
#impute missing data under the MLE
MAR_imp <- imp.norm(MAR1,thetahat1,data)  
# repalace the Pressure column in data with the imputed Pressure column
data$Pressure..millibars. <- MAR_imp$Pressure..millibars.

# Summary statistics of the Pressure column after imputation
summary(data$Pressure..millibars.)


summary(data)
str(data)

# Save the cleaned dataset to a new CSV file in the Data folder separated by semicolon
write.csv(data, file = "Data/Szeged_cleaned.csv", row.names = FALSE)

