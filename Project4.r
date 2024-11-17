setwd("C:/Users/Sonia Afrose/Documents/RProject/PhiladelphiaProjectbyR/KaggleData/crime.csv")

# Load necessary libraries# Install tidyverse if not already installed
if (!require(tidyverse)) {
  install.packages("tidyverse")
}

# Load the data
crime_data <- read.csv("crime.csv")
crime_data

# Inspect the structure
head(crime_data)

# Convert Dispatch_Date to Date format
crime_data$Dispatch_Date <- as.Date(crime_data$Dispatch_Date, format = "%Y-%m-%d")

# Extract Year and Month and Day
crime_data <- crime_data %>%
  mutate(Year = format(Dispatch_Date, "%Y"),    # Extract Year
         Month = format(Dispatch_Date, "%m"),Day = format(Dispatch_Date, "%d"))  # Extract Month and Day

# Optional: Filter data for a specific year range (e.g., 2010 to 2015)
# Modify this range as needed
start_year <- 2006
end_year <- 2016
crime_data <- crime_data %>%
  filter(year(Dispatch_Date) >= start_year & year(Dispatch_Date) <= end_year)
###############################################################################
# Count the number of crimes by day
daily_crime_count <- crime_data %>%
  group_by(Dispatch_Date) %>%
  summarise(Daily_Crimes = n())

# Count the number of crimes by month
monthly_crime_count <- crime_data %>%
  mutate(YearMonth = format(Dispatch_Date, "%Y-%m")) %>%
  group_by(YearMonth) %>%
  summarise(Monthly_Crimes = n())

# Count the number of crimes by year
yearly_crime_count <- crime_data %>%
  mutate(Year = year(Dispatch_Date)) %>%
  group_by(Year) %>%
  summarise(Yearly_Crimes = n())

# Print the summaries
print("Daily Crime Count:")
print(head(daily_crime_count))

print("Monthly Crime Count:")
print(head(monthly_crime_count))

print("Yearly Crime Count:")
print(head(yearly_crime_count))
#################################################################################
# Aggregate crimes by Year
crimes_by_year <- crime_data %>%
  group_by(Year) %>%
  summarize(Crimes = n())
# Aggregate crimes by Month
crimes_by_month <- crime_data %>%
  group_by(Month) %>%
  summarize(Crimes = n())
# Aggregate crimes by Day
crimes_by_day <- crime_data %>%
  group_by(Day) %>%
  summarize(Crimes = n())
# Plot: Number of Crimes by Year
ggplot(crimes_by_year, aes(x = Year, y = Crimes)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Number of Crimes by Year", x = "Year", y = "Number of Crimes") +
  theme_minimal()
# Plot: Number of Crimes by Month
ggplot(crimes_by_month, aes(x = Month, y = Crimes)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of Crimes by Month", x = "Month", y = "Number of Crimes") +
  theme_minimal()
# Plot: Number of Crimes by Day
ggplot(crimes_by_day, aes(x = Day, y = Crimes)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Number of Crimes by Day of the Week", x = "Day of the Week", y = "Number of Crimes") +
  theme_minimal()
###############################################################################
#how it changes the total number of crimes per year in each of the Police_Districts
# Aggregate crimes by Year and Police District
crimes_by_year_district <- crime_data %>%
  group_by(Year, Police_Districts) %>%
  summarize(Crimes = n(), .groups = "drop")
# Visualization: Facet Grid
ggplot(crimes_by_year_district, aes(x = Year, y = Crimes, fill = Police_Districts)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Police_Districts, scales = "free_y") +
  labs(title = "Crimes per Year in Each Police District",
       x = "Year",
       y = "Number of Crimes") +
  theme_minimal()

# Visualization: Grouped Bar Plot
ggplot(crimes_by_year_district, aes(x = Year, y = Crimes, fill = Police_Districts)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Crimes per Year in Each Police District",
       x = "Year",
       y = "Number of Crimes",
       fill = "Police Districts") +
  theme_minimal()
#how it changes the total number of crimes per year for each Text_General_Code
# Aggregate crimes by Year and Text_General_Code
crimes_by_year_code <- crime_data %>%
  group_by(Year, Text_General_Code) %>%
  summarize(Crimes = n(), .groups = "drop")  # Include .groups argument to avoid warnings
# Plot: Facet Grid
ggplot(crimes_by_year_code, aes(x = Year, y = Crimes, fill = Text_General_Code)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Text_General_Code, scales = "free_y") +
  labs(title = "Crimes per Year for Each Text General Code",
       x = "Year",
       y = "Number of Crimes") +
  theme_minimal()
# Plot: Stacked Bar Plot
ggplot(crimes_by_year_code, aes(x = Year, y = Crimes, fill = Text_General_Code)) +
  geom_bar(stat = "identity") +
  labs(title = "Crimes per Year for Each Text General Code",
       x = "Year",
       y = "Number of Crimes",
       fill = "Text General Code") +
  theme_minimal()

