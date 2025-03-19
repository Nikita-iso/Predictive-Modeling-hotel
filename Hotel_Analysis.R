# Loading the Main Library
library(tidyverse)
library(ggplot2)
library(readxl)
library(caret)

# Loading the Package to read xls file
library("readxl")
hotel_data=read_excel("C:/Users/akulg/OneDrive/Desktop/BA Project/Hotel Data Updated.xlsx")
View(hotel_data)

# Check the Structure of the Dataset
str(hotel_data)

# Summary Statistics of the Dataset
print(summary(hotel_data))
view(summary(hotel_data))

# Check the Missing Values
colSums(is.na(hotel_data))

# Remove duplicates
hotel_clean<-hotel_data%>%distinct()
view(hotel_clean)

# Exploratory Data Analysis (EDA)
## Q1. Distribution of occupancy rates across all properties

hotel_data %>% 
  summarise(
    min_occupancy = min(occupancy, na.rm = TRUE),
    max_occupancy = max(occupancy, na.rm = TRUE),
    mean_occupancy = mean(occupancy, na.rm = TRUE),
    median_occupancy = median(occupancy, na.rm = TRUE),
    sd_occupancy = sd(occupancy, na.rm = TRUE)
  )

# Graphical representation for Q1
ggplot(hotel_data, aes(x = occupancy)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Occupancy Rates",
    x = "Occupancy Rate",
    y = "Count"
  ) +
  theme_minimal()

# Q2. Average occupancy rate per city
city_occupancy <- hotel_data %>%
  group_by(city) %>%
  summarize(
    avg_occupancy = mean(occupancy, na.rm = TRUE),
    sd_occupancy = sd(occupancy, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(avg_occupancy))

# Graphical representation for Q2
ggplot(city_occupancy, aes(x = reorder(city, avg_occupancy), y = avg_occupancy)) +
  geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Average Occupancy Rates by City",
       x = "City",
       y = "Average Occupancy Rate (%)") +
  theme_minimal()

# Q3. Occupancy variation between weekdays and weekends
occupancy_summary <- hotel_data %>%
  group_by(day_type) %>%
  summarize(
    avg_occupancy = mean(occupancy, na.rm = TRUE),
    sd_occupancy = sd(occupancy, na.rm = TRUE),
    median_occupancy = median(occupancy, na.rm = TRUE),
    count = n()
  )

# Graphical representation for Q3
ggplot(hotel_data, aes(x = day_type, y = occupancy, fill = day_type)) +
  geom_violin (alpha = 0.5) +
  geom_boxplot(width = 0.2, alpha = 0.8) +
  labs(title = "Occupancy Rate Distribution: Weekday vs Weekend",
       x = "Day Type",
       y = "Occupancy Rate",
       fill = "Day Type") +
  theme_minimal()

# Q4. Distribution of successful bookings across cities
booking_stats <- hotel_data %>%
  group_by(city) %>%
  summarize(
    total_bookings = n(),
    successful_bookings = sum(successful_bookings),
    success_rate = (successful_bookings / total_bookings) * 100
  ) %>%
  arrange(desc(successful_bookings))

# Graphical representation for Q4
ggplot(booking_stats, aes(x = reorder(city, successful_bookings), y = successful_bookings)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = successful_bookings), vjust = -0.5) +
  labs(title = "Number of Successful Bookings by City",
       x = "City",
       y = "Number of Successful Bookings") +
  theme_minimal()

# Q5. Room category with highest average successful bookings
room_stats <- hotel_data %>%
  group_by(room_category) %>%
  summarize(
    total_bookings = n(),
    avg_bookings = mean(successful_bookings, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_bookings))

# Graphical representation for Q5
ggplot(room_stats, aes(x = reorder(room_category, avg_bookings), 
                       y = avg_bookings)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", avg_bookings)), 
            vjust = -0.5) +
  labs(title = "Average Successful Bookings by Room Category",
       x = "Room Category",
       y = "Average Successful Bookings (%)") +
  theme_minimal()

# Q6. Property category distribution differ across cities
property_distribution <- hotel_data %>%
  group_by(city, category) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(category, city, desc(count))

# Graphical representation for Q6
ggplot(property_distribution, 
       aes(x = city, y = count, fill = category)) +
  geom_tile(stat = "identity", position = "stack") +
  labs(title = "Property Category Distribution by City",
       x = "City",
       y = "Number of Properties",
       fill = "Property Category") +
  theme_minimal()

# Q7. Which city has the most properties listed
city_properties <- hotel_data %>%
  group_by(city) %>%
  summarize(
    property_count = n(),
    percentage = (n() / nrow(hotel_data) * 100)
  ) %>%
  arrange(desc(property_count))

# Graphical representation for Q7
ggplot(city_properties, aes(x = "", y = property_count, fill = city)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Properties Across Cities",
       fill = "City") +
  theme_minimal()

# Q8. Room categories more popular in specific cities
room_city_dist <- hotel_data %>%
  group_by(city, room_category) %>%
  summarize(
    total_bookings = n(),
      ) %>%
  group_by(city) %>%
  mutate(
    percentage = (total_bookings / sum(total_bookings)) * 100,
  )

# Graphical Representation for Q8
ggplot(room_city_dist, 
       aes(x = city, y = percentage, fill = room_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Room Category Distribution by City",
       subtitle = "Percentage of Each Room Category",
       x = "City",
       y = "Percentage",
       fill = "Room Category") +
  theme_minimal()

# Q9. Highest property contribution in each city
category_analysis <- hotel_data %>%
  group_by(city, category) %>%
  summarise(
    total_bookings = sum(successful_bookings),
    .groups = 'drop'
  ) %>%
  group_by(city) %>%
  mutate(
    booking_percentage = (total_bookings / sum(total_bookings) * 100),
    booking_percentage = round(booking_percentage, 2)
  ) %>%
  arrange(city, desc(total_bookings))

# Graphical representation for Q9
ggplot(category_analysis, 
       aes(x = category, 
           y = total_bookings)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_bookings), 
            vjust = -0.5, 
            size = 3) +
  facet_wrap(~city, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Total Bookings by Property Category and City",
    subtitle = "Showing absolute number of bookings",
    x = "Property Category",
    y = "Number of Bookings"
  )

# Q10. Proportion of weekends to weekdays
day_type_analysis <- hotel_data %>%
  group_by(day_type) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    proportion = count / sum(count),
    percentage = round(proportion * 100, 2)
  )

# Graphical representation for Q10
ggplot(day_type_analysis, 
       aes(x = "", y = percentage, fill = day_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(
    title = "Distribution of Weekends vs Weekdays",
    fill = "Day Type"
  ) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5))

# Q11. Significant difference in occupancy rates between weekdays and weekends

# Perform t-test
t_test_weekday_weekend <- t.test(
  occupancy ~ day_type,
  data = hotel_clean
)
print("T-Test: Occupancy Rates (Weekday vs Weekend)")
print(t_test_weekday_weekend)

# Q12. Relationship between room category and successful bookings (Chi-square test)

# Create a contingency table
if ("room_category" %in% names(hotel_clean) 
    && "successful_bookings" %in% 
    names(hotel_clean)) {
  table_room_booking <- table(hotel_clean$room_category, 
                              hotel_clean$successful_bookings > 0)
  
  # Perform chi-square test
  chi_square_test <- chisq.test(table_room_booking)
  print("Chi-Square Test: Room Category vs Successful Bookings")
  print(chi_square_test)
} else {
  print("Columns 'room_category' or 'successful_bookings' are missing!")
}

# Q13. Successful bookings higher during specific months

if ("mm-yy" %in% names(hotel_clean) && "successful_bookings" %in% names(hotel_clean)) {
  
  # Filter data for May and December
  data_selected_months <- hotel_clean %>% 
    filter(format(as.Date(`mm-yy`, format="%m-%y"), "%m") %in% c("05", "12"))
  
  # Create a month column
  data_selected_months$month <- format(as.Date(data_selected_months$`mm-yy`, 
                                               format="%m-%y"), "%m")
  
  # Check unique values in the 'month' column
  print(unique(data_selected_months$month))  
  
  # If the filtering is working, there should be only May (05) and December (12)
  if(length(unique(data_selected_months$month)) == 2) {
    
    # Ensure month is treated as a factor with only two levels
    data_selected_months$month <- factor(data_selected_months$month, levels = c("05", "12"))
    
    # Perform t-test
    t_test_months <- t.test(successful_bookings ~ month, data = data_selected_months)
    print("T-Test: Successful Bookings (May vs December)")
    print(t_test_months)
  } else {
    print("Error: More than two months found.")
  }
}

# Q14. Occupancy rates of properties in Luxury and Business categories (t-test)

if ("category" %in% names(hotel_clean)) {
  
  # Filter data for Luxury and Business categories
  data_luxury_business <- hotel_clean %>% filter(category %in% c("Luxury", "Business"))
  
  # Perform t-test
  t_test_luxury_business <- t.test(
    occupancy ~ category,
    data = data_luxury_business
  )
  print("T-Test: Occupancy Rates (Luxury vs Business Categories)")
  print(t_test_luxury_business)
}

# Q15. Weekend vs weekday occupancy rates across all cities (Paired t-test)
if ("city" %in% names(hotel_clean) && "day_type" %in% names(hotel_clean)) {
  
  # Prepare data for paired t-test
  paired_data <- hotel_clean %>%
    group_by(city, day_type) %>%
    summarize(mean_occupancy = mean(occupancy, na.rm = TRUE)) %>%
    pivot_wider(names_from = day_type, values_from = mean_occupancy)
  
  if ("Weekday" %in% names(paired_data) && "Weekend" %in% names(paired_data)) {
    
    # Perform paired t-test
    t_test_paired <- t.test(
      paired_data$Weekday,
      paired_data$Weekend,
      paired = TRUE
    )
    print("Paired T-Test: Occupancy Rates (Weekend vs Weekday)")
    print(t_test_paired)
  } else {
    print("Paired data is incomplete! Ensure both 'Weekday' and 'Weekend' columns are present.")
  }
}

# Q16. Ensure `day_type` is a factor and predictors are numeric
hotel_data$day_type <- as.factor(hotel_data$day_type)
hotel_data$occupancy <- as.numeric(hotel_data$occupancy)
hotel_data$capacity <- as.numeric(hotel_data$capacity)

# Check for NA values (optional, but recommended)
if (anyNA(hotel_data)) {
  print("Data contains NA values. Handling them is recommended.")
  
}

# Split data into training and testing sets
set.seed(123)  
train_index <- createDataPartition(hotel_data$day_type, p = 0.8, list = FALSE)
train_data <- hotel_data[train_index, ]
test_data <- hotel_data[-train_index, ]

# Fit a logistic regression model
log_model <- glm(day_type ~ occupancy + capacity, data = train_data, family = "binomial")

# Summary of the model
summary(log_model)

# Predict on the test set
test_data$predicted <- predict(log_model, newdata = test_data, type = "response")
test_data$predicted_class <- ifelse(test_data$predicted > 0.5, "Weekend", "Weekday")

# Check unique levels in the test set
actual_levels <- levels(test_data$day_type)
predicted_levels <- levels(factor(test_data$predicted_class))

# If necessary, adjust levels of predicted class to match the actual class levels
predicted_factor <- factor(test_data$predicted_class, levels = actual_levels)

# Evaluate the model
confusion_matrix <- confusionMatrix(predicted_factor, test_data$day_type)
print(confusion_matrix)

# Q17. Assume an average room price (update value if needed)
average_room_price <- 500  

# Calculate total revenue
hotel_data <- hotel_data %>%
  mutate(total_revenue = successful_bookings * average_room_price)

# Ensure predictors are numeric
hotel_data$occupancy <- as.numeric(hotel_data$occupancy)
hotel_data$capacity <- as.numeric(hotel_data$capacity)
hotel_data$total_revenue <- as.numeric(hotel_data$total_revenue)

# Split data into training and testing sets
set.seed(123)  
train_index <- createDataPartition(hotel_data$total_revenue, p = 0.8, list = FALSE)
train_data <- hotel_data[train_index, ]
test_data <- hotel_data[-train_index, ]

# Fit a linear regression model
revenue_model <- lm(total_revenue ~ occupancy + capacity, data = train_data)

# Summary of the model
summary(revenue_model)

# Predict on the test set
test_data$predicted_revenue <- predict(revenue_model, newdata = test_data)

# Evaluate model performance
actual_vs_predicted <- data.frame(
  Actual = test_data$total_revenue,
  Predicted = test_data$predicted_revenue
)

# Calculate R-squared and RMSE
r_squared <- cor(actual_vs_predicted$Actual, actual_vs_predicted$Predicted)^2
rmse <- sqrt(mean((actual_vs_predicted$Actual - actual_vs_predicted$Predicted)^2))

cat("R-squared: ", r_squared, "\n")
cat("RMSE: ", rmse, "\n")

# Q18. Convert occupancy to binary: 1 if > 75%, else 0
hotel_data <- hotel_data %>%
  mutate(occupancy_high = ifelse(occupancy > 0.75, 1, 0))

# Calculate median bookings and classify performance
median_bookings <- median(hotel_data$successful_bookings, na.rm = TRUE)
hotel_data <- hotel_data %>%
  mutate(performance = ifelse(successful_bookings > median_bookings, "High", "Low"))

# One-hot encode categorical variables
hotel_data <- hotel_data %>%
  select(check_in_date,mm_yy) %>%  # Remove date columns if not needed for modeling
  mutate_if(is.character, as.factor) %>%  # Convert character columns to factors
  mutate_if(is.factor, as.numeric)  # One-hot encode factors

# Split data into training and test sets
set.seed(123)
train_index <- createDataPartition(hotel_data$occupancy_high, p = 0.8, list = FALSE)
train_data <- hotel_data[train_index,]
test_data <-hotel_data[-train_index,]

# Train a logistic regression model for occupancy
occupancy_model <- train(occupancy_high ~., hotel_data = train_data, method = "glm", family = "binomial")

# Predict on test data and evaluate
occupancy_predictions <- predict(occupancy_model, newdata = test_data)
confusionMatrix(occupancy_predictions, test_data$occupancy_high)

# Train a classification model for performance
performance_model <- train(performance ~ ., data = train_data, method = "rpart")

# Predict on test data and evaluate
performance_predictions <- predict(performance_model, newdata = test_data)
confusionMatrix(performance_predictions, test_data$performance)

# Clustering with K-means
# Select numeric columns for clustering
clustering_data <-hotel_data %>% select(capacity, successful_bookings, occupancy)

# Normalize the data
clustering_data <- scale(clustering_data)

# Perform K-means clustering
set.seed(123)
kmeans_result <- kmeans(clustering_data, centers = 3, nstart = 25)

# Add cluster results to the original data
data$cluster <- kmeans_result$cluster

# Visualize clusters
ggplot(data, aes(x = capacity, y = occupancy, color = as.factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering of Properties", color = "Cluster")
