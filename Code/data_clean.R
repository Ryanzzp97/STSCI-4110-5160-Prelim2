# Load data from CSV file
data <- read.csv("/Users/ryanzhou/Documents/Study/Cornell/2023/Course/STSCI 5160/Prelim/STSCI-4110-5160-Prelim2/Handout/final_cardiac_data.csv")
# Data Overview
head(data)

# Remove unique identification column
data <- data[, !(names(data) %in% c("X", "seqn"))]
# Data Overview
head(data)

# Analyze data analysis
summary(data)

### TODO
# Provide the detail data analysis individually and the explaination
# Find the correlation between each predictor
cor_matrix <- cor(na.omit(data)); cor_matrix
heatmap(cor_matrix)

# Precise data analysis and process
# Event
table(data$event)
data$event <- as.factor(data$event)
# Gender
table(data$gender)
data$gender <- as.factor(data$gender)
# Age
hist(data$age, labels = T)
data$age <- cut(data$age, breaks = seq(20, 80, by = 5), include.lowest = TRUE, right = TRUE, labels = c("20-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", "71-75", "76-80"))
# ethnic1
table(data$ethnic1)
data$ethnic1 <- as.factor(data$ethnic1)
# educ
table(data$educ)
# Since educ = 9 means Don't know, it's hard to assign value and with only one record. Remove the record
data <- data[data$educ != 9, ]

# N/A data process
# Use linear regression or logistic regression to predict the value of the NA
# Only keep records without N/A value to do the proedict
data_no_na <- na.omit(data)

# 1. sleep.hrs
# According to the correlation, use the highest possible predictors
sleep_fit <- lm(sleep.hrs ~ gender+age+ethnic1+educ, data)
summary(sleep_fit)
# Based on first try, use the most significant predictors
sleep_fit1 <- lm(sleep.hrs ~ gender+age, data)
summary(sleep_fit1)
# Evaluate Model Performance
# Predicted values from the model
predicted_sleep <- predict(sleep_fit1, newdata = data_no_na)
# Mean Absolute Error (MAE)
mae <- mean(abs(data_no_na$sleep.hrs - predicted_sleep)); mae
# Mean Squared Error (MSE)
mse <- mean((data_no_na$sleep.hrs - predicted_sleep)^2); mse
# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse); rmse
# R-squared (R²)
rsquared <- 1 - (sum((data_no_na$sleep.hrs - predicted_sleep)^2) / sum((data_no_na$sleep.hrs - mean(data_no_na$sleep.hrs))^2)); rsquared
# Replace the missing value using the predict model
# Find rows with missing values
rows_with_na <- is.na(data$sleep.hrs)
# Select the rows with missing values
data_sleep_missing <- data[rows_with_na, ]
# Replace the missing values with the predicted value by linear regression model
data$sleep.hrs[rows_with_na] <- predict(sleep_fit1, newdata = data_sleep_missing)
# Check Result
sum(is.na(data$sleep.hrs))
summary(data$sleep.hrs)

# 2. diabetes
table(data$diabetes)
# For Categorical response, first Categorize the result to (0-No diabetes; 1-Have diabetes)
# Treat "border line" as "yes", because border line is only a little away from "having diabetes" but far away from "not having diabetes"
data$diabetes[data$diabetes == 3] <- 1 
# Regard Don't know as NaN, which will be processed later.
data$diabetes[data$diabetes == 9] <- NaN 
# turn 2 into 0, which not only turns data to correct format of data for logistic regression but also more intuitive.
data$diabetes[data$diabetes == 2] <- 0 
diab_pr<-glm(diabetes ~ educ + gender, data=data, family=binomial)
# Model evaluation
summary(diab_pr)
data_no_na$diabetes[data_no_na$diabetes == 3] <- 1 
data_no_na$diabetes[data_no_na$diabetes == 2] <- 0 
predicted_diabetes <- ifelse(predict(diab_pr, newdata = data_no_na) >= 0.5, 1, 0)
mean(data_no_na$diabetes == predicted_diabetes)
# Missing value replace
rows_with_na <- is.na(data$diabetes)
predicted_probabilities <- predict(diab_pr, newdata = data[rows_with_na, ], type = "response")
data$diabetes[rows_with_na] <- ifelse(predicted_probabilities >= 0.5, 1, 0)
# Check Result
sum(is.na(data$diabetes))
summary(data$diabetes)

# 3. smoker
table(data$smoker)
# For Categorical response, first Categorize the result to (0-No smoke; 1-smoke)
# turn 2 into 0, which not only turns data to correct format of data for logistic regression but also more intuitive.
data$smoker[data$smoker == 2] <- 0
smoke_pr <- glm(smoker ~ gender + educ + age, data = data, family = binomial)
# Model evaluation
summary(smoke_pr)
data_no_na$smoker[data_no_na$smoker == 2] <- 0
predicted_smoker <- ifelse(predict(smoke_pr, newdata = data_no_na) >= 0.5, 1, 0)
mean(data_no_na$smoker == predicted_smoker)
# Missing value replace
rows_with_na <- is.na(data$smoker)
predicted_probabilities <- predict(smoke_pr, newdata = data[rows_with_na, ], type = "response")
data$smoker[rows_with_na] <- ifelse(predicted_probabilities >= 0.5, 1, 0)
# Check Result
sum(is.na(data$smoker))
summary(data$smoker)

# 4. bmi
# According to the correlation, use the highest possible predictors
bmi_model = lm(bmi ~ educ + age + gender + ethnic1, data)
summary(bmi_model)
# Based on first try, use the most significant predictors
bmi_model1 = lm(bmi ~ age + gender + ethnic1, data)
summary(bmi_model1)
# Evaluate Model Performance
# Predicted values from the model
predicted_bmi <- predict(bmi_model1, newdata = data_no_na)
# Mean Absolute Error (MAE)
mae <- mean(abs(data_no_na$bmi - predicted_bmi)); mae
# Mean Squared Error (MSE)
mse <- mean((data_no_na$bmi - predicted_bmi)^2); mse
# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse); rmse
# R-squared (R²)
rsquared <- 1 - (sum((data_no_na$bmi - predicted_bmi)^2) / sum((data_no_na$bmi - mean(data_no_na$bmi))^2)); rsquared
# Have terrible result in model, so we decided to use the median value to replace the missing value
rows_with_na <- is.na(data$bmi)
data$bmi[rows_with_na] <- median(data_no_na$bmi)
# Check Result
sum(is.na(data$bmi))
summary(data$bmi)
