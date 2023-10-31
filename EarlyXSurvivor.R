head(Early)
Early

# Create a vector of region numbers
regions <- unique(RT$Region)

# Initialize an empty dataframe to store the subsetted data
EarlySpecific <- data.frame()

x_values

# Subset the data based on the specific number of days for each region, do 8 for region 1 to not skip out on a bunch of samples
subset_data <- subset(RT, Region == 1 & Days < 9)
EarlySpecific <- rbind(EarlySpecific, subset_data)

subset_data <- subset(RT, Region == 2 & Days < 178)
EarlySpecific <- rbind(EarlySpecific, subset_data)

subset_data <- subset(RT, Region == 3 & Days < 126)
EarlySpecific <- rbind(EarlySpecific, subset_data)

subset_data <- subset(RT, Region == 4 & Days < 87)
EarlySpecific <- rbind(EarlySpecific, subset_data)

subset_data <- subset(RT, Region == 5 & Days < 112)
EarlySpecific <- rbind(EarlySpecific, subset_data)

subset_data <- subset(RT, Region == 6 & Days < 44)
EarlySpecific <- rbind(EarlySpecific, subset_data)

summary(RT$ColonyNumber)
summary(EarlySpecific$ColonyNumber)

head(EarlySpecific)


# Group the data by ColonyNumber and find the maximum Pred value for each group
Early <- Early %>%
  group_by(ColonyNumber) %>%
  mutate(MaxPred = max(Pred)) %>%
  ungroup()

# Assign the maximum Pred value to all rows of each ColonyNumber
Early$MaxPred <- Early$MaxPred

head(Early$MaxPred)

# Subset Early to include rows where MaxPred is greater than 0
EarlyPreds <- subset(Early, MaxPred > 0)

# Subset Early to include rows where MaxPred is 0
EarlyNone <- subset(Early, MaxPred == 0)

# Sort the Early dataframe by ColonyNumber and Days in descending order
Early <- Early[order(Early$ColonyNumber, -Early$Days), ]

# Subset Early to include the last row for each unique ColonyNumber
End <- Early[!duplicated(Early$ColonyNumber), ]

# Create a new variable EarlyPredYN in End
End$EarlyPredYN <- ifelse(End$ColonyNumber %in% EarlyPreds$ColonyNumber, "yes", "no")

End <- subset(End, Status != "missing")

head(End)

#######Find the subset of dead colony numbers and when they were first seen as dead

# Subset RT for rows where Status is "dead"
subset_dead <- RT[RT$Status == "dead", ]

# Subset the data to include only a single row for each unique ColonyNumber with the lowest Days value
subset_lowest_days <- subset_dead %>%
  group_by(ColonyNumber) %>%
  filter(Days == min(Days)) %>%
  ungroup()

# Check the number of unique ColonyNumber in the subset
numUnique <- n_distinct(subset_lowest_days$ColonyNumber)
print(numUnique)

##############

# Create a vector of unique ColonyNumbers in subset_dead
unique_colonynum <- unique(subset_dead$ColonyNumber)

# Update End dataframe
End$Status[End$ColonyNumber %in% unique_colonynum] <- "dead"
End$MonitoringInterval[End$ColonyNumber %in% unique_colonynum] <- subset_dead$MonitoringInterval

# View the updated End dataframe
print(End)

numUnique <- n_distinct(subset(End, End$Status == "dead"))
print(numUnique)

# Create a contingency table
cont_table <- table(End$Status, End$EarlyPredYN)

# Run the chi-squared test
chi_sq <- chisq.test(cont_table)
cont_table

# Print the test results
print(chi_sq)

##########################################################################################
################### Do the same thing but for EarlySpecific

# Group the data by ColonyNumber and find the maximum Pred value for each group
EarlySpecific <- EarlySpecific %>%
  group_by(ColonyNumber) %>%
  mutate(MaxPred = max(Pred)) %>%
  ungroup()

# Assign the maximum Pred value to all rows of each ColonyNumber
EarlySpecific$MaxPred <- EarlySpecific$MaxPred

head(EarlySpecific$MaxPred)

# Subset EarlySpecific to include rows where MaxPred is greater than 0
EarlySpecificPreds <- subset(EarlySpecific, MaxPred > 0)

# Subset EarlySpecific to include rows where MaxPred is 0
EarlySpecificNone <- subset(EarlySpecific, MaxPred == 0)

# Sort the EarlySpecific dataframe by ColonyNumber and Days in descending order
EarlySpecific <- EarlySpecific[order(EarlySpecific$ColonyNumber, -EarlySpecific$Days), ]

# Create a new variable EarlySpecificPredYN in End
End$EarlySpecificPredYN <- ifelse(End$ColonyNumber %in% EarlySpecificPreds$ColonyNumber, "yes", "no")

End <- subset(End, Status != "missing")

head(End)

# Create a contingency table
cont_table <- table(End$Status, End$EarlySpecificPredYN)

# Run the chi-squared test
chi_sq <- chisq.test(cont_table)
cont_table

# Print the test results
print(chi_sq)

###################################################################
################### Reproduce the Random Forest using EarlySpecific

RF <- End

RF$PredCat <- ifelse(RF$MaxPred == 0, "None",
                        ifelse(RF$MaxPred <= 40, "Mild", "Severe"))

head(RF)

# Subset the relevant columns from the dataframe, treat categories as factors!
RF <- RF[, c("ColonySource", "Region", "Species", "SourceType",  "PredCat", "mean_parrotfish", "mean_butterflyfish", "InitialSize")]
RF[, c("ColonySource", "Region", "Species", "SourceType", "PredCat")] <- lapply(RF[, c("ColonySource", "Region", "Species", "SourceType", "PredCat")], as.factor)
RF$mean_parrotfish <- as.numeric(RF$mean_parrotfish)
RF$mean_butterflyfish <- as.numeric(RF$mean_butterflyfish)

####Remove comments from above lines for testing, else leave it alone!
## What happens if we remove region 1??? Only include region 3???
#RF <- subset(RF, RF$Region == "3")
## What about predicting categories of non-zero predation?
#RF <- RF[RF$PredCat != "0", ]
#RF$PredCat <- droplevels(RF$PredCat, exclude = "0")

table(RF$PredCat)

head(RF)

RF <- na.omit(RF)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(nrow(RF), nrow(RF)*0.75)
train <- RF[train_index, ]
test <- RF[-train_index, ]

# Train the model using random forest
rf_model <- randomForest(PredCat ~ . , data = train)

# Predict the "PredCat" column of the test set using the trained model
test$pred <- predict(rf_model, newdata = test, type = "class")

#How much representation is in the training data?
table(train$PredCat)

# Create a confusion matrix to evaluate the model's accuracy
table(test$PredCat, test$pred)

rf_model

# Generate a random forest model with 1000 trees
rf_model <- randomForest(PredCat ~ . , data = train, ntree = 1000)

par(mfrow =c(1,1))

# Plot the variable importance of the random forest model
varImpPlot(rf_model)

# Predict the "Success" column of the test set using the trained model
test$pred <- predict(rf_model, newdata = test, type = "class")

# Create a confusion matrix to evaluate the model's accuracy
table(test$PredCat, test$pred)

rf_model

# Load the e1071 package
library(e1071)

# Create a confusion matrix
cm <- table(test$PredCat, test$pred)

# Print the confusion matrix
print(cm)

# Calculate and print the class error
class_error <- 1 - sum(diag(cm)) / sum(cm)
print(class_error)

# Access the average decision tree from the random forest model
avg_tree <- getTree(rf_model, k = 1, labelVar = TRUE)

# Plot the average decision tree
plot(avg_tree, main = "Average Decision Tree")

#############################################################################################
######################## Chance of Survival vs Early Predation Intensity

EndBin <- End
EndBin$Status <- ifelse(End$Status == "alive", 1, 0)

# Fit a logistic regression model
model <- glm(Status ~ MaxPred, data = EndBin, family = binomial)

summary(model)

# Generate a sequence of MaxPred values for prediction
pred_values <- seq(min(EndBin$MaxPred), max(EndBin$MaxPred), length.out = 100)

# Make predictions using the fitted model
predictions <- predict(model, newdata = data.frame(MaxPred = pred_values), type = "response")

# Plot the predicted percentage chance of survival
plot(pred_values, predictions * 100, type = "l", xlab = "Early Intensity", ylab = "Percentage Chance of Survival", ylim = c(0, 100))

############

# Create MaxPredCat variable
EndBin$MaxPredCat <- EndBin$MaxPred <- round(EndBin$MaxPred / 5) * 5

# Calculate the percentage of "alive" status for each MaxPredCat value
alive_percentage <- aggregate(Status ~ MaxPredCat, data = EndBin, FUN = function(x) mean(x == 1) * 100)

# Plot the percentage alive against MaxPred
plot(alive_percentage$MaxPredCat, alive_percentage$Status, xlab = "Early Predation Bin", ylab = "Proportion Alive by End", pch = 16)

### Plot them together
lines(pred_values, predictions * 100, type = "l", xlab = "Early Intensity", ylab = "Proportion Alive by End", ylim = c(0, 100))

##########################################







alive_percentage
alive_percentage$Status <- round(alive_percentage$Status/100)

model <- glm(Status ~ MaxPredCat, data = alive_percentage, family = binomial)

summary(model)

# Generate a sequence of MaxPred values for prediction
pred_values <- seq(min(EndBin$MaxPred), max(EndBin$MaxPred), length.out = 100)

# Make predictions using the fitted model
predictions <- predict(model, newdata = data.frame(MaxPred = pred_values), type = "response")

# Plot the predicted percentage chance of survival
plot(pred_values, predictions * 100, type = "l", xlab = "Early Intensity", ylab = "Percentage Chance of Survival", ylim = c(0, 100))

############

# Create MaxPredCat variable
EndBin$MaxPredCat <- EndBin$MaxPred <- round(EndBin$MaxPred / 5) * 5

# Calculate the percentage of "alive" status for each MaxPredCat value
alive_percentage <- aggregate(Status ~ MaxPredCat, data = EndBin, FUN = function(x) mean(x == 1) * 100)

# Plot the percentage alive against MaxPred
plot(alive_percentage$MaxPredCat, alive_percentage$Status, xlab = "Early Predation Bin", ylab = "Proportion Alive by End", pch = 16)

### Plot them together
lines(pred_values, predictions * 100, type = "l", xlab = "Early Intensity", ylab = "Proportion Alive by End", ylim = c(0, 100))












EndTest <- End

# Convert Status to binary variable
EndTest$Status <- ifelse(EndTest$Status == "alive", 1, 0)

# Fit a logistic regression model
model <- glm(Status ~ MaxPred, data = EndTest, family = binomial)

# Extract the estimated coefficients
coef <- coef(model)

# Calculate the odds of success for different MaxPred values
maxpred_values <- seq(0, 100, by = 1)  # Specify the range of MaxPred values
odds <- exp(coef[1] + coef[2] * maxpred_values)  # Calculate the odds using the model coefficients

# Plot the odds of success against MaxPred
plot(maxpred_values, odds, type = "l", xlab = "MaxPred", ylab = "Odds of Success")

# Plot the odds of success against MaxPred
plot(EndTest$MaxPred, EndTest$Status, pch = 16, col = "blue", xlab = "Early Predation Intensity", ylab = "Final Status", main = "Longterm Survivorship vs Early Predation Intensity")
lines(maxpred_values, odds, type = "l", col = "red")

maxpred_values

# Calculate the percent chance of success for different MaxPred values
percent_chance <- odds / (1 + odds)  # Convert odds to percent chance

# Plot the percent chance of success against MaxPred
plot(EndTest$MaxPred, EndTest$Status, pch = 16, col = "blue", xlab = "Early Predation Intensity", ylab = "Final Status", main = "Longterm Survivorship vs Early Predation Intensity")
lines(maxpred_values, percent_chance, type = "l", col = "red")

summary(model)






################################## End of Early Period Only
summary(RT$ColonyNumber)
summary(EarlySpecific$ColonyNumber)

EndTestEarly <- EarlySpecific
RTTest <- RT[!duplicated(RT$ColonyNumber), ]
RTTest <- subset(RTTest, RTTest$Status != "missing")
summary(RTTest$ColonyNumber)

# Subset EarlySpecific to include the last row for each unique ColonyNumber
EndTestEarly <- EarlySpecific[!duplicated(EarlySpecific$ColonyNumber), ]
summary(EndTestEarly$ColonyNumber)

# Create a new variable EarlySpecificPredYN in End
EndTestEarly$EarlySpecificPredYN <- ifelse(EndTestEarly$ColonyNumber %in% EarlySpecificPreds$ColonyNumber, "yes", "no")

EndTestEarly <- subset(EndTestEarly, Status != "missing")

head(EndTestEarly)



# Convert Status to binary variable
EndTestEarly$Status <- ifelse(EndTestEarly$Status == "alive", 1, 0)

# Fit a logistic regression model
model <- glm(Status ~ MaxPred, data = EndTestEarly, family = binomial)

summary(model)

# Extract the estimated coefficients

# Calculate the odds of success for different MaxPred values
maxpred_values <- seq(0, 100, by = 1)  # Specify the range of MaxPred values
odds <- exp(coef[1] + coef[2] * maxpred_values)  # Calculate the odds using the model coefficients

# Plot the odds of success against MaxPred
plot(maxpred_values, odds, type = "l", xlab = "MaxPred", ylab = "Odds of Success")

# Plot the odds of success against MaxPred
plot(EndTest$MaxPred, EndTestEarly$Status, pch = 16, col = "blue", xlab = "MaxPred", ylab = "Status")
lines(maxpred_values, odds, type = "l", col = "red")

maxpred_values

# Calculate the percent chance of success for different MaxPred values
percent_chance <- odds / (1 + odds)  # Convert odds to percent chance

# Plot the percent chance of success against MaxPred
plot(EndTestEarly$MaxPred, EndTestEarly$Status, pch = 16, col = "blue", xlab = "Early Predation Intensity", ylab = "End of Early Period Status", main = "Early Survivorship vs Predation Intensity" )
lines(maxpred_values, percent_chance, type = "l", col = "red")






EndBin <- EndTestEarly
EndBin$Status <- ifelse(End$Status == "alive", 1, 0)

# Fit a logistic regression model
model <- glm(Status ~ MaxPred, data = EndBin, family = binomial)

summary(model)

# Generate a sequence of MaxPred values for prediction
pred_values <- seq(min(EndBin$MaxPred), max(EndBin$MaxPred), length.out = 100)

# Make predictions using the fitted model
predictions <- predict(model, newdata = data.frame(MaxPred = pred_values), type = "response")

# Plot the predicted percentage chance of survival
plot(pred_values, predictions * 100, type = "l", xlab = "Early Intensity", ylab = "Percentage Chance of Survival", ylim = c(0, 100))

############

# Create MaxPredCat variable
EndBin$MaxPredCat <- EndBin$MaxPred <- round(EndBin$MaxPred / 5) * 5

# Calculate the percentage of "alive" status for each MaxPredCat value
alive_percentage <- aggregate(Status ~ MaxPredCat, data = EndBin, FUN = function(x) mean(x == 1) * 100)

# Plot the percentage alive against MaxPred
plot(alive_percentage$MaxPredCat, alive_percentage$Status, xlab = "Early Predation Bin", ylab = "Proportion Alive by End", ylim = c(0, 100), pch = 16)

### Plot them together
lines(pred_values, predictions * 100, type = "l", xlab = "Early Intensity", ylab = "Proportion Alive by End", ylim = c(0, 100))












############################################### Another Random Forest????

RFF <- End

RFF$PredCat <- ifelse(RFF$MaxPred == 0, "None",
                     ifelse(RFF$MaxPred <= 40, "Mild", "Severe"))

head(RFF)

# Subset the relevant columns from the dataframe, treat categories as factors!
RFF <- RFF[, c("Status", "MaxPred", "ColonySource", "Region", "Species", "SourceType",  "PredCat", "mean_parrotfish", "mean_butterflyfish", "InitialSize")]
RFF[, c("Status", "ColonySource", "Region", "Species", "SourceType", "PredCat")] <- lapply(RFF[, c("Status", "ColonySource", "Region", "Species", "SourceType", "PredCat")], as.factor)
RFF$mean_parrotfish <- as.numeric(RFF$mean_parrotfish)
RFF$mean_butterflyfish <- as.numeric(RFF$mean_butterflyfish)

####Remove comments from above lines for testing, else leave it alone!
## What happens if we remove region 1??? Only include region 3???
#RF <- subset(RF, RF$Region == "3")
## What about predicting categories of non-zero predation?
#RF <- RF[RF$PredCat != "0", ]
#RF$PredCat <- droplevels(RF$PredCat, exclude = "0")

table(RFF$PredCat)
table(RFF$Status)
table(RFF$MaxPred)

head(RFF)

RFF <- na.omit(RFF)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(nrow(RFF), nrow(RFF)*0.75)
train <- RFF[train_index, ]
test <- RFF[-train_index, ]

# Train the model using random forest
rf_model <- randomForest(Status ~ . , data = train)

# Predict the "PredCat" column of the test set using the trained model
test$pred <- predict(rf_model, newdata = test, type = "class")

#How much representation is in the training data?
table(train$Status)

# Create a confusion matrix to evaluate the model's accuracy
table(test$Status, test$pred)

rf_model

# Generate a random forest model with 1000 trees
rf_model <- randomForest(Status ~ . , data = train, ntree = 1000)

par(mfrow =c(1,1))

# Plot the variable importance of the random forest model
varImpPlot(rf_model)

# Predict the "Success" column of the test set using the trained model
test$pred <- predict(rf_model, newdata = test, type = "class")

# Create a confusion matrix to evaluate the model's accuracy
table(test$Status, test$pred)

rf_model

# Load the e1071 package
library(e1071)

# Create a confusion matrix
cm <- table(test$Status, test$pred)

# Print the confusion matrix
print(cm)

# Calculate and print the class error
class_error <- 1 - sum(diag(cm)) / sum(cm)
print(class_error)

# Test data confusion matrix
test_conf_matrix <- table(test$Status, test$pred)

# Error rates for each class
error_rate_alive <- test_conf_matrix[1, 2] / sum(test_conf_matrix[1, ])
error_rate_dead <- test_conf_matrix[2, 1] / sum(test_conf_matrix[2, ])

# Print the error rates
cat("Error rate for 'alive' class:", error_rate_alive, "\n")
cat("Error rate for 'dead' class:", error_rate_dead, "\n")



# Test data confusion matrix
test_conf_matrix <- table(test$Status, test$pred)

# Calculate total misclassifications (sum of off-diagonal elements)
total_misclassifications <- sum(test_conf_matrix) - sum(diag(test_conf_matrix))

# Calculate the total number of instances in the test data
total_instances <- sum(test_conf_matrix)

# Calculate the total error rate
total_error_rate <- total_misclassifications / total_instances

# Print the total error rate
cat("Total error rate:", total_error_rate, "\n")




# Access the average decision tree from the random forest model
avg_tree <- getTree(rf_model, k = 1, labelVar = TRUE)

# Plot the average decision tree
plot(avg_tree, main = "Average Decision Tree")
























######################### Post facto index

# Create combined scatter plot and smooth line plot
ggplot() +
  geom_point(data = prop_df_long, aes(x = Days, y = prop_yes)) +
  #geom_smooth(data = Early, aes(x = Days, y = predicted, color = Region), method = "gam", formula = y ~ s(x, bs = "cr", k = 4)) +
  geom_smooth(data = RT, aes(x = Days, y = predicted, color = Region)) +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0.1, linetype = "dashed") +
  geom_vline(xintercept = 178, linetype = "dashed") +
  labs(x = "Days Since Outplanting", y = "Proportion of Clusters that Experienced New Bites",
       title = "Partial Predation Prevalence Throughout the Survey")

x_values

### Find the integral of the model curves for each region up to the respective early interval ending days

# Convert 'Region' to a factor variable
RT$Region <- factor(RT$Region)

RT1 <- subset(complete_data, complete_data$Region == "1")
RT2 <- subset(complete_data, complete_data$Region == "2")
RT3 <- subset(complete_data, complete_data$Region == "3")
RT4 <- subset(complete_data, complete_data$Region == "4")
RT5 <- subset(complete_data, complete_data$Region == "5")
RT6 <- subset(complete_data, complete_data$Region == "6")

# Function to calculate the integral of the curve for a specific region
calculate_integral_for_region <- function(region_data, lower_limit, upper_limit) {
  # Function to calculate the curve values (y values) for a given x value
  calculate_curve_value <- function(x) {
    # Interpolate the y value for the given x using linear interpolation
    approx_x <- approx(region_data$Days, region_data$Predicted_Y, xout = x)
    return(approx_x$y)
  }
  
  # Calculate the integral using numerical integration
  integral_value <- integrate(calculate_curve_value, lower_limit, upper_limit)
  
  return(integral_value$value)
}

# Define the limits for each region
limits <- data.frame(
  Region = 1:6,
  Lower_Limit = c(7, 7, 7, 7, 7, 7),
  Upper_Limit = c(7, 178, 126, 87, 112, 44)
)

# Create an empty dataframe to store the results
PredationIndex <- data.frame(Region = integer(), Integral = numeric())

# Iterate through each region and calculate the integral
for (i in 1:nrow(limits)) {
  region_number <- limits$Region[i]
  lower_limit <- limits$Lower_Limit[i]
  upper_limit <- limits$Upper_Limit[i]
  
  # Get the corresponding dataframe for the region
  region_data <- get(paste0("RT", region_number))
  
  # Calculate the integral for the region
  integral_value <- calculate_integral_for_region(region_data, lower_limit, upper_limit)
  
  # Add the results to the PredationIndex dataframe
  PredationIndex <- rbind(PredationIndex, data.frame(Region = region_number, Prevalence = integral_value))
}

# Print the resulting dataframe
print(PredationIndex)

### Find the median max pred intensity value

summary(subset(EarlySpecificPreds, EarlySpecificPreds$Region == "6")$MaxPred)

# Convert Region to character if it's numeric (only needed if Region is numeric)
EarlySpecificPreds$Region <- as.character(EarlySpecificPreds$Region)

# Create an empty dataframe to store the medians
Medians <- data.frame(Region = integer(), MedianMaxPred = numeric())

# Iterate through each region and calculate the median
for (region in 1:6) {
  # Subset the data for the specific region
  region_data <- subset(EarlySpecificPreds, Region == as.character(region))
  
  # Calculate the median of MaxPred for the region
  median_value <- median(region_data$MaxPred)
  
  # Add the results to the Medians dataframe
  Medians <- rbind(Medians, data.frame(Region = region, MedianMaxPred = median_value))
}

# Print the resulting dataframe
print(Medians)

# Replace NAs with 0 in the MedianMaxPred column
Medians$MedianMaxPred <- replace(Medians$MedianMaxPred, is.na(Medians$MedianMaxPred), 0)

Median <- Medians

Median$MedianMaxPred <- Median$MedianMaxPred/100+ 1

print(Median)

PredationIndex$Intensity <- Median$MedianMaxPred

print(PredationIndex)

#Multiply 1 + the early Prevalence by the early Intensity

PredationIndex$Index <- round((1 + PredationIndex$Prevalence) * PredationIndex$Intensity)

### PLOP!

print(PredationIndex)

############################## AUC for each region

library(pROC)

# Make predictions using the fitted model
RT$Predicted_Probability <- predict(glm_reduced, type = "response")

# Calculate the AUC using the roc function from pROC
roc_obj <- roc(RT$FishBites, RT$Predicted_Probability)

# Access the AUC value
auc_value <- auc(roc_obj)

# Print the AUC value
print(auc_value)


# Assuming you have your glm_reduced model already fitted and stored in the variable glm_reduced
# and your dataframe is RT

# Define the x_values for each Region
x_values <- data.frame(
  Region = 1:6,
  X_Value = c(7, 178, 126, 87, 112, 44)
)

# Function to calculate AUC for a specific Region up to its corresponding x value
calculate_auc_for_region <- function(region_data, x_value) {
  # Calculate the predicted probabilities using the fitted model
  region_data$Predicted_Probability <- predict(glm_reduced, newdata = region_data, type = "response")
  
  # Filter the data up to the corresponding x value
  region_data <- region_data[region_data$Days <= x_value, ]
  
  # Calculate the AUC using the roc function from pROC
  roc_obj <- roc(region_data$FishBites, region_data$Predicted_Probability)
  
  # Access the AUC value
  auc_value <- auc(roc_obj)
  
  return(auc_value)
}

# Iterate through each Region and calculate AUC
for (i in 1:nrow(x_values)) {
  region_number <- x_values$Region[i]
  x_value <- x_values$X_Value[i]
  
  # Filter the data for the specific Region
  region_data <- RT[RT$Region == region_number, ]
  
  # Calculate AUC for the Region up to its corresponding x value
  auc_value <- calculate_auc_for_region(region_data, x_value)
  
  # Print the AUC value along with the Region number
  cat("Region", region_number, "| AUC:", auc_value, "\n")
}

