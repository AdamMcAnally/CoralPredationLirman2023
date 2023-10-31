# Load Packages

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(changepoint)
library(ISwR)
library(MASS)
library(FactoMineR)
library(factoextra)
library(gplots)
library(graphics)
library(corrplot)
library(rstatix)
library(rpart.plot)
library(randomForest)
library(dplyr)
library(DHARMa)

################################################################################## Set up the dataframe
##########################################################################################################################
##########################################################################################################################

# Read in the original data
RT<-read.csv('C:/Users/Adam/Desktop/Spring_2023/RT_Lirman/Original_RT.csv')
head(RT)

# Clean up the empty fields, "missing" values, correct miss-dated samples, add source type
class(RT$Date)
RT$Date <- gsub("07-Jan-21", "07-Jan-22", RT$Date)
RT$Miss <- gsub("-999", "100", RT$Miss)
RT$Width1cm <- gsub("-999", "NA", RT$Width1cm)
RT$Width1cm <- gsub("(?<!1)0", "NA", RT$Width1cm, perl = TRUE)
RT$Width1cm <- gsub("#NULL!", "NA", RT$Width1cm)
RT$Width2cm <- gsub("-999", "NA", RT$Width2cm)
RT$Width2cm <- gsub("(?<!1)0", "NA", RT$Width2cm, perl = TRUE)
RT$Width2cm <- gsub("#NULL!", "NA", RT$Width2cm)
RT$Snails <- ifelse(RT$Snails == "", "no", RT$Snails)
RT$FishBites <- ifelse(RT$FishBites == "", "no", RT$FishBites)
RT$Bleach <- ifelse(RT$Bleach == "", "N", RT$Bleach)
RT$DiseaseCondition <- ifelse(RT$DiseaseCondition == "", "H", RT$DiseaseCondition)

# Replace ColonySource values for specific rows
RT$ColonySource[13092] <- "MML"
RT$ColonySource[18550] <- "UM"

RT <- RT %>%
  mutate(SourceType = case_when(
    ColonySource %in% c("RR", "CRF", "FWC") ~ "in_situ",
    ColonySource %in% c("MML", "UM") ~ "ex_situ",
    TRUE ~ NA_character_ # handles any other values that aren't explicitly specified
  ))

# View the dataset

head(RT)

############# Convert dates to number of days since outplanting

### Original Code, assumed same outplanting day for all which is less realistic and skews results. New code will assume outplanting occurred 7 days before the first survey in each region
# Add leading zero to day part of the date if it is a single digit
#RT$Date <- gsub("^([0-9])-", "0\\1-", RT$Date)

# Convert Date column to date format
#RT$Date <- as.Date(paste0(substr(RT$Date, 1, 2), "-", substr(RT$Date, 4, 6), "-", substr(RT$Date, 8, 9)), format = "%d-%b-%y")

#RT$Days <- as.integer(RT$Date-min(RT$Date))


# Add leading zero to day part of the date if it is a single digit
RT$Date <- gsub("^([0-9])-", "0\\1-", RT$Date)

# Convert Date column to date format
RT$Date <- as.Date(paste0(substr(RT$Date, 1, 2), "-", substr(RT$Date, 4, 6), "-", substr(RT$Date, 8, 9)), format = "%d-%b-%y")

# Calculate the number of days since the first date value in each Region
RT <- RT %>%
  group_by(Region) %>%
  mutate(Days = as.integer(Date - min(Date))) %>%
  ungroup()

RT$Days <- RT$Days + 7





summary(RT$Days)

######################################################################## Add information from the fish surveys
##########################################################################################################################
##########################################################################################################################

#### Previous analyses do show that the surveys should be reliable/representative enough to use this way, can add the code here later

# Read in the dataframe

Finfish<-read.csv('C:/Users/Adam/Desktop/Spring_2023/RT_Lirman/Corrected_Fish.csv')
head(Finfish)

Finfish <- Finfish %>%
  rename(Parrotfish = `X..Parrotfishgt10cm`)

Finfish <- Finfish %>%
  rename(Butterflyfish = `X..Butterflyfish`)  

# Calculate mean parrotfish abundance for each site within each region
Finfish_avg <- Finfish %>%
  group_by(Region, Site) %>%
  summarize(mean_parrotfish = mean(Parrotfish), mean_butterflyfish = mean(Butterflyfish), .groups="drop")

head(Finfish_avg)

# Merge the two data frames based on Region and Site columns
RT <- merge(RT, Finfish_avg, by = c("Region", "Site"), all.x = TRUE)

###################################################################################### Add Initial Size Information
##########################################################################################################################
##########################################################################################################################

# More reliable data available later hopefully?

# Group the data by ColonyNumber, get the row with the lowest MonitoringInterval, then create a new variable called "InitialSize" = product of the first recorded Width1cm and Width2cm
Size_subset <- RT %>%
  group_by(ColonyNumber) %>%
  slice(which.min(MonitoringInterval))

head(Size_subset)
Size_subset$Width1cm <- as.numeric(Size_subset$Width1cm)
Size_subset$Width2cm <- as.numeric(Size_subset$Width2cm)
Size_subset$InitialSize <- Size_subset$Width1cm*Size_subset$Width2cm

RT$InitialSize <- Size_subset$InitialSize

######################################################################################## Let's start looking for patterns
##########################################################################################################################
##########################################################################################################################

head(RT)

# Subset the required columns from the original dataframe
corrRT <- RT[, c("ColonyNumber", "MonitoringInterval", "Days", "Species", "Genotype", "ColonySource", "SourceType", "Region", "Site", "mean_parrotfish", "mean_butterflyfish", "Status", "Live", "Miss", "DZ", "Pred", "DiseaseCondition", "Snails", "FishBites", "Bleach")]

# Assign dummy variables for non-numeric values
corrRT$DiseaseCondition <- ifelse(RT$DiseaseCondition == "H", 0, 1)
corrRT$Snails <- ifelse(RT$Snails == "no", 0, 1)
corrRT$FishBites <- ifelse(RT$FishBites == "no", 0, 1)

# Convert "Bleach" variable to numeric categories
corrRT$Bleach <- ifelse(RT$Bleach == "N", 0, ifelse(RT$Bleach == "P", 1, ifelse(RT$Bleach == "PB", 2, 3)))

# Assign dummy variables for non-numeric values in "SourceType"
corrRT$SourceType <- ifelse(RT$SourceType == "in_situ", 1, 2)

# Convert unique strings to numeric values in "ColonySource," "Genotype," and "Species" columns
corrRT$ColonySource <- as.numeric(as.factor(RT$ColonySource))
corrRT$Genotype <- as.numeric(as.factor(RT$Genotype))
corrRT$Species <- as.numeric(as.factor(RT$Species))

# Assign numeric values to "Status" column (1 for "alive" and 0 for any other value)
corrRT$Status <- ifelse(RT$Status == "alive", 1, ifelse(is.na(RT$Status), -1, 0))

# Print the head of the corrRT dataframe
head(corrRT)

# Convert all variables in corrRT to numeric
corrRT <- as.data.frame(lapply(corrRT, as.numeric))

# Check the class of each variable in corrRT
for (col in names(corrRT)) {
  variable_class <- class(corrRT[[col]])
  print(paste("Variable:", col, "| Class:", variable_class))
}

#### Just bugsplatting things
# Replace ColonySource values for specific rows
RT$ColonySource[13092] <- "MML"
RT$ColonySource[18550] <- "UM"

RT <- RT %>%
  mutate(SourceType = case_when(
    ColonySource %in% c("RR", "CRF", "FWC") ~ "in_situ",
    ColonySource %in% c("MML", "UM") ~ "ex_situ",
    TRUE ~ NA_character_ # handles any other values that aren't explicitly specified
  ))
########################

# Check for missing values in each variable
for (col in names(corrRT)) {
  missing_values <- sum(is.na(corrRT[[col]]))
  print(paste("Variable:", col, "| Missing Values:", missing_values))
}

########################################################## FishBites positive relationship with Pred (duh), FishBites and Pred negative relationship with time
##################################Relationship with region/site may be nonlinear, or may be masked by later timepoints 

#### STOP THIS TAKES FOREVER
#pairs(corrRT)

################################################################################################################
##### Time series of proportion of clusters with fish bites per time series to visualize predation signature
### By Monitoring Interval

# Compute proportion of "yes" values in FishBites column, grouped by Time Interval
prop_df_o <- RT %>%
  group_by(`Days`) %>%
  summarize(prop_yes = mean(FishBites == "yes"))

# Create scatter plot
ggplot(prop_df_o, aes(x = `Days`, y = `1`,`2`,`3`,`4`,`5`,`6`, color = )) +
  geom_point() +
  theme(panel.grid = element_blank()) +
  labs(x = "Days Since Outplanting", y = "Proportion of Clusters that Experienced New Bites", title = "Partial Predation Prevalence Throughout the Survey")

prop_df_o

prop_df <- RT %>%
  group_by(Region, `Days`) %>%
  summarize(prop_yes = mean(FishBites == "yes")) %>%
  pivot_wider(names_from = Region, values_from = prop_yes)

prop_df

# Rename the region columns
col_names <- c("Days", "Region 1", "Region 2", "Region 3", "Region 4", "Region 5", "Region 6")
names(prop_df) <- col_names

# Reshape the data from wide to long format
prop_df_long <- prop_df %>%
  pivot_longer(cols = -Days, names_to = "Region", values_to = "prop_yes") %>%
  na.omit()

# Create scatter plot
ggplot(prop_df_long, aes(x = Days, y = prop_yes, color = Region)) +
  geom_point() +
  theme(panel.grid = element_blank()) +
  labs(x = "Days Since Outplanting", y = "Proportion of Clusters that Experienced New Bites", title = "Partial Predation Prevalence Throughout the Survey")

table(RT$FishBites)
RT$FishBites <- as.factor(RT$FishBites)
RT$Region <- as.factor(RT$Region)
RT$Site <- as.factor(RT$Site)

RT$Predbi <- RT$Pred/100

# Fit GLM to proportion data with logit link function and quadratic term for date
glm_fit <- glm(FishBites ~ poly(Days, 2) * Region/Site + Species + ColonySource + mean_parrotfish + mean_butterflyfish, family = "binomial", data = RT)
#plot(glm_fit)

anova(glm_fit, test = "Chisq")

glm_reduced <- glm(FishBites ~ poly(Days, 2) * Region, family = "binomial", data = RT)
anova(glm_reduced, test = "Chisq")

library(DHARMa)
temp <- simulateResiduals(glm_fit)
#plot(temp)

summary(glm_reduced)

RT$predicted <- predict(glm_reduced, type = "response")

head(RT)

# Create line plot with reduced GLM curve for each region
ggplot(RT, aes(x = Days, y = predicted, color = Region)) +
  geom_smooth() +
  theme(panel.grid = element_blank()) +
  labs(x = "Days Since Outplanting", y = "Predicted Proportion of Bitten Clusters by Region")

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



#### Search to find test material

summary(RT$Days)

# Create X_values as every whole number from 7 to 573
X_values <- seq(7, 573, by = 1)

# Create a dataframe with all possible combinations of Region and Days
complete_data <- expand.grid(Region = unique(RT$Region), Days = X_values)

# Convert Region in complete_data to a factor with correct levels
complete_data$Region <- as.factor(complete_data$Region)

# Predict the response variable for the glm_reduced model using the complete_data
predicted_values <- predict(glm_reduced, newdata = complete_data, type = "response")

# Add the predicted values to the complete_data dataframe
complete_data$Predicted_Y <- predicted_values

# Plot smooth curves for each region
ggplot(complete_data, aes(x = Days, y = Predicted_Y, color = as.factor(Region))) +
  geom_line(size = 1) +
  labs(x = "Days", y = "Predicted Y", title = "Smooth Curves for Each Region") +
  theme_minimal() +
  theme(legend.position = "right")




# Set the desired y value
y_value <- 0.1

# Predict the response variable for the reduced model
RT$predicted <- predict(glm_reduced, type = "response")

# Find the x value for each region in complete_data
x_values <- by(complete_data, complete_data$Region, function(df) {
  # Get the predicted values for the current region
  predicted <- predict(glm_reduced, newdata = df, type = "response")
  
  # Find the index where predicted drops below 0.1
  below_threshold_index <- which(predicted < y_value)[1]
  
  if (is.na(below_threshold_index)) {
    # If all predicted values are below the threshold, set x as 0
    x <- 0
  } else {
    # Get the x value corresponding to the index where predicted drops below 0.1
    x <- df$Days[below_threshold_index]
  }
  
  return(x)
})

# Print the x values for each region
print(x_values)

#### OLD CODE, NOT AS PRECISE
# Set the desired y value
#y_value <- 0.1

# Predict the response variable for the reduced model
#RT$predicted <- predict(glm_reduced, type = "response")

# Find the x value for each region
#x_values <- by(RT, RT$Region, function(df) {
#  x <- with(df, Days[which.min(abs(predicted - y_value))])
#  return(x)
#})

# Print the x values for each region
#print(x_values)

#summary(subset(RT, RT$Region == 1),RT$predicted)

#glm_reduced_early <- glm(FishBites ~ poly(Days, 2) * Region, family = "binomial", data = subset(RT, RT$Days < 177))
#anova(glm_reduced_early, test = "Chisq")

Early <- subset(RT, RT$Days < 178)
# Predict the response variable for the reduced model
#Early$predicted <- predict(glm_reduced_early, type = "response")


# Predict values and confidence intervals
#RT <- transform(RT, predicted = predict(glm_fit, newdata = RT, type = "response"))
#RT <- cbind(RT, conf_int = predict(glm_fit, newdata = RT, type = "link", se.fit = TRUE)$se.fit)

# Convert confidence intervals to response scale
#RT$lower <- plogis(RT$predicted - 1.96 * RT$conf_int)
#RT$upper <- plogis(RT$predicted + 1.96 * RT$conf_int)

##############################################################################################
############################################################### Predation Prevalence and Intensity in First Monitoring Interval
####################################### Differences between regions? Differences between species?
####################################################Prevalence, Chi-Squared

library(corrplot)

RTFirst <- subset(RT, RT$MonitoringInterval == 1)
RTAlive <- subset(RT, RT$Status == "alive")
RTFirstAlive <- subset(RTFirst, RTFirst$Status == "alive")

RTCS <- chisq.test(RTAlive$FishBites, RTAlive$Species)
RTCS
corrplot(RTCS$residuals, is.cor = FALSE)

# Create a plot of residuals
corrplot(RTCS$residuals, is.cor = FALSE,
         title = "Residual Plot for Predation Prevalence by Species",
         xlab = "Species",
         ylab = "Presence of Parrotfish Bites",
         cl.length = 5,  # Set the length of the color bar to 5
         mar = c(4, 4, 2, 2),  # Adjust the margin size
         cex.main = 2.5,  # Adjust the font size of the title
         tl.cex = 2)  # Adjust the font size of the labels

         
contrib <- 100*RTCS$residuals^2/RTCS$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

RTCR <- chisq.test(RTFirstAlive$FishBites, RTFirstAlive$Region)
RTCR
corrplot(RTCR$residuals, is.cor = FALSE,
         title = "Residual Plot for Predation Prevalence by Region",
         xlab = "Species",
         ylab = "Presence of Parrotfish Bites",
         cl.length = 5,  # Set the length of the color bar to 5
         mar = c(4, 4, 2, 2),
         cex.main = 2.5,  # Adjust the font size of the title
         tl.cex = 2)  # Adjust the font size of the labels

contrib <- 100*RTCR$residuals^2/RTCS$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

##################################################################### Intensity, Kruskal-Wallace

FirstInt <- subset(RTFirst, RTFirst$Pred != 0)
FirstInt <- subset(FirstInt, FirstInt$FishBites == "yes" & Snails == "no")

# Perform the Kruskal-Wallis test
kruskal_result <- kruskal.test(Pred ~ Species, data = FirstInt)

# Create a box and whisker plot
boxplot(Pred ~ Species, data = FirstInt,
        xlab = "Species",
        ylab = "Percent Tissue Removed",
        main = "Boxplot of Percent Tissue Removed by Species")

# Calculate median values for each group
medians <- aggregate(Pred ~ Species, data = FirstInt, median)

# Add median values to the plot
text(x = seq_along(medians$Species), y = medians$Pred, labels = medians$Pred, pos = 3, col = "red")

# Add p-value to the plot
p_value <- format(kruskal_result$p.value, digits = 4)
text(x = 1, y = max(FirstInt$Pred), labels = paste("p-value =", p_value))

# Perform post-hoc tests
posthoc_result <- pairwise.wilcox.test(FirstInt$Pred, FirstInt$Species, p.adjust.method = "holm")

# Print the post-hoc test results
print(posthoc_result)

# Create a boxplot
p <- ggplot(FirstInt, aes(x = Species, y = Pred)) +
  geom_boxplot() +
  xlab("Species") +
  ylab("Percent Tissue Removed") +
  ggtitle("Boxplot of Percent Tissue Removed by Species")

library(ggpubr)

# Add p-value annotations
Species <- p + stat_compare_means(comparisons = list(c("MCAV", "OFAV"), c("PCLI", "OFAV"), c("MCAV", "PCLI")),
                            method = "wilcox.test",
                            label = "p.signif",
                            hide.ns = TRUE,
                            tip.length = 0.02)

# Add median values to the plot
Species <- Species + text(x = seq_along(medians$Species), y = medians$Pred, labels = medians$Pred, pos = 3, col = "red")

# Display the plot
print(Species)

kruskal_result <- kruskal.test(Pred ~ Region, data = RTFirst)

# Create a box and whisker plot
boxplot(Pred ~ Region, data = FirstInt,
        xlab = "Region",
        ylab = "Percent Tissue Removed",
        main = "Boxplot of Percent Tissue Removed by Region")

# Calculate median values for each group
medians <- aggregate(Pred ~ Region, data = FirstInt, median)

# Add median values to the plot
text(x = seq_along(medians$Region), y = medians$Pred, labels = medians$Pred, pos = 3, col = "red")

# Add p-value to the plot
p_value <- format(kruskal_result$p.value, digits = 4)
text(x = 1, y = max(FirstInt$Pred), labels = paste("p-value =", p_value))

# Perform pairwise Wilcoxon rank sum tests with ties
posthoc_result <- pairwise.wilcox.test(FirstInt$Pred, FirstInt$Region, p.adjust.method = "holm", exact = FALSE)

# Print the post-hoc test results
print(posthoc_result)

# Print the post-hoc test results
print(posthoc_result)

# Create a boxplot
p <- ggplot(FirstInt, aes(x = Region, y = Pred)) +
  geom_boxplot() +
  xlab("Region") +
  ylab("Percent Tissue Removed") +
  ggtitle("Boxplot of Percent Tissue Removed by Region")

# Define the pairwise comparisons
pairwise_comparisons <- list(c("1", "2"), c("1", "3"), c("1", "4"), c("1", "5"), c("1", "6"),
                             c("2", "3"), c("2", "4"), c("2", "5"), c("2", "6"),
                             c("3", "4"), c("3", "5"), c("3", "6"),
                             c("4", "5"), c("4", "6"),
                             c("5", "6"))

# Add p-value annotations
Region <- p + stat_compare_means(comparisons = pairwise_comparisons,
                            method = "wilcox.test",
                            label = "p.signif",
                            hide.ns = TRUE,
                            tip.length = 0.02,
                            exact = FALSE)
# Display the plot
print(Region)

#################################################################################################################
############################################################## RANDOM FOREST
head(Early)

# Group the data by ColonyNumber and find the maximum Pred value for each group
Early <- Early %>%
  group_by(ColonyNumber) %>%
  mutate(MaxPred = max(Pred)) %>%
  ungroup()

# Assign the maximum Pred value to all rows of each ColonyNumber
Early$MaxPred <- Early$MaxPred

Early$PredCat <- ifelse(Early$MaxPred == 0, "None",
                        ifelse(Early$MaxPred <= 40, "Mild", "Severe"))

# Only include the final MonitoringInterval for the random forest, to look at classifying predation occurrence and intensity at the end of the early feeding frenzy!
RF <- subset(Early, Early$MonitoringInterval == "1")

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

#################################################################### Random Forest for Realzies??? Look at this idea more later

set.seed(123)

# Number of iterations for training the model
num_iterations <- 10

# Initialize an empty vector to store predictions
predictions <- rep(NA, nrow(RF))

# Loop through the iterations and train the model
for (i in 1:num_iterations) {
  # Split the data into training and testing sets using random sampling
  train_index <- sample(nrow(RF), nrow(RF)*0.75)
  train <- RF[train_index, ]
  test <- RF[-train_index, ]
  
  # Train the model using random forest
  rf_model <- randomForest(PredCat ~ . , data = train)
  
  # Predict the "PredCat" column of the test set using the trained model
  predictions[-train_index] <- predict(rf_model, newdata = test, type = "class")
}

rf_model

# Plot the variable importance of the random forest model
varImpPlot(rf_model)

# Create a confusion matrix to evaluate the model's accuracy
table(RF$PredCat, predictions)

