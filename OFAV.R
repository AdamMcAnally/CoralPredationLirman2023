##############################################################################################
############################################################### Predation Prevalence and Intensity for OFAV
####################################### Differences between regions? Differences between genotypes?
####################################################Prevalence, Chi-Squared

#Re-verify which genotypes are distributed evenly for easy analysis
##### For now just run this after CheckingCovariate.R

ofavsub

# Run the Fisher's exact test
fisher_result <- fisher.test(ofavsub)

# Print the test results
print(fisher_result)

# Get the column names as a list
column_names <- colnames(ofavsub)

# Print the list of column names
print(column_names)

OFAV <- subset(RT, RT$Species == "OFAV")

# Subset the rows in OFAV based on matching values in Genotype column
GenOFAV <- OFAV[OFAV$Genotype %in% column_names, ]

# Print the subsetted dataframe
head(GenOFAV)

count(GenOFAV)

OFirst <- subset(GenOFAV, GenOFAV$MonitoringInterval == 1)
OSecond <- subset(GenOFAV, GenOFAV$MonitoringInterval == 2)

###### Let's look at bootstrapping!
library(boot)

GenOFAV$FishBitesN <- as.numeric(GenOFAV$FishBites == "yes")

# Define a function to calculate the statistic of interest
statistic <- function(data, i) {
  # Perform calculations on the bootstrap sample
  # Here you can specify the statistic you want to calculate
  mean(data$FishBitesN[i])
}

# Perform bootstrapping
boot_results <- boot(data = GenOFAV, statistic = statistic, R = 100)

# Print the bootstrap results
print(boot_results)

####### Let's make random subsets of 5 over 100 iterations and conglomerate them

library(dplyr)
library(purrr)

# Set the number of repetitions and number of random rows
repetitions <- 100
num_random_rows <- 5

# Create a function to perform the subsetting
subset_random_rows <- function(df) {
  df %>% group_by(Genotype) %>% sample_n(num_random_rows, replace = TRUE)
}

# Perform the subsetting 100 times and compile the resulting dataframes
CGLM <- replicate(repetitions, subset_random_rows(OFirst), simplify = FALSE) %>%
  bind_rows()

# Print the resulting dataframe
head(CGLM)







#################################################### First Monitoring Interval

library(corrplot)

OCS <- chisq.test(OFirst$FishBites, OFirst$Genotype)
OCS
corrplot(OCS$residuals, is.cor = FALSE, 
         title = "Residual Plot for Predation Prevalence by Genotype (First Interval)",
         xlab = "Genotype",
         ylab = "Presence of Parrotfish Bites",
         cl.length = 5,  # Set the length of the color bar to 5
         mar = c(4, 4, 2, 2),  # Adjust the margin size
         cex.main = 2.5,  # Adjust the font size of the title
         tl.cex = 2)  # Adjust the font size of the labels)

# Create a plot of residuals
corrplot(OCS$residuals, is.cor = FALSE,
         title = "Residual Plot for Predation Prevalence by Genotype of OFAV (First Interval)",
         xlab = "Species",
         ylab = "Presence of Parrotfish Bites",
         cl.length = 5,  # Set the length of the color bar to 5
         mar = c(4, 4, 2, 2),  # Adjust the margin size
         cex.main = 2.5,  # Adjust the font size of the title
         tl.cex = 2)  # Adjust the font size of the labels


contrib <- 100*OCS$residuals^2/OCS$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

OCR <- chisq.test(OFirst$FishBites, OFirst$Region)
OCR
corrplot(OCR$residuals, is.cor = FALSE,
         title = "Residual Plot for Predation Prevalence by Region (First Interval)",
         xlab = "Region",
         ylab = "Presence of Parrotfish Bites",
         cl.length = 5,  # Set the length of the color bar to 5
         mar = c(4, 4, 2, 2),
         cex.main = 2.5,  # Adjust the font size of the title
         tl.cex = 2)  # Adjust the font size of the labels

contrib <- 100*OCR$residuals^2/OCS$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)





##################################################################### Intensity, Kruskal-Wallace

OFirstInt <- subset(OFirst, OFirst$Pred != 0)
OFirstInt <- subset(OFirstInt, OFirstInt$FishBites == "yes" & Snails == "no")

# Perform the Kruskal-Wallis test
kruskal_result <- kruskal.test(Pred ~ Genotype, data = OFirstInt)

kruskal_result

# Create a box and whisker plot
boxplot(Pred ~ Genotype, data = OFirstInt,
        xlab = "Genotype",
        ylab = "Percent Tissue Removed",
        main = "Boxplot of Percent Tissue Removed by Region")

# Calculate median values for each group
medians <- aggregate(Pred ~ Genotype, data = OFirstInt, median)

# Add median values to the plot
text(x = seq_along(medians$Genotype), y = medians$Pred, labels = medians$Pred, pos = 3, col = "red")

# Add p-value to the plot
p_value <- format(kruskal_result$p.value, digits = 4)
text(x = 1, y = max(OFirstInt$Pred), labels = paste("p-value =", p_value))

# Perform pairwise Wilcoxon rank sum tests with ties
posthoc_result <- pairwise.wilcox.test(OFirstInt$Pred, OFirstInt$Genotype, p.adjust.method = "holm", exact = FALSE)

# Print the post-hoc test results
print(posthoc_result)

# Create a boxplot
p <- ggplot(OFirstInt, aes(x = Genotype, y = Pred)) +
  geom_boxplot() +
  xlab("Genotype") +
  ylab("Percent Tissue Removed") +
  ggtitle("Boxplot of Percent Tissue Removed by Genotype")

# Define the pairwise comparisons
pairwise_comparisons <- list(c("14", "15"), c("14", "36"), c("14", "50"), c("15", "36"), c("15", "50"), c("36", "50"))

# Add p-value annotations
Genotype <- p + stat_compare_means(comparisons = pairwise_comparisons,
                                   method = "wilcox.test",
                                   label = "p.signif",
                                   hide.ns = TRUE,
                                   tip.length = 0.02,
                                   p.adjust.method = "holm",  # Specify the p-value adjustment method
                                   exact = FALSE)
# Display the plot
print(Genotype)

########################################################################################

# Perform the Kruskal-Wallis test
kruskal_result <- kruskal.test(Pred ~ Region, data = OFirstInt)

kruskal_result

# Create a box and whisker plot
boxplot(Pred ~ Region, data = OFirstInt,
        xlab = "Region",
        ylab = "Percent Tissue Removed",
        main = "Boxplot of Percent Tissue Removed by Region")

# Calculate median values for each group
medians <- aggregate(Pred ~ Region, data = OFirstInt, median)

# Add median values to the plot
text(x = seq_along(medians$Region), y = medians$Pred, labels = medians$Pred, pos = 3, col = "red")

# Add p-value to the plot
p_value <- format(kruskal_result$p.value, digits = 4)
text(x = 1, y = max(OFirstInt$Pred), labels = paste("p-value =", p_value))

# Perform pairwise Wilcoxon rank sum tests with ties
posthoc_result <- pairwise.wilcox.test(OFirstInt$Pred, OFirstInt$Region, p.adjust.method = "holm", exact = FALSE)

# Print the post-hoc test results
print(posthoc_result)

# Create a boxplot
p <- ggplot(OFirstInt, aes(x = Region, y = Pred)) +
  geom_boxplot() +
  xlab("Region") +
  ylab("Percent Tissue Removed") +
  ggtitle("Boxplot of Percent Tissue Removed by Region")

# Define the pairwise comparisons
pairwise_comparisons <- list(c("2", "3"), c("2", "4"), c("2", "5"), c("2", "6"),
                             c("3", "4"), c("3", "5"), c("3", "6"),
                             c("4", "5"), c("4", "6"),
                             c("5", "6"))

# Add p-value annotations
Region <- p + stat_compare_means(comparisons = pairwise_comparisons,
                                  method = "wilcox.test",
                                  label = "p.signif",
                                  hide.ns = TRUE,
                                  tip.length = 0.02,
                                  p.adjust.method = "holm",  # Specify the p-value adjustment method
                                  exact = FALSE)
# Display the plot
print(Region)
##########################################################################################################################################################



















#################################################### Second Monitoring Interval

library(corrplot)

OCS <- chisq.test(OSecond$FishBites, OSecond$Genotype)
OCS
corrplot(OCS$residuals, is.cor = FALSE,
         title = "Residual Plot for Predation Prevalence by Genotype (Second Interval)",
         xlab = "Genotype",
         ylab = "Presence of Parrotfish Bites",
         cl.length = 5,  # Set the length of the color bar to 5
         mar = c(4, 4, 2, 2),  # Adjust the margin size
         cex.main = 2.5,  # Adjust the font size of the title
         tl.cex = 2)  # Adjust the font size of the labels)

# Create a plot of residuals
corrplot(OCS$residuals, is.cor = FALSE,
         title = "Residual Plot for Predation Prevalence by Genotype of OFAV",
         xlab = "Species",
         ylab = "Presence of Parrotfish Bites",
         cl.length = 5,  # Set the length of the color bar to 5
         mar = c(4, 4, 2, 2),  # Adjust the margin size
         cex.main = 2.5,  # Adjust the font size of the title
         tl.cex = 2)  # Adjust the font size of the labels


contrib <- 100*OCS$residuals^2/OCS$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

OCR <- chisq.test(OSecond$FishBites, OSecond$Region)
OCR
corrplot(OCR$residuals, is.cor = FALSE,
         title = "Residual Plot for Predation Prevalence by Region (Second Interval)",
         xlab = "Region",
         ylab = "Presence of Parrotfish Bites",
         cl.length = 5,  # Set the length of the color bar to 5
         mar = c(4, 4, 2, 2),
         cex.main = 2.5,  # Adjust the font size of the title
         tl.cex = 2)  # Adjust the font size of the labels

contrib <- 100*OCR$residuals^2/OCS$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

##################################################################### Intensity, Kruskal-Wallace

OSecondInt <- subset(OSecond, OSecond$Pred != 0)
OSecondInt <- subset(OSecondInt, OSecondInt$FishBites == "yes" & Snails == "no")

# Perform the Kruskal-Wallis test
kruskal_result <- kruskal.test(Pred ~ Genotype, data = OSecondInt)

kruskal_result

# Create a box and whisker plot
boxplot(Pred ~ Genotype, data = OSecondInt,
        xlab = "Genotype",
        ylab = "Percent Tissue Removed",
        main = "Boxplot of Percent Tissue Removed by Genotype")

# Calculate median values for each group
medians <- aggregate(Pred ~ Genotype, data = OSecondInt, median)

# Add median values to the plot
text(x = seq_along(medians$Genotype), y = medians$Pred, labels = medians$Pred, pos = 3, col = "red")

# Add p-value to the plot
p_value <- format(kruskal_result$p.value, digits = 4)
text(x = 1, y = max(OSecondInt$Pred), labels = paste("p-value =", p_value))

# Perform pairwise Wilcoxon rank sum tests with ties
posthoc_result <- pairwise.wilcox.test(OSecondInt$Pred, OSecondInt$Genotype, p.adjust.method = "holm", exact = FALSE)

# Print the post-hoc test results
print(posthoc_result)

# Create a boxplot
p <- ggplot(OSecondInt, aes(x = Genotype, y = Pred)) +
  geom_boxplot() +
  xlab("Genotype") +
  ylab("Percent Tissue Removed") +
  ggtitle("Boxplot of Percent Tissue Removed by Genotype")

# Define the pairwise comparisons
pairwise_comparisons <- list(c("14", "15"), c("14", "36"), c("14", "50"), c("15", "36"), c("15", "50"), c("36", "50"))

# Add p-value annotations
Genotype <- p + stat_compare_means(comparisons = pairwise_comparisons,
                                   method = "wilcox.test",
                                   label = "p.signif",
                                   hide.ns = TRUE,
                                   tip.length = 0.02,
                                   p.adjust.method = "holm",  # Specify the p-value adjustment method
                                   exact = FALSE)
# Display the plot
print(Genotype)

########################################################################################

# Perform the Kruskal-Wallis test
kruskal_result <- kruskal.test(Pred ~ Region, data = OSecondInt)

kruskal_result

# Create a box and whisker plot
boxplot(Pred ~ Region, data = OSecondInt,
        xlab = "Region",
        ylab = "Percent Tissue Removed",
        main = "Boxplot of Percent Tissue Removed by Region")

# Calculate median values for each group
medians <- aggregate(Pred ~ Region, data = OSecondInt, median)

# Add median values to the plot
text(x = seq_along(medians$Region), y = medians$Pred, labels = medians$Pred, pos = 3, col = "red")

# Add p-value to the plot
p_value <- format(kruskal_result$p.value, digits = 4)
text(x = 1, y = max(OSecondInt$Pred), labels = paste("p-value =", p_value))

# Perform pairwise Wilcoxon rank sum tests with ties
posthoc_result <- pairwise.wilcox.test(OSecondInt$Pred, OSecondInt$Region, p.adjust.method = "holm", exact = FALSE)

# Print the post-hoc test results
print(posthoc_result)

# Create a boxplot
p <- ggplot(OSecondInt, aes(x = Region, y = Pred)) +
  geom_boxplot() +
  xlab("Region") +
  ylab("Percent Tissue Removed") +
  ggtitle("Boxplot of Percent Tissue Removed by Region")

# Define the pairwise comparisons
pairwise_comparisons <- list(c("2", "3"), c("2", "4"), c("2", "5"), c("2", "6"),
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























###########################################################################

library(lme4)

Int <- OFirstInt

# Duplicate all rows in the dataframe MC
Int_duplicate <- rbind(Int, Int)

# Change the value in the Genotype column to "OFAV" for the duplicated rows
Int_duplicate$Genotype <- ifelse(seq_len(nrow(Int_duplicate)) > nrow(Int), "OFAV", Int_duplicate$Genotype)

Int <- Int_duplicate

# Identify rows with "OFAV" in the Genotype column
ofav_rows <- Int$Genotype == "OFAV"

# Create a new dataframe with "OFAV" rows at the beginning
Int_reordered <- rbind(Int[ofav_rows, ], Int[!ofav_rows, ])

# Reset row names of the reordered dataframe
rownames(Int_reordered) <- NULL

# Check the reordered dataframe
head(Int_reordered)

Int <- Int_reordered

head(Int)

# Create the boxplot
ggplot(Int, aes(x = Genotype, y = Pred)) +
  geom_boxplot() +
  labs(x = "Genotype", y = "Pred") +
  theme_minimal()

#Gaussian assumes normality. The response variable isn't normal but this is a start

library(multcomp)

Int$Genotype <- as.factor(Int$Genotype)

class(Int$Genotype)

lm_model_intensity <- lm(Pred ~ Genotype + Region, data = Int)

pairwise_results_intensity <- glht(lm_model_intensity, linfct = mcp(Genotype = "Tukey"))
summary(pairwise_results_intensity)

library(ggplot2)

# Create a data frame with the estimated effects and standard errors
effects_intensity <- c(-1.12270, 0.03218, 2.62718, -1.68004)
genotypes_intensity <- c("Genotype50", "Genotype36", "Genotype15", "Genotype14")
se_intensity <- c(1.45805, 1.30496, 1.40899, 1.42419)
data_intensity <- data.frame(genotypes_intensity, effects_intensity, se_intensity)

# Create the bar plot
ggplot(data_intensity, aes(x = genotypes_intensity, y = effects_intensity, fill = genotypes_intensity)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_errorbar(aes(ymin = effects_intensity - se_intensity, ymax = effects_intensity + se_intensity),
                width = 0.2, position = position_dodge(width = 0.5)) +
  labs(x = "Genotype", y = "Effect") +
  theme_minimal()




library(multcomp)

Int$Genotype <- as.factor(Int$Genotype)

# Perform the Kruskal-Wallis test
kruskal_result <- kruskal.test(Pred ~ Genotype, data = Int)

# Perform pairwise comparisons using the Wilcoxon rank sum test
pairwise_results_intensity <- pairwise.wilcox.test(Int$Pred, Int$Genotype, p.adjust.method = "holm")

# Print the results
print(kruskal_result)
print(pairwise_results_intensity)

library(ggplot2)







################################################## Compare the effects of genotype within regions on predation prevalance against the rest of the OFAV

MC <- OFAV

###### Change this monitoring interval to produce different figures!
MC <- subset(MC, MC$MonitoringInterval == "1")

# Duplicate all rows in the dataframe MC
MC_duplicate <- rbind(MC, MC)

MC_duplicate <- subset(MC_duplicate, !(Genotype %in% c(14, 15, 36, 50)))

MC_duplicate$Genotype <- "OFAV"

MC <- subset(MC, Genotype %in% c(14, 15, 36, 50))

MC <- rbind(MC, MC_duplicate)

# Identify rows with "OFAV" in the Genotype column
ofav_rows <- MC$Genotype == "OFAV"

# Create a new dataframe with "OFAV" rows at the beginning
MC_reordered <- rbind(MC[ofav_rows, ], MC[!ofav_rows, ])

# Reset row names of the reordered dataframe
rownames(MC_reordered) <- NULL

# Check the reordered dataframe
head(MC_reordered)

MC <- MC_reordered

head(MC)

# Convert the Genotype column to a factor with the desired value as the first level
MC$Genotype <- factor(MC$Genotype, levels = c("OFAV", unique(MC$Genotype)[-1]))

# Create a bar plot with proportions
ggplot(MC, aes(x = Genotype, fill = factor(FishBites))) +
  geom_bar(position = "fill") +
  labs(x = "Genotype", y = "Proportion") +
  scale_fill_discrete(name = "FishBites") +
  theme_minimal()

# Create a stacked bar plot for each genotype and region with separate stacks for "yes" and "no"
ggplot(MC, aes(x = Genotype, fill = factor(FishBites))) +
  geom_bar(position = "fill", color = "black") +
  facet_grid(Region ~ .) +
  labs(x = "Genotype", y = "Proportion") +
  scale_fill_discrete(name = "FishBites", labels = c("No", "Yes")) +
  theme_minimal()

# Calculate the proportions of "Yes" for each combination of Region and Genotype
PropMC <- MC %>%
  group_by(Region, Genotype) %>%
  summarize(Prop = mean(FishBites == "yes"))

# View the resulting dataframe
print(PropMC)

# Create a heatmap of proportions
ggplot(PropMC, aes(x = Genotype, y = Region, fill = Prop)) +
  geom_tile() +
  labs(title = "Heatmap of Predation Prevalance (Interval One)", x = "Genotype", y = "Region", fill = "Proportion of Clusters Experiencing Predation") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()

# Fit models with different link functions
model_logit <- glmer(FishBites ~ Genotype + (1 | Region), data = MC, family = binomial(link = "logit"))
model_probit <- glmer(FishBites ~ Genotype + (1 | Region), data = MC, family = binomial(link = "probit"))

summary(model_logit)
summary(model_probit)

# Generate predicted probabilities for each model
MC$logit_pred <- predict(model_logit, type = "response")
MC$probit_pred <- predict(model_probit, type = "response")

# Create density plots for logit and probit predictions
library(ggplot2)
ggplot(MC, aes(x = FishBites, fill = Genotype)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Genotype, ncol = 2) +
  labs(x = "FishBites", y = "Density") +
  scale_fill_discrete(name = "Genotype") +
  theme_minimal() +
  ggtitle("Density Plot: Logit vs. Probit")

# Fit a GLMM with a binomial family
glmm_model <- glmer(FishBites ~ Genotype + (1 | Region), data = MC, family = binomial(link = "logit"))

# Print the model summary
summary(glmm_model)

# Perform model-based comparisons of genotypes within each region
pairwise_results <- glht(glmm_model, linfct = mcp(Genotype = "Tukey"))

# Summarize the pairwise comparison results
summary(pairwise_results)

# Adjust p-values for multiple comparisons
adjusted_pvalues <- p.adjust(pairwise_results$pvalues, method = "bonferroni")

# Print adjusted p-values
adjusted_pvalues

################################################# Monitoring Interval One

### Summary vs baseline

# Extract the estimates and standard errors from the model summary
estimates <- summary(glmm_model)$coefficients[, "Estimate"]
std_errors <- summary(glmm_model)$coefficients[, "Std. Error"]
genotypes <- rownames(summary(glmm_model)$coefficients)

# Create a data frame with the estimates, standard errors, and genotypes
data <- data.frame(Genotype = genotypes, Estimate = estimates, Std.Error = std_errors)

# Sort the data frame by estimate values
data <- data[order(data$Estimate), ]

data

# Remove the (Intercept) row from the output
GLMM_One <- data[-2, ]

# Print the updated output
GLMM_One

# Create the bar plot
ggplot(GLMM_One, aes(x = Genotype, y = Estimate, fill = Genotype)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_errorbar(aes(ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "Comparison of the Effects of Genotypes within Regions with Error Bars (Interval One)", x = "Genotype", y = "Effect") +
  theme_minimal()

################################################## Monitoring Interval Two

### Summary vs baseline

# Extract the estimates and standard errors from the model summary
estimates <- summary(glmm_model)$coefficients[, "Estimate"]
std_errors <- summary(glmm_model)$coefficients[, "Std. Error"]
genotypes <- rownames(summary(glmm_model)$coefficients)

# Create a data frame with the estimates, standard errors, and genotypes
data <- data.frame(Genotype = genotypes, Estimate = estimates, Std.Error = std_errors)

# Sort the data frame by estimate values
data <- data[order(data$Estimate), ]

data

# Remove the (Intercept) row from the output
GLMM_Two <- data[-1, ]

# Print the updated output
GLMM_Two

# Create the bar plot
ggplot(GLMM_Two, aes(x = Genotype, y = Estimate, fill = Genotype)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_errorbar(aes(ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "Comparison of the Effects of Genotypes within Regions with Error Bars (Interval Two)", x = "Genotype", y = "Effect") +
  theme_minimal()








#### Limitations

table(subset(MC, Genotype != "OFAV")$SourceType)

