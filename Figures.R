# Define the custom labels and colors for the legend
labels <- c("St. Lucie, Martin, PB (FAU)",
            "Broward (NSU)",
            "Miami-Dade (UM, BNP)",
            "Upper Keys (CRF/FWC)",
            "Middle Keys (FWC)",
            "Lower Keys (MML)")

colors <- c("red", "black", "orange", "green", "yellow", "blue")

colors

# Create combined scatter plot and smooth line plot
ggplot() +
  geom_point(data = prop_df_long, aes(x = Days, y = prop_yes)) +
  #geom_smooth(data = Early, aes(x = Days, y = predicted, color = Region), method = "gam", formula = y ~ s(x, bs = "cr", k = 4)) +
  geom_smooth(data = RT, aes(x = Days, y = predicted, color = Region)) +
  scale_color_manual(values = colors, labels = labels) +  # Set custom colors and labels for the legend
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0.1, linetype = "dashed") +
  geom_vline(xintercept = 178, linetype = "dashed") +
  labs(x = "Days Since Outplanting", y = "Proportion of Clusters that Experienced New Bites",
       title = "Partial Predation Prevalence Throughout the Survey")

# Create combined scatter plot and smooth line plot
ggplot() +
  geom_point(data = prop_df_long, aes(x = Days, y = prop_yes)) +
  #geom_smooth(data = Early, aes(x = Days, y = predicted, color = Region), method = "gam", formula = y ~ s(x, bs = "cr", k = 4)) +
  geom_smooth(data = RT, aes(x = Days, y = predicted, color = Region)) +
  scale_color_manual(values = colors, labels = labels) +  # Set custom colors and labels for the legend
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 18),  # You can adjust the font size for x-axis here
        axis.text.y = element_text(size = 18)) +  # You can adjust the font size for y-axis here
  geom_hline(yintercept = 0.1, linetype = "dashed") +
  geom_vline(xintercept = 178, linetype = "dashed") +
  labs(x = "Days Since Outplanting", y = "Proportion of Clusters that Experienced New Bites",
       title = "Partial Predation Prevalence Throughout the Survey")




# Prevalence by Region
# Create the residual bubble plot
RTCR <- chisq.test(RTFirstAlive$FishBites, RTFirstAlive$Region)
RTCR

RTCR$residuals

corrplot(RTCR$residuals, is.cor = FALSE,
         title = "Chi-Square Residual Bubble Plot for Predation Prevalence by Region",
         xlab = "",
         ylab = "Fish Bite Presence",
         cl.length = 5,  # Set the length of the color bar to 5
         mar = c(4, 4, 2, 2),
         cex.main = 2.5,  # Adjust the font size of the title
         tl.cex = 2,  # Adjust the font size of the labels
         addgrid.col = "gray")  # Add grid lines to the plot

# Add custom region labels to the x-axis with color-coded text
axis(1, at = seq_along(labels), labels = NA, tick = FALSE)
text(seq_along(labels), par("usr")[3] - 0.15, labels = labels, col = colors, srt = 45, adj = 1, xpd = TRUE)

# Add custom y-axis labels
mtext(c("Absence of Fish Bites", "Presence of Fish Bites"), side = 2, line = 3, las = 1)










# Build the contingency table
contingency_table <- table(RTFirstAlive$FishBites, RTFirstAlive$Region)

# Display the contingency table
print(contingency_table)

# Calculate the column sums (total count for each column)
col_sums <- colSums(contingency_table)

# Calculate the proportion of "yes" for each column
proportions_table <- contingency_table["yes", ] / col_sums * 100

# Print the proportions table
print(proportions_table)

# Perform the chi-squared test with Monte Carlo simulation
chisq_result <- chisq.test(RTFirstAlive$FishBites, RTFirstAlive$Region, simulate.p.value = TRUE)
chisq_result









# Get the expected values assuming no relationship
expected_values <- chisq_result$expected

# Print the expected values
print(expected_values)

# Create the data frame for the proportions by regions
proportions_data <- data.frame(
  Region = c("St. Lucie, Martin, PB (FAU)",
             "Broward (NSU)",
             "Miami-Dade (UM, BNP)",
             "Upper Keys (CRF/FWC)",
             "Middle Keys (FWC)",
             "Lower Keys (MML)"),
  Proportion = c(2.1, 55.4, 72.4, 44.7, 32.8, 23.2)
)

# Print the proportions_data dataframe
print(proportions_data)

unique_regions <- unique(proportions_data$Region)
print(unique_regions)


# FIGURE: Prevalence by Region Week 1
# Create the bar plot with custom colors
# Define the desired order of regions
desired_order <- c("St. Lucie, Martin, PB (FAU)", "Broward (NSU)", "Miami-Dade (UM, BNP)",
                   "Upper Keys (CRF/FWC)", "Middle Keys (FWC)", "Lower Keys (MML)")

# Convert 'Region' to factor with the desired order
proportions_data$Region <- factor(proportions_data$Region, levels = desired_order)

# Create the bar plot with custom colors and the desired order of regions
ggplot(proportions_data, aes(x = Region, y = Proportion, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 1)), vjust = -0.5, size = 4) +
  labs(title = "Prevalence in First Week by Region",
       x = "Region",
       y = "Percentage of Bases Experiencing Predation",
       fill = "Region") +
  scale_fill_manual(values = colors) +  # Use the custom colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#Prevalence by Species
# Build the contingency table
contingency_table <- table(RTFirstAlive$FishBites, RTFirstAlive$Species)

# Display the contingency table
print(contingency_table)

# Calculate the column sums (total count for each column)
col_sums <- colSums(contingency_table)

# Calculate the proportion of "yes" for each column
proportions_table <- contingency_table["yes", ] / col_sums * 100

# Print the proportions table
print(proportions_table)

# Perform the chi-squared test with Monte Carlo simulation
chisq_result <- chisq.test(RTFirstAlive$FishBites, RTFirstAlive$Species, simulate.p.value = TRUE)

chisq_result

# Get the expected values assuming no relationship
expected_values <- chisq_result$expected

# Print the expected values
print(expected_values)

# Create the data frame for the proportions
proportions_data <- data.frame(
  Species = c("MCAV", "OFAV", "PCLI"),
  Proportion = c(44.59, 34.15, 43.31)
)


###Proportions Species
# Create the bar chart
ggplot(proportions_data, aes(x = Species, y = Proportion, fill = Species)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 3)), position = position_stack(vjust = 0.5)) +
  labs(title = "Percent Live Coral Bases with Fish Predation in the First Week by Species",
       x = "Species",
       y = "Percent",
       fill = "Species") +
  theme_minimal()

# Define the new labels for species
new_labels <- c("MCAV", "OFAV", "PCLI")

# Create the bar chart
ggplot(proportions_data, aes(x = Species, y = Proportion, fill = Species)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Proportion, 3)), position = position_stack(vjust = 0.5)) +
  labs(title = "Percent Live Coral Bases with Fish Predation in the First Week by Species",
       x = "Species",
       y = "Percent",
       fill = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 18),  # You can adjust the font size for x-axis values here
        axis.text.y = element_text(size = 18),  # You can adjust the font size for y-axis values here
        legend.text = element_text(size = 18)) +  # You can adjust the font size for the legend here
  scale_x_discrete(labels = new_labels) +
  scale_fill_discrete(labels = new_labels)






# Intensity by Species
# Create a box and whisker plot
ggplot(FirstInt, aes(x = Species, y = Pred, fill = Species)) +
  geom_boxplot() +
  labs(x = "Species",
       y = "Percent Tissue Removed",
       title = "Boxplot of Percent Tissue Removed by Species")

# Create a box and whisker plot
ggplot(FirstInt, aes(x = Species, y = Pred, fill = Species)) +
  geom_boxplot() +
  labs(x = "Species",
       y = "Percent Tissue Removed",
       title = "Boxplot of Percent Tissue Removed by Species") +
  theme(axis.text.x = element_text(size = 18),  # Adjust the font size for x-axis
        axis.text.y = element_text(size = 18))  # Adjust the font size for y-axis


# Intensity by Region

# Create a boxplot
p <- ggplot(FirstInt, aes(x = Region, y = Pred)) +
  geom_boxplot(fill = colors) +  # Color-code the boxes with the object colors
  xlab("Region") +
  ylab("Percent Tissue Removed") +
  theme(axis.text.y = element_text(size = 18)) +  # You can adjust the font size
  ggtitle("Boxplot of Percent Tissue Removed by Region")

# Modify the font size of y-axis numbers
p + theme(axis.text.y = element_text(size = 18))  # You can adjust the font size



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


#Heatmap

# Define the desired order for the x-axis categories
desired_order <- c("OFAV", "14", "15", "36", "50")

# Create a heatmap of proportions
ggplot(PropMC, aes(x = factor(Genotype, levels = desired_order), y = Region, fill = Prop)) +
  geom_tile() +
  labs(title = "Heatmap of Predation Prevalence (Interval One)",
       x = "Genotype",
       y = "Region",
       fill = "Proportion of Clusters Experiencing Predation") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  scale_x_discrete(labels = desired_order) +  # Reorder x-axis categories
  theme(legend.text = element_text(size = 16))  # Adjust the legend font size





FindPic <- subset(RT,RT$Region == '2')
FindPic <- subset(FindPic,FindPic$Pred > '0')
FindPic <- subset(FindPic,FindPic$MonitoringInterval == '2')
FindPic <- subset(FindPic,FindPic$ColonyNumber > '384')




#### Genotype analyses, converting bubble plots to bar plots


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
