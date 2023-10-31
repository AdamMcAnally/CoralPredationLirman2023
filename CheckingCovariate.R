################################ Are species evenly represented across regions? Sites? What about genotypes? 
#Will clean this up with a function later to explore genotypes.
#For now, code needs to be functional, not elegant

head(RT[1:7])

# Load the necessary library for the chi-squared test
library(stats)

# Conduct the chi-squared test
chisq.test(RT$Species, RT$Region)

##### Differences between sites per region?

# Subset the data by Region and Site
OneRT <- subset(RT, RT$Region == "1")
TwoRT <- subset(RT, RT$Region == "2")
ThreeRT <- subset(RT, RT$Region == "3")
FourRT <- subset(RT, RT$Region == "4")
FiveRT <- subset(RT, RT$Region == "5")
SixRT <- subset(RT, RT$Region == "6")

#Lets run the tests
chisq.test(OneRT$Species, OneRT$Site)
chisq.test(TwoRT$Species, TwoRT$Site)
chisq.test(ThreeRT$Species, ThreeRT$Site)
chisq.test(FourRT$Species, FourRT$Site)
chisq.test(FiveRT$Species, FiveRT$Site)
chisq.test(SixRT$Species, SixRT$Site)

### Differences between Genotypes per Region?
chisq.test(RT$Genotype, RT$Region)

# Create a table of counts for each combination of Genotype and Region
count_table <- table(RT$Genotype, RT$Region)

# Filter the table to exclude cells with less than 5 observations
count_table_filtered <- count_table[count_table >= 5]

# Convert the filtered table to a data frame
count_df <- as.data.frame(count_table_filtered)

# Run the chi-squared test
result <- chisq.test(count_df)

# Print the results
print(result)

######Let's re-use the time series functions but only use the first time point
### Prepare to conduct a series of chi-square tests for independence

Con.Table <- function(data, time_var, num_time_points, var1, var2) {
  # initialize list to store contingency tables
  con_tables <- list()
  
  # loop through each time point
  for (i in 1:num_time_points) {
    # subset data for current time point
    current_data <- data[data[[time_var]] == i, ]
    
    # build contingency table
    con_table <- table(current_data[[var1]], current_data[[var2]])
    
    # store contingency table in list
    con_tables[[i]] <- con_table
  }
  
  return(con_tables)
}

# Build contingency tables
head(RT)
con_tables <- Con.Table(RT, "MonitoringInterval", 1, 2, 7)
### For subsetting to check individual regions.
###con_tables <- Con.Table(subset(EarlyRT, Region == 2), "MonitoringInterval", 4)
### Only regions 2 and 3 have enough individuals in every bin for Chi squared. Proceed with aggregate for now, but comparisons in region 3 could be useful
### Similar signature in every region, but would like to make sure regions with fewer parrotfish/bite occurrences didn't have over-representation of in situ corals grown 

# Print contingency table for time points
con_tables

# Function to convert contingency tables to tables of proportions
con.prop <- function(con_tables) {
  prop_table <- round(prop.table(con_tables, margin = 1), digits = 3)
  return(prop_table)
}

# Apply function to each contingency table in the list
prop_tables <- lapply(con_tables, con.prop)

# Print the resulting tables of proportions
prop_tables

# Function to convert proportion tables to data frames
prop.df <- function(prop_tables) {
  df <- as.data.frame(prop_tables)
  return(df)
}

# Apply function to each proportion table in the list
df_list <- lapply(prop_tables, prop.df)

# Combine data frames into a single data frame
df <- do.call(rbind, df_list)

# Remove rows containing "No" in the second column
df_filtered <- subset(df, Var2 != "no")

df_filtered

## Test for significance of the contingency tables
# Create an empty list to store the results
chisq <- list()

# Loop through the list of contingency tables and perform chi-squared tests on each
for (i in seq_along(con_tables)) {
  chisq[[i]] <- chisq.test(con_tables[[i]])
}

# Print the results
chisq

################################################
##########################################################################
######Let's re-use the time series functions but switch it to run through regions instead
### Prepare to conduct a series of chi-square tests for independence

Con.Table <- function(data, loop_var, num_loop, var1, var2) {
  # initialize list to store contingency tables
  con_tables <- list()
  
  # loop through each time point
  for (i in 1:num_loop) {
    # subset data for current time point
    current_data <- data[data[[loop_var]] == i, ]
    
    # build contingency table
    con_table <- table(current_data[[var1]], current_data[[var2]])
    
    # store contingency table in list
    con_tables[[i]] <- con_table
  }
  
  return(con_tables)
}

# Build contingency tables
head(RT)
con_tables <- Con.Table(RT, "Region", 6, 2, 7)
### For subsetting to check individual regions.
###con_tables <- Con.Table(subset(EarlyRT, Region == 2), "MonitoringInterval", 4)
### Only regions 2 and 3 have enough individuals in every bin for Chi squared. Proceed with aggregate for now, but comparisons in region 3 could be useful
### Similar signature in every region, but would like to make sure regions with fewer parrotfish/bite occurrences didn't have over-representation of in situ corals grown 

# Print contingency table for time points
con_tables

# Function to convert contingency tables to tables of proportions
con.prop <- function(con_tables) {
  prop_table <- round(prop.table(con_tables, margin = 1), digits = 3)
  return(prop_table)
}

# Apply function to each contingency table in the list
prop_tables <- lapply(con_tables, con.prop)

# Print the resulting tables of proportions
prop_tables

# Function to convert proportion tables to data frames
prop.df <- function(prop_tables) {
  df <- as.data.frame(prop_tables)
  return(df)
}

# Apply function to each proportion table in the list
df_list <- lapply(prop_tables, prop.df)

# Combine data frames into a single data frame
df <- do.call(rbind, df_list)

# Remove rows containing "No" in the second column
df_filtered <- subset(df, Var2 != "no")

df_filtered

## Test for significance of the contingency tables
# Create an empty list to store the results
chisq <- list()

# Loop through the list of contingency tables and perform chi-squared tests on each
for (i in seq_along(con_tables)) {
  chisq[[i]] <- chisq.test(con_tables[[i]])
}

# Print the results
chisq

###### Regions 1-5 show differences. Where are they?

RTCS <- chisq.test(con_tables[[5]])

# Get the chi-squared residuals and contributions for the 6th contingency table

corrplot(RTCS$residuals, is.cor = FALSE)
contrib <- 100*RTCS$residuals^2/RTCS$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

################################

##################### By genotype?

#Split by species first
OFAV <- subset(RT, RT$Species == "OFAV" & RT$MonitoringInterval == 1)
MCAV <- subset(RT, RT$Species == "MCAV" & RT$MonitoringInterval == 1)
PCLI <- subset(RT, RT$Species == "PCLI" & RT$MonitoringInterval == 1)

# Build contingency tables, switch up the dataframe as needed
head(RT)
head(OFAV)

count(OFAV)
count(MCAV)
count(PCLI)

#Do the thing for MCAV

con_tables <- Con.Table(MCAV, "MonitoringInterval", 1, 1, 6)

# Print contingency table for time points
mcavt<-con_tables[[1]]
mcavt

# subset data frame to only include columns where all values are at least 5
mcavsub <- mcavt[, apply(mcavt, 2, function(x) all(x >= 5))]

# print resulting data frame
mcavsub

# Apply function to each contingency table in the list
prop_tables <- lapply(con_tables, con.prop)

# Print the resulting tables of proportions
prop_tables
# Apply function to each proportion table in the list
df_list <- lapply(prop_tables, prop.df)

# Combine data frames into a single data frame
df <- do.call(rbind, df_list)

# Remove rows containing "No" in the second column
df_filtered <- subset(df, Var2 != "no")

df_filtered

## Test for significance of the contingency tables
# Create an empty list to store the results
chisq <- list()

# Loop through the list of contingency tables and perform chi-squared tests on each
for (i in seq_along(con_tables)) {
  chisq[[i]] <- chisq.test(con_tables[[i]])
}

# Print the results
chisq

#Do the thing for OFAV

con_tables <- Con.Table(OFAV, "MonitoringInterval", 1, 1, 6)

# Print contingency table for time points
ofavt<-con_tables[[1]]
ofavt

# subset data frame to only include columns where all values are at least 5
ofavsub <- ofavt[, apply(ofavt, 2, function(x) all(x >= 5))]

# print resulting data frame
ofavsub
chisq.test(ofavsub)

# Apply function to each contingency table in the list
prop_tables <- lapply(con_tables, con.prop)

# Print the resulting tables of proportions
prop_tables
# Apply function to each proportion table in the list
df_list <- lapply(prop_tables, prop.df)

# Combine data frames into a single data frame
df <- do.call(rbind, df_list)

# Remove rows containing "No" in the second column
df_filtered <- subset(df, Var2 != "no")

df_filtered

## Test for significance of the contingency tables
# Create an empty list to store the results
chisq <- list()

# Loop through the list of contingency tables and perform chi-squared tests on each
for (i in seq_along(con_tables)) {
  chisq[[i]] <- chisq.test(con_tables[[i]])
}

# Print the results
chisq

#Do the thing for PCLI

con_tables <- Con.Table(PCLI, "MonitoringInterval", 1, 1, 6)

# Print contingency table for time points
pclit<-con_tables[[1]]
pclit

# subset data frame to only include columns where all values are at least 5
pclisub <- pclit[, apply(pclit, 2, function(x) all(x >= 5))]

# print resulting data frame
pclisub

# Apply function to each contingency table in the list
prop_tables <- lapply(con_tables, con.prop)

# Print the resulting tables of proportions
prop_tables
# Apply function to each proportion table in the list
df_list <- lapply(prop_tables, prop.df)

# Combine data frames into a single data frame
df <- do.call(rbind, df_list)

# Remove rows containing "No" in the second column
df_filtered <- subset(df, Var2 != "no")

df_filtered

## Test for significance of the contingency tables
# Create an empty list to store the results
chisq <- list()

# Loop through the list of contingency tables and perform chi-squared tests on each
for (i in seq_along(con_tables)) {
  chisq[[i]] <- chisq.test(con_tables[[i]])
}

# Print the results
chisq

##############################################
#### Table showing species and prevalence by region in first 6 months

SixMonthPred <- subset(RT, RT$FishBites == "yes" & RT$TimeInterval %in% c(1, 2, 3, 4, 5, 6, 7))

SixMonthNone <- RT %>%
  filter(TimeInterval %in% c(1, 2, 3, 4, 5, 6, 7)) %>%
  group_by(ColonyNumber) %>%
  filter(all(FishBites == "no"))

SixMonthPred
SixMonthNone

firstRowPerColony <- SixMonthPred %>%
  group_by(ColonyNumber) %>%
  slice(1)
firstRowPerColony

NoPredColonies <- SixMonthNone %>%
  group_by(ColonyNumber) %>%
  slice(1)
NoPredColonies

combinedData <- bind_rows(firstRowPerColony, NoPredColonies)

con_tables <- Con.Table(combinedData, "Region", 6, 7, 19)
con_tables

###########################################
########################################################
################## Nurseries
#############################

# Build contingency tables

######Let's re-use the time series functions but only use the first time point
### Prepare to conduct a series of chi-square tests for independence

Con.Table <- function(data, time_var, num_time_points, var1, var2) {
  # initialize list to store contingency tables
  con_tables <- list()
  
  # loop through each time point
  for (i in 1:num_time_points) {
    # subset data for current time point
    current_data <- data[data[[time_var]] == i, ]
    
    # build contingency table
    con_table <- table(current_data[[var1]], current_data[[var2]])
    
    # store contingency table in list
    con_tables[[i]] <- con_table
  }
  
  return(con_tables)
}

# Build contingency tables
head(RT)
con_tables <- Con.Table(RT, "MonitoringInterval", 1, 2, 4)
### For subsetting to check individual regions.
###con_tables <- Con.Table(subset(EarlyRT, Region == 2), "MonitoringInterval", 4)
### Only regions 2 and 3 have enough individuals in every bin for Chi squared. Proceed with aggregate for now, but comparisons in region 3 could be useful
### Similar signature in every region, but would like to make sure regions with fewer parrotfish/bite occurrences didn't have over-representation of in situ corals grown 

# Print contingency table for time points
con_tables

# Function to convert contingency tables to tables of proportions
con.prop <- function(con_tables) {
  prop_table <- round(prop.table(con_tables, margin = 1), digits = 3)
  return(prop_table)
}

# Apply function to each contingency table in the list
prop_tables <- lapply(con_tables, con.prop)

# Print the resulting tables of proportions
prop_tables

# Function to convert proportion tables to data frames
prop.df <- function(prop_tables) {
  df <- as.data.frame(prop_tables)
  return(df)
}

# Apply function to each proportion table in the list
df_list <- lapply(prop_tables, prop.df)

# Combine data frames into a single data frame
df <- do.call(rbind, df_list)

# Remove rows containing "No" in the second column
df_filtered <- subset(df, Var2 != "no")

df_filtered

## Test for significance of the contingency tables
# Create an empty list to store the results
chisq <- list()

# Loop through the list of contingency tables and perform chi-squared tests on each
for (i in seq_along(con_tables)) {
  chisq[[i]] <- chisq.test(con_tables[[i]])
}

# Print the results
chisq



#############################
head(RT)

con_tables <- Con.Table(subset(RT,RT$MonitoringInterval == "1"), "Region", 6, 4, 7)

# Print contingency table for time points
con_tables

# Function to convert contingency tables to tables of proportions
con.prop <- function(con_tables) {
  prop_table <- round(prop.table(con_tables, margin = 1), digits = 3)
  return(prop_table)
}

# Apply function to each contingency table in the list
prop_tables <- lapply(con_tables, con.prop)

# Print the resulting tables of proportions
prop_tables

# Function to convert proportion tables to data frames
prop.df <- function(prop_tables) {
  df <- as.data.frame(prop_tables)
  return(df)
}

# Apply function to each proportion table in the list
df_list <- lapply(prop_tables, prop.df)

# Combine data frames into a single data frame
df <- do.call(rbind, df_list)

# Remove rows containing "No" in the second column
df_filtered <- subset(df, Var2 != "no")

df_filtered

## Test for significance of the contingency tables
# Create an empty list to store the results
#chisq <- list()

# Loop through the list of contingency tables and perform chi-squared tests on each
#for (i in seq_along(con_tables)) {
#  chisq[[i]] <- chisq.test(con_tables[[i]])
#}

# Print the results
#chisq

# Initialize a list to store the Fisher's exact test results
fisher_results <- list()

# Loop through each contingency table
for (i in 1:length(con_tables)) {
  # Run Fisher's exact test on the current contingency table with simulated p-value
  result <- fisher.test(con_tables[[i]], simulate.p.value = TRUE)
  
  # Store the Fisher's exact test result in the list
  fisher_results[[i]] <- result
}

# Print the Fisher's exact test results
for (i in 1:length(fisher_results)) {
  cat("Region", i, "\n")
  print(fisher_results[[i]])
  cat("\n")
}

##################################################

#Let's look at differences in source nursery distribution

# Build contingency tables
head(RT)
con_tables <- Con.Table(RT, "MonitoringInterval", 1, 2, 30)

# Print contingency table for time points
con_tables

# Function to convert contingency tables to tables of proportions
con.prop <- function(con_tables) {
  prop_table <- round(prop.table(con_tables, margin = 1), digits = 3)
  return(prop_table)
}

# Apply function to each contingency table in the list
prop_tables <- lapply(con_tables, con.prop)

# Print the resulting tables of proportions
prop_tables

# Function to convert proportion tables to data frames
prop.df <- function(prop_tables) {
  df <- as.data.frame(prop_tables)
  return(df)
}

# Apply function to each proportion table in the list
df_list <- lapply(prop_tables, prop.df)

# Combine data frames into a single data frame
df <- do.call(rbind, df_list)

# Remove rows containing "No" in the second column
df_filtered <- subset(df, Var2 != "no")

df_filtered

## Test for significance of the contingency tables
# Create an empty list to store the results
chisq <- list()

# Loop through the list of contingency tables and perform chi-squared tests on each
for (i in seq_along(con_tables)) {
  chisq[[i]] <- chisq.test(con_tables[[i]])
}

# Print the results
chisq

