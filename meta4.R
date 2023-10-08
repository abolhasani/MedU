# Set the working directory to the directory where the CSV file is located
setwd("C:/Drive/UM")

# Load the `readr` package
library(readr)

# Read the CSV file into a data frame
df <- read_csv("data.csv")

# Check for NA variables and replace with zero
df[is.na(df)] <- 0

# Initialize the new data frame to store the results
table_df <- data.frame(FirstAuthorYear = character(),
                       StudyType = character(),
                       Spike = character(),
                       HFO = character(),
                       `Engel 1A` = character(),
                       `Not Engel 1A` = character(),
                       `Engel 1 and 2` = character(),
                       `Engel>2` = character())

# Fill in the data frame
for (i in 1:nrow(df)) {
  table_df[i, ] <- list(
    FirstAuthorYear = df[i, 1],
    StudyType = df[i, 3],
    Spike = ifelse(df[i, 8] == 0, "-", df[i, 8]),
    HFO = ifelse(df[i, 10] == 0, "-", df[i, 10]),
    `Engel 1A` = ifelse(df[i, 43] == 0, "-", paste0(df[i, 42], "%")),
    `Not Engel 1A` = ifelse(df[i, 45] == 0, "-", paste0(df[i, 44], "%")),
    `Engel 1 and 2` = ifelse(df[i, 47] == 0, "-", paste0(df[i, 46], "%")),
    `Engel>2` = ifelse(df[i, 49] == 0, "-", paste0(df[i, 48], "%"))
  )
}

# Print the data frame
print(table_df)
print(df)
dim(df)
library(dplyr)
# Remove the first row (headers)
#df <- df[-1, ]
#colnames(df) <- NULL

print(df)
dim(df)

# Create a copy of df as df_t_2
df_t_2 <- df

# Remove rows where columns 5, 6, or 7 have a value of 0
df_t_2 <- df_t_2[!(df_t_2[,5] == 0 | df_t_2[,6] == 0 | df_t_2[,7] == 0), ]

ecog_plus <- df_t_2[df_t_2[,8] != 0, ]
ecog_minus <- df_t_2[df_t_2[,10] != 0, ]

print(ecog_plus)
print(ecog_minus)
# Calculate medians for ecog_plus and ecog_minus groups
# Convert the relevant columns of ecog_plus and ecog_minus to numeric
# Convert tibbles to traditional dataframes
ecog_plus <- as.data.frame(ecog_plus)
ecog_minus <- as.data.frame(ecog_minus)

print(ecog_plus)
print(ecog_minus)

# Convert the relevant columns of ecog_plus and ecog_minus to numeric
ecog_plus[,5:7] <- lapply(ecog_plus[,5:7], function(x) as.numeric(as.character(x)))
ecog_minus[,5:7] <- lapply(ecog_minus[,5:7], function(x) as.numeric(as.character(x)))

# Calculate medians for ecog_plus and ecog_minus groups
medians_plus <- c(
  median(ecog_plus[,5], na.rm = TRUE),
  median(ecog_plus[,6], na.rm = TRUE),
  median(ecog_plus[,7], na.rm = TRUE)
)

medians_minus <- c(
  median(ecog_minus[,5], na.rm = TRUE),
  median(ecog_minus[,6], na.rm = TRUE),
  median(ecog_minus[,7], na.rm = TRUE)
)

# Statistical Testing
p_values <- c(
  wilcox.test(ecog_plus[,5], ecog_minus[,5], na.rm = TRUE)$p.value,
  wilcox.test(ecog_plus[,6], ecog_minus[,6], na.rm = TRUE)$p.value,
  wilcox.test(ecog_plus[,7], ecog_minus[,7], na.rm = TRUE)$p.value
)

# Create a summary table
result_table <- data.frame(
  Row = c("Median Age", "Median Age of Onset", "Median Follow-Up"),
  ECoG_Spike = medians_plus,
  ECoG_HFO = medians_minus,
  P_Value = p_values
)

print(result_table)

dfdf <- df
dfdf <- as.data.frame(dfdf)

# Assuming you've defined ecog_plus and ecog_minus using the previous criteria:
ecog_plus <- dfdf[dfdf[,8] != 0, ]
ecog_minus <- dfdf[dfdf[,10] != 0, ]

# Summation for Temporal and Extratemporal for ECoG+ Spike and ECoG- HFO
temporal_spike_sum <- sum(ecog_plus[,14], na.rm = TRUE)
extratemporal_spike_sum <- sum(ecog_plus[,16], na.rm = TRUE)

temporal_hfo_sum <- sum(ecog_minus[,14], na.rm = TRUE)
extratemporal_hfo_sum <- sum(ecog_minus[,16], na.rm = TRUE)

# Compute total counts for percentage computation
total_spike <- sum(ecog_plus[,8], na.rm = TRUE)
total_hfo <- sum(ecog_minus[,10], na.rm = TRUE)

# Compute the totals for percentage computation
total_temporal_plus <- temporal_spike_sum
total_extratemporal_plus <- extratemporal_spike_sum

total_temporal_minus <- temporal_hfo_sum
total_extratemporal_minus <- extratemporal_hfo_sum

# Percentages computation based on ECoG+ and ECoG-
temporal_spike_percent <- (temporal_spike_sum / (total_temporal_plus + total_extratemporal_plus)) * 100
extratemporal_spike_percent <- (extratemporal_spike_sum / (total_temporal_plus + total_extratemporal_plus)) * 100

temporal_hfo_percent <- (temporal_hfo_sum / (total_temporal_minus + total_extratemporal_minus)) * 100
extratemporal_hfo_percent <- (extratemporal_hfo_sum / (total_temporal_minus + total_extratemporal_minus)) * 100

# Organize the result
result <- data.frame(
  Site = c("Temporal", "Extratemporal"),
  ECoG_Spike = sprintf("%d (%.2f%%)", c(temporal_spike_sum, extratemporal_spike_sum), c(temporal_spike_percent, extratemporal_spike_percent)),
  ECoG_HFO = sprintf("%d (%.2f%%)", c(temporal_hfo_sum, extratemporal_hfo_sum), c(temporal_hfo_percent, extratemporal_hfo_percent))
)

# Print the result
print(result)

# Create a contingency table
contingency_table <- matrix(c(temporal_spike_sum, extratemporal_spike_sum, temporal_hfo_sum, extratemporal_hfo_sum), 
                            nrow = 2)

# Name the rows and columns for clarity
rownames(contingency_table) <- c("Temporal", "Extratemporal")
colnames(contingency_table) <- c("ECoG+ Spike", "ECoG- HFO")

# Conduct chi-squared test
test_result <- chisq.test(contingency_table)

# Print the p-value
print(test_result$p.value)

# Filter rows where columns 8 and 10 are not zero
ecog_plus_indices <- which(dfdf[,8] != 0)
ecog_minus_indices <- which(dfdf[,10] != 0)

# Define the column indices for the variables
variables <- c(18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40)
var_names <- c("Normal", "MTS", "Cortical Dysplasia", "Vascular Lesion", "Trauma", "Infection", "Heterotopia", 
               "Tumor", "Stroke, Gliosis, Inflammation", "Congenital", "Other", "MTS + FCD")

# Initialize a data frame to store the results
results_df <- data.frame(Variable = var_names, Spike = NA, Spike_Percent = NA, HFO = NA, HFO_Percent = NA, 
                         p_value = NA, stringsAsFactors = FALSE)

# Calculate total counts for all the variables combined for ECoG+ Spike and ECoG- HFO groups
total_spike_for_vars <- sum(dfdf[ecog_plus_indices, variables], na.rm = TRUE)
total_hfo_for_vars <- sum(dfdf[ecog_minus_indices, variables], na.rm = TRUE)

# Iterate over the variables to compute the sums, percentages, and p-values
for (i in 1:length(variables)) {
  # Get the sum for each variable
  spike_sum <- sum(dfdf[ecog_plus_indices, variables[i]], na.rm = TRUE)
  hfo_sum <- sum(dfdf[ecog_minus_indices, variables[i]], na.rm = TRUE)
  
  # Compute the percentages
  spike_percent <- (spike_sum / total_spike_for_vars) * 100
  hfo_percent <- (hfo_sum / total_hfo_for_vars) * 100
  
  # Conduct a Fisher's exact test
  contingency_table <- matrix(c(spike_sum, total_spike_for_vars - spike_sum, hfo_sum, total_hfo_for_vars - hfo_sum), ncol = 2)
  test_result <- fisher.test(contingency_table)
  
  # Store the results in the data frame
  results_df[i, 2:6] <- c(spike_sum, spike_percent, hfo_sum, hfo_percent, test_result$p.value)
}

# Add a row for the total summation
total_row <- c("Total", sum(results_df$Spike), NA, sum(results_df$HFO), NA, NA)
results_df <- rbind(results_df, total_row)

# Print the results data frame
print(results_df)

results_df <- data.frame(Variable = var_names, 
                         Spike = NA, 
                         HFO = NA, 
                         stringsAsFactors = FALSE)

# Create containers to hold the sums of the variables for the chi-squared test
all_spike_sums <- numeric()
all_hfo_sums <- numeric()

# Iterate over the variables to compute the sums and percentages
for (i in 1:length(variables)) {
  # Get the sum for each variable
  spike_sum <- sum(dfdf[ecog_plus_indices, variables[i]], na.rm = TRUE)
  hfo_sum <- sum(dfdf[ecog_minus_indices, variables[i]], na.rm = TRUE)
  
  # Compute the percentages
  spike_percent <- (spike_sum / total_spike_for_vars) * 100
  hfo_percent <- (hfo_sum / total_hfo_for_vars) * 100
  
  # Store the results in the data frame in the format "Number (Percentage%)"
  results_df[i, "Spike"] <- sprintf("%d (%.2f%%)", spike_sum, spike_percent)
  results_df[i, "HFO"] <- sprintf("%d (%.2f%%)", hfo_sum, hfo_percent)
  
  # Store the sums for chi-squared test
  all_spike_sums <- c(all_spike_sums, spike_sum)
  all_hfo_sums <- c(all_hfo_sums, hfo_sum)
}


# Extract data for males and females from the provided columns
male_data <- dfdf[, 51]
female_data <- dfdf[, 50]
# Filter rows based on ecog_plus and ecog_minus conditions
male_data_plus <- male_data[dfdf[,8] != 0]
female_data_plus <- female_data[dfdf[,8] != 0]
male_data_minus <- male_data[dfdf[,10] != 0]
female_data_minus <- female_data[dfdf[,10] != 0]

# Calculate the counts for males and females in the ECoG+ and ECoG- groups
male_spike <- sum(male_data_plus, na.rm = TRUE)
female_spike <- sum(female_data_plus, na.rm = TRUE)
male_hfo <- sum(male_data_minus, na.rm = TRUE)
female_hfo <- sum(female_data_minus, na.rm = TRUE)

# Calculate the total counts for each group (ECoG+ and ECoG-)
total_spike <- male_spike + female_spike
total_hfo <- male_hfo + female_hfo

# Calculate the percentages for males and females in each group
male_spike_percent <- (male_spike / total_spike) * 100
female_spike_percent <- (female_spike / total_spike) * 100
male_hfo_percent <- (male_hfo / total_hfo) * 100
female_hfo_percent <- (female_hfo / total_hfo) * 100

# Create a data frame to display the results
gender_df <- data.frame(
  Gender = c("Male", "Female", "Total"),
  Spike = c(sprintf("%d (%.2f%%)", male_spike, male_spike_percent),
            sprintf("%d (%.2f%%)", female_spike, female_spike_percent),
            total_spike),
  HFO = c(sprintf("%d (%.2f%%)", male_hfo, male_hfo_percent),
          sprintf("%d (%.2f%%)", female_hfo, female_hfo_percent),
          total_hfo)
)

# Print the results data frame
print(gender_df)

# Conduct a chi-squared test
contingency_table_gender <- matrix(c(male_spike, female_spike, male_hfo, female_hfo), ncol = 2)
chi_squared_result_gender <- chisq.test(contingency_table_gender)

# Print the p-value
cat("\nP-Value for gender distribution:", chi_squared_result_gender$p.value, "\n")

# Extract data for Engel classifications for both ECoG+ Spike and ECoG- HFO groups
engel_1a_data <- dfdf[, 42]
engel_not_1a_data <- dfdf[, 44]
engel_1_and_2_data <- dfdf[, 46]
engel_greater_than_2_data <- dfdf[, 48]

# Subset the dataframe based on Spike and HFO
ecog_plus_indices <- which(dfdf[,8] != 0)
ecog_minus_indices <- which(dfdf[,10] != 0)

# Calculate the sum for each classification
engel_1a_spike <- sum(engel_1a_data[ecog_plus_indices], na.rm = TRUE)
engel_1a_hfo <- sum(engel_1a_data[ecog_minus_indices], na.rm = TRUE)

# Calculate the sum for each classification
engel_1a_spike <- sum(engel_1a_data[ecog_plus_indices], na.rm = TRUE)
engel_1a_hfo <- sum(engel_1a_data[ecog_minus_indices], na.rm = TRUE)

engel_not_1a_spike <- sum(engel_not_1a_data[ecog_plus_indices], na.rm = TRUE)
engel_not_1a_hfo <- sum(engel_not_1a_data[ecog_minus_indices], na.rm = TRUE)

engel_1_and_2_spike <- sum(engel_1_and_2_data[ecog_plus_indices], na.rm = TRUE)
engel_1_and_2_hfo <- sum(engel_1_and_2_data[ecog_minus_indices], na.rm = TRUE)

engel_greater_than_2_spike <- sum(engel_greater_than_2_data[ecog_plus_indices], na.rm = TRUE)
engel_greater_than_2_hfo <- sum(engel_greater_than_2_data[ecog_minus_indices], na.rm = TRUE)

# Calculate total counts for all the variables combined for ECoG+ Spike and ECoG- HFO groups
total_spike_for_engel <- sum(engel_1a_spike, engel_not_1a_spike, engel_1_and_2_spike, engel_greater_than_2_spike)
total_hfo_for_engel <- sum(engel_1a_hfo, engel_not_1a_hfo, engel_1_and_2_hfo, engel_greater_than_2_hfo)

# Compile the results
engel_results <- data.frame(
  Classification = c("Engel 1A", "Engel Not 1A", "Engel 1 and 2", "Engel > 2", "Total"),
  Spike = c(engel_1a_spike, engel_not_1a_spike, engel_1_and_2_spike, engel_greater_than_2_spike, total_spike_for_engel),
  Spike_Percent = c((engel_1a_spike / total_spike_for_engel) * 100, 
                    (engel_not_1a_spike / total_spike_for_engel) * 100,
                    (engel_1_and_2_spike / total_spike_for_engel) * 100,
                    (engel_greater_than_2_spike / total_spike_for_engel) * 100, NA),
  HFO = c(engel_1a_hfo, engel_not_1a_hfo, engel_1_and_2_hfo, engel_greater_than_2_hfo, total_hfo_for_engel),
  HFO_Percent = c((engel_1a_hfo / total_hfo_for_engel) * 100,
                  (engel_not_1a_hfo / total_hfo_for_engel) * 100,
                  (engel_1_and_2_hfo / total_hfo_for_engel) * 100,
                  (engel_greater_than_2_hfo / total_hfo_for_engel) * 100, NA)
)

# Format the columns for better readability
engel_results$Spike <- sapply(1:nrow(engel_results), function(i) {
  if (is.na(engel_results$Spike_Percent[i])) {
    return(as.character(engel_results$Spike[i]))
  }
  return(paste0(engel_results$Spike[i], " (", round(engel_results$Spike_Percent[i], 2), "%)"))
})

engel_results$HFO <- sapply(1:nrow(engel_results), function(i) {
  if (is.na(engel_results$HFO_Percent[i])) {
    return(as.character(engel_results$HFO[i]))
  }
  return(paste0(engel_results$HFO[i], " (", round(engel_results$HFO_Percent[i], 2), "%)"))
})

# Drop the percentage columns as they've been merged with the count columns
engel_results$Spike_Percent <- NULL
engel_results$HFO_Percent <- NULL

# Conduct a chi-squared test for the Engel classifications
contingency_table_engel <- matrix(c(engel_1a_spike, engel_not_1a_spike, engel_1_and_2_spike, engel_greater_than_2_spike,
                                    engel_1a_hfo, engel_not_1a_hfo, engel_1_and_2_hfo, engel_greater_than_2_hfo), ncol = 2)
chi_squared_result_engel <- chisq.test(contingency_table_engel)

# Print the results
print(engel_results)
cat("\nP-Value for Engel classifications:", chi_squared_result_engel$p.value, "\n")



# Subset the data based on Spike and HFO
ecog_plus_indices <- which(dfdf[,8] != 0)
ecog_minus_indices <- which(dfdf[,10] != 0)

# Extract data for Engel 1A and Engel Not 1A
engel_1a_spike <- sum(dfdf[ecog_plus_indices, 42], na.rm = TRUE)
engel_1a_hfo <- sum(dfdf[ecog_minus_indices, 42], na.rm = TRUE)

engel_not_1a_spike <- sum(dfdf[ecog_plus_indices, 44], na.rm = TRUE)
engel_not_1a_hfo <- sum(dfdf[ecog_minus_indices, 44], na.rm = TRUE)

# Calculate total counts for ECoG+ Spike and ECoG- HFO groups
total_spike_for_engel <- engel_1a_spike + engel_not_1a_spike
total_hfo_for_engel <- engel_1a_hfo + engel_not_1a_hfo

# Compile the results
engel_results <- data.frame(
  Classification = c("Engel 1A", "Engel Not 1A", "Total"),
  Spike = c(engel_1a_spike, engel_not_1a_spike, total_spike_for_engel),
  Spike_Percent = c((engel_1a_spike / total_spike_for_engel) * 100,
                    (engel_not_1a_spike / total_spike_for_engel) * 100, NA),
  HFO = c(engel_1a_hfo, engel_not_1a_hfo, total_hfo_for_engel),
  HFO_Percent = c((engel_1a_hfo / total_hfo_for_engel) * 100,
                  (engel_not_1a_hfo / total_hfo_for_engel) * 100, NA)
)

# Format the columns for better readability
engel_results$Spike <- sapply(1:nrow(engel_results), function(i) {
  if (is.na(engel_results$Spike_Percent[i])) {
    return(as.character(engel_results$Spike[i]))
  }
  return(paste0(engel_results$Spike[i], " (", round(engel_results$Spike_Percent[i], 2), "%)"))
})

engel_results$HFO <- sapply(1:nrow(engel_results), function(i) {
  if (is.na(engel_results$HFO_Percent[i])) {
    return(as.character(engel_results$HFO[i]))
  }
  return(paste0(engel_results$HFO[i], " (", round(engel_results$HFO_Percent[i], 2), "%)"))
})

# Drop the percentage columns as they've been merged with the count columns
engel_results$Spike_Percent <- NULL
engel_results$HFO_Percent <- NULL

# Conduct a chi-squared test for the Engel classifications
contingency_table_engel <- matrix(c(engel_1a_spike, engel_not_1a_spike, engel_1a_hfo, engel_not_1a_hfo), ncol = 2)
chi_squared_result_engel <- chisq.test(contingency_table_engel)

# Print the results
print(engel_results)
cat("\nP-Value for Engel 1A and Engel Not 1A:", chi_squared_result_engel$p.value, "\n")


# Extract data for Engel 1 and 2 and Engel >2
engel_1_and_2_spike <- sum(dfdf[ecog_plus_indices, 46], na.rm = TRUE)
engel_1_and_2_hfo <- sum(dfdf[ecog_minus_indices, 46], na.rm = TRUE)

engel_gt_2_spike <- sum(dfdf[ecog_plus_indices, 48], na.rm = TRUE)
engel_gt_2_hfo <- sum(dfdf[ecog_minus_indices, 48], na.rm = TRUE)

# Calculate total counts for ECoG+ Spike and ECoG- HFO groups
total_spike_for_engel <- engel_1_and_2_spike + engel_gt_2_spike
total_hfo_for_engel <- engel_1_and_2_hfo + engel_gt_2_hfo

# Compile the results
engel_results <- data.frame(
  Classification = c("Engel 1 and 2", "Engel >2", "Total"),
  Spike = c(engel_1_and_2_spike, engel_gt_2_spike, total_spike_for_engel),
  Spike_Percent = c((engel_1_and_2_spike / total_spike_for_engel) * 100,
                    (engel_gt_2_spike / total_spike_for_engel) * 100, NA),
  HFO = c(engel_1_and_2_hfo, engel_gt_2_hfo, total_hfo_for_engel),
  HFO_Percent = c((engel_1_and_2_hfo / total_hfo_for_engel) * 100,
                  (engel_gt_2_hfo / total_hfo_for_engel) * 100, NA)
)

# Format the columns for better readability
engel_results$Spike <- sapply(1:nrow(engel_results), function(i) {
  if (is.na(engel_results$Spike_Percent[i])) {
    return(as.character(engel_results$Spike[i]))
  }
  return(paste0(engel_results$Spike[i], " (", round(engel_results$Spike_Percent[i], 2), "%)"))
})

engel_results$HFO <- sapply(1:nrow(engel_results), function(i) {
  if (is.na(engel_results$HFO_Percent[i])) {
    return(as.character(engel_results$HFO[i]))
  }
  return(paste0(engel_results$HFO[i], " (", round(engel_results$HFO_Percent[i], 2), "%)"))
})

# Drop the percentage columns as they've been merged with the count columns
engel_results$Spike_Percent <- NULL
engel_results$HFO_Percent <- NULL

# Conduct a chi-squared test for the Engel classifications
contingency_table_engel <- matrix(c(engel_1_and_2_spike, engel_gt_2_spike, engel_1_and_2_hfo, engel_gt_2_hfo), ncol = 2)
chi_squared_result_engel <- chisq.test(contingency_table_engel)

# Print the results
print(engel_results)
cat("\nP-Value for Engel 1 and 2 vs Engel >2:", chi_squared_result_engel$p.value, "\n")






library(survival)
library(ggplot2)
library(survival)
library(survminer)
library(reshape2)
# Convert follow-up months to years
dfdf$duration_years <- dfdf[,7] / 12

# Consider zeros in column 43 as censored
dfdf$censored <- ifelse(dfdf[,43] == 0, 1, 0) 

dfdf$Spike_group <- dfdf[,8] != 0
dfdf$HFO_group <- dfdf[,10] != 0

# Separate data for the two groups
dfdf$group <- ifelse(dfdf[,8] != 0, "Spike", ifelse(dfdf[,10] != 0, "HFO", NA))

# Kaplan-Meier survival curves
#fit_combined <- survfit(Surv(dfdf$duration_years, dfdf$censored) ~ group, data = dfdf)

# Plot the survival curves with 95% CI
#ggsurvplot(fit_combined, data = dfdf, conf.int = TRUE, legend = c(0.7, 0.25), risk.table = "absolute")

# Reshape the data
#melted_data <- melt(dfdf, id.vars = c("duration_years", "censored"), 
                    #measure.vars = c("Spike_group", "HFO_group"))

melted_data <- melt(dfdf, id.vars = c("duration_years", "censored"), 
                    measure.vars = c("Spike_group", "HFO_group"))

# Filter only relevant rows
filtered_data <- melted_data[melted_data$value != 0, ]

#print(table(filtered_data$variable))

# Filter only relevant rows
#filtered_data <- melted_data[melted_data$value == TRUE, ]

# Kaplan-Meier survival curves
fit_combined <- survfit(Surv(filtered_data$duration_years, filtered_data$censored) ~ variable, data = filtered_data)

# Plot the survival curves with 95% CI
#ggsurvplot(fit_combined, conf.int = TRUE, legend = c(0.7, 0.25), risk.table = "absolute", palette = c("blue", "red"))
ggsurvplot(fit_combined, data = filtered_data, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

print(fit_combined)

logrank_test <- survdiff(Surv(filtered_data$duration_years, filtered_data$censored) ~ variable, data = filtered_data)
print(logrank_test)

print(fit_combined)

#fit_spike <- survfit(Surv(dfdf$duration_years, dfdf$censored) ~ Spike_group, data = dfdf)
#fit_hfo <- survfit(Surv(dfdf$duration_years, dfdf$censored) ~ HFO_group, data = dfdf)
# Create the group variable that combines the Spike and HFO groups
#dfdf$combined_group <- ifelse(dfdf$Spike_group, "Spike", ifelse(dfdf$HFO_group, "HFO", NA))

# Fit Kaplan-Meier survival curve
#fit_combined <- survfit(Surv(dfdf$duration_years, dfdf$censored) ~ combined_group, data = dfdf)

# Plot the survival curves with 95% CI
#ggsurvplot(fit_combined, data = dfdf, conf.int = TRUE, legend = c(0.7, 0.25), risk.table = "absolute", palette = c("blue", "red"))







dfdf$duration_years <- dfdf[,7] / 12
dfdf$censored <- ifelse(dfdf[,47] == 0, 1, 0) 
dfdf$Spike_group <- dfdf[,8] != 0
dfdf$HFO_group <- dfdf[,10] != 0
dfdf$group <- ifelse(dfdf[,8] != 0, "Spike", ifelse(dfdf[,10] != 0, "HFO", NA))
melted_data <- melt(dfdf, id.vars = c("duration_years", "censored"), 
                    measure.vars = c("Spike_group", "HFO_group"))
filtered_data <- melted_data[melted_data$value != 0, ]
fit_combined <- survfit(Surv(filtered_data$duration_years, filtered_data$censored) ~ variable, data = filtered_data)
ggsurvplot(fit_combined, data = filtered_data, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))
print(fit_combined)
logrank_test <- survdiff(Surv(filtered_data$duration_years, filtered_data$censored) ~ variable, data = filtered_data)
print(logrank_test)
print(fit_combined)




# Convert follow-up months to years
dfdf$duration_years <- dfdf[,7] / 12

# Combine Engel scores: 1 for Success (Engel 1A or Engel 1 & 2) and 0 for Failure (Not Engel 1A or Engel > 2)
dfdf$engel_status <- ifelse(dfdf[,42] > 0 | dfdf[,46] > 0, 1, 
                            ifelse(dfdf[,44] > 0 | dfdf[,48] > 0, 0, NA))

# Remove records with NA in engel_status
dfdf <- dfdf[!is.na(dfdf$engel_status),]

# Create groups for Spike and HFO
dfdf$group <- ifelse(dfdf[,8] > 0, "Spike", 
                     ifelse(dfdf[,10] > 0, "HFO", NA))

# Fit the combined Kaplan-Meier survival curves
fit_combined <- survfit(Surv(dfdf$duration_years, dfdf$engel_status) ~ group, data = dfdf)

# Plot the survival curves with 95% CI
ggsurvplot(fit_combined, data = dfdf, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

# Conduct log-rank test
logrank_test <- survdiff(Surv(dfdf$duration_years, dfdf$engel_status) ~ group, data = dfdf)
print(logrank_test)





dfdf$duration_years <- dfdf[,7] / 12
dfdf$censored <- ifelse(dfdf[,42] == 0, 1, 0)

dfdf$MTS_group <- dfdf[,20] != 0
dfdf$CorticalDysplasia_group <- dfdf[,22] != 0
dfdf$VascularLesion_group <- dfdf[,24] != 0
dfdf$Tumor_group <- dfdf[,32] != 0

melted_data <- melt(dfdf, id.vars = c("duration_years", "censored"), 
                    measure.vars = c("MTS_group", "CorticalDysplasia_group", "VascularLesion_group", "Tumor_group"))
filtered_data <- melted_data[melted_data$value != 0, ]

fit_combined <- survfit(Surv(filtered_data$duration_years, filtered_data$censored) ~ variable, data = filtered_data)
ggsurvplot(fit_combined, data = filtered_data, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red", "green", "purple"))

logrank_test <- survdiff(Surv(filtered_data$duration_years, filtered_data$censored) ~ variable, data = filtered_data)
print(logrank_test)
print(fit_combined)





dfdf$duration_years <- dfdf[,7] / 12
dfdf$censored <- ifelse(dfdf[,46] == 0, 1, 0)

dfdf$MTS_group <- dfdf[,20] != 0
dfdf$CorticalDysplasia_group <- dfdf[,22] != 0
dfdf$VascularLesion_group <- dfdf[,24] != 0
dfdf$Tumor_group <- dfdf[,32] != 0

melted_data <- melt(dfdf, id.vars = c("duration_years", "censored"), 
                    measure.vars = c("MTS_group", "CorticalDysplasia_group", "VascularLesion_group", "Tumor_group"))
filtered_data <- melted_data[melted_data$value != 0, ]

fit_combined <- survfit(Surv(filtered_data$duration_years, filtered_data$censored) ~ variable, data = filtered_data)
ggsurvplot(fit_combined, data = filtered_data, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red", "green", "purple"))

logrank_test <- survdiff(Surv(filtered_data$duration_years, filtered_data$censored) ~ variable, data = filtered_data)
print(logrank_test)
print(fit_combined)





# Convert follow-up months to years
dfdf$duration_years <- dfdf[,7] / 12

# Combine Engel scores
dfdf$engel_status <- ifelse(dfdf[,42] > 0 | dfdf[,46] > 0, 1, 
                            ifelse(dfdf[,44] > 0 | dfdf[,48] > 0, 0, NA))

# Remove records with NA in engel_status
dfdf <- dfdf[!is.na(dfdf$engel_status),]

# Create groups for MTS, Cortical Dysplasia, Vascular Lesion, and Tumor
dfdf$group <- ifelse(dfdf[,20] > 0, "MTS", 
                     ifelse(dfdf[,22] > 0, "Cortical Dysplasia",
                            ifelse(dfdf[,24] > 0, "Vascular Lesion", 
                                   ifelse(dfdf[,32] > 0, "Tumor", NA))))

# Fit the combined Kaplan-Meier survival curves
fit_combined <- survfit(Surv(dfdf$duration_years, dfdf$engel_status) ~ group, data = dfdf)

# Plot the survival curves with 95% CI
ggsurvplot(fit_combined, data = dfdf, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red", "green", "purple"))

# Conduct log-rank test
logrank_test <- survdiff(Surv(dfdf$duration_years, dfdf$engel_status) ~ group, data = dfdf)
print(logrank_test)




dfdf$duration_years <- dfdf[,7] / 12
dfdf$censored <- ifelse(dfdf[,46] == 0, 1, 0)

# Filter data for MTS patients
dfdf_MTS <- dfdf[dfdf[,20] != 0, ]

dfdf_MTS$Spike_group <- dfdf_MTS[,8] != 0
dfdf_MTS$HFO_group <- dfdf_MTS[,10] != 0

melted_data_MTS <- melt(dfdf_MTS, id.vars = c("duration_years", "censored"), 
                        measure.vars = c("Spike_group", "HFO_group"))
filtered_data_MTS <- melted_data_MTS[melted_data_MTS$value != 0, ]

fit_combined_MTS <- survfit(Surv(filtered_data_MTS$duration_years, filtered_data_MTS$censored) ~ variable, data = filtered_data_MTS)
ggsurvplot(fit_combined_MTS, data = filtered_data_MTS, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

logrank_test_MTS <- survdiff(Surv(filtered_data_MTS$duration_years, filtered_data_MTS$censored) ~ variable, data = filtered_data_MTS)
print(logrank_test_MTS)
print(fit_combined_MTS)

dfdf$duration_years <- dfdf[,7] / 12
dfdf$censored <- ifelse(dfdf[,42] == 0, 1, 0)

# Filter data for MTS patients
dfdf_MTS <- dfdf[dfdf[,20] != 0, ]

dfdf_MTS$Spike_group <- dfdf_MTS[,8] != 0
dfdf_MTS$HFO_group <- dfdf_MTS[,10] != 0

melted_data_MTS <- melt(dfdf_MTS, id.vars = c("duration_years", "censored"), 
                        measure.vars = c("Spike_group", "HFO_group"))
filtered_data_MTS <- melted_data_MTS[melted_data_MTS$value != 0, ]

fit_combined_MTS <- survfit(Surv(filtered_data_MTS$duration_years, filtered_data_MTS$censored) ~ variable, data = filtered_data_MTS)
ggsurvplot(fit_combined_MTS, data = filtered_data_MTS, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

logrank_test_MTS <- survdiff(Surv(filtered_data_MTS$duration_years, filtered_data_MTS$censored) ~ variable, data = filtered_data_MTS)
print(logrank_test_MTS)
print(fit_combined_MTS)

# Convert follow-up months to years
dfdf$duration_years <- dfdf[,7] / 12

# Combine Engel scores
dfdf$engel_status <- ifelse(dfdf[,42] > 0 | dfdf[,46] > 0, 1, 
                            ifelse(dfdf[,44] > 0 | dfdf[,48] > 0, 0, NA))

# Remove records with NA in engel_status
dfdf <- dfdf[!is.na(dfdf$engel_status),]

# Filter data for MTS patients
dfdf_MTS <- dfdf[dfdf[,20] != 0, ]

dfdf_MTS$group <- ifelse(dfdf_MTS[,8] > 0, "Spike", 
                         ifelse(dfdf_MTS[,10] > 0, "HFO", NA))

# Fit the combined Kaplan-Meier survival curves for MTS patients
fit_combined_MTS <- survfit(Surv(dfdf_MTS$duration_years, dfdf_MTS$engel_status) ~ group, data = dfdf_MTS)

# Plot the survival curves with 95% CI
ggsurvplot(fit_combined_MTS, data = dfdf_MTS, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

# Conduct log-rank test for MTS patients
logrank_test_MTS <- survdiff(Surv(dfdf_MTS$duration_years, dfdf_MTS$engel_status) ~ group, data = dfdf_MTS)
print(logrank_test_MTS)





dfdf$duration_years <- dfdf[,7] / 12
dfdf$censored <- ifelse(dfdf[,46] == 0, 1, 0)

# Filter data for CD patients
dfdf_CD <- dfdf[dfdf[,22] != 0, ]

dfdf_CD$Spike_group <- dfdf_CD[,8] != 0
dfdf_CD$HFO_group <- dfdf_CD[,10] != 0

melted_data_CD <- melt(dfdf_CD, id.vars = c("duration_years", "censored"), 
                        measure.vars = c("Spike_group", "HFO_group"))
filtered_data_CD <- melted_data_CD[melted_data_CD$value != 0, ]

fit_combined_CD <- survfit(Surv(filtered_data_CD$duration_years, filtered_data_CD$censored) ~ variable, data = filtered_data_CD)
ggsurvplot(fit_combined_CD, data = filtered_data_CD, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

logrank_test_CD <- survdiff(Surv(filtered_data_CD$duration_years, filtered_data_CD$censored) ~ variable, data = filtered_data_CD)
print(logrank_test_CD)
print(fit_combined_CD)

dfdf$duration_years <- dfdf[,7] / 12
dfdf$censored <- ifelse(dfdf[,42] == 0, 1, 0)

# Filter data for CD patients
dfdf_CD <- dfdf[dfdf[,22] != 0, ]

dfdf_CD$Spike_group <- dfdf_CD[,8] != 0
dfdf_CD$HFO_group <- dfdf_CD[,10] != 0

melted_data_CD <- melt(dfdf_CD, id.vars = c("duration_years", "censored"), 
                       measure.vars = c("Spike_group", "HFO_group"))
filtered_data_CD <- melted_data_CD[melted_data_CD$value != 0, ]

fit_combined_CD <- survfit(Surv(filtered_data_CD$duration_years, filtered_data_CD$censored) ~ variable, data = filtered_data_CD)
ggsurvplot(fit_combined_CD, data = filtered_data_CD, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

logrank_test_CD <- survdiff(Surv(filtered_data_CD$duration_years, filtered_data_CD$censored) ~ variable, data = filtered_data_CD)
print(logrank_test_CD)
print(fit_combined_CD)

# Convert follow-up months to years
dfdf$duration_years <- dfdf[,7] / 12

# Combine Engel scores
dfdf$engel_status <- ifelse(dfdf[,42] > 0 | dfdf[,46] > 0, 1, 
                            ifelse(dfdf[,44] > 0 | dfdf[,48] > 0, 0, NA))

# Remove records with NA in engel_status
dfdf <- dfdf[!is.na(dfdf$engel_status),]

# Filter data for MTS patients
dfdf_CD <- dfdf[dfdf[,22] != 0, ]

dfdf_CD$group <- ifelse(dfdf_CD[,8] > 0, "Spike", 
                         ifelse(dfdf_CD[,10] > 0, "HFO", NA))

# Fit the combined Kaplan-Meier survival curves for MTS patients
fit_combined_CD <- survfit(Surv(dfdf_CD$duration_years, dfdf_CD$engel_status) ~ group, data = dfdf_CD)

# Plot the survival curves with 95% CI
ggsurvplot(fit_combined_CD, data = dfdf_CD, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

# Conduct log-rank test for MTS patients
logrank_test_CD <- survdiff(Surv(dfdf_CD$duration_years, dfdf_CD$engel_status) ~ group, data = dfdf_CD)
print(logrank_test_CD)





dfdf$duration_years <- dfdf[,7] / 12
dfdf$censored <- ifelse(dfdf[,46] == 0, 1, 0)

# Filter data for CD patients
dfdf_VL <- dfdf[dfdf[,24] != 0, ]

dfdf_VL$Spike_group <- dfdf_VL[,8] != 0
dfdf_VL$HFO_group <- dfdf_VL[,10] != 0

melted_data_VL <- melt(dfdf_VL, id.vars = c("duration_years", "censored"), 
                       measure.vars = c("Spike_group", "HFO_group"))
filtered_data_VL <- melted_data_VL[melted_data_VL$value != 0, ]

fit_combined_VL <- survfit(Surv(filtered_data_VL$duration_years, filtered_data_VL$censored) ~ variable, data = filtered_data_VL)
ggsurvplot(fit_combined_VL, data = filtered_data_VL, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

logrank_test_VL <- survdiff(Surv(filtered_data_VL$duration_years, filtered_data_VL$censored) ~ variable, data = filtered_data_VL)
print(logrank_test_VL)
print(fit_combined_VL)

dfdf$duration_years <- dfdf[,7] / 12
dfdf$censored <- ifelse(dfdf[,42] == 0, 1, 0)

# Filter data for CD patients
dfdf_VL <- dfdf[dfdf[,24] != 0, ]

dfdf_VL$Spike_group <- dfdf_VL[,8] != 0
dfdf_VL$HFO_group <- dfdf_VL[,10] != 0

melted_data_VL <- melt(dfdf_VL, id.vars = c("duration_years", "censored"), 
                       measure.vars = c("Spike_group", "HFO_group"))
filtered_data_VL <- melted_data_VL[melted_data_VL$value != 0, ]

fit_combined_VL <- survfit(Surv(filtered_data_VL$duration_years, filtered_data_VL$censored) ~ variable, data = filtered_data_VL)
ggsurvplot(fit_combined_VL, data = filtered_data_VL, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

logrank_test_VL <- survdiff(Surv(filtered_data_VL$duration_years, filtered_data_VL$censored) ~ variable, data = filtered_data_VL)
print(logrank_test_VL)
print(fit_combined_VL)

# Convert follow-up months to years
dfdf$duration_years <- dfdf[,7] / 12

# Combine Engel scores
dfdf$engel_status <- ifelse(dfdf[,42] > 0 | dfdf[,46] > 0, 1, 
                            ifelse(dfdf[,44] > 0 | dfdf[,48] > 0, 0, NA))

# Remove records with NA in engel_status
dfdf <- dfdf[!is.na(dfdf$engel_status),]

# Filter data for MTS patients
dfdf_VL <- dfdf[dfdf[,24] != 0, ]

dfdf_VL$group <- ifelse(dfdf_VL[,8] > 0, "Spike", 
                        ifelse(dfdf_VL[,10] > 0, "HFO", NA))

# Fit the combined Kaplan-Meier survival curves for MTS patients
fit_combined_VL <- survfit(Surv(dfdf_VL$duration_years, dfdf_VL$engel_status) ~ group, data = dfdf_VL)

# Plot the survival curves with 95% CI
ggsurvplot(fit_combined_VL, data = dfdf_VL, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

# Conduct log-rank test for MTS patients
logrank_test_VL <- survdiff(Surv(dfdf_VL$duration_years, dfdf_VL$engel_status) ~ group, data = dfdf_VL)
print(logrank_test_VL)





dfdf$duration_years <- dfdf[,7] / 12
dfdf$censored <- ifelse(dfdf[,46] == 0, 1, 0)

# Filter data for CD patients
dfdf_T <- dfdf[dfdf[,32] != 0, ]

dfdf_T$Spike_group <- dfdf_T[,8] != 0
dfdf_T$HFO_group <- dfdf_T[,10] != 0

melted_data_T <- melt(dfdf_T, id.vars = c("duration_years", "censored"), 
                       measure.vars = c("Spike_group", "HFO_group"))
filtered_data_T <- melted_data_T[melted_data_T$value != 0, ]

fit_combined_T <- survfit(Surv(filtered_data_T$duration_years, filtered_data_T$censored) ~ variable, data = filtered_data_T)
ggsurvplot(fit_combined_T, data = filtered_data_T, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

logrank_test_T <- survdiff(Surv(filtered_data_T$duration_years, filtered_data_T$censored) ~ variable, data = filtered_data_T)
print(logrank_test_T)
print(fit_combined_T)

dfdf$duration_years <- dfdf[,7] / 12
dfdf$censored <- ifelse(dfdf[,42] == 0, 1, 0)

# Filter data for CD patients
dfdf_T <- dfdf[dfdf[,32] != 0, ]

dfdf_T$Spike_group <- dfdf_T[,8] != 0
dfdf_T$HFO_group <- dfdf_T[,10] != 0

melted_data_T <- melt(dfdf_T, id.vars = c("duration_years", "censored"), 
                      measure.vars = c("Spike_group", "HFO_group"))
filtered_data_T <- melted_data_T[melted_data_T$value != 0, ]

fit_combined_T <- survfit(Surv(filtered_data_T$duration_years, filtered_data_T$censored) ~ variable, data = filtered_data_T)
ggsurvplot(fit_combined_T, data = filtered_data_T, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

logrank_test_T <- survdiff(Surv(filtered_data_T$duration_years, filtered_data_T$censored) ~ variable, data = filtered_data_T)
print(logrank_test_T)
print(fit_combined_T)

# Convert follow-up months to years
dfdf$duration_years <- dfdf[,7] / 12

# Combine Engel scores
dfdf$engel_status <- ifelse(dfdf[,42] > 0 | dfdf[,46] > 0, 1, 
                            ifelse(dfdf[,44] > 0 | dfdf[,48] > 0, 0, NA))

# Remove records with NA in engel_status
dfdf <- dfdf[!is.na(dfdf$engel_status),]

# Filter data for MTS patients
dfdf_T <- dfdf[dfdf[,32] != 0, ]

dfdf_T$group <- ifelse(dfdf_T[,8] > 0, "Spike", 
                        ifelse(dfdf_T[,10] > 0, "HFO", NA))

# Fit the combined Kaplan-Meier survival curves for MTS patients
fit_combined_T <- survfit(Surv(dfdf_T$duration_years, dfdf_T$engel_status) ~ group, data = dfdf_T)

# Plot the survival curves with 95% CI
ggsurvplot(fit_combined_T, data = dfdf_T, conf.int = TRUE, legend = "bottom", risk.table = "absolute", palette = c("blue", "red"))

# Conduct log-rank test for MTS patients
logrank_test_T <- survdiff(Surv(dfdf_T$duration_years, dfdf_T$engel_status) ~ group, data = dfdf_T)
print(logrank_test_T)
