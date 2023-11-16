################################################################################
################## Remove ZEROs from the dataset as N/R values #################
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
                       `Engel 1` = character(),
                       `Not Engel 1` = character(),
                       `Engel 1 and 2` = character(),
                       `Engel>2` = character())

# Fill in the data frame
for (i in 1:nrow(df)) {
  table_df[i, ] <- list(
    FirstAuthorYear = df[i, 1],
    StudyType = df[i, 3],
    Spike = ifelse(df[i, 8] == 0, "-", df[i, 8]),
    HFO = ifelse(df[i, 10] == 0, "-", df[i, 10]),
    `Engel 1` = ifelse(df[i, 43] == 0, "-", paste0(df[i, 42], "%")),
    `Not Engel 1` = ifelse(df[i, 45] == 0, "-", paste0(df[i, 44], "%")),
    `Engel 1 and 2` = ifelse(df[i, 47] == 0, "-", paste0(df[i, 46], "%")),
    `Engel>2` = ifelse(df[i, 49] == 0, "-", paste0(df[i, 48], "%"))
  )
}



# Print the data frame
print(table_df)
#print(df)
#dim(df)
library(dplyr)
# Remove the first row (headers)
#df <- df[-1, ]
#colnames(df) <- NULL

print(df)
#dim(df)
################################################################################

################################################################################
######################## HFO and Spike number of patients ######################
df_t_2 <- df
#df_t_2 <- df_t_2[!(df_t_2[,5] == 0 | df_t_2[,6] == 0 | df_t_2[,7] == 0), ]
spike_p <- df_t_2[df_t_2[,8] != 0, ]
spike_p <- as.data.frame(spike_p)
print(spike_p)
total_spike_patients <- sum(spike_p[,8])
print(paste("Total number of patients with Spike:", total_spike_patients))

df_t_2 <- df
#df_t_2 <- df_t_2[!(df_t_2[,5] == 0 | df_t_2[,6] == 0 | df_t_2[,7] == 0), ]
hfo_m <- df_t_2[df_t_2[,10] != 0, ]
hfo_m <- as.data.frame(hfo_m)
print(hfo_m)
total_hfo_patients <- sum(hfo_m[,10])
print(paste("Total number of patients with HFO:", total_hfo_patients))
################################################################################

ecog_plus <- spike_p
ecog_minus <- hfo_m
dfdf <- df
dfdf <- as.data.frame(dfdf)
################################################################################
#TABLE 1
# Load the dataframe (assuming 'dfdf' is already loaded in your R environment)

# Create a new dataframe to store the results
results <- data.frame(
  FirstAuthorYear = dfdf[, 1],
  StudyDesign = dfdf[, 3],
  SpikePatients = dfdf[, 8],
  HFOPatients = dfdf[, 10],
  SpikeSeizureFreedom = ifelse(dfdf[, 8] == 0, 'N/A', dfdf[, 42] / dfdf[, 8] * 100),
  HFOSeizureFreedom = ifelse(dfdf[, 10] == 0, 'N/A', dfdf[, 42] / dfdf[, 10] * 100)
)

# Print the results
print(results)
################################################################################

################################################################################
# TABLE 2 - AGE
# Assuming your dataframe is named dfdf and the age at surgery is in column 5, stratified by Spike (Ecog+) in column 8 and HFO (Ecog-) in column 10.

# First, split the age data into two groups based on Spike and HFO
spike_ages <- dfdf[dfdf[, 8] > 0, 5]  # Ages for Spike patients
hfo_ages <- dfdf[dfdf[, 10] > 0, 5]   # Ages for HFO patients

# Calculate the median and interquartile range (IQR) for each group
spike_median <- median(spike_ages, na.rm = TRUE)
spike_iqr <- IQR(spike_ages, na.rm = TRUE)
hfo_median <- median(hfo_ages, na.rm = TRUE)
hfo_iqr <- IQR(hfo_ages, na.rm = TRUE)

# Output the results
cat("Spike group - Median Age at Surgery:", spike_median, "IQR:", spike_iqr, "\n")
cat("HFO group - Median Age at Surgery:", hfo_median, "IQR:", hfo_iqr, "\n")

# If you need to test for a statistically significant difference in medians, you can use the Mann-Whitney U test
# This is a non-parametric test appropriate for comparing medians from independent samples
mw_test <- wilcox.test(spike_ages, hfo_ages)

# For the effect size, you can calculate the rank-biserial correlation as a measure of effect size for the Mann-Whitney U test
effect_size <- mw_test$statistic / (length(spike_ages) * length(hfo_ages))

# Output the test results and effect size
cat("Mann-Whitney U test p-value:", mw_test$p.value, "\n")
cat("Effect size (rank-biserial correlation):", effect_size, "\n")
################################################################################

################################################################################
# TABLE 2 - Epilepsy Duration
# Assuming your dataframe is named dfdf and the relevant columns are 5 and 6.
# Assuming dfdf is your original dataframe and you want to preserve it
dfdf_copy <- dfdf  # Make a copy of the original dataframe

# Replace zeros with NA in the copy for the relevant columns
dfdf_copy[, 5][dfdf_copy[, 5] == 0] <- NA
dfdf_copy[, 6][dfdf_copy[, 6] == 0] <- NA

# Calculate epilepsy duration by subtracting the values in column 6 from column 5
dfdf_copy$EpilepsyDuration <- dfdf_copy[, 5] - dfdf_copy[, 6]

# Split the epilepsy duration data into two groups based on Spike and HFO
spike_durations <- dfdf_copy[dfdf_copy[, 8] > 0, 'EpilepsyDuration']
hfo_durations <- dfdf_copy[dfdf_copy[, 10] > 0, 'EpilepsyDuration']

# Perform the Mann-Whitney U test to compare the distributions between the two groups
mw_test <- wilcox.test(spike_durations, hfo_durations, exact = FALSE)

# Calculate the median and IQR for each group, excluding NA values
spike_median <- median(spike_durations, na.rm = TRUE)
spike_iqr <- IQR(spike_durations, na.rm = TRUE)
hfo_median <- median(hfo_durations, na.rm = TRUE)
hfo_iqr <- IQR(hfo_durations, na.rm = TRUE)

# Output the results
cat("Spike group - Median Epilepsy Duration:", spike_median, "IQR:", spike_iqr, "\n")
cat("HFO group - Median Epilepsy Duration:", hfo_median, "IQR:", hfo_iqr, "\n")

# Output the Mann-Whitney U test results
cat("Mann-Whitney U test p-value:", mw_test$p.value, "\n")

# Calculate effect size using rank-biserial correlation
effect_size <- (mw_test$statistic - (length(spike_durations) * (length(spike_durations) + 1) / 2)) / (length(spike_durations) * length(hfo_durations))

# Output the effect size
cat("Effect size (rank-biserial correlation):", effect_size, "\n")

# Make a copy of the dataframe to avoid altering the original
dfdf_copy <- dfdf

# Replace zeros with NA for the analysis in the copy
dfdf_copy[, 5][dfdf_copy[, 5] == 0] <- NA
dfdf_copy[, 6][dfdf_copy[, 6] == 0] <- NA

# Calculate epilepsy duration
dfdf_copy$EpilepsyDuration <- dfdf_copy[, 5] - dfdf_copy[, 6]

# Split the data into Spike and HFO groups
spike_durations <- dfdf_copy[dfdf_copy[, 8] > 0, 'EpilepsyDuration']
hfo_durations <- dfdf_copy[dfdf_copy[, 10] > 0, 'EpilepsyDuration']

# Mann-Whitney U test
mw_test <- wilcox.test(spike_durations, hfo_durations, exact = FALSE)

# Rank-biserial correlation as effect size
effect_size <- (mw_test$statistic - (length(spike_durations) * (length(spike_durations) + 1) / 2)) / (length(spike_durations) * length(hfo_durations))

# Output the results
cat("Spike group - Median Epilepsy Duration:", median(spike_durations, na.rm = TRUE), "IQR:", IQR(spike_durations, na.rm = TRUE), "\n")
cat("HFO group - Median Epilepsy Duration:", median(hfo_durations, na.rm = TRUE), "IQR:", IQR(hfo_durations, na.rm = TRUE), "\n")
cat("Mann-Whitney U test p-value:", mw_test$p.value, "\n")
cat("Effect size (rank-biserial correlation):", effect_size, "\n")

################################################################################
#TABLE 2 - Gender
# Create a copy of the dataframe to keep the original dfdf intact
dfdf_gender <- dfdf

# Calculate the number of males and females in Spike and HFO groups
spike_males <- sum(dfdf_gender[dfdf_gender[, 8] > 0, 51], na.rm = TRUE)
spike_females <- sum(dfdf_gender[dfdf_gender[, 8] > 0, 50], na.rm = TRUE)
hfo_males <- sum(dfdf_gender[dfdf_gender[, 10] > 0, 51], na.rm = TRUE)
hfo_females <- sum(dfdf_gender[dfdf_gender[, 10] > 0, 50], na.rm = TRUE)

# Calculate percentages
spike_males_pct <- spike_males / (spike_males + spike_females) * 100
spike_females_pct <- spike_females / (spike_males + spike_females) * 100
hfo_males_pct <- hfo_males / (hfo_males + hfo_females) * 100
hfo_females_pct <- hfo_females / (hfo_males + hfo_females) * 100

# Create a contingency table
gender_table <- matrix(c(spike_males, spike_females, hfo_males, hfo_females), nrow = 2, byrow = TRUE)

# Perform the Chi-squared test and calculate the odds ratio
chi_test_gender <- chisq.test(gender_table, correct = TRUE)
odds_ratio_gender <- (gender_table[1,1] / gender_table[1,2]) / (gender_table[2,1] / gender_table[2,2])

# Print the table with counts and percentages
cat("Spike - Male:", spike_males, "(", sprintf("%.1f%%", spike_males_pct), ") Female:", spike_females, "(", sprintf("%.1f%%", spike_females_pct), ")\n")
cat("HFO - Male:", hfo_males, "(", sprintf("%.1f%%", hfo_males_pct), ") Female:", hfo_females, "(", sprintf("%.1f%%", hfo_females_pct), ")\n")
cat("Chi-squared test p-value:", chi_test_gender$p.value, "\n")
cat("Effect size (Odds Ratio):", odds_ratio_gender, "\n")


################################################################################

################################################################################
# TABLE 2 - surgical site
# Create a copy of the dataframe to keep the original dfdf intact
dfdf_surgery <- dfdf

# Calculate the number of temporal and extratemporal surgeries in Spike and HFO groups
spike_temporal <- sum(dfdf_surgery[dfdf_surgery[, 8] > 0, 14], na.rm = TRUE)
spike_extratemporal <- sum(dfdf_surgery[dfdf_surgery[, 8] > 0, 16], na.rm = TRUE)
hfo_temporal <- sum(dfdf_surgery[dfdf_surgery[, 10] > 0, 14], na.rm = TRUE)
hfo_extratemporal <- sum(dfdf_surgery[dfdf_surgery[, 10] > 0, 16], na.rm = TRUE)

# Calculate percentages
spike_temporal_pct <- spike_temporal / (spike_temporal + spike_extratemporal) * 100
spike_extratemporal_pct <- spike_extratemporal / (spike_temporal + spike_extratemporal) * 100
hfo_temporal_pct <- hfo_temporal / (hfo_temporal + hfo_extratemporal) * 100
hfo_extratemporal_pct <- hfo_extratemporal / (hfo_temporal + hfo_extratemporal) * 100

# Create a contingency table
surgery_table <- matrix(c(spike_temporal, spike_extratemporal, hfo_temporal, hfo_extratemporal), nrow = 2, byrow = TRUE)

# Perform Fisher's exact test
fisher_test_surgery <- fisher.test(surgery_table)

# Calculate odds ratio for effect size
odds_ratio <- (surgery_table[1,1] / surgery_table[2,1]) / (surgery_table[1,2] / surgery_table[2,2])

# Print the table with counts and percentages
cat("Spike - Temporal:", spike_temporal, "(", sprintf("%.1f", spike_temporal_pct), "%) Extratemporal:", spike_extratemporal, "(", sprintf("%.1f", spike_extratemporal_pct), "%)\n")
cat("HFO - Temporal:", hfo_temporal, "(", sprintf("%.1f", hfo_temporal_pct), "%) Extratemporal:", hfo_extratemporal, "(", sprintf("%.1f", hfo_extratemporal_pct), "%)\n")
cat("Fisher's exact test p-value:", fisher_test_surgery$p.value, "\n")
cat("Effect size (Odds Ratio):", odds_ratio, "\n")
################################################################################

################################################################################
# TABLE 2 - Pathology
# Create a copy of the dataframe to keep the original dfdf intact
dfdf_pathology <- dfdf

# Define the columns for the pathological issues and their respective names
#pathology_cols <- c(8, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40)
#pathology_names <- c("Normal", "MTS", "Cortical Dysplasia", "Vascular Lesion", "Trauma", 
#                     "Infection", "Heterotopia", "Tumor", "Stroke, Gliosis, Inflammation", 
#                     "Congenital", "Other", "MTS + FCD")
#pathology_cols <- c(8, 20, 22, 24, 32)
#pathology_names <- c("Normal", "MTS", "Cortical Dysplasia", "Vascular Lesion",  "Tumor")
pathology_cols <- c( 20, 22, 24, 32, 38)
pathology_names <- c( "MTS", "Cortical Dysplasia", "Vascular Lesion",  "Tumor", "Other")
# Initialize a matrix to hold the counts for each pathology
pathology_counts <- matrix(nrow = 2, ncol = length(pathology_cols))

# Fill the matrix with counts for Spike and HFO groups
for (i in seq_along(pathology_cols)) {
  pathology_counts[1, i] <- sum(dfdf_pathology[dfdf_pathology[, 8] > 0, pathology_cols[i]], na.rm = TRUE)  # Spike counts
  pathology_counts[2, i] <- sum(dfdf_pathology[dfdf_pathology[, 10] > 0, pathology_cols[i]], na.rm = TRUE) # HFO counts
}

# Calculate percentages
pathology_percentages <- prop.table(pathology_counts, 1) * 100  # Row-wise proportions to get percentages

# Perform the Chi-squared test
chi_test_pathology <- chisq.test(pathology_counts)

# Print the table with counts and percentages
cat("Pathology Distribution\n")
for (i in seq_along(pathology_names)) {
  cat(pathology_names[i], "- Spike:", pathology_counts[1, i], "(", sprintf("%.1f%%", pathology_percentages[1, i]), 
      ") HFO:", pathology_counts[2, i], "(", sprintf("%.1f%%", pathology_percentages[2, i]), ")\n")
}

# Output the Chi-squared test results
cat("Chi-squared test p-value:", chi_test_pathology$p.value, "\n")

# Note: Since we have multiple categories, Cramér's V can be calculated for the overall association
n <- sum(pathology_counts)  # Total observations
phi_square <- chi_test_pathology$statistic / n
cramer_v <- sqrt(phi_square / min(dim(pathology_counts)[1] - 1, dim(pathology_counts)[2] - 1))

cat("Effect size (Cramér's V):", cramer_v, "\n")

################################################################################

################################################################################
# TABLE 2 - Engel 1
# Create a copy of the dataframe to keep the original dfdf intact
dfdf_engel1 <- dfdf

# Calculate the counts for Engel 1 and not Engel 1 in Spike and HFO groups
spike_engel1 <- sum(dfdf_engel1[dfdf_engel1[, 8] > 0, 42], na.rm = TRUE)
spike_not_engel1 <- sum(dfdf_engel1[dfdf_engel1[, 8] > 0, 44], na.rm = TRUE)
hfo_engel1 <- sum(dfdf_engel1[dfdf_engel1[, 10] > 0, 42], na.rm = TRUE)
hfo_not_engel1 <- sum(dfdf_engel1[dfdf_engel1[, 10] > 0, 44], na.rm = TRUE)

# Create a contingency table
engel1_table <- matrix(c(spike_engel1, spike_not_engel1, hfo_engel1, hfo_not_engel1), nrow = 2, byrow = TRUE)

# Perform Fisher's exact test
fisher_test_engel1 <- fisher.test(engel1_table)

# Calculate odds ratio for effect size
odds_ratio_engel1 <- (engel1_table[1,1] / engel1_table[1,2]) / (engel1_table[2,1] / engel1_table[2,2])

# Print the results
cat("Engel Class 1 - Spike:", spike_engel1, "HFO:", hfo_engel1, "\n")
cat("Not Engel Class 1 - Spike:", spike_not_engel1, "HFO:", hfo_not_engel1, "\n")
cat("Fisher's exact test p-value:", fisher_test_engel1$p.value, "\n")
cat("Effect size (Odds Ratio):", odds_ratio_engel1, "\n")
################################################################################

################################################################################
# TABLE 2 - Engel >2
# Create a copy of the dataframe to keep the original dfdf intact
dfdf_engel12 <- dfdf

# Calculate the counts for Engel 1&2 and Engel>2 in Spike and HFO groups
spike_engel12 <- sum(dfdf_engel12[dfdf_engel12[, 8] > 0, 46], na.rm = TRUE)
spike_engel_gt2 <- sum(dfdf_engel12[dfdf_engel12[, 8] > 0, 48], na.rm = TRUE)
hfo_engel12 <- sum(dfdf_engel12[dfdf_engel12[, 10] > 0, 46], na.rm = TRUE)
hfo_engel_gt2 <- sum(dfdf_engel12[dfdf_engel12[, 10] > 0, 48], na.rm = TRUE)

# Create a contingency table
engel12_table <- matrix(c(spike_engel12, spike_engel_gt2, hfo_engel12, hfo_engel_gt2), nrow = 2, byrow = TRUE)

# Perform Fisher's exact test
fisher_test_engel12 <- fisher.test(engel12_table)

# Calculate odds ratio for effect size
odds_ratio_engel12 <- (engel12_table[1,1] / engel12_table[1,2]) / (engel12_table[2,1] / engel12_table[2,2])

# Print the results
cat("Engel Class 1&2 - Spike:", spike_engel12, "HFO:", hfo_engel12, "\n")
cat("Engel Class >2 - Spike:", spike_engel_gt2, "HFO:", hfo_engel_gt2, "\n")
cat("Fisher's exact test p-value:", fisher_test_engel12$p.value, "\n")
cat("Effect size (Odds Ratio):", odds_ratio_engel12, "\n")

################################################################################

################################################################################
# Figure 3 - Spike vs HFO
# Load necessary libraries
library(survival)
library(survminer)

# Assuming dfdf is the dataframe with the data
# Convert follow-up time from months to years for each patient
dfdf$FollowUpYears <- dfdf[, 7] / 12

# Create a binary event indicator for seizure freedom, 1 if seizure-free and 0 otherwise
dfdf$SeizureFreeEvent <- with(dfdf, ifelse(dfdf[, 42] > 0, 1, 0))

# Prepare the data for survival analysis, considering the count of seizure-free patients
# We create a separate row for each patient, repeating the follow-up time accordingly
spike_data <- data.frame(FollowUpYears = rep(dfdf$FollowUpYears, dfdf[, 8]), 
                         SeizureFreeEvent = rep(dfdf$SeizureFreeEvent, dfdf[, 8]), 
                         Group = 'Spike')
hfo_data <- data.frame(FollowUpYears = rep(dfdf$FollowUpYears, dfdf[, 10]), 
                       SeizureFreeEvent = rep(dfdf$SeizureFreeEvent, dfdf[, 10]), 
                       Group = 'HFO')

# Combine spike and hfo data
combined_data <- rbind(spike_data, hfo_data)

# Create the survival object
SurvObj <- with(combined_data, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit <- survfit(SurvObj ~ Group, data = combined_data)

# Plot the Kaplan-Meier survival curves
g <- ggsurvplot(fit, data = combined_data, pval = TRUE, conf.int = TRUE, 
                risk.table = TRUE, risk.table.col = "strata",
                xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                legend.title = "Group", palette = c("#E7B800", "#2E9FDF"))

# Print the plot
print(g)

# Perform Log-Rank Test
log_rank_test <- survdiff(SurvObj ~ Group, data = combined_data)

# Output the Log-Rank Test result
cat("Log-rank test p-value:", pvalue(log_rank_test), "\n")

cat("Log-rank test p-value:", 1 - pchisq(log_rank_test$chisq, df = log_rank_test$n - 1), "\n")

################################################################################

################################################################################
#Figure 3 - pathology
library(survival)
library(survminer)

# Assuming dfdf is your dataframe and it's already loaded in your R environment
# Convert follow-up time from months to years for each patient
dfdf$FollowUpYears <- dfdf[, 7] / 12

# Define the pathology columns and names
pathology_cols <- c(20, 22, 24, 32, 38)
pathology_names <- c("MTS", "Cortical Dysplasia", "Vascular Lesion", "Tumor", "Other")

# Prepare the data for survival analysis
pathology_data <- list()

for (i in 1:length(pathology_cols)) {
  for (j in 1:nrow(dfdf)) {
    if (dfdf[j, pathology_cols[i]] > 0) {
      pathology_data[[length(pathology_data) + 1]] <- data.frame(
        FollowUpYears = rep(dfdf[j, "FollowUpYears"], dfdf[j, pathology_cols[i]]),
        SeizureFreeEvent = rep(as.numeric(dfdf[j, 42] > 0), dfdf[j, pathology_cols[i]]),
        Pathology = pathology_names[i]
      )
    }
  }
}

# Combine all individual patient data into one data frame
combined_pathology_data <- do.call(rbind, pathology_data)

# Create the survival object
SurvObj <- with(combined_pathology_data, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit <- survfit(SurvObj ~ Pathology, data = combined_pathology_data)

# Plot the Kaplan-Meier survival curves
g <- ggsurvplot(fit, data = combined_pathology_data, pval = TRUE, conf.int = TRUE,
                risk.table = TRUE, risk.table.col = "strata",
                xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                legend.title = "Pathology", palette = c("#E7B800", "#2E9FDF", "#AEC7E8", "#FFBB78", "#98DF8A"))

# Print the plot
print(g)

# Perform Log-Rank Test
log_rank_test <- survdiff(SurvObj ~ Pathology, data = combined_pathology_data)

# Output the Log-Rank Test result using the chi-squared distribution
log_rank_p_value <- 1 - pchisq(log_rank_test$chisq, df = log_rank_test$n - 1)
cat("Log-rank test p-value:", log_rank_p_value, "\n")


################################################################################

################################################################################
#Figure 3 - Spike Vs. HFO - CD
library(survival)
library(survminer)

# Assuming dfdf is your dataframe and it's already loaded in your R environment
# Make a copy of the dataframe
dfdf_copy <- dfdf

# Convert follow-up time from months to years for each patient
dfdf_copy$FollowUpYears <- dfdf_copy[, 7] / 12

# Filter the data to include only patients with CD (Column 22)
dfdf_cd <- dfdf_copy[dfdf_copy[, 22] > 0, ]

# Disaggregate the data for Spike and HFO groups
spike_data_cd <- data.frame(
  FollowUpYears = rep(dfdf_cd$FollowUpYears, dfdf_cd[, 8]),
  SeizureFreeEvent = rep(dfdf_cd[, 42] > 0, dfdf_cd[, 8]),
  Group = 'Spike'
)
hfo_data_cd <- data.frame(
  FollowUpYears = rep(dfdf_cd$FollowUpYears, dfdf_cd[, 10]),
  SeizureFreeEvent = rep(dfdf_cd[, 42] > 0, dfdf_cd[, 10]),
  Group = 'HFO'
)

# Combine spike and hfo data
combined_data_cd <- rbind(spike_data_cd, hfo_data_cd)

# Create the survival object
SurvObj <- with(combined_data_cd, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit <- survfit(SurvObj ~ Group, data = combined_data_cd)

# Plot the Kaplan-Meier survival curves
g <- ggsurvplot(fit, data = combined_data_cd, pval = TRUE, conf.int = TRUE,
                risk.table = TRUE, risk.table.col = "strata",
                xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                legend.title = "Group", palette = c("#E7B800", "#2E9FDF"))

# Print the plot
print(g)

# Perform Log-Rank Test
log_rank_test <- survdiff(SurvObj ~ Group, data = combined_data_cd)

# Output the Log-Rank Test result using the chi-squared distribution
log_rank_p_value <- 1 - pchisq(log_rank_test$chisq, df = log_rank_test$n - 1)
cat("Log-rank test p-value:", log_rank_p_value, "\n")

library(survival)
library(survminer)
# Filter the data to include only studies with patients who have CD (Column 22)
dfdf_cd <- dfdf[dfdf[, 22] > 0, ]

# Prepare the data for survival analysis considering CD
dfdf_cd$Group <- ifelse(dfdf_cd[, 8] > 0, "Spike", ifelse(dfdf_cd[, 10] > 0, "HFO", NA))
dfdf_cd$SeizureFreeEvent <- as.numeric(dfdf_cd[, 42] > 0)

# Filter out rows with NA in Group
dfdf_cd <- na.omit(dfdf_cd)

# Create the survival object
SurvObj_cd <- with(dfdf_cd, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit_cd <- survfit(SurvObj_cd ~ Group, data = dfdf_cd)

# Plot the Kaplan-Meier survival curves
g_cd <- ggsurvplot(fit_cd, data = dfdf_cd, pval = TRUE, conf.int = TRUE,
                   xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                   legend.title = "Group", palette = c("#E7B800", "#2E9FDF"))

# Print the plot
print(g_cd)

# Perform Log-Rank Test between Spike and HFO
log_rank_test_cd <- survdiff(SurvObj_cd ~ Group, data = dfdf_cd)

# Output the Log-Rank Test result
log_rank_p_value_cd <- 1 - pchisq(log_rank_test_cd$chisq, df = 1)
cat("Log-rank test p-value for CD:", log_rank_p_value_cd, "\n")

################################################################################

################################################################################
#Figure 3 - Spike Vs. HFO - MTS
library(survival)
library(survminer)

# Assuming dfdf is your dataframe and it's already loaded in your R environment
# Make a copy of the dataframe
dfdf_copy <- dfdf

# Convert follow-up time from months to years for each patient
dfdf_copy$FollowUpYears <- dfdf_copy[, 7] / 12

# Filter the data to include only patients with MTS (Column 20)
dfdf_mts <- dfdf_copy[dfdf_copy[, 20] > 0, ]

# Disaggregate the data for Spike and HFO groups
spike_data_mts <- data.frame(
  FollowUpYears = rep(dfdf_mts$FollowUpYears, dfdf_mts[, 8]),
  SeizureFreeEvent = rep(dfdf_mts[, 42] > 0, dfdf_mts[, 8]),
  Group = 'Spike'
)
hfo_data_mts <- data.frame(
  FollowUpYears = rep(dfdf_mts$FollowUpYears, dfdf_mts[, 10]),
  SeizureFreeEvent = rep(dfdf_mts[, 42] > 0, dfdf_mts[, 10]),
  Group = 'HFO'
)

# Combine spike and hfo data
combined_data_mts <- rbind(spike_data_mts, hfo_data_mts)

# Create the survival object
SurvObj <- with(combined_data_mts, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit <- survfit(SurvObj ~ Group, data = combined_data_mts)

# Plot the Kaplan-Meier survival curves
g <- ggsurvplot(fit, data = combined_data_mts, pval = TRUE, conf.int = TRUE,
                risk.table = TRUE, risk.table.col = "strata",
                xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                legend.title = "Group", palette = c("#E7B800", "#2E9FDF"))

# Print the plot
print(g)

# Perform Log-Rank Test
log_rank_test <- survdiff(SurvObj ~ Group, data = combined_data_mts)

# Output the Log-Rank Test result using the chi-squared distribution
log_rank_p_value <- 1 - pchisq(log_rank_test$chisq, df = log_rank_test$n - 1)
cat("Log-rank test p-value:", log_rank_p_value, "\n")

library(survival)
library(survminer)

# Convert follow-up time from months to years for each patient
dfdf$FollowUpYears <- dfdf[, 7] / 12

# Filter the data to include only studies with patients who have MTS (Column 20)
dfdf_mts <- dfdf[dfdf[, 20] > 0, ]

# Prepare the data for survival analysis considering MTS
dfdf_mts$Group <- ifelse(dfdf_mts[, 8] > 0, "Spike", ifelse(dfdf_mts[, 10] > 0, "HFO", NA))
dfdf_mts$SeizureFreeEvent <- as.numeric(dfdf_mts[, 42] > 0)

# Filter out rows with NA in Group
dfdf_mts <- na.omit(dfdf_mts)

# Create the survival object
SurvObj <- with(dfdf_mts, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit <- survfit(SurvObj ~ Group, data = dfdf_mts)

# Plot the Kaplan-Meier survival curves
g <- ggsurvplot(fit, data = dfdf_mts, pval = TRUE, conf.int = TRUE,
                xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                legend.title = "Group", palette = c("#E7B800", "#2E9FDF"))

# Print the plot
print(g)

# Perform Log-Rank Test between Spike and HFO
log_rank_test <- survdiff(SurvObj ~ Group, data = dfdf_mts)

# Output the Log-Rank Test result
log_rank_p_value <- 1 - pchisq(log_rank_test$chisq, df = 1)
cat("Log-rank test p-value:", log_rank_p_value, "\n")


################################################################################

################################################################################
#Figure 3 - Spike Vs. HFO - Tumor
library(survival)
library(survminer)

# Convert follow-up time from months to years for each patient
dfdf$FollowUpYears <- dfdf[, 7] / 12

# Filter the data to include only patients with Tumor (Column 32)
dfdf_tumor <- dfdf[dfdf[, 32] > 0, ]

# Disaggregate the data for Spike and HFO groups
spike_data_tumor <- data.frame(
  FollowUpYears = rep(dfdf_tumor$FollowUpYears, dfdf_tumor[, 8]),
  SeizureFreeEvent = rep(dfdf_tumor[, 42] > 0, dfdf_tumor[, 8]),
  Group = 'Spike'
)
hfo_data_tumor <- data.frame(
  FollowUpYears = rep(dfdf_tumor$FollowUpYears, dfdf_tumor[, 10]),
  SeizureFreeEvent = rep(dfdf_tumor[, 42] > 0, dfdf_tumor[, 10]),
  Group = 'HFO'
)

# Combine spike and hfo data
combined_data_tumor <- rbind(spike_data_tumor, hfo_data_tumor)

# Create the survival object
SurvObj <- with(combined_data_tumor, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit <- survfit(SurvObj ~ Group, data = combined_data_tumor)

# Plot the Kaplan-Meier survival curves
g <- ggsurvplot(fit, data = combined_data_tumor, pval = TRUE, conf.int = TRUE,
                risk.table = TRUE, risk.table.col = "strata",
                xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                legend.title = "Group", palette = c("#E7B800", "#2E9FDF"))

# Print the plot
print(g)

# Perform Log-Rank Test
log_rank_test <- survdiff(SurvObj ~ Group, data = combined_data_tumor)

# Output the Log-Rank Test result using the chi-squared distribution
log_rank_p_value <- 1 - pchisq(log_rank_test$chisq, df = log_rank_test$n - 1)
cat("Log-rank test p-value:", log_rank_p_value, "\n")

library(survival)
library(survminer)

# Convert follow-up time from months to years for each patient
dfdf$FollowUpYears <- dfdf[, 7] / 12

# Filter the data to include only studies with patients who have Tumor (Column 32)
dfdf_tumor <- dfdf[dfdf[, 32] > 0, ]

# Prepare the data for survival analysis considering Tumor
dfdf_tumor$Group <- ifelse(dfdf_tumor[, 8] > 0, "Spike", ifelse(dfdf_tumor[, 10] > 0, "HFO", NA))
dfdf_tumor$SeizureFreeEvent <- as.numeric(dfdf_tumor[, 42] > 0)

# Filter out rows with NA in Group
dfdf_tumor <- na.omit(dfdf_tumor)

# Create the survival object
SurvObj_tumor <- with(dfdf_tumor, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit_tumor <- survfit(SurvObj_tumor ~ Group, data = dfdf_tumor)

# Plot the Kaplan-Meier survival curves
g_tumor <- ggsurvplot(fit_tumor, data = dfdf_tumor, pval = TRUE, conf.int = TRUE,
                      xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                      legend.title = "Group", palette = c("#E7B800", "#2E9FDF"))

# Print the plot
print(g_tumor)

# Perform Log-Rank Test between Spike and HFO
log_rank_test_tumor <- survdiff(SurvObj_tumor ~ Group, data = dfdf_tumor)

# Output the Log-Rank Test result
log_rank_p_value_tumor <- 1 - pchisq(log_rank_test_tumor$chisq, df = 1)
cat("Log-rank test p-value for Tumor:", log_rank_p_value_tumor, "\n")

################################################################################

################################################################################
#Figure 3 - Spike Vs. HFO - VL
# Filter the data to include only patients with Vascular Lesion (Column 24)
dfdf_vl <- dfdf[dfdf[, 24] > 0, ]

# Disaggregate the data for Spike and HFO groups
spike_data_vl <- data.frame(
  FollowUpYears = rep(dfdf_vl$FollowUpYears, dfdf_vl[, 8]),
  SeizureFreeEvent = rep(dfdf_vl[, 42] > 0, dfdf_vl[, 8]),
  Group = 'Spike'
)
hfo_data_vl <- data.frame(
  FollowUpYears = rep(dfdf_vl$FollowUpYears, dfdf_vl[, 10]),
  SeizureFreeEvent = rep(dfdf_vl[, 42] > 0, dfdf_vl[, 10]),
  Group = 'HFO'
)

# Combine spike and hfo data
combined_data_vl <- rbind(spike_data_vl, hfo_data_vl)

# Create the survival object
SurvObj <- with(combined_data_vl, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit <- survfit(SurvObj ~ Group, data = combined_data_vl)

# Plot the Kaplan-Meier survival curves
g <- ggsurvplot(fit, data = combined_data_vl, pval = TRUE, conf.int = TRUE,
                risk.table = TRUE, risk.table.col = "strata",
                xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                legend.title = "Group", palette = c("#E7B800", "#2E9FDF"))

# Print the plot
print(g)

# Perform Log-Rank Test
log_rank_test <- survdiff(SurvObj ~ Group, data = combined_data_vl)

# Output the Log-Rank Test result using the chi-squared distribution
log_rank_p_value <- 1 - pchisq(log_rank_test$chisq, df = log_rank_test$n - 1)
cat("Log-rank test p-value:", log_rank_p_value, "\n")

# Filter the data to include only studies with patients who have Vascular Lesion (Column 24)
dfdf_vl <- dfdf[dfdf[, 24] > 0, ]

# Prepare the data for survival analysis considering VL
dfdf_vl$Group <- ifelse(dfdf_vl[, 8] > 0, "Spike", ifelse(dfdf_vl[, 10] > 0, "HFO", NA))
dfdf_vl$SeizureFreeEvent <- as.numeric(dfdf_vl[, 42] > 0)

# Filter out rows with NA in Group
dfdf_vl <- na.omit(dfdf_vl)

# Create the survival object
SurvObj_vl <- with(dfdf_vl, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit_vl <- survfit(SurvObj_vl ~ Group, data = dfdf_vl)

# Plot the Kaplan-Meier survival curves
g_vl <- ggsurvplot(fit_vl, data = dfdf_vl, pval = TRUE, conf.int = TRUE,
                   xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                   legend.title = "Group", palette = c("#E7B800", "#2E9FDF"))

# Print the plot
print(g_vl)

# Perform Log-Rank Test between Spike and HFO
log_rank_test_vl <- survdiff(SurvObj_vl ~ Group, data = dfdf_vl)

# Output the Log-Rank Test result
log_rank_p_value_vl <- 1 - pchisq(log_rank_test_vl$chisq, df = 1)
cat("Log-rank test p-value for VL:", log_rank_p_value_vl, "\n")

################################################################################

################################################################################
#Figure 3 - Spike Vs. HFO - Other
# Assuming dfdf is your dataframe and it's already loaded in your R environment
# Convert follow-up time from months to years for each patient
dfdf$FollowUpYears <- dfdf[, 7] / 12

# Filter the data to include only patients with "Other" condition (Column 38)
dfdf_other <- dfdf[dfdf[, 38] > 0, ]

# Disaggregate the data for Spike and HFO groups
spike_data_other <- data.frame(
  FollowUpYears = rep(dfdf_other$FollowUpYears, dfdf_other[, 8]),
  SeizureFreeEvent = rep(dfdf_other[, 42] > 0, dfdf_other[, 8]),
  Group = 'Spike'
)
hfo_data_other <- data.frame(
  FollowUpYears = rep(dfdf_other$FollowUpYears, dfdf_other[, 10]),
  SeizureFreeEvent = rep(dfdf_other[, 42] > 0, dfdf_other[, 10]),
  Group = 'HFO'
)

# Combine spike and hfo data
combined_data_other <- rbind(spike_data_other, hfo_data_other)

# Create the survival object
SurvObj <- with(combined_data_other, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit <- survfit(SurvObj ~ Group, data = combined_data_other)

# Plot the Kaplan-Meier survival curves
g <- ggsurvplot(fit, data = combined_data_other, pval = TRUE, conf.int = TRUE,
                risk.table = TRUE, risk.table.col = "strata",
                xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                legend.title = "Group", palette = c("#E7B800", "#2E9FDF"))

# Print the plot
print(g)

# Perform Log-Rank Test
log_rank_test <- survdiff(SurvObj ~ Group, data = combined_data_other)

# Output the Log-Rank Test result using the chi-squared distribution
log_rank_p_value <- 1 - pchisq(log_rank_test$chisq, df = log_rank_test$n - 1)
cat("Log-rank test p-value:", log_rank_p_value, "\n")

# Filter the data to include only studies with patients who have "Other" condition (Column 38)
dfdf_other <- dfdf[dfdf[, 38] > 0, ]

# Prepare the data for survival analysis considering "Other"
dfdf_other$Group <- ifelse(dfdf_other[, 8] > 0, "Spike", ifelse(dfdf_other[, 10] > 0, "HFO", NA))
dfdf_other$SeizureFreeEvent <- as.numeric(dfdf_other[, 42] > 0)

# Filter out rows with NA in Group
dfdf_other <- na.omit(dfdf_other)

# Create the survival object
SurvObj_other <- with(dfdf_other, Surv(time = FollowUpYears, event = SeizureFreeEvent))

# Fit the Kaplan-Meier survival curves
fit_other <- survfit(SurvObj_other ~ Group, data = dfdf_other)

# Plot the Kaplan-Meier survival curves
g_other <- ggsurvplot(fit_other, data = dfdf_other, pval = TRUE, conf.int = TRUE,
                      xlab = "Years of Follow-up", ylab = "Seizure-Free Probability",
                      legend.title = "Group", palette = c("#E7B800", "#2E9FDF"))

# Print the plot
print(g_other)

# Perform Log-Rank Test between Spike and HFO
log_rank_test_other <- survdiff(SurvObj_other ~ Group, data = dfdf_other)

# Output the Log-Rank Test result
log_rank_p_value_other <- 1 - pchisq(log_rank_test_other$chisq, df = 1)
cat("Log-rank test p-value for Other:", log_rank_p_value_other, "\n")

################################################################################

################################################################################
#Figure 2 - Forest
library(metafor)
library(meta)



# Assuming dfdf is your dataframe and it's already loaded in your R environment
# Filter out studies that report both HFO and Spike
df_meta_spike <- dfdf[dfdf[, 8] > 0 & dfdf[, 10] == 0, ]
df_meta_hfo <- dfdf[dfdf[, 10] > 0 & dfdf[, 8] == 0, ]

# Create two separate vectors for each group (Spike and HFO)
event.e <- df_meta_spike[, 42]
n.e <- df_meta_spike[, 4]
event.c <- df_meta_hfo[, 42]
n.c <- df_meta_hfo[, 4]

# Ensure that the vectors are of the same length
len <- min(length(event.e), length(event.c))
event.e <- event.e[1:len]
n.e <- n.e[1:len]
event.c <- event.c[1:len]
n.c <- n.c[1:len]
studlab <- dfdf$Study[1:len]  # Assuming the 'Study' column has the study labels

# Prepare the data for meta-analysis
meta_data <- data.frame(studlab, event.e, n.e, event.c, n.c)

# Calculate Risk Ratios (RR) and perform meta-analysis using DerSimonian-Laird method
meta_analysis <- rma(measure="RR", ai=event.e, n1i=n.e, ci=event.c, n2i=n.c, method="DL", data=meta_data)

# Generate forest plot
forest(meta_analysis)

# Output the heterogeneity statistics
cat("I-squared:", meta_analysis$I2, "\n")
cat("Tau-squared:", meta_analysis$tau2, "\n")

# Output the test for overall effect
cat("Test for overall effect: Z =", meta_analysis$zval, ", p =", meta_analysis$pval, "\n")



################################################################################
