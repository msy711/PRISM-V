### Baseline characeristics: suicidal VS non-suicidal 

# Load necessary library
library(dplyr)
library(readr)
library(e1071)  

suicidal_group <- function(data){
  data2 <- data %>%
    mutate(suicidal = ifelse(SSI>=15,1,0))
  return(data2) }
data_between_suicidal <- suicidal_group(data_between)

table(data_between_suicidal$suicidal) # count recordings by suicidal risk

# Subset the data to include only baseline cases
baseline_data <- data_between_suicidal %>%
  filter(case_episode == "baseline")

# Calculate the count and percentage of suicidal vs. non-suicidal cases
suicidal_count <- baseline_data %>%
  group_by(suicidal) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)
print(suicidal_count)

# Export suicidal count and percentage to CSV
write.csv(suicidal_count, "C:/Users/msy71/OneDrive/¹ÙÅÁ È­¸é/PRISM-V/prism_datacleaned/suicidal_count.csv", row.names = FALSE)

# Perform t-test for continuous variables and calculate mean and standard deviation
continuous_vars <- c("age", "bmi", "edu_yrs", "income", "AP_dose", "HAMD", "SSI", "PHQ", "BAI", "BHOL", "BIS")

t_test_results <- lapply(continuous_vars, function(var) {
  t_result <- t.test(baseline_data[[var]] ~ baseline_data$suicidal)
  mean_sd <- baseline_data %>%
    group_by(suicidal) %>%
    summarise(mean = mean(get(var), na.rm = TRUE), sd = sd(get(var), na.rm = TRUE))
  
  data.frame(
    Variable = var,
    Mean_suicidal = mean_sd$mean[mean_sd$suicidal == 1],
    SD_suicidal = mean_sd$sd[mean_sd$suicidal == 1],
    Mean_non_suicidal = mean_sd$mean[mean_sd$suicidal == 0],
    SD_non_suicidal = mean_sd$sd[mean_sd$suicidal == 0],
    t_statistic = t_result$statistic,
    p_value = t_result$p.value
  )
})

t_test_results <- do.call(rbind, t_test_results)
print(t_test_results)

# Function to calculate median and range
median_range <- function(x) {
  median_value <- median(x, na.rm = TRUE)
  range_values <- range(x, na.rm = TRUE)
  c(median = median_value, min = range_values[1], max = range_values[2])
}

# Calculate median and range for income and AP_dose for each suicidal group
income_stats <- baseline_data %>%
  group_by(suicidal) %>%
  summarise(across(income, median_range))

ap_dose_stats <- baseline_data %>%
  group_by(suicidal) %>%
  summarise(across(AP_dose, median_range))

# Perform Mann-Whitney U test for income and AP_dose
income_test <- wilcox.test(income ~ suicidal, data = baseline_data)
ap_dose_test <- wilcox.test(AP_dose ~ suicidal, data = baseline_data)

# Print the results
print("Income Statistics by Suicidal Group:")
print(income_stats)

print("AP_dose Statistics by Suicidal Group:")
print(ap_dose_stats)

print(paste("Mann-Whitney U test for Income: p-value =", income_test$p.value))
print(paste("Mann-Whitney U test for AP_dose: p-value =", ap_dose_test$p.value))

bipolar_group <- function(data){
  data2 <- data %>%
    mutate(bpd = ifelse(Dx %in% c("MDD"),0,1))
  return(data2)
}
baseline_data_dx <- bipolar_group(baseline_data)

# Create a contingency table for sex by suicidal
table_sex_suicidal <- table(baseline_data$sex, baseline_data$suicidal)

# Calculate the counts and percentages
counts <- as.data.frame.matrix(table_sex_suicidal)
percentages <- round(prop.table(table_sex_suicidal, 2) * 100, 1)

# Combine counts and percentages into one table
combined_table <- counts
for (i in seq_len(nrow(counts))) {
  for (j in seq_len(ncol(counts))) {
    combined_table[i, j] <- paste0(counts[i, j], " (", percentages[i, j], "%)")
  }
}

# Perform the chi-square test
chi_square_test <- chisq.test(table_sex_suicidal)

# Print the combined table and the p-value of the chi-square test
print(combined_table)
print(paste("Chi-square test p-value:", chi_square_test$p.value))

# Create a contingency table for bpd by suicidal
table_bpd_suicidal <- table(baseline_data_dx$bpd, baseline_data_dx$suicidal)

# Calculate the counts and percentages
counts <- as.data.frame.matrix(table_bpd_suicidal)
percentages <- round(prop.table(table_bpd_suicidal, 2) * 100, 1)

# Combine counts and percentages into one table
combined_table <- counts
for (i in seq_len(nrow(counts))) {
  for (j in seq_len(ncol(counts))) {
    combined_table[i, j] <- paste0(counts[i, j], " (", percentages[i, j], "%)")
  }
}

# Perform the chi-square test
chi_square_test <- chisq.test(table_bpd_suicidal)

# Print the combined table and the p-value of the chi-square test
print(combined_table)
print(paste("Chi-square test p-value:", chi_square_test$p.value))

# Perform the Fisher's Exact Test
fisher_test <- fisher.test(table_bpd_suicidal)

# Print the combined table and the p-value of the Fisher's Exact Test
print(paste("Fisher's Exact Test p-value:", fisher_test$p.value))

# Create a contingency table for marriage by suicidal
table_marriage_suicidal <- table(baseline_data_dx$marriage, baseline_data_dx$suicidal)

# Calculate the counts and percentages
counts <- as.data.frame.matrix(table_marriage_suicidal)
percentages <- round(prop.table(table_marriage_suicidal, 2) * 100, 1)

# Combine counts and percentages into one table
combined_table <- counts
for (i in seq_len(nrow(counts))) {
  for (j in seq_len(ncol(counts))) {
    combined_table[i, j] <- paste0(counts[i, j], " (", percentages[i, j], "%)")
  }
}

# Perform the chi-square test
chi_square_test <- chisq.test(table_marriage_suicidal)

# Print the combined table and the p-value of the chi-square test
print(combined_table)
print(paste("Chi-square test p-value:", chi_square_test$p.value))

# Perform the Fisher's Exact Test
fisher_test <- fisher.test(table_marriage_suicidal)

# Print the combined table and the p-value of the Fisher's Exact Test
print(paste("Fisher's Exact Test p-value:", fisher_test$p.value))

# Perform ANOVA to see if marriage is associated with age
anova_result <- aov(age ~ marriage, data = baseline_data)

# Print the ANOVA results
summary(anova_result)

# Create a contingency table for mhx_distress by suicidal
table_mhx_suicidal <- table(baseline_data_dx$mhx_distress, baseline_data_dx$suicidal)

# Calculate the counts and percentages
counts <- as.data.frame.matrix(table_mhx_suicidal)
percentages <- round(prop.table(table_mhx_suicidal, 2) * 100, 1)

# Combine counts and percentages into one table
combined_table <- counts
for (i in seq_len(nrow(counts))) {
  for (j in seq_len(ncol(counts))) {
    combined_table[i, j] <- paste0(counts[i, j], " (", percentages[i, j], "%)")
  }
}

# Perform the chi-square test
chi_square_test <- chisq.test(table_mhx_suicidal)

# Print the combined table and the p-value of the chi-square test
print(combined_table)
print(paste("Chi-square test p-value:", chi_square_test$p.value))

# Perform the Fisher's Exact Test
fisher_test <- fisher.test(table_mhx_suicidal)

# Print the combined table and the p-value of the Fisher's Exact Test
print(paste("Fisher's Exact Test p-value:", fisher_test$p.value))

# Create a contingency table for suicide_hx by suicidal
table_shx_suicidal <- table(baseline_data_dx$suicide_hx, baseline_data_dx$suicidal)

# Calculate the counts and percentages
counts <- as.data.frame.matrix(table_shx_suicidal)
percentages <- round(prop.table(table_shx_suicidal, 2) * 100, 1)

# Combine counts and percentages into one table
combined_table <- counts
for (i in seq_len(nrow(counts))) {
  for (j in seq_len(ncol(counts))) {
    combined_table[i, j] <- paste0(counts[i, j], " (", percentages[i, j], "%)")
  }
}

# Perform the chi-square test
chi_square_test <- chisq.test(table_shx_suicidal)

# Print the combined table and the p-value of the chi-square test
print(combined_table)
print(paste("Chi-square test p-value:", chi_square_test$p.value))

# Perform the Fisher's Exact Test
fisher_test <- fisher.test(table_shx_suicidal)

# Print the combined table and the p-value of the Fisher's Exact Test
print(paste("Fisher's Exact Test p-value:", fisher_test$p.value))
