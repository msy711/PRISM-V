### Linguistic characteristics of suicidal speech 

## Between-person, t-test 
## linguistic 

start_col <- which(names(data_between_suicidal) == "sentiment_mean")
end_col <- which(names(data_between_suicidal) == "liwc_avoidance")
selected_columns <- names(data_between_suicidal)[start_col:end_col]
variables_to_test2 <- paste(selected_columns, collapse=", ")
variables_to_test2 <- strsplit(variables_to_test2, ",\\s*")[[1]]
print(variables_to_test2)


# Function to perform t-tests and return results
run_analysis <- function(data, variables) {
  results <- data.frame(
    Variable = character(),
    Mean_SD_Suicidal = character(),
    Mean_SD_NonSuicidal = character(),
    T_Statistic = numeric(),
    P_Value = numeric(),
    Adjusted_P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  p_values <- numeric()  # Vector to store all p-values
  
  # Loop through each variable and perform t-test
  for (var in variables) {
    # Ensure the variable exists in the dataset
    if (!var %in% names(data)) next
    
    # Calculate means and standard deviations
    mean_sd_suicidal <- data %>%
      filter(suicidal == 1) %>%
      summarise(
        mean = round(mean(get(var), na.rm = TRUE), 5),
        sd = round(sd(get(var), na.rm = TRUE), 5)
      )
    
    mean_sd_nonsuicidal <- data %>%
      filter(suicidal == 0) %>%
      summarise(
        mean = round(mean(get(var), na.rm = TRUE), 5),
        sd = round(sd(get(var), na.rm = TRUE), 5)
      )
    
    # Format Mean (SD)
    mean_sd_suicidal_formatted <- paste(mean_sd_suicidal$mean, " (", mean_sd_suicidal$sd, ")", sep="")
    mean_sd_nonsuicidal_formatted <- paste(mean_sd_nonsuicidal$mean, " (", mean_sd_nonsuicidal$sd, ")", sep="")
    
    # T-test
    t_test_results <- t.test(get(var) ~ suicidal, data = data)
    p_values <- c(p_values, t_test_results$p.value)  # Store p-value
    
    # Collect initial results
    results <- rbind(results, data.frame(
      Variable = var,
      Mean_SD_Suicidal = mean_sd_suicidal_formatted,
      Mean_SD_NonSuicidal = mean_sd_nonsuicidal_formatted,
      T_Statistic = round(t_test_results$statistic, 2),
      P_Value = round(t_test_results$p.value, 4)
    ))
  }
  
  # Apply Benjamini-Hochberg correction outside the loop
  results$Adjusted_P_Value <- round(p.adjust(p_values, method = "BH"), 3)
  
  return(results)
}

# Apply the function
results <- run_analysis(data_bewteen_suicidal, variables_to_test2)


## Between-person, linear regression
## linguistic 

start_col <- which(names(data_bewteen_suicidal) == "sentiment_mean")
end_col <- which(names(data_bewteen_suicidal) == "liwc_avoidance")
selected_columns <- names(data_bewteen_suicidal)[start_col:end_col]
variables_to_test2 <- paste(selected_columns, collapse=", ")
variables_to_test2 <- strsplit(variables_to_test2, ",\\s*")[[1]]
print(variables_to_test2)

# Initialize the list to store models
results <- list()

for (feature in variables_to_test2) {
  formula <- as.formula(paste(feature, "~ suicidal + age + sex + edu_yrs"))
  model <- lm(formula, data = transformed_data)
  results[[feature]] <- model
}

# Initialize the dataframe to store coefficients
coef_df <- data.frame(
  Feature = character(),
  Coefficient = numeric(),
  CI_low = numeric(),
  CI_high = numeric(),
  BH_Adjusted_PValue = numeric(),
  stringsAsFactors = FALSE  # Avoid factors to make plotting easier
)

# Extract coefficients from each model and add to the dataframe
# Extract coefficients and p-values from each model and add to the dataframe
for (feature in variables_to_test2) {
  model <- results[[feature]]  # Retrieve the model from results
  if (!is.null(model) && "suicidal" %in% rownames(summary(model)$coefficients)) {
    coefs <- coef(summary(model))
    suicidal_coef <- coefs["suicidal", "Estimate"]
    std_error <- coefs["suicidal", "Std. Error"]
    p_value <- coefs["suicidal", "Pr(>|t|)"]
    ci_low <- suicidal_coef - 1.96 * std_error
    ci_high = suicidal_coef + 1.96 * std_error
    
    # Append to coef_df
    coef_df <- rbind(coef_df, data.frame(
      Feature = feature,
      Coefficient = suicidal_coef,
      CI_low = ci_low,
      CI_high = ci_high,
      StdError = std_error,
      PValue = p_value,
      BH_Adjusted_PValue = NA  # Placeholder for adjusted p-value
    ))
  }
}

# Apply Benjamini-Hochberg adjustment
coef_df$BH_Adjusted_PValue <- p.adjust(coef_df$PValue, method = "BH")

# Add a 'Significant' column to the dataframe
coef_df$Significant <- with(coef_df,  BH_Adjusted_PValue < 0.05)
coef_df

# Reverse the order of variables_to_test for the plot
variables_to_test2 <- rev(variables_to_test2)

# Update the Feature column in coef_df to be a factor with the levels in reversed order
coef_df$Feature <- factor(coef_df$Feature, levels = variables_to_test2)

# Create the forest plot
library(ggplot2)

ggplot(coef_df, aes(y = Feature, x = Coefficient)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(color = Significant)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, color = Significant), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Association between high suicide risk and linguistic features",
       x = "Beta Coefficient",
       y = "Feature",
       subtitle = "Significant after B-H adjustment in red") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 4))  # Adjust text size for feature names


# Print the results table
print(coef_df)


# Subgroup by sex 
data_female <- subset(data_bewteen_suicidal, sex==1)
data_male <- subset(data_bewteen_suicidal, sex==0)


# Female:
start_col <- which(names(data_female) == "sentiment_mean")
end_col <- which(names(data_female) == "liwc_avoidance")
selected_columns <- names(data_female)[start_col:end_col]
variables_to_test2 <- paste(selected_columns, collapse=", ")
variables_to_test2 <- strsplit(variables_to_test2, ",\\s*")[[1]]
print(variables_to_test2)

# Initialize the list to store models
results <- list()

for (feature in variables_to_test2) {
  formula <- as.formula(paste(feature, "~ suicidal + age + edu_yrs"))
  model <- lm(formula, data = data_female)
  results[[feature]] <- model
}

# Initialize the dataframe to store coefficients
coef_df <- data.frame(
  Feature = character(),
  Coefficient = numeric(),
  CI_low = numeric(),
  CI_high = numeric(),
  BH_Adjusted_PValue = numeric(),
  stringsAsFactors = FALSE  # Avoid factors to make plotting easier
)

# Extract coefficients from each model and add to the dataframe
# Extract coefficients and p-values from each model and add to the dataframe
for (feature in variables_to_test2) {
  model <- results[[feature]]  # Retrieve the model from results
  if (!is.null(model) && "suicidal" %in% rownames(summary(model)$coefficients)) {
    coefs <- coef(summary(model))
    suicidal_coef <- coefs["suicidal", "Estimate"]
    std_error <- coefs["suicidal", "Std. Error"]
    p_value <- coefs["suicidal", "Pr(>|t|)"]
    ci_low <- suicidal_coef - 1.96 * std_error
    ci_high = suicidal_coef + 1.96 * std_error
    
    # Append to coef_df
    coef_df <- rbind(coef_df, data.frame(
      Feature = feature,
      Coefficient = suicidal_coef,
      CI_low = ci_low,
      CI_high = ci_high,
      StdError = std_error,
      PValue = p_value,
      BH_Adjusted_PValue = NA  # Placeholder for adjusted p-value
    ))
  }
}

# Apply Benjamini-Hochberg adjustment
coef_df$BH_Adjusted_PValue <- p.adjust(coef_df$PValue, method = "BH")

# Add a 'Significant' column to the dataframe
coef_df$Significant <- with(coef_df,  BH_Adjusted_PValue < 0.05)
coef_df

# Reverse the order of variables_to_test for the plot
variables_to_test2 <- rev(variables_to_test2)

# Update the Feature column in coef_df to be a factor with the levels in reversed order
coef_df$Feature <- factor(coef_df$Feature, levels = variables_to_test2)

# Create the forest plot
library(ggplot2)

# Create the forest plot
ggplot(coef_df, aes(y = Feature, x = Coefficient)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(color = Significant)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, color = Significant), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Association between high suicide risk and linguistic features (Female)",
       x = "Beta Coefficient",
       y = "Feature",
       subtitle = "Significant after B-H adjustment in red") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 4))  # Adjust text size for feature names

# Print the results table
print(coef_df)


# Male:
start_col <- which(names(data_male) == "sentiment_mean")
end_col <- which(names(data_male) == "liwc_avoidance")
selected_columns <- names(data_male)[start_col:end_col]
variables_to_test2 <- paste(selected_columns, collapse=", ")
variables_to_test2 <- strsplit(variables_to_test2, ",\\s*")[[1]]
print(variables_to_test2)

# Initialize the list to store models
results <- list()

for (feature in variables_to_test2) {
  formula <- as.formula(paste(feature, "~ suicidal + age + edu_yrs"))
  model <- lm(formula, data = data_male)
  results[[feature]] <- model
}

# Initialize the dataframe to store coefficients
coef_df <- data.frame(
  Feature = character(),
  Coefficient = numeric(),
  CI_low = numeric(),
  CI_high = numeric(),
  BH_Adjusted_PValue = numeric(),
  stringsAsFactors = FALSE  # Avoid factors to make plotting easier
)

# Extract coefficients from each model and add to the dataframe
# Extract coefficients and p-values from each model and add to the dataframe
for (feature in variables_to_test2) {
  model <- results[[feature]]  # Retrieve the model from results
  if (!is.null(model) && "suicidal" %in% rownames(summary(model)$coefficients)) {
    coefs <- coef(summary(model))
    suicidal_coef <- coefs["suicidal", "Estimate"]
    std_error <- coefs["suicidal", "Std. Error"]
    p_value <- coefs["suicidal", "Pr(>|t|)"]
    ci_low <- suicidal_coef - 1.96 * std_error
    ci_high = suicidal_coef + 1.96 * std_error
    
    # Append to coef_df
    coef_df <- rbind(coef_df, data.frame(
      Feature = feature,
      Coefficient = suicidal_coef,
      CI_low = ci_low,
      CI_high = ci_high,
      StdError = std_error,
      PValue = p_value,
      BH_Adjusted_PValue = NA  # Placeholder for adjusted p-value
    ))
  }
}

# Apply Benjamini-Hochberg adjustment
coef_df$BH_Adjusted_PValue <- p.adjust(coef_df$PValue, method = "BH")

# Add a 'Significant' column to the dataframe
coef_df$Significant <- with(coef_df,  BH_Adjusted_PValue < 0.05)
coef_df

# Reverse the order of variables_to_test for the plot
variables_to_test2 <- rev(variables_to_test2)

# Update the Feature column in coef_df to be a factor with the levels in reversed order
coef_df$Feature <- factor(coef_df$Feature, levels = variables_to_test2)

# Create the forest plot
library(ggplot2)

# Create the forest plot
ggplot(coef_df, aes(y = Feature, x = Coefficient)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(color = Significant)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, color = Significant), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Association between high suicide risk and linguistic features (Male)",
       x = "Beta Coefficient",
       y = "Feature",
       subtitle = "Significant after B-H adjustment in red") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 4))  # Adjust text size for feature names

# Print the results table
print(coef_df)


#### Within-person 

# Define the file paths
internal_file <- "C:/Users/msy71/OneDrive/¹ÙÅÁ È­¸é/PRISM-V/prism_datacleaned/longi_internal.csv"
external_file <- "C:/Users/msy71/OneDrive/¹ÙÅÁ È­¸é/PRISM-V/prism_datacleaned/longi_external.csv"

# Read the CSV files
internal_df <- read.csv(internal_file)
external_df <- read.csv(external_file)

# Merge the dataframes by column names
# Assuming you want to merge by common columns
merged_df <- merge(internal_df, external_df, by = intersect(names(internal_df), names(external_df)), all = TRUE)

# Print the first few rows of the merged dataframe to check the result
head(merged_df)
write.csv(merged_df, "C:/Users/msy71/OneDrive/¹ÙÅÁ È­¸é/PRISM-V/prism_datacleaned/npj_longitudinal_all.csv", row.names = FALSE)

data_within_all <- read.csv("C:/Users/msy71/OneDrive/¹ÙÅÁ È­¸é/PRISM-V/prism_datacleaned/npj_longitudinal_all.csv", header=TRUE, stringsAsFactors=FALSE)

table(data_within_all$label) # count recordings by suicidal risk

## Within-person, t-test 
## Linguistic


start_col <- which(names(data_within_all) == "liwc_analytical_thinking")
end_col <- which(names(data_within_all) == "pos_NNP")
selected_columns <- names(data_within_all)[start_col:end_col]
variables_to_test2 <- paste(selected_columns, collapse=", ")
variables_to_test2 <- strsplit(variables_to_test2, ",\\s*")[[1]]
print(variables_to_test2)


# Function to perform t-tests and return results
run_analysis <- function(data, variables) {
  results <- data.frame(
    Variable = character(),
    Mean_SD_Suicidal = character(),
    Mean_SD_NonSuicidal = character(),
    T_Statistic = numeric(),
    P_Value = numeric(),
    Adjusted_P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  p_values <- numeric()  # Vector to store all p-values
  
  # Loop through each variable and perform t-test
  for (var in variables) {
    # Ensure the variable exists in the dataset
    if (!var %in% names(data)) next
    
    # Calculate means and standard deviations
    mean_sd_suicidal <- data %>%
      filter(label == 1) %>%
      summarise(
        mean = round(mean(get(var), na.rm = TRUE), 5),
        sd = round(sd(get(var), na.rm = TRUE), 5)
      )
    
    mean_sd_nonsuicidal <- data %>%
      filter(label == 0) %>%
      summarise(
        mean = round(mean(get(var), na.rm = TRUE), 5),
        sd = round(sd(get(var), na.rm = TRUE), 5)
      )
    
    # Format Mean (SD)
    mean_sd_suicidal_formatted <- paste(mean_sd_suicidal$mean, " (", mean_sd_suicidal$sd, ")", sep="")
    mean_sd_nonsuicidal_formatted <- paste(mean_sd_nonsuicidal$mean, " (", mean_sd_nonsuicidal$sd, ")", sep="")
    
    # T-test
    t_test_results <- t.test(get(var) ~ label, data = data)
    p_values <- c(p_values, t_test_results$p.value)  # Store p-value
    
    # Collect initial results
    results <- rbind(results, data.frame(
      Variable = var,
      Mean_SD_Suicidal = mean_sd_suicidal_formatted,
      Mean_SD_NonSuicidal = mean_sd_nonsuicidal_formatted,
      T_Statistic = round(t_test_results$statistic, 2),
      P_Value = round(t_test_results$p.value, 4)
    ))
  }
  
  # Apply Benjamini-Hochberg correction outside the loop
  results$Adjusted_P_Value <- round(p.adjust(p_values, method = "BH"), 3)
  
  return(results)
}

# Apply the function
results <- run_analysis(data_within_all, variables_to_test2)


## Linear regression
## Linguistic 

data_within_all <- read.csv("C:/Users/msy71/OneDrive/¹ÙÅÁ È­¸é/PRISM-V/prism_datacleaned/npj_longi_all.csv", header=TRUE, stringsAsFactors=FALSE) # with AP data

data_within_all <- data_within_all %>%
  mutate(
    AP_change = case_when(
      !is.na(AP_dose_2) & !is.na(AP_dose_0) ~ AP_dose_2 - AP_dose_0,
      !is.na(AP_dose_4) & !is.na(AP_dose_2) ~ AP_dose_4 - AP_dose_2,
      TRUE ~ NA_real_  # Assign NA where conditions do not meet
    )
  )

# Filter to get the edu_yrs where case_episode is baseline
edu_yrs_baseline <- transformed_data %>%
  filter(case_episode == "baseline") %>%
  select(id, edu_yrs)

# Join this information with data_within_all
data_within_all <- data_within_all %>%
  left_join(edu_yrs_baseline, by = "id")

# View the updated dataset
head(data_within_all)

# Define the variables to test
start_col <- which(names(data_within_all) == "liwc_analytical_thinking")
end_col <- which(names(data_within_all) == "pos_NNP")
selected_columns <- names(data_within_all)[start_col:end_col]
variables_to_test2 <- paste(selected_columns, collapse=", ")
variables_to_test2 <- strsplit(variables_to_test2, ",\\s*")[[1]]
print(variables_to_test2)

# Initialize the list to store models
results <- list()

for (feature in variables_to_test2) {
  formula <- as.formula(paste(feature, "~ label + age + sex + edu_yrs"))
  model <- lm(formula, data = data_within_all)
  results[[feature]] <- model
}

# Initialize the dataframe to store coefficients
coef_df <- data.frame(
  Feature = character(),
  Coefficient = numeric(),
  CI_low = numeric(),
  CI_high = numeric(),
  BH_Adjusted_PValue = numeric(),
  stringsAsFactors = FALSE  # Avoid factors to make plotting easier
)

# Extract coefficients from each model and add to the dataframe
# Extract coefficients and p-values from each model and add to the dataframe
for (feature in variables_to_test2) {
  model <- results[[feature]]  # Retrieve the model from results
  if (!is.null(model) && "label" %in% rownames(summary(model)$coefficients)) {
    coefs <- coef(summary(model))
    suicidal_coef <- coefs["label", "Estimate"]
    std_error <- coefs["label", "Std. Error"]
    p_value <- coefs["label", "Pr(>|t|)"]
    ci_low <- suicidal_coef - 1.96 * std_error
    ci_high = suicidal_coef + 1.96 * std_error
    
    # Append to coef_df
    coef_df <- rbind(coef_df, data.frame(
      Feature = feature,
      Coefficient = suicidal_coef,
      CI_low = ci_low,
      CI_high = ci_high,
      StdError = std_error,
      PValue = p_value,
      BH_Adjusted_PValue = NA  # Placeholder for adjusted p-value
    ))
  }
}

# Apply Benjamini-Hochberg adjustment
coef_df$BH_Adjusted_PValue <- p.adjust(coef_df$PValue, method = "BH")

# Add a 'Significant' column to the dataframe
coef_df$Significant <- with(coef_df,  BH_Adjusted_PValue < 0.05)
coef_df

# Remove rows with NA values in Feature column (if any)
coef_df <- coef_df %>% filter(!is.na(Feature))
coef_df

# Update the Feature column in coef_df to be a factor with the levels in reversed order
coef_df$Feature <- factor(coef_df$Feature, levels = variables_to_test2)
coef_df
# Create the forest plot
library(ggplot2)

# Create the forest plot
ggplot(coef_df, aes(y = Feature, x = Coefficient)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(color = Significant)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, color = Significant), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Association between increase in suicide risk and changes in linguistic features",
       x = "Beta Coefficient",
       y = "Feature",
       subtitle = "Significant after B-H adjustment in red") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 6)) # Reduce the text size for better readability

# Print the results table
print(coef_df)