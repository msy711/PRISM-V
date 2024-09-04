### Acoustic characteristics of suicidal speech 

## Between-person, t-test
## Acoustic 

variables_to_test <- c("time", "cent", "band","roll","rmse","tempo","formant0","formant1","formant2","bw0","bw1","bw2","mean_p","error_p","change_p",
                       "mean_m","error_m","change_m","mean_z","delay_p","mfcc_0","mfcc_1","mfcc_2","mfcc_3","mfcc_4","mfcc_5","mfcc_6","mfcc_7",
                       "mfcc_8","mfcc_9","mfcc_10","mfcc_11","mfcc_12","mfcc_13","mfcc_14","mfcc_15","mfcc_16","mfcc_17","mfcc_18","mfcc_19",
                       "mfcc_20","mfcc_21","mfcc_22","mfcc_23","mfcc_24","mfcc_25","mfcc_26","mfcc_27","mfcc_28","mfcc_29","mfcc_30","mfcc_31",
                       "mfcc_32","mfcc_33","mfcc_34","mfcc_35","mfcc_36","mfcc_37","mfcc_38","mfcc_39")  

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
        mean = round(mean(get(var), na.rm = TRUE), 2),
        sd = round(sd(get(var), na.rm = TRUE), 2)
      )
    
    mean_sd_nonsuicidal <- data %>%
      filter(suicidal == 0) %>%
      summarise(
        mean = round(mean(get(var), na.rm = TRUE), 2),
        sd = round(sd(get(var), na.rm = TRUE), 2)
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
  results$Adjusted_P_Value <- round(p.adjust(p_values, method = "BH"), 2)
  
  return(results)
}

# Apply the function
results <- run_analysis(data_between_suicidal, variables_to_test)


## Between-person, linear regression
## Acoustic 

# Define the variables to test
variables_to_test <- c("time", "cent", "band", "roll", "rmse", "tempo", 
                       "formant0", "formant1", "formant2", "bw0", "bw1", "bw2", 
                       "mean_p", "error_p", "change_p", "mean_m", "error_m", "change_m", 
                       "mean_z", "delay_p", "mfcc_0", "mfcc_1", "mfcc_2", "mfcc_3", 
                       "mfcc_4", "mfcc_5", "mfcc_6", "mfcc_7", "mfcc_8", "mfcc_9", 
                       "mfcc_10", "mfcc_11", "mfcc_12", "mfcc_13", "mfcc_14", "mfcc_15", 
                       "mfcc_16", "mfcc_17", "mfcc_18", "mfcc_19", "mfcc_20", "mfcc_21", 
                       "mfcc_22", "mfcc_23", "mfcc_24", "mfcc_25", "mfcc_26", "mfcc_27", 
                       "mfcc_28", "mfcc_29", "mfcc_30", "mfcc_31", "mfcc_32", "mfcc_33", 
                       "mfcc_34", "mfcc_35", "mfcc_36", "mfcc_37", "mfcc_38", "mfcc_39")

# Initialize the list to store models
results <- list()

for (feature in variables_to_test) {
  formula <- as.formula(paste(feature, "~ suicidal + age + sex + AP_dose"))
  model <- lm(formula, data = data_between_suicidal)
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
for (feature in variables_to_test) {
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

# Mapping of original feature names to new names
name_changes <- c(time="time", cent="spectral_centroid", band="spectral_bandwidth", roll="spectral_rolloff",
                  rmse="rmse", tempo="tempo", formant0="formant1", formant1="formant2", formant2="formant3",
                  bw0="formant1_bw",bw1="formant2_bw",bw2="formant3_bw",mean_p="pitch_mean",error_p="pitch_error",
                  change_p="pitch_change", mean_m="magnitude_mean", error_m="magnitude_error",change_m="magnitude_change",
                  mean_z="zcr_mean", delay_p="delay_p", mfcc_0="mfcc_1", mfcc_1="mfcc_2", mfcc_2="mfcc_3",
                  mfcc_3="mfcc_4",mfcc_4="mfcc_5",mfcc_5="mfcc_6", mfcc_6="mfcc_7",mfcc_7="mfcc_8",mfcc_8="mfcc_9",mfcc_9="mfcc_10",
                  mfcc_10="mfcc_11",mfcc_11="mfcc_12",mfcc_12="mfcc_13",mfcc_13="mfcc_14",mfcc_14="mfcc_15",
                  mfcc_16="mfcc_17",mfcc_17="mfcc_18",mfcc_18="mfcc_19",mfcc_19="mfcc_20",mfcc_20="mfcc_21",mfcc_21="mfcc_22",
                  mfcc_22="mfcc_23",mfcc_23="mfcc_24",mfcc_24="mfcc_25",mfcc_25="mfcc_26",mfcc_26="mfcc_27",mfcc_27="mfcc_28",
                  mfcc_28="mfcc_29",mfcc_29="mfcc_30",mfcc_30="mfcc_31",mfcc_31="mfcc_32",mfcc_32="mfcc_33",mfcc_33="mfcc_34",
                  mfcc_34="mfcc_35",mfcc_35="mfcc_36",mfcc_36="mfcc_37",mfcc_37="mfcc_38",mfcc_38="mfcc_39",mfcc_39="mfcc_40")

# Apply name changes
coef_df$Feature <- ifelse(coef_df$Feature %in% names(name_changes), name_changes[coef_df$Feature], coef_df$Feature)

# First, apply the name changes to ensure all mappings are updated in the variable list
variables_to_test <- as.character(name_changes[variables_to_test])

# Reverse the order of variables_to_test for the plot
variables_to_test <- rev(variables_to_test)

# Update the Feature column in coef_df to be a factor with the levels in reversed order
coef_df$Feature <- factor(coef_df$Feature, levels = variables_to_test)

# Create the forest plot
library(ggplot2)

# Create the forest plot
ggplot(coef_df, aes(y = Feature, x = Coefficient)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(color = Significant)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, color = Significant), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Association between high suicide risk and acoustic features",
       x = "Beta Coefficient",
       y = "Feature",
       subtitle = "Significant after B-H adjustment in red") +
  theme_minimal() +
  theme(legend.position = "none")

# Print the results table
print(coef_df)

# Subgroup by sex 
data_female <- subset(data_between_suicidal, sex==1)
data_male <- subset(data_between_suicidal, sex==0)


# Female:
# Define the variables to test
variables_to_test <- c("time", "cent", "band", "roll", "rmse", "tempo", 
                       "formant0", "formant1", "formant2", "bw0", "bw1", "bw2", 
                       "mean_p", "error_p", "change_p", "mean_m", "error_m", "change_m", 
                       "mean_z", "delay_p", "mfcc_0", "mfcc_1", "mfcc_2", "mfcc_3", 
                       "mfcc_4", "mfcc_5", "mfcc_6", "mfcc_7", "mfcc_8", "mfcc_9", 
                       "mfcc_10", "mfcc_11", "mfcc_12", "mfcc_13", "mfcc_14", "mfcc_15", 
                       "mfcc_16", "mfcc_17", "mfcc_18", "mfcc_19", "mfcc_20", "mfcc_21", 
                       "mfcc_22", "mfcc_23", "mfcc_24", "mfcc_25", "mfcc_26", "mfcc_27", 
                       "mfcc_28", "mfcc_29", "mfcc_30", "mfcc_31", "mfcc_32", "mfcc_33", 
                       "mfcc_34", "mfcc_35", "mfcc_36", "mfcc_37", "mfcc_38", "mfcc_39")

# Initialize the list to store models
results <- list()

for (feature in variables_to_test) {
  formula <- as.formula(paste(feature, "~ suicidal + age + AP_dose"))
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
for (feature in variables_to_test) {
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

# Mapping of original feature names to new names
name_changes <- c(time="time", cent="spectral_centroid", band="spectral_bandwidth", roll="spectral_rolloff",
                  rmse="rmse", tempo="tempo", formant0="formant1", formant1="formant2", formant2="formant3",
                  bw0="formant1_bw",bw1="formant2_bw",bw2="formant3_bw",mean_p="pitch_mean",error_p="pitch_error",
                  change_p="pitch_change", mean_m="magnitude_mean", error_m="magnitude_error",change_m="magnitude_change",
                  mean_z="zcr_mean", delay_p="delay_p", mfcc_0="mfcc_1", mfcc_1="mfcc_2", mfcc_2="mfcc_3",
                  mfcc_3="mfcc_4",mfcc_4="mfcc_5",mfcc_5="mfcc_6", mfcc_6="mfcc_7",mfcc_7="mfcc_8",mfcc_8="mfcc_9",mfcc_9="mfcc_10",
                  mfcc_10="mfcc_11",mfcc_11="mfcc_12",mfcc_12="mfcc_13",mfcc_13="mfcc_14",mfcc_14="mfcc_15",
                  mfcc_16="mfcc_17",mfcc_17="mfcc_18",mfcc_18="mfcc_19",mfcc_19="mfcc_20",mfcc_20="mfcc_21",mfcc_21="mfcc_22",
                  mfcc_22="mfcc_23",mfcc_23="mfcc_24",mfcc_24="mfcc_25",mfcc_25="mfcc_26",mfcc_26="mfcc_27",mfcc_27="mfcc_28",
                  mfcc_28="mfcc_29",mfcc_29="mfcc_30",mfcc_30="mfcc_31",mfcc_31="mfcc_32",mfcc_32="mfcc_33",mfcc_33="mfcc_34",
                  mfcc_34="mfcc_35",mfcc_35="mfcc_36",mfcc_36="mfcc_37",mfcc_37="mfcc_38",mfcc_38="mfcc_39",mfcc_39="mfcc_40")

# Apply name changes
coef_df$Feature <- ifelse(coef_df$Feature %in% names(name_changes), name_changes[coef_df$Feature], coef_df$Feature)

# First, apply the name changes to ensure all mappings are updated in the variable list
variables_to_test <- as.character(name_changes[variables_to_test])

# Reverse the order of variables_to_test for the plot
variables_to_test <- rev(variables_to_test)

# Update the Feature column in coef_df to be a factor with the levels in reversed order
coef_df$Feature <- factor(coef_df$Feature, levels = variables_to_test)

# Create the forest plot
library(ggplot2)

# Create the forest plot
ggplot(coef_df, aes(y = Feature, x = Coefficient)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(color = Significant)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, color = Significant), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Association between high suicide risk and acoustic features (Female)",
       x = "Beta Coefficient",
       y = "Feature",
       subtitle = "Significant after B-H adjustment in red") +
  theme_minimal() +
  theme(legend.position = "none")

# Print the results table
print(coef_df)


# Male:
# Define the variables to test
variables_to_test <- c("time", "cent", "band", "roll", "rmse", "tempo", 
                       "formant0", "formant1", "formant2", "bw0", "bw1", "bw2", 
                       "mean_p", "error_p", "change_p", "mean_m", "error_m", "change_m", 
                       "mean_z", "delay_p", "mfcc_0", "mfcc_1", "mfcc_2", "mfcc_3", 
                       "mfcc_4", "mfcc_5", "mfcc_6", "mfcc_7", "mfcc_8", "mfcc_9", 
                       "mfcc_10", "mfcc_11", "mfcc_12", "mfcc_13", "mfcc_14", "mfcc_15", 
                       "mfcc_16", "mfcc_17", "mfcc_18", "mfcc_19", "mfcc_20", "mfcc_21", 
                       "mfcc_22", "mfcc_23", "mfcc_24", "mfcc_25", "mfcc_26", "mfcc_27", 
                       "mfcc_28", "mfcc_29", "mfcc_30", "mfcc_31", "mfcc_32", "mfcc_33", 
                       "mfcc_34", "mfcc_35", "mfcc_36", "mfcc_37", "mfcc_38", "mfcc_39")

# Initialize the list to store models
results <- list()

for (feature in variables_to_test) {
  formula <- as.formula(paste(feature, "~ suicidal + age + AP_dose"))
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
for (feature in variables_to_test) {
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

# Mapping of original feature names to new names
name_changes <- c(time="time", cent="spectral_centroid", band="spectral_bandwidth", roll="spectral_rolloff",
                  rmse="rmse", tempo="tempo", formant0="formant1", formant1="formant2", formant2="formant3",
                  bw0="formant1_bw",bw1="formant2_bw",bw2="formant3_bw",mean_p="pitch_mean",error_p="pitch_error",
                  change_p="pitch_change", mean_m="magnitude_mean", error_m="magnitude_error",change_m="magnitude_change",
                  mean_z="zcr_mean", delay_p="delay_p", mfcc_0="mfcc_1", mfcc_1="mfcc_2", mfcc_2="mfcc_3",
                  mfcc_3="mfcc_4",mfcc_4="mfcc_5",mfcc_5="mfcc_6", mfcc_6="mfcc_7",mfcc_7="mfcc_8",mfcc_8="mfcc_9",mfcc_9="mfcc_10",
                  mfcc_10="mfcc_11",mfcc_11="mfcc_12",mfcc_12="mfcc_13",mfcc_13="mfcc_14",mfcc_14="mfcc_15",
                  mfcc_16="mfcc_17",mfcc_17="mfcc_18",mfcc_18="mfcc_19",mfcc_19="mfcc_20",mfcc_20="mfcc_21",mfcc_21="mfcc_22",
                  mfcc_22="mfcc_23",mfcc_23="mfcc_24",mfcc_24="mfcc_25",mfcc_25="mfcc_26",mfcc_26="mfcc_27",mfcc_27="mfcc_28",
                  mfcc_28="mfcc_29",mfcc_29="mfcc_30",mfcc_30="mfcc_31",mfcc_31="mfcc_32",mfcc_32="mfcc_33",mfcc_33="mfcc_34",
                  mfcc_34="mfcc_35",mfcc_35="mfcc_36",mfcc_36="mfcc_37",mfcc_37="mfcc_38",mfcc_38="mfcc_39",mfcc_39="mfcc_40")

# Apply name changes
coef_df$Feature <- ifelse(coef_df$Feature %in% names(name_changes), name_changes[coef_df$Feature], coef_df$Feature)

# First, apply the name changes to ensure all mappings are updated in the variable list
variables_to_test <- as.character(name_changes[variables_to_test])

# Reverse the order of variables_to_test for the plot
variables_to_test <- rev(variables_to_test)

# Update the Feature column in coef_df to be a factor with the levels in reversed order
coef_df$Feature <- factor(coef_df$Feature, levels = variables_to_test)

# Create the forest plot
library(ggplot2)

# Create the forest plot
ggplot(coef_df, aes(y = Feature, x = Coefficient)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(color = Significant)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, color = Significant), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Association between high suicide risk and acoustic features (male)",
       x = "Beta Coefficient",
       y = "Feature",
       subtitle = "Significant after B-H adjustment in red") +
  theme_minimal() +
  theme(legend.position = "none")

# Print the results table
print(coef_df)

# Save the results table as a CSV file
write.csv(coef_df, "C:/Users/msy71/OneDrive/¹ÙÅÁ È­¸é/PRISM-V/prism_datacleaned/npj_cross_regression_results(clean)_male.csv", row.names = FALSE)



## Within-person, t-test 
## Acoustic

variables_to_test <- c("time", "cent", "band","roll","rmse","tempo","formant0","formant1","formant2","bw0","bw1","bw2","mean_p","error_p","change_p",
                       "mean_m","error_m","change_m","mean_z","delay_p","mfcc_0","mfcc_1","mfcc_2","mfcc_3","mfcc_4","mfcc_5","mfcc_6","mfcc_7",
                       "mfcc_8","mfcc_9","mfcc_10","mfcc_11","mfcc_12","mfcc_13","mfcc_14","mfcc_15","mfcc_16","mfcc_17","mfcc_18","mfcc_19",
                       "mfcc_20","mfcc_21","mfcc_22","mfcc_23","mfcc_24","mfcc_25","mfcc_26","mfcc_27","mfcc_28","mfcc_29","mfcc_30","mfcc_31",
                       "mfcc_32","mfcc_33","mfcc_34","mfcc_35","mfcc_36","mfcc_37","mfcc_38","mfcc_39")  

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
        mean = round(mean(get(var), na.rm = TRUE), 2),
        sd = round(sd(get(var), na.rm = TRUE), 2)
      )
    
    mean_sd_nonsuicidal <- data %>%
      filter(label == 0) %>%
      summarise(
        mean = round(mean(get(var), na.rm = TRUE), 2),
        sd = round(sd(get(var), na.rm = TRUE), 2)
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
  results$Adjusted_P_Value <- round(p.adjust(p_values, method = "BH"), 2)
  
  return(results)
}

results <- run_analysis(data_within_all, variables_to_test)

## Linear regression
## Acoustic 

data_within_all <- data_within_all %>%
  mutate(
    AP_change = case_when(
      !is.na(AP_dose_2) & !is.na(AP_dose_0) ~ AP_dose_2 - AP_dose_0,
      !is.na(AP_dose_4) & !is.na(AP_dose_2) ~ AP_dose_4 - AP_dose_2,
      TRUE ~ NA_real_  # Assign NA where conditions do not meet
    )
  )


# Define the variables to test
variables_to_test <- c("time", "cent", "band", "roll", "rmse", "tempo", 
                       "formant0", "formant1", "formant2", "bw0", "bw1", "bw2", 
                       "mean_p", "error_p", "change_p", "mean_m", "error_m", "change_m", 
                       "mean_z", "delay_p", "mfcc_0", "mfcc_1", "mfcc_2", "mfcc_3", 
                       "mfcc_4", "mfcc_5", "mfcc_6", "mfcc_7", "mfcc_8", "mfcc_9", 
                       "mfcc_10", "mfcc_11", "mfcc_12", "mfcc_13", "mfcc_14", "mfcc_15", 
                       "mfcc_16", "mfcc_17", "mfcc_18", "mfcc_19", "mfcc_20", "mfcc_21", 
                       "mfcc_22", "mfcc_23", "mfcc_24", "mfcc_25", "mfcc_26", "mfcc_27", 
                       "mfcc_28", "mfcc_29", "mfcc_30", "mfcc_31", "mfcc_32", "mfcc_33", 
                       "mfcc_34", "mfcc_35", "mfcc_36", "mfcc_37", "mfcc_38", "mfcc_39")

# Initialize the list to store models
results <- list()

for (feature in variables_to_test) {
  formula <- as.formula(paste(feature, "~ label + age + sex + AP_change"))
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
for (feature in variables_to_test) {
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

# Mapping of original feature names to new names
name_changes <- c(time="time", cent="spectral_centroid", band="spectral_bandwidth", roll="spectral_rolloff",
                  rmse="rmse", tempo="tempo", formant0="formant1", formant1="formant2", formant2="formant3",
                  bw0="formant1_bw",bw1="formant2_bw",bw2="formant3_bw",mean_p="pitch_mean",error_p="pitch_error",
                  change_p="pitch_change", mean_m="magnitude_mean", error_m="magnitude_error",change_m="magnitude_change",
                  mean_z="zcr_mean", delay_p="delay_p", mfcc_0="mfcc_1", mfcc_1="mfcc_2", mfcc_2="mfcc_3",
                  mfcc_3="mfcc_4",mfcc_4="mfcc_5",mfcc_5="mfcc_6", mfcc_6="mfcc_7",mfcc_7="mfcc_8",mfcc_8="mfcc_9",mfcc_9="mfcc_10",
                  mfcc_10="mfcc_11",mfcc_11="mfcc_12",mfcc_12="mfcc_13",mfcc_13="mfcc_14",mfcc_14="mfcc_15",
                  mfcc_16="mfcc_17",mfcc_17="mfcc_18",mfcc_18="mfcc_19",mfcc_19="mfcc_20",mfcc_20="mfcc_21",mfcc_21="mfcc_22",
                  mfcc_22="mfcc_23",mfcc_23="mfcc_24",mfcc_24="mfcc_25",mfcc_25="mfcc_26",mfcc_26="mfcc_27",mfcc_27="mfcc_28",
                  mfcc_28="mfcc_29",mfcc_29="mfcc_30",mfcc_30="mfcc_31",mfcc_31="mfcc_32",mfcc_32="mfcc_33",mfcc_33="mfcc_34",
                  mfcc_34="mfcc_35",mfcc_35="mfcc_36",mfcc_36="mfcc_37",mfcc_37="mfcc_38",mfcc_38="mfcc_39",mfcc_39="mfcc_40")

# Apply name changes
coef_df$Feature <- ifelse(coef_df$Feature %in% names(name_changes), name_changes[coef_df$Feature], coef_df$Feature)

# First, apply the name changes to ensure all mappings are updated in the variable list
variables_to_test <- as.character(name_changes[variables_to_test])

# Reverse the order of variables_to_test for the plot
variables_to_test <- rev(variables_to_test)

# Update the Feature column in coef_df to be a factor with the levels in reversed order
coef_df$Feature <- factor(coef_df$Feature, levels = variables_to_test)

# Create the forest plot
library(ggplot2)

# Create the forest plot
ggplot(coef_df, aes(y = Feature, x = Coefficient)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(color = Significant)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, color = Significant), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Association between increase in suicide risk and change in acoustic features",
       x = "Beta Coefficient",
       y = "Feature",
       subtitle = "Significant after B-H adjustment in red") +
  theme_minimal() +
  theme(legend.position = "none")

# Print the results table
print(coef_df)

# Subgroup by sex 
data_female <- subset(data_within_all, sex==1)
data_male <- subset(data_within_all, sex==0)


# Female:

# Define the variables to test
variables_to_test <- c("time", "cent", "band", "roll", "rmse", "tempo", 
                       "formant0", "formant1", "formant2", "bw0", "bw1", "bw2", 
                       "mean_p", "error_p", "change_p", "mean_m", "error_m", "change_m", 
                       "mean_z", "delay_p", "mfcc_0", "mfcc_1", "mfcc_2", "mfcc_3", 
                       "mfcc_4", "mfcc_5", "mfcc_6", "mfcc_7", "mfcc_8", "mfcc_9", 
                       "mfcc_10", "mfcc_11", "mfcc_12", "mfcc_13", "mfcc_14", "mfcc_15", 
                       "mfcc_16", "mfcc_17", "mfcc_18", "mfcc_19", "mfcc_20", "mfcc_21", 
                       "mfcc_22", "mfcc_23", "mfcc_24", "mfcc_25", "mfcc_26", "mfcc_27", 
                       "mfcc_28", "mfcc_29", "mfcc_30", "mfcc_31", "mfcc_32", "mfcc_33", 
                       "mfcc_34", "mfcc_35", "mfcc_36", "mfcc_37", "mfcc_38", "mfcc_39")

# Initialize the list to store models
results <- list()

for (feature in variables_to_test) {
  formula <- as.formula(paste(feature, "~ label + age + AP_change"))
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
for (feature in variables_to_test) {
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

# Mapping of original feature names to new names
name_changes <- c(time="time", cent="spectral_centroid", band="spectral_bandwidth", roll="spectral_rolloff",
                  rmse="rmse", tempo="tempo", formant0="formant1", formant1="formant2", formant2="formant3",
                  bw0="formant1_bw",bw1="formant2_bw",bw2="formant3_bw",mean_p="pitch_mean",error_p="pitch_error",
                  change_p="pitch_change", mean_m="magnitude_mean", error_m="magnitude_error",change_m="magnitude_change",
                  mean_z="zcr_mean", delay_p="delay_p", mfcc_0="mfcc_1", mfcc_1="mfcc_2", mfcc_2="mfcc_3",
                  mfcc_3="mfcc_4",mfcc_4="mfcc_5",mfcc_5="mfcc_6", mfcc_6="mfcc_7",mfcc_7="mfcc_8",mfcc_8="mfcc_9",mfcc_9="mfcc_10",
                  mfcc_10="mfcc_11",mfcc_11="mfcc_12",mfcc_12="mfcc_13",mfcc_13="mfcc_14",mfcc_14="mfcc_15",
                  mfcc_16="mfcc_17",mfcc_17="mfcc_18",mfcc_18="mfcc_19",mfcc_19="mfcc_20",mfcc_20="mfcc_21",mfcc_21="mfcc_22",
                  mfcc_22="mfcc_23",mfcc_23="mfcc_24",mfcc_24="mfcc_25",mfcc_25="mfcc_26",mfcc_26="mfcc_27",mfcc_27="mfcc_28",
                  mfcc_28="mfcc_29",mfcc_29="mfcc_30",mfcc_30="mfcc_31",mfcc_31="mfcc_32",mfcc_32="mfcc_33",mfcc_33="mfcc_34",
                  mfcc_34="mfcc_35",mfcc_35="mfcc_36",mfcc_36="mfcc_37",mfcc_37="mfcc_38",mfcc_38="mfcc_39",mfcc_39="mfcc_40")

# Apply name changes
coef_df$Feature <- ifelse(coef_df$Feature %in% names(name_changes), name_changes[coef_df$Feature], coef_df$Feature)

# First, apply the name changes to ensure all mappings are updated in the variable list
variables_to_test <- as.character(name_changes[variables_to_test])

# Reverse the order of variables_to_test for the plot
variables_to_test <- rev(variables_to_test)

# Update the Feature column in coef_df to be a factor with the levels in reversed order
coef_df$Feature <- factor(coef_df$Feature, levels = variables_to_test)

# Create the forest plot
library(ggplot2)

# Create the forest plot
ggplot(coef_df, aes(y = Feature, x = Coefficient)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(color = Significant)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, color = Significant), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Association between high suicide risk and acoustic features",
       x = "Beta Coefficient",
       y = "Feature",
       subtitle = "Significant after B-H adjustment in red") +
  theme_minimal() +
  theme(legend.position = "none")

# Print the results table
print(coef_df)

# Male:

# Define the variables to test
variables_to_test <- c("time", "cent", "band", "roll", "rmse", "tempo", 
                       "formant0", "formant1", "formant2", "bw0", "bw1", "bw2", 
                       "mean_p", "error_p", "change_p", "mean_m", "error_m", "change_m", 
                       "mean_z", "delay_p", "mfcc_0", "mfcc_1", "mfcc_2", "mfcc_3", 
                       "mfcc_4", "mfcc_5", "mfcc_6", "mfcc_7", "mfcc_8", "mfcc_9", 
                       "mfcc_10", "mfcc_11", "mfcc_12", "mfcc_13", "mfcc_14", "mfcc_15", 
                       "mfcc_16", "mfcc_17", "mfcc_18", "mfcc_19", "mfcc_20", "mfcc_21", 
                       "mfcc_22", "mfcc_23", "mfcc_24", "mfcc_25", "mfcc_26", "mfcc_27", 
                       "mfcc_28", "mfcc_29", "mfcc_30", "mfcc_31", "mfcc_32", "mfcc_33", 
                       "mfcc_34", "mfcc_35", "mfcc_36", "mfcc_37", "mfcc_38", "mfcc_39")

# Initialize the list to store models
results <- list()

for (feature in variables_to_test) {
  formula <- as.formula(paste(feature, "~ label + age + AP_change"))
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
for (feature in variables_to_test) {
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

# Mapping of original feature names to new names
name_changes <- c(time="time", cent="spectral_centroid", band="spectral_bandwidth", roll="spectral_rolloff",
                  rmse="rmse", tempo="tempo", formant0="formant1", formant1="formant2", formant2="formant3",
                  bw0="formant1_bw",bw1="formant2_bw",bw2="formant3_bw",mean_p="pitch_mean",error_p="pitch_error",
                  change_p="pitch_change", mean_m="magnitude_mean", error_m="magnitude_error",change_m="magnitude_change",
                  mean_z="zcr_mean", delay_p="delay_p", mfcc_0="mfcc_1", mfcc_1="mfcc_2", mfcc_2="mfcc_3",
                  mfcc_3="mfcc_4",mfcc_4="mfcc_5",mfcc_5="mfcc_6", mfcc_6="mfcc_7",mfcc_7="mfcc_8",mfcc_8="mfcc_9",mfcc_9="mfcc_10",
                  mfcc_10="mfcc_11",mfcc_11="mfcc_12",mfcc_12="mfcc_13",mfcc_13="mfcc_14",mfcc_14="mfcc_15",
                  mfcc_16="mfcc_17",mfcc_17="mfcc_18",mfcc_18="mfcc_19",mfcc_19="mfcc_20",mfcc_20="mfcc_21",mfcc_21="mfcc_22",
                  mfcc_22="mfcc_23",mfcc_23="mfcc_24",mfcc_24="mfcc_25",mfcc_25="mfcc_26",mfcc_26="mfcc_27",mfcc_27="mfcc_28",
                  mfcc_28="mfcc_29",mfcc_29="mfcc_30",mfcc_30="mfcc_31",mfcc_31="mfcc_32",mfcc_32="mfcc_33",mfcc_33="mfcc_34",
                  mfcc_34="mfcc_35",mfcc_35="mfcc_36",mfcc_36="mfcc_37",mfcc_37="mfcc_38",mfcc_38="mfcc_39",mfcc_39="mfcc_40")

# Apply name changes
coef_df$Feature <- ifelse(coef_df$Feature %in% names(name_changes), name_changes[coef_df$Feature], coef_df$Feature)

# First, apply the name changes to ensure all mappings are updated in the variable list
variables_to_test <- as.character(name_changes[variables_to_test])

# Reverse the order of variables_to_test for the plot
variables_to_test <- rev(variables_to_test)

# Update the Feature column in coef_df to be a factor with the levels in reversed order
coef_df$Feature <- factor(coef_df$Feature, levels = variables_to_test)

# Create the forest plot
library(ggplot2)

# Create the forest plot
ggplot(coef_df, aes(y = Feature, x = Coefficient)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(color = Significant)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, color = Significant), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Association between high suicide risk and acoustic features",
       x = "Beta Coefficient",
       y = "Feature",
       subtitle = "Significant after B-H adjustment in red") +
  theme_minimal() +
  theme(legend.position = "none")

# Print the results table
print(coef_df)
