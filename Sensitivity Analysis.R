# Sensitivity Analysis for Afghanistan Measles Model

run_sensitivity <- function(param_name, param_value) {
  # My baseline parameters
  new_params <- parameters
  
  # Handle coverage parameters by converting to rates
  if(param_name == "cov_MCV1") {
    new_params$rate1 <- -log(1 - param_value) / 365
  } else if(param_name == "cov_MCV2") {
    new_params$rate2 <- -log(1 - param_value) / 365
  } else if(param_name == "sia_cov") {
    new_params$sia_rate <- -log(1 - param_value) / 30
  } else {
    # For other parameters, direct assignment
    new_params[[param_name]] <- param_value
  }
  
  # Run the model
  results <- ode(y = Y, times = times, func = Afghanistan_model, parms = new_params, method = "lsoda")
  
  return(results)
}

# Parameter: gamma
gamma_values <- round(seq(1/30, 1/10, length.out = 3), 3)
gamma_values
# [1] 0.033 0.050 0.100

# Parameter: p
p_values <- round(seq(0.02, 0.08, length.out = 3), 3)
p_values
# [1] 0.020 0.050 0.080

# Parameter: gamma_h
gamma_h_values <- round(seq(1/30, 1/10, length.out = 3), 3)
gamma_h_values
# [1] 0.033 0.050 0.100

# Parameter: ph
ph_values <- round(seq(0.05, 0.20, length.out = 3), 3)
ph_values
# [1] 0.050 0.125 0.200

# Parameter: cov_MCV1
cov_MCV1_values <- round(seq(0.5, 0.95, length.out = 3), 3)
cov_MCV1_values
# [1] 0.500 0.725 0.950

# Parameter: cov_MCV2
cov_MCV2_values <- round(seq(0.4, 0.9, length.out = 3), 3)
cov_MCV2_values
# [1] 0.400 0.650 0.900

# Parameter: sia_cov
sia_cov_values <- round(seq(0.7, 0.95, length.out = 3), 3)
sia_cov_values
# [1] 0.700 0.825 0.950

# Parameter: sia_cycle
sia_cycle_values <- round(seq(365, 4*365, length.out = 3), 0)
sia_cycle_values
# [1]  365  1095 1460
                           

# Empty lists to store results
gamma_results <- list()
p_results <- list()
gamma_h_results <- list()
ph_results <- list()
cov_MCV1_results <- list()
cov_MCV2_results <- list()
sia_cov_results <- list()
sia_cycle_results <- list()

# Run sensitivity analysis for each parameter
# Parameter: gamma
for (i in 1:length(gamma_values)) {
  gamma_results[[i]] <- run_sensitivity("gamma", gamma_values[i])
}
# Parameter: p
for (i in 1:length(p_values)) {
  p_results[[i]] <- run_sensitivity("p", p_values[i])
}
# Parameter: gamma_h
for (i in 1:length(gamma_h_values)) {
  gamma_h_results[[i]] <- run_sensitivity("gamma_h", gamma_h_values[i])
}
# Parameter: ph
for (i in 1:length(ph_values)) {
  ph_results[[i]] <- run_sensitivity("ph", ph_values[i])
}

# Parameter: cov_MCV1
for (i in 1:length(cov_MCV1_values)) {
  cov_MCV1_results[[i]] <- run_sensitivity("cov_MCV1", cov_MCV1_values[i])
}

# Parameter: cov_MCV2
for (i in 1:length(cov_MCV2_values)) {
  cov_MCV2_results[[i]] <- run_sensitivity("cov_MCV2", cov_MCV2_values[i])
}

# Parameter: sia_cov
for (i in 1:length(sia_cov_values)) {
  sia_cov_results[[i]] <- run_sensitivity("sia_cov", sia_cov_values[i])
}

# Parameter: sia_cycle
for (i in 1:length(sia_cycle_values)) {
  sia_cycle_results[[i]] <- run_sensitivity("sia_cycle", sia_cycle_values[i])
}

# Process results for plotting

process_sensitivity_results <- function(results_list, param_values, param_name) {
  # Create an empty dataframe to store processed results
  processed_results <- data.frame()
  # Process each set of results
  for (i in 1:length(results_list)) {
    df <- as.data.frame(results_list[[i]])                                                  # Convert to dataframe
    
    # Calculate infectious and immune based on your model structure
    # Infectious compartments: I, V1I, V2I, SIAI (columns 2 to n_age+1, etc.)
    I_cols <- 2:(n_age+1)
    V1I_cols <- (4*n_age+2):(5*n_age+1)
    V2I_cols <- (7*n_age+2):(8*n_age+1)
    SIAI_cols <- (10*n_age+2):(11*n_age+1)
    
    df$Totalcases <- rowSums(df[, I_cols]) + rowSums(df[, V1I_cols]) + 
      rowSums(df[, V2I_cols]) + rowSums(df[, SIAI_cols])
    
    # Immune compartments: R, V1R, V2R, SIAR
    R_cols <- (2*n_age+2):(3*n_age+1)
    V1R_cols <- (5*n_age+2):(6*n_age+1)
    V2R_cols <- (8*n_age+2):(9*n_age+1)
    SIAR_cols <- (11*n_age+2):(12*n_age+1)
    
    df$Totalimmune <- rowSums(df[, R_cols]) + rowSums(df[, V1R_cols]) + 
      rowSums(df[, V2R_cols]) + rowSums(df[, SIAR_cols])
    
    # Aggregate to monthly data for easier visualization
    df$Month <- ceiling(df$time / 30.4)
    monthly_data <- aggregate(cbind(Totalcases, Totalimmune) ~ Month, data = df, FUN = sum)
    # Parameter value information
    monthly_data$ParamValue <- param_values[i]
    # Processed results
    processed_results <- rbind(processed_results, monthly_data)
  }
  # Parameter name
  processed_results$Parameter <- param_name
  return(processed_results)
}

# Process results for each parameter

gamma_processed <- process_sensitivity_results(gamma_results, gamma_values, "gamma")
p_processed <- process_sensitivity_results(p_results, p_values, "p")
gamma_h_processed <- process_sensitivity_results(gamma_h_results, gamma_h_values, "gamma_h")
ph_processed <- process_sensitivity_results(ph_results, ph_values, "ph")
cov_MCV1_processed <- process_sensitivity_results(cov_MCV1_results, cov_MCV1_values, "cov_MCV1")
cov_MCV2_processed <- process_sensitivity_results(cov_MCV2_results, cov_MCV2_values, "cov_MCV2")
sia_cov_processed <- process_sensitivity_results(sia_cov_results, sia_cov_values, "sia_cov")
sia_cycle_processed <- process_sensitivity_results(sia_cycle_results, sia_cycle_values, "sia_cycle")

# Combine all processed results
all_sensitivity_results <- rbind(gamma_processed, p_processed, gamma_h_processed, ph_processed, cov_MCV1_processed, cov_MCV2_processed, sia_cov_processed, sia_cycle_processed)

# More informative labels for the parameters
all_sensitivity_results$ParameterLabel <- factor(
  all_sensitivity_results$Parameter,
  levels = c("gamma", "p", "gamma_h", "ph", "cov_MCV1", "cov_MCV2", "sia_cov", "sia_cycle"),
  labels = c("gamma", "p", "gamma_h", 
             "ph", "MCV1_cov", "MCV2_cov", 
             "SIA_cov", "SIA_cycle (days)")
)
#
#labels = c("Recovery Rate (gamma)", "Transmission Prob (p)", "Hospital Recovery (gamma_h)", 
#          "Hospitalization Rate (ph)", "MCV1 Coverage", "MCV2 Coverage", 
#          "SIA Coverage", "SIA Cycle (days)")


# Format parameter values (for display)
all_sensitivity_results$ParamValueLabel <- sprintf("%.3f", all_sensitivity_results$ParamValue)

# Filter to only include data after some burn-in period 
filtered_results <- subset(all_sensitivity_results, Month > 12)

# Plot total incidence sensitivity
p7 <- ggplot(filtered_results, aes(x = Month, y = Totalcases, color = ParamValueLabel, group = ParamValue)) +
  geom_line(size = 1) +
  facet_wrap(~ ParameterLabel, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(title = "Sensitivity Analysis: Monthly Infectious Cases",
       x = "Month",
       y = "Monthly Infectious Cases",
       color = "Parameter Value") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "bottom")

print(p7)

# Plot total immune sensitivity
p8 <- ggplot(filtered_results, aes(x = Month, y = Totalimmune, color = ParamValueLabel, group = ParamValue)) +
  geom_line(size = 1) +
  facet_wrap(~ ParameterLabel, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(title = "Sensitivity Analysis: Total Immune Population",
       x = "Month",
       y = "Total Immune Population",
       color = "Parameter Value") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "bottom")

print(p8)

# Calculate relative impact of each parameter (percent change from baseline)
process_sensitivity_relative <- function(results_list, param_values, param_name) {
  # Get the middle parameter value as "baseline"
  baseline_idx <- ceiling(length(param_values)/2)
  baseline_results <- results_list[[baseline_idx]]
  # Convert baseline to data frame and calculate baseline metrics
  baseline_df <- as.data.frame(baseline_results)
  
  # Calculate infectious cases for baseline
  I_cols <- 2:(n_age+1)
  V1I_cols <- (4*n_age+2):(5*n_age+1)
  V2I_cols <- (7*n_age+2):(8*n_age+1)
  SIAI_cols <- (10*n_age+2):(11*n_age+1)
  
  baseline_df$Totalcases <- rowSums(baseline_df[, I_cols]) + rowSums(baseline_df[, V1I_cols]) + 
    rowSums(baseline_df[, V2I_cols]) + rowSums(baseline_df[, SIAI_cols])
  
  baseline_df$Year <- baseline_df$time/365
  baseline_df$YearInt <- floor(baseline_df$Year)
  # Aggregate baseline to yearly
  baseline_yearly <- aggregate(Totalcases ~ YearInt, data = baseline_df, FUN = sum)
  names(baseline_yearly) <- c("Year", "BaselineInc")
  # Create empty data frame for results
  relative_results <- data.frame()
  # Process each parameter value
  for (i in 1:length(results_list)) {
    df <- as.data.frame(results_list[[i]])
    
    df$Totalcases <- rowSums(df[, I_cols]) + rowSums(df[, V1I_cols]) + 
      rowSums(df[, V2I_cols]) + rowSums(df[, SIAI_cols])
    
    df$Year <- df$time/365
    df$YearInt <- floor(df$Year)
    # Aggregate to yearly
    yearly <- aggregate(Totalcases ~ YearInt, data = df, FUN = sum)
    names(yearly) <- c("Year", "Incidence")
    # Merge with baseline and calculate percent change
    merged <- merge(yearly, baseline_yearly, by = "Year")
    merged$PercentChange <- (merged$Incidence - merged$BaselineInc) / merged$BaselineInc * 100
    merged$ParamValue <- param_values[i]
    merged$Parameter <- param_name
    # Add to results
    relative_results <- rbind(relative_results, merged)
  }
  return(relative_results)
}

# Process each parameter for relative changes
gamma_relative <- process_sensitivity_relative(gamma_results, gamma_values, "Recovery Rate (gamma)")
p_relative <- process_sensitivity_relative(p_results, p_values, "Transmission Prob (p)")
gamma_h_relative <- process_sensitivity_relative(gamma_h_results, gamma_h_values, "Hospital Recovery (gamma_h)")
ph_relative <- process_sensitivity_relative(ph_results, ph_values, "Hospitalization Rate (ph)")
cov_MCV1_relative <- process_sensitivity_relative(cov_MCV1_results, cov_MCV1_values, "MCV1 Coverage")
cov_MCV2_relative <- process_sensitivity_relative(cov_MCV2_results, cov_MCV2_values, "MCV2 Coverage")

# Combine all relative results
all_relative <- rbind(gamma_relative, p_relative, gamma_h_relative, ph_relative, cov_MCV1_relative, cov_MCV2_relative)

# Filter to recent years 
recent_relative <- subset(all_relative, Year >= 1)

# Format parameter values
recent_relative$ParamValueLabel <- sprintf("%.3f", recent_relative$ParamValue)

# Plot percent change heatmap
p9 <- ggplot(recent_relative, aes(x = Year, y = ParamValue, fill = PercentChange)) +
  geom_tile() +
  facet_wrap(~ Parameter, scales = "free_y") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Sensitivity Analysis: Percent Change in Measles Incidence",
       x = "Year",
       y = "Parameter Value",
       fill = "% Change\nfrom Baseline") +
  theme(legend.position = "right")

print(p9)

# Plot percent change line plot
p10 <- ggplot(recent_relative, aes(x = Year, y = PercentChange, color = as.factor(ParamValue), group = ParamValue)) +
  geom_line(size = 1) +
  facet_wrap(~ Parameter, scales = "free_y") +
  theme_minimal() +
  labs(title = "Sensitivity Analysis: Impact on Measles Incidence",
       x = "Year",
       y = "Percent change from baseline (%)",
       color = "Parameter Value") +
  theme(legend.position = "bottom")

print(p10)

# Box plot to summarize parameter sensitivity
p11 <- ggplot(recent_relative, aes(x = as.factor(ParamValue), y = PercentChange)) +
  geom_boxplot(aes(fill = as.factor(ParamValue))) +
  facet_wrap(~ Parameter, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Parameter Effects on Measles Incidence",
       x = "Parameter Value",
       y = "Percent Change from Baseline (%)",
       fill = "Parameter Value") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p11)
# Save the plot
ggsave("sensitivity_analysis_measles_Afghanistan.png",plot=p11,width=12, height=10,dpi=300)
