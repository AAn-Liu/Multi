
########################################################################################################
############################################# Hazard Model #############################################
########################################################################################################

library(dplyr)
library(stringr)
library(tidyr)

Flood_Insurance_Loss <- read.csv("D:/MRes_Project_desktop/Multi_/Data/Vulnerability/FimaNfipClaims.csv")
# Data preparation
######### 


# View the first few rows of the dataset
head(Flood_Insurance_Loss)


# Clean column names
Flood_Insurance_Loss <- Flood_Insurance_Loss %>%
  rename_all(~ str_trim(.)) %>%
  rename_all(~ str_replace_all(., "\\s+", "_"))

# Check the cleaned column names
colnames(Flood_Insurance_Loss)

# View the first few rows of the dataset
head(Flood_Insurance_Loss)

# Summary of the dataset
summary(Flood_Insurance_Loss)


# Filter the dataset for rain-related damages and state TX
filtered_rain_only <- Flood_Insurance_Loss #%>% 
  #filter(causeOfDamage == "4" ) # & state == "TX"

# Select the waterDepth and dateOfLoss columns explicitly using dplyr::select
waterDepth_data <- filtered_rain_only %>%
  dplyr::select(dateOfLoss, waterDepth) %>%
  filter(!is.na(waterDepth))

# View the first few rows of the selected data
head(waterDepth_data)

# Summary of the selected data
summary(waterDepth_data)

# Convert waterDepth to numeric if it is not already
waterDepth_data$waterDepth <- as.numeric(waterDepth_data$waterDepth)

# Check for any non-numeric values (if any conversion warnings were issued)
sum(is.na(waterDepth_data$waterDepth))

# Convert dateOfLoss to Date type
waterDepth_data$dateOfLoss <- as.Date(waterDepth_data$dateOfLoss, format="%Y-%m-%d")

# Handle potential outliers 
# removing extreme outliers using IQR method
Q1 <- quantile(waterDepth_data$waterDepth, 0.25)
Q3 <- quantile(waterDepth_data$waterDepth, 0.75)
IQR <- Q3 - Q1

# Define the lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter out the outliers
waterDepth_data <- waterDepth_data %>%
  filter(waterDepth >= lower_bound & waterDepth <= upper_bound)

# View the cleaned waterDepth data
summary(waterDepth_data)

# Remove any rows where waterDepth is below 0
waterDepth_data <- waterDepth_data %>%
  filter(waterDepth >= 0)

# Calculate the average water depth for each unique date
average_waterDepth <- waterDepth_data %>%
  group_by(dateOfLoss) %>%
  summarise(average_waterDepth = mean(waterDepth))

# View the resulting data
head(average_waterDepth)

# Load additional data
#data <- read.csv("D:/MRes_Project_desktop/MCCOMB_AIRPORT.csv")
data <- read.csv("D:/MRes_Project_desktop/ANGLETON_LAKE_JACKSON_BRAZORIA_CO_AIRPORT.csv")

# Inspect the dataset
head(data)
summary(data)

# Convert relevant columns to character
data <- data %>%
  mutate(
    PRCP = as.character(PRCP),
    WSF2 = as.character(WSF2),
    WSF5 = as.character(WSF5)
  ) %>%
  mutate(across(c(PRCP, WSF2, WSF5), ~ na_if(., "")))

# Convert the columns back to numeric
data <- data %>%
  mutate(
    PRCP = as.numeric(PRCP),
    WSF2 = as.numeric(WSF2),
    WSF5 = as.numeric(WSF5)
  )

# Remove rows with NA in PRCP, WSF2, or WSF5
cleaned_data <- data %>%
  drop_na(PRCP, WSF2, WSF5)

# Ensure the date columns are of the same type
cleaned_data$DATE <- as.Date(cleaned_data$DATE, format="%Y-%m-%d")
average_waterDepth$dateOfLoss <- as.Date(average_waterDepth$dateOfLoss, format="%Y-%m-%d")

# Perform the join to combine the datasets based on the date
final_combined_data <- inner_join(cleaned_data, average_waterDepth, by = c("DATE" = "dateOfLoss"))

# Ensure average_waterDepth is numeric
final_combined_data <- final_combined_data %>%
  mutate(average_waterDepth = as.numeric(average_waterDepth))

# Verify the changes
summary(final_combined_data$average_waterDepth)

# View the final combined data
head(final_combined_data)

###### End of data preparation 



###### Basic Summary Statistics

#install.packages("qqplotr")
library(dplyr)
library(tidyr)
library(ggplot2)
library(e1071)
library(qqplotr)


# Define a function to create histograms, skewness, and QQ plots
create_plots <- function(data, variable_name) {
  # Extract the variable data
  variable_data <- data[[variable_name]]
  
  # Calculate skewness
  skew_value <- skewness(variable_data, na.rm = TRUE)
  cat(paste("Skewness of", variable_name, ":", skew_value, "\n"))
  
  # Create histogram
  hist(variable_data, main = paste("Histogram of", variable_name), xlab = variable_name, breaks = 30, col = "lightblue", border = "black")
  
  # Create QQ plot using base R
  qqnorm(variable_data, main = paste("Normal Probability Plot of", variable_name), xlab = "Standard Normal Quantiles", ylab = "Sample Quantiles")
  qqline(variable_data, col = "red")
  
  # Add dashed lines at the beginning and tail
  qqplot <- qqnorm(variable_data, main = paste("Normal Probability Plot of", variable_name), xlab = "Standard Normal Quantiles", ylab = "Sample Quantiles")
  qqline(variable_data, col = "red")
  abline(h = c(min(qqplot$y), max(qqplot$y)), col = "gray", lty = 2)
  abline(v = c(min(qqplot$x), max(qqplot$x)), col = "gray", lty = 2)
}

# Create plots for PRCP
create_plots(final_combined_data, "PRCP")

# Create plots for WSF5
create_plots(final_combined_data, "WSF5")

# Create plots for average_waterDepth
create_plots(final_combined_data, "average_waterDepth")




######### 
# BM

library(extRemes)


# Create a function to extract yearly maxima for a given variable
extract_yearly_maxima <- function(data, date_column, variable) {
  data %>%
    mutate(year = format(as.Date(get(date_column)), "%Y")) %>%
    group_by(year) %>%
    summarize(max_value = max(get(variable), na.rm = TRUE)) %>%
    ungroup()
}

# Extract yearly maxima for PRCP, WSF5, and average_waterDepth
yearly_maxima_PRCP <- extract_yearly_maxima(final_combined_data, "DATE", "PRCP")
yearly_maxima_WSF5 <- extract_yearly_maxima(final_combined_data, "DATE", "WSF5")
yearly_maxima_waterDepth <- extract_yearly_maxima(final_combined_data, "DATE", "average_waterDepth")

# Fit GEV distribution to PRCP maxima
gev_fit_PRCP <- fevd(yearly_maxima_PRCP$max_value, type = "GEV")
summary(gev_fit_PRCP)

# Fit GEV distribution to WSF5 maxima
gev_fit_WSF5 <- fevd(yearly_maxima_WSF5$max_value, type = "GEV")
summary(gev_fit_WSF5)

# Fit GEV distribution to average_waterDepth maxima
gev_fit_waterDepth <- fevd(yearly_maxima_waterDepth$max_value, type = "GEV")
summary(gev_fit_waterDepth)


#install.packages("evd")
library(evd)


# Function to extract yearly maxima
extract_yearly_maxima <- function(data, date_column, variable) {
  data %>%
    mutate(year = format(as.Date(get(date_column)), "%Y")) %>%
    group_by(year) %>%
    summarize(max_value = max(get(variable), na.rm = TRUE)) %>%
    ungroup()
}

# Extract yearly maxima for PRCP, WSF5, and average_waterDepth
yearly_maxima_PRCP <- extract_yearly_maxima(final_combined_data, "DATE", "PRCP")
yearly_maxima_WSF5 <- extract_yearly_maxima(final_combined_data, "DATE", "WSF5")
yearly_maxima_waterDepth <- extract_yearly_maxima(final_combined_data, "DATE", "average_waterDepth")

# Fit GEV distribution using fgev
gev_fit_PRCP <- fgev(yearly_maxima_PRCP$max_value)
gev_fit_WSF5 <- fgev(yearly_maxima_WSF5$max_value)
gev_fit_waterDepth <- fgev(yearly_maxima_waterDepth$max_value)

# Print summaries
print(gev_fit_PRCP)
print(gev_fit_WSF5)
print(gev_fit_waterDepth)


### Diagnostics Plots  
# PRCP
par(mfrow = c(2, 2))
plot(gev_fit_PRCP)

# WSF5
par(mfrow = c(2, 2))
plot(gev_fit_WSF5)

# average waterDepth
par(mfrow = c(2, 2))
plot(gev_fit_waterDepth)






# Return Level
# Define a function to calculate the return level
return_level <- function(loc, scale, shape, return_period) {
  return(loc + (scale / shape) * ((-log(1 - 1/return_period))^(-shape) - 1))
}

# Parameters from GEV fit for PRCP
loc_PRCP <- 2.7224
scale_PRCP <- 0.9068
shape_PRCP <- 0.2686

# Parameters from GEV fit for WSF5
loc_WSF5 <- 54.932
scale_WSF5 <- 15.721
shape_WSF5 <- 1.054

# Parameters from GEV fit for Water Depth
loc_waterDepth <- 2.21514
scale_waterDepth <- 0.55906
shape_waterDepth <- 0.05922

# Calculate 50-year return levels
return_period <- 50

return_level_PRCP <- return_level(loc_PRCP, scale_PRCP, shape_PRCP, return_period)
return_level_WSF5 <- return_level(loc_WSF5, scale_WSF5, shape_WSF5, return_period)
return_level_waterDepth <- return_level(loc_waterDepth, scale_waterDepth, shape_waterDepth, return_period)

# Print the results
print(paste("50-year return level for PRCP:", return_level_PRCP))
print(paste("50-year return level for WSF5:", return_level_WSF5))
print(paste("50-year return level for Water Depth:", return_level_waterDepth))




# log-likelihood plots
# Function to create profile log-likelihood plots
profile_plot <- function(gev_fit, param_name, main_title) {
  profile_lik <- profile(gev_fit, param = param_name)
  x <- profile_lik[[1]][,1]
  y <- -profile_lik[[1]][,2]/2  # Profile log-likelihood
  plot(x, y, type = "l", main = main_title, 
       xlab = param_name, ylab = "profile log-likelihood")
  abline(h = max(y) - qchisq(0.95, 1)/2, lty = 2)
}


# Create profile log-likelihood plots for PRCP
par(mfrow = c(2, 2))
profile_plot(gev_fit_PRCP, "loc", "Profile Log-likelihood of Loc (PRCP)")
profile_plot(gev_fit_PRCP, "scale", "Profile Log-likelihood of Scale (PRCP)")
profile_plot(gev_fit_PRCP, "shape", "Profile Log-likelihood of Shape (PRCP)")
profile_plot(gev_fit_PRCP, "quantile", "Profile Log-likelihood of Quantile (PRCP)")

# Create profile log-likelihood plots for WSF5
par(mfrow = c(2, 2))
profile_plot(gev_fit_WSF5, "loc", "Profile Log-likelihood of Loc (WSF5)")
profile_plot(gev_fit_WSF5, "scale", "Profile Log-likelihood of Scale (WSF5)")
profile_plot(gev_fit_WSF5, "shape", "Profile Log-likelihood of Shape (WSF5)")
profile_plot(gev_fit_WSF5, "quantile", "Profile Log-likelihood of Quantile (WSF5)")

# Create profile log-likelihood plots for average_waterDepth
par(mfrow = c(2, 2))
profile_plot(gev_fit_waterDepth, "loc", "Profile Log-likelihood of Loc (Water Depth)")
profile_plot(gev_fit_waterDepth, "scale", "Profile Log-likelihood of Scale (Water Depth)")
profile_plot(gev_fit_waterDepth, "shape", "Profile Log-likelihood of Shape (Water Depth)")
profile_plot(gev_fit_waterDepth, "quantile", "Profile Log-likelihood of Quantile (Water Depth)")


#95% confidence interval of the profile likelihoods for the parameters
library(ismev)
# Function to calculate and print 95% confidence intervals for GEV parameters
calculate_confidence_intervals <- function(gev_fit) {
  # Obtain confidence intervals for all parameters at once
  conf_intervals <- confint(gev_fit, level = 0.95)
  
  # Extract confidence intervals for loc, scale, and shape
  loc_conf <- conf_intervals["loc",]
  scale_conf <- conf_intervals["scale",]
  shape_conf <- conf_intervals["shape",]
  
  return(list(
    loc_conf = loc_conf,
    scale_conf = scale_conf,
    shape_conf = shape_conf
  ))
}

# Calculate confidence intervals for PRCP
conf_intervals_PRCP <- calculate_confidence_intervals(gev_fit_PRCP)
print("95% confidence intervals for PRCP:")
print(conf_intervals_PRCP)

# Calculate confidence intervals for WSF5
conf_intervals_WSF5 <- calculate_confidence_intervals(gev_fit_WSF5)
print("95% confidence intervals for WSF5:")
print(conf_intervals_WSF5)

# Calculate confidence intervals for Water Depth
conf_intervals_waterDepth <- calculate_confidence_intervals(gev_fit_waterDepth)
print("95% confidence intervals for Water Depth:")
print(conf_intervals_waterDepth)




# Likelihood Ratio Test

# Fit the Gumbel model (with shape fixed at 0)
bm_gum_PRCP <- fgev(yearly_maxima_PRCP$max_value, shape = 0)
bm_gum_WSF5 <- fgev(yearly_maxima_WSF5$max_value, shape = 0)
bm_gum_waterDepth <- fgev(yearly_maxima_waterDepth$max_value, shape = 0)

# Compute the difference in deviance for PRCP
deviance_diff_PRCP <- deviance(bm_gum_PRCP) - deviance(gev_fit_PRCP)
print(paste("Deviance difference for PRCP:", deviance_diff_PRCP))

# Compute the difference in deviance for WSF5
deviance_diff_WSF5 <- deviance(bm_gum_WSF5) - deviance(gev_fit_WSF5)
print(paste("Deviance difference for WSF5:", deviance_diff_WSF5))

# Compute the difference in deviance for Water Depth
deviance_diff_waterDepth <- deviance(bm_gum_waterDepth) - deviance(gev_fit_waterDepth)
print(paste("Deviance difference for Water Depth:", deviance_diff_waterDepth))



# Function to perform K-S and C-vM tests
if (!requireNamespace("evd", quietly = TRUE)) install.packages("evd")
if (!requireNamespace("goftest", quietly = TRUE)) install.packages("goftest")

library(evd)
library(goftest)


perform_tests <- function(fit, data) {
  # Extract fitted parameters
  loc <- fit$estimate[1]
  scale <- fit$estimate[2]
  shape <- fit$estimate[3]
  
  # Check if parameters are valid
  if (is.na(loc) || is.na(scale) || is.na(shape) || scale <= 0) {
    return(list(error = "Invalid parameters", ks_test = NULL, cvm_test = NULL))
  }
  
  # Calculate the theoretical CDF
  cdf_theoretical <- try(pgev(data, loc = loc, scale = scale, shape = shape), silent = TRUE)
  
  if (inherits(cdf_theoretical, "try-error")) {
    return(list(error = "Error in calculating CDF", ks_test = NULL, cvm_test = NULL))
  }
  
  # Kolmogorov-Smirnov Test
  ks_test <- try(ks.test(data, "pgev", loc = loc, scale = scale, shape = shape), silent = TRUE)
  
  # Cramer-von Mises Test
  cvm_test <- try(cvm.test(data, "pgev", loc = loc, scale = scale, shape = shape), silent = TRUE)
  
  if (inherits(ks_test, "try-error") || inherits(cvm_test, "try-error")) {
    return(list(error = "Error in performing tests", ks_test = NULL, cvm_test = NULL))
  }
  
  list(ks_test = ks_test, cvm_test = cvm_test)
}



# Perform tests for PRCP
ks_cvm_tests_PRCP <- perform_tests(gev_fit_PRCP, yearly_maxima_PRCP$max_value)
print(ks_cvm_tests_PRCP)

# Perform tests for WSF5
ks_cvm_tests_WSF5 <- perform_tests(gev_fit_WSF5, yearly_maxima_WSF5$max_value)
print(ks_cvm_tests_WSF5)

# Perform tests for average_waterDepth
ks_cvm_tests_waterDepth <- perform_tests(gev_fit_waterDepth, yearly_maxima_waterDepth$max_value)
print(ks_cvm_tests_waterDepth)



print(gev_fit_PRCP$estimate)
# loc       scale     shape 
# 2.7223748 0.9067520 0.2685702 

print(gev_fit_WSF5$estimate)
# loc      scale     shape 
# 54.93244 15.72114  1.05432 

print(gev_fit_waterDepth$estimate)
# loc        scale      shape 
# 2.21513946 0.55906304 0.05921872 

### End of GEV






####### 
###Peak Over Threshold (POT)
library(ggplot2)
library(gridExtra)
library(evd)
library(grid)  # Add this line to load the grid package

create_gpd_stability_plots <- function(data, variable_name, n_thresholds = 50) {
  # Extract the variable
  variable <- data[[variable_name]]
  
  # Determine range of thresholds
  min_threshold <- quantile(variable, 0.5)
  max_threshold <- quantile(variable, 0.995)
  thresholds <- seq(min_threshold, max_threshold, length.out = n_thresholds)
  
  # Initialize vectors to store parameter estimates and their standard errors
  shape <- numeric(length(thresholds))
  scale <- numeric(length(thresholds))
  shape_se <- numeric(length(thresholds))
  scale_se <- numeric(length(thresholds))
  mean_excess <- numeric(length(thresholds))
  
  # Fit GPD for each threshold and calculate mean excess
  for (i in seq_along(thresholds)) {
    u <- thresholds[i]
    exceedances <- variable[variable > u] - u
    fit <- tryCatch(
      fpot(exceedances, threshold = 0),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      shape[i] <- fit$estimate["shape"]
      scale[i] <- fit$estimate["scale"]
      shape_se[i] <- fit$std.err["shape"]
      scale_se[i] <- fit$std.err["scale"]
    }
    mean_excess[i] <- mean(exceedances)
  }
  
  # Create data frames for plotting
  df_shape <- data.frame(threshold = thresholds, estimate = shape, se = shape_se)
  df_scale <- data.frame(threshold = thresholds, estimate = scale, se = scale_se)
  df_mean_excess <- data.frame(threshold = thresholds, mean_excess = mean_excess)
  
  # Create the plots
  p1 <- ggplot(df_mean_excess, aes(x = threshold, y = mean_excess)) +
    geom_line() +
    geom_point() +
    labs(x = "Threshold", y = "Mean Excess") +
    theme_minimal() +
    theme(axis.title.x = element_blank()) +
    ggtitle(paste("Mean Excess Plot for", variable_name))
  
  p2 <- ggplot(df_scale, aes(x = threshold, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se), width = 0) +
    labs(x = "Threshold", y = "Modified Scale") +
    theme_minimal() +
    theme(axis.title.x = element_blank())
  
  p3 <- ggplot(df_shape, aes(x = threshold, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se), width = 0) +
    labs(x = "Threshold", y = "Shape") +
    theme_minimal()
  
  # Combine the plots
  grid.arrange(p1, p2, p3, ncol = 1, heights = c(1, 1, 1),
               top = textGrob(paste("GPD Analysis for", variable_name),
                              gp = gpar(fontsize = 16, font = 2)))
}

# For PRCP
create_gpd_stability_plots(final_combined_data, "PRCP")

# For WSF5
create_gpd_stability_plots(final_combined_data, "WSF5")

# For average_waterDepth
create_gpd_stability_plots(final_combined_data, "average_waterDepth")




# Selected thresholds
selected_threshold_PRCP <- 1.6
selected_threshold_WSF5 <- 45
selected_threshold_average_waterDepth <- 1.5



### GPD Parameter Estimation
library(ggplot2)
library(gridExtra)
library(evd)
library(grid)

create_gpd_fit_plots <- function(data, variable_name, threshold, return_period = 100) {
  # Extract the variable
  variable <- data[[variable_name]]
  
  # Calculate exceedances
  exceedances <- variable[variable > threshold] - threshold
  
  # Fit GPD
  fit <- fpot(exceedances, threshold = 0)
  
  # Extract parameters
  scale <- fit$estimate["scale"]
  shape <- fit$estimate["shape"]
  
  # Generate theoretical quantiles and probabilities
  p <- ppoints(100)
  theo_quantiles <- qgpd(p, loc = 0, scale = scale, shape = shape)
  emp_quantiles <- quantile(exceedances, p)
  
  # Create QQ plot data
  qq_data <- data.frame(theoretical = theo_quantiles, empirical = emp_quantiles)
  
  # Create PP plot data
  pp_data <- data.frame(
    theoretical = p,
    empirical = pgpd(emp_quantiles, loc = 0, scale = scale, shape = shape)
  )
  
  # Create density plot data
  empirical_density <- density(exceedances)
  x <- empirical_density$x
  density_data <- data.frame(
    x = x,
    empirical = empirical_density$y,
    theoretical = dgpd(x, loc = 0, scale = scale, shape = shape)
  )
  
  # Create return level plot data
  n_years <- nrow(data) / 365.25  # Assuming daily data
  rate <- length(exceedances) / n_years
  return_periods <- seq(1, return_period, length.out = 100)
  return_levels <- threshold + qgpd(1 - 1 / (rate * return_periods), 
                                    loc = 0, scale = scale, shape = shape)
  return_level_data <- data.frame(
    return_period = return_periods,
    return_level = return_levels
  )
  
  # Create plots
  p1 <- ggplot(qq_data, aes(x = theoretical, y = empirical)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(x = "Theoretical Quantiles", y = "Empirical Quantiles") +
    ggtitle("Q-Q Plot") +
    theme_minimal()
  
  p2 <- ggplot(pp_data, aes(x = theoretical, y = empirical)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(x = "Theoretical Probabilities", y = "Empirical Probabilities") +
    ggtitle("P-P Plot") +
    theme_minimal()
  
  p3 <- ggplot(density_data) +
    geom_line(aes(x = x, y = empirical, color = "Empirical")) +
    geom_line(aes(x = x, y = theoretical, color = "Theoretical")) +
    labs(x = "Exceedance", y = "Density") +
    ggtitle("Density Plot") +
    scale_color_manual(values = c("Empirical" = "black", "Theoretical" = "red")) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  p4 <- ggplot(return_level_data, aes(x = return_period, y = return_level)) +
    geom_line() +
    scale_x_log10() +
    labs(x = "Return Period (years)", y = "Return Level") +
    ggtitle("Return Level Plot") +
    theme_minimal()
  
  # Combine plots
  grid.arrange(p1, p2, p3, p4, ncol = 2,
               top = textGrob(paste("GPD Fit for", variable_name, "at threshold =", threshold),
                              gp = gpar(fontsize = 16, font = 2)))
  
  # Return the full fit object instead of just the parameters
  return(fit)
}

# Fit the models
prcp_fit <- create_gpd_fit_plots(final_combined_data, "PRCP", threshold = selected_threshold_PRCP)
wsf5_fit <- create_gpd_fit_plots(final_combined_data, "WSF5", threshold = selected_threshold_WSF5)
water_depth_fit <- create_gpd_fit_plots(final_combined_data, "average_waterDepth", threshold = selected_threshold_average_waterDepth)

# Print the full fit results
print(prcp_fit)
print(wsf5_fit)
print(water_depth_fit)


# 50-year Return Levels
calculate_return_level <- function(fit, threshold, data, return_period = 50) {
  # Extract parameters from the fit
  scale <- fit$estimate["scale"]
  shape <- fit$estimate["shape"]
  
  # Calculate rate of exceedances
  n_years <- length(data) / 365.25
  exceedances <- data[data > threshold] - threshold
  rate <- length(exceedances) / n_years
  
  # Calculate return level
  p <- 1 - 1 / (return_period * rate)
  return_level <- threshold + qgpd(p, loc = 0, scale = scale, shape = shape)
  
  # Print debug information and return level
  cat("Scale:", scale, "Shape:", shape, "\n")
  cat("Number of years:", n_years, "\n")
  cat("Number of exceedances:", length(exceedances), "\n")
  cat("Rate:", rate, "\n")
  cat("p value:", p, "\n")
  cat(return_period, "-year Return Level:", return_level, "\n")
  
  return(return_level)
}

# Calculate 50-year return levels
prcp_return_level_50 <- calculate_return_level(prcp_fit, threshold = selected_threshold_PRCP, data = final_combined_data$PRCP)
wsf5_return_level_50 <- calculate_return_level(wsf5_fit, threshold = selected_threshold_WSF5, data = final_combined_data$WSF5)
water_depth_return_level_50 <- calculate_return_level(water_depth_fit, threshold = selected_threshold_average_waterDepth, data = final_combined_data$average_waterDepth)

# Print final results
cat("\nFinal 50-year Return Levels:\n")
cat("PRCP:", prcp_return_level_50, "\n")
cat("WSF5:", wsf5_return_level_50, "\n")
cat("average_waterDepth:", water_depth_return_level_50, "\n")





## Log-profile 
library(evd)


# Function to create profile log-likelihood plots for GPD
create_gpd_profile_plots <- function(fit, param_name, main_title) {
  profile_lik <- profile(fit, param = param_name)
  x <- profile_lik[[1]][,1]
  y <- -profile_lik[[1]][,2]/2  # Profile log-likelihood
  plot(x, y, type = "l", main = main_title, 
       xlab = param_name, ylab = "Profile Log-likelihood")
  abline(h = max(y) - qchisq(0.95, 1)/2, lty = 2)
}

# Create profile log-likelihood plots for PRCP
par(mfrow = c(1, 2))
create_gpd_profile_plots(prcp_fit, "scale", "Profile Log-likelihood of Scale (PRCP)")
create_gpd_profile_plots(prcp_fit, "shape", "Profile Log-likelihood of Shape (PRCP)")

# Create profile log-likelihood plots for WSF5
par(mfrow = c(1, 2))
create_gpd_profile_plots(wsf5_fit, "scale", "Profile Log-likelihood of Scale (WSF5)")
create_gpd_profile_plots(wsf5_fit, "shape", "Profile Log-likelihood of Shape (WSF5)")

# Create profile log-likelihood plots for average_waterDepth
par(mfrow = c(1, 2))
create_gpd_profile_plots(water_depth_fit, "scale", "Profile Log-likelihood of Scale (Water Depth)")
create_gpd_profile_plots(water_depth_fit, "shape", "Profile Log-likelihood of Shape (Water Depth)")

# 95% of CI (Shape, scale)
library(evd)

# Function to create profile log-likelihood plots and calculate confidence intervals for GPD
create_gpd_profile_plots_with_ci <- function(fit, param_name, main_title) {
  profile_lik <- profile(fit, param = param_name)
  x <- profile_lik[[1]][,1]
  y <- -profile_lik[[1]][,2]/2  # Profile log-likelihood
  
  # Calculate the 95% confidence interval
  threshold <- max(y) - qchisq(0.95, 1)/2
  ci_indices <- which(y >= threshold)
  ci_lower <- min(x[ci_indices])
  ci_upper <- max(x[ci_indices])
  
  # Plot
  plot(x, y, type = "l", main = main_title, 
       xlab = param_name, ylab = "Profile Log-likelihood")
  abline(h = threshold, lty = 2)
  abline(v = c(ci_lower, ci_upper), col = "red", lty = 2)
  
  # Return confidence interval
  return(c(ci_lower, ci_upper))
}

# Function to process and print results for each variable
process_gpd_fit <- function(fit, var_name) {
  cat("\n", var_name, "GPD Fit Results:\n")
  print(fit)
  
  cat("\nProfile Likelihood 95% Confidence Intervals:\n")
  scale_ci <- create_gpd_profile_plots_with_ci(fit, "scale", paste("Profile Log-likelihood of Scale (", var_name, ")"))
  shape_ci <- create_gpd_profile_plots_with_ci(fit, "shape", paste("Profile Log-likelihood of Shape (", var_name, ")"))
  
  cat("Scale parameter 95% CI: [", scale_ci[1], ", ", scale_ci[2], "]\n")
  cat("Shape parameter 95% CI: [", shape_ci[1], ", ", shape_ci[2], "]\n\n")
}

# Process each fit
par(mfrow = c(2, 2))
process_gpd_fit(prcp_fit, "PRCP")

par(mfrow = c(2, 2))
process_gpd_fit(wsf5_fit, "WSF5")

par(mfrow = c(2, 2))
process_gpd_fit(water_depth_fit, "Water Depth")

##Kolmogorov-Smirnov and Cramer von Mise test
library(evd)
library(goftest)

# Function to perform K-S and C-vM tests for GPD
perform_gpd_gof_tests <- function(data, threshold, fit) {
  # Extract parameters from the fit
  scale <- fit$estimate["scale"]
  shape <- fit$estimate["shape"]
  
  # Calculate exceedances
  exceedances <- data[data > threshold] - threshold
  
  # Kolmogorov-Smirnov Test
  ks_test <- ks.test(exceedances, "pgpd", scale = scale, shape = shape)
  
  # Cramer-von Mises Test
  cvm_test <- cvm.test(exceedances, "pgpd", scale = scale, shape = shape)
  
  # Return results
  return(list(ks_test = ks_test, cvm_test = cvm_test))
}

# Function to process and print goodness-of-fit results for each variable
process_gpd_gof <- function(fit, data, threshold, var_name) {
  cat("\n", var_name, "GPD Goodness-of-Fit Tests:\n")
  
  # Perform goodness-of-fit tests
  gof_tests <- perform_gpd_gof_tests(data, threshold, fit)
  
  cat("Kolmogorov-Smirnov Test:\n")
  print(gof_tests$ks_test)
  cat("\nCramer-von Mises Test:\n")
  print(gof_tests$cvm_test)
  cat("\n")
}

# Process each fit
process_gpd_gof(prcp_fit, final_combined_data$PRCP, threshold = selected_threshold_PRCP, "PRCP")
process_gpd_gof(wsf5_fit, final_combined_data$WSF5, threshold = selected_threshold_WSF5, "WSF5")
process_gpd_gof(water_depth_fit, final_combined_data$average_waterDepth, threshold = selected_threshold_average_waterDepth, "Water Depth")






#######
# Copulas
library(evd)
library(copula)
library(VineCopula)
library(ggplot2)
library(dplyr)

# Combine the yearly maxima into one dataset
yearly_maxima_combined <- data.frame(
  PRCP = yearly_maxima_PRCP$max_value,
  WSF5 = yearly_maxima_WSF5$max_value,
  average_waterDepth = yearly_maxima_waterDepth$max_value
)

# Step 1: Use the GEV fits from previous analysis
gev_fit_PRCP <- fgev(yearly_maxima_PRCP$max_value)
gev_fit_WSF5 <- fgev(yearly_maxima_WSF5$max_value)
gev_fit_waterDepth <- fgev(yearly_maxima_waterDepth$max_value)

# Step 2: Transform data to uniform margins
transform_to_uniform <- function(data, gev_fit) {
  pgev(data, loc = gev_fit$estimate["loc"], scale = gev_fit$estimate["scale"], shape = gev_fit$estimate["shape"])
}

u_rain <- transform_to_uniform(yearly_maxima_combined$PRCP, gev_fit_PRCP)
u_wind <- transform_to_uniform(yearly_maxima_combined$WSF5, gev_fit_WSF5)
u_flood <- transform_to_uniform(yearly_maxima_combined$average_waterDepth, gev_fit_waterDepth)

# Combine transformed data
u_data <- cbind(u_rain, u_wind, u_flood)

# Step 3: Fit various copula models
# Gumbel copula (an extreme value copula)
fit_gumbel <- tryCatch({
  fitCopula(gumbelCopula(dim = 3), u_data, method="mpl")
}, warning = function(w) {
  message("Warning in Gumbel copula fit: ", w$message)
  fitCopula(gumbelCopula(dim = 3), u_data, method="mpl")
}, error = function(e) {
  message("Error in Gumbel copula fit: ", e$message)
  NULL
})

# Clayton copula
fit_clayton <- tryCatch({
  fitCopula(claytonCopula(dim = 3), u_data, method="mpl", start = c(1,1,1))
}, error = function(e) {
  message("Error in Clayton copula fit: ", e$message)
  NULL
})

# Frank copula
fit_frank <- tryCatch({
  fitCopula(frankCopula(dim = 3), u_data, method="mpl")
}, error = function(e) {
  message("Error in Frank copula fit: ", e$message)
  NULL
})

# t-copula with fixed degrees of freedom
fit_t <- tryCatch({
  fitCopula(tCopula(dim = 3, df.fixed = TRUE), u_data, method="mpl")
}, error = function(e) {
  message("Error in t-copula fit: ", e$message)
  NULL
})

# Step 4: Compare copula fits
# Function to calculate AIC
calc_aic <- function(fit) {
  if (is.null(fit)) return(Inf)
  -2 * fit@loglik + 2 * length(fit@estimate)
}

# Calculate AIC for each copula
aic_results <- data.frame(
  Copula = c("Gumbel", "Clayton", "Frank", "t"),
  AIC = c(calc_aic(fit_gumbel), calc_aic(fit_clayton), calc_aic(fit_frank), calc_aic(fit_t))
)

# Remove rows with Inf AIC (failed fits)
aic_results <- aic_results[is.finite(aic_results$AIC),]

print(aic_results)

if (nrow(aic_results) > 0) {
  # Select the best copula based on lowest AIC
  best_copula <- aic_results$Copula[which.min(aic_results$AIC)]
  cat("The best fitting copula is:", best_copula, "\n")
  
  # Goodness-of-fit test for the best copula
  gof_test <- tryCatch({
    if (best_copula == "Gumbel") {
      gofCopula(gumbelCopula(dim = 3), u_data, N = 100)
    } else if (best_copula == "Clayton") {
      gofCopula(claytonCopula(dim = 3), u_data, N = 100)
    } else if (best_copula == "Frank") {
      gofCopula(frankCopula(dim = 3), u_data, N = 100)
    } else if (best_copula == "t") {
      gofCopula(tCopula(dim = 3, df.fixed = TRUE), u_data, N = 100, method = "Kendall")
    }
  }, error = function(e) {
    message("Error in goodness-of-fit test: ", e$message)
    NULL
  })
  
  if (!is.null(gof_test)) print(gof_test)
} else {
  cat("No valid copula fits were found.\n")
}

# Visualize the dependence structure
par(mfrow = c(1, 3))
plot(u_rain, u_wind, main = "Rain vs Wind")
plot(u_rain, u_flood, main = "Rain vs Flood")
plot(u_wind, u_flood, main = "Wind vs Flood")





### Copula POT
library(evd)
library(copula)
library(VineCopula)
library(ggplot2)
library(dplyr)

# Function to extract exceedances
extract_exceedances <- function(data, threshold) {
  exceedances <- data[data > threshold] - threshold
  return(exceedances)
}

# Extract exceedances for each variable
exceedances_PRCP <- extract_exceedances(final_combined_data$PRCP, selected_threshold_PRCP)
exceedances_WSF5 <- extract_exceedances(final_combined_data$WSF5, selected_threshold_WSF5)
exceedances_waterDepth <- extract_exceedances(final_combined_data$average_waterDepth, selected_threshold_average_waterDepth)

# Ensure all vectors have the same length (use the minimum length)
min_length <- min(length(exceedances_PRCP), length(exceedances_WSF5), length(exceedances_waterDepth))
exceedances_PRCP <- exceedances_PRCP[1:min_length]
exceedances_WSF5 <- exceedances_WSF5[1:min_length]
exceedances_waterDepth <- exceedances_waterDepth[1:min_length]

# Transform exceedances to uniform margins using the fitted GPD
transform_to_uniform_gpd <- function(exceedances, fit) {
  pgpd(exceedances, loc = 0, scale = fit$estimate["scale"], shape = fit$estimate["shape"])
}

u_rain_GPD <- transform_to_uniform_gpd(exceedances_PRCP, prcp_fit)
u_wind_GPD <- transform_to_uniform_gpd(exceedances_WSF5, wsf5_fit)
u_flood_GPD <- transform_to_uniform_gpd(exceedances_waterDepth, water_depth_fit)

# Combine transformed data
u_data <- cbind(u_rain_GPD, u_wind_GPD, u_flood_GPD)

# Fit various copula models
fit_copula <- function(copula, u_data, method = "mpl") {
  tryCatch({
    fitCopula(copula, u_data, method = method)
  }, error = function(e) {
    message("Error fitting copula: ", e$message)
    NULL
  })
}

# Fit copulas
fit_gumbel <- fit_copula(gumbelCopula(dim = 3), u_data)
fit_clayton <- fit_copula(claytonCopula(dim = 3), u_data)
fit_frank <- fit_copula(frankCopula(dim = 3), u_data)
fit_t <- fit_copula(tCopula(dim = 3, df.fixed = TRUE), u_data)

# Function to calculate AIC, BIC, and log-likelihood
calc_ic <- function(fit) {
  if (is.null(fit)) return(c(AIC = Inf, BIC = Inf, logLik = -Inf))
  n <- nobs(fit)
  k <- length(fit@estimate)
  logLik <- fit@loglik
  AIC <- -2 * logLik + 2 * k
  BIC <- -2 * logLik + k * log(n)
  return(c(AIC = AIC, BIC = BIC, logLik = logLik))
}

# Calculate information criteria for each copula
ic_results <- data.frame(
  Copula = c("Gumbel", "Clayton", "Frank", "t"),
  do.call(rbind, lapply(list(fit_gumbel, fit_clayton, fit_frank, fit_t), calc_ic))
)

# Remove rows with Inf values (failed fits)
ic_results <- ic_results[is.finite(ic_results$AIC),]

print(ic_results)

if (nrow(ic_results) > 0) {
  # Select the best copula based on highest log-likelihood
  best_copula <- ic_results$Copula[which.max(ic_results$logLik)]
  cat("The best fitting copula (based on log-likelihood) is:", best_copula, "\n")
  
  # Print details of the best-fitting copula
  if (best_copula == "Gumbel") {
    print(summary(fit_gumbel))
  } else if (best_copula == "Clayton") {
    print(summary(fit_clayton))
  } else if (best_copula == "Frank") {
    print(summary(fit_frank))
  } else if (best_copula == "t") {
    print(summary(fit_t))
  }
  
  # Goodness-of-fit test for the best copula
  gof_test <- tryCatch({
    if (best_copula == "Gumbel") {
      gofCopula(gumbelCopula(dim = 3), u_data, N = 1000)
    } else if (best_copula == "Clayton") {
      gofCopula(claytonCopula(dim = 3), u_data, N = 1000)
    } else if (best_copula == "Frank") {
      gofCopula(frankCopula(dim = 3), u_data, N = 1000)
    } else if (best_copula == "t") {
      gofCopula(tCopula(dim = 3, df.fixed = TRUE), u_data, N = 1000, simulation = "pb")
    }
  }, error = function(e) {
    message("Error in goodness-of-fit test: ", e$message)
    NULL
  })
  
  if (!is.null(gof_test)) print(gof_test)
} else {
  cat("No valid copula fits were found.\n")
}

# Calculate and print Kendall's tau for each pair of variables
kendall_tau <- cor(u_data, method = "kendall")
cat("\nKendall's tau:\n")
print(kendall_tau)

# Visualize the dependence structure
par(mfrow = c(1, 3))
plot(u_rain, u_wind, main = "Rain vs Wind", xlab = "Rain", ylab = "Wind")
plot(u_rain, u_flood, main = "Rain vs Flood", xlab = "Rain", ylab = "Flood")
plot(u_wind, u_flood, main = "Wind vs Flood", xlab = "Wind", ylab = "Flood")


library(MASS)  # Add this at the beginning of script with other library() calls

# Additional visualization: Scatter plot matrix
pairs(u_data, main = "Scatter Plot Matrix of Transformed Variables",
      labels = c("Rain", "Wind", "Flood"))

# Contour plots for each pair of variables
par(mfrow = c(1, 3))

# Rain vs Wind
kde_rain_wind <- kde2d(u_rain_GPD, u_wind_GPD)
contour(kde_rain_wind, main = "Rain vs Wind", xlab = "Rain", ylab = "Wind")

# Rain vs Flood
kde_rain_flood <- kde2d(u_rain_GPD, u_flood_GPD)
contour(kde_rain_flood, main = "Rain vs Flood", xlab = "Rain", ylab = "Flood")

# Wind vs Flood
kde_wind_flood <- kde2d(u_wind_GPD, u_flood_GPD)
contour(kde_wind_flood, main = "Wind vs Flood", xlab = "Wind", ylab = "Flood")






## Results (Prediction)
# Determine the best-fitting copula based on the highest log-likelihood
best_copula_name <- ic_results$Copula[which.max(ic_results$logLik)]
cat("The best fitting copula is:", best_copula_name, "\n")

# Get the corresponding fitted copula object
fit_best <- switch(best_copula_name,
                   "Gumbel" = fit_gumbel,
                   "Clayton" = fit_clayton,
                   "Frank" = fit_frank,
                   "t" = fit_t)

print(summary(fit_best))
print(class(fit_best@copula))
library(copula)

generate_conditional_samples <- function(wind_speed, copula, n_samples = 1000) {
  # Transform wind speed to uniform scale
  u_wind <- pgpd(wind_speed - selected_threshold_WSF5, 
                 loc = 0, 
                 scale = wsf5_fit$estimate["scale"], 
                 shape = wsf5_fit$estimate["shape"])
  
  # Extract parameters from the t-copula
  rho <- copula@parameters[1]  # Correlation parameter
  df <- copula@parameters[2]   # Degrees of freedom
  
  # Generate conditional samples for t-copula
  conditional_samples <- matrix(0, nrow = n_samples, ncol = 3)
  conditional_samples[,2] <- qt(u_wind, df = df)
  
  # Conditional mean and variance
  mu2 <- conditional_samples[,2]
  sigma2 <- sqrt((df + mu2^2) / (df + 1))
  
  # Generate conditional samples for variables 1 and 3
  for (i in c(1,3)) {
    mu <- rho * mu2
    sigma <- sqrt((1 - rho^2) * (df + mu2^2) / (df + 1))
    conditional_samples[,i] <- mu + sigma * rt(n_samples, df = df + 1)
  }
  
  # Transform to uniform scale
  conditional_samples <- pt(conditional_samples, df = df)
  
  # Transform back to original scales
  rain <- qgpd(conditional_samples[,1], 
               loc = selected_threshold_PRCP, 
               scale = prcp_fit$estimate["scale"], 
               shape = prcp_fit$estimate["shape"])
  
  flood <- qgpd(conditional_samples[,3], 
                loc = selected_threshold_average_waterDepth, 
                scale = water_depth_fit$estimate["scale"], 
                shape = water_depth_fit$estimate["shape"])
  
  return(data.frame(PRCP = rain, WaterDepth = flood))
}

predict_rain_and_flood <- function(wind_speed) {
  # Generate conditional samples
  samples <- generate_conditional_samples(wind_speed, fit_best@copula)
  
  # Calculate statistics
  results <- data.frame(
    Variable = c("Rainfall", "Flood Level"),
    Mean = c(mean(samples$PRCP), mean(samples$WaterDepth)),
    Median = c(median(samples$PRCP), median(samples$WaterDepth)),
    SD = c(sd(samples$PRCP), sd(samples$WaterDepth)),
    Lower_CI = c(quantile(samples$PRCP, 0.025), quantile(samples$WaterDepth, 0.025)),
    Upper_CI = c(quantile(samples$PRCP, 0.975), quantile(samples$WaterDepth, 0.975))
  )
  
  # Print results in the desired format with an empty line before and after
  cat("\n")  # Add an empty line before
  print(paste("Predictions for wind speed", wind_speed))
  print(results)
  cat("\n")  # Add an empty line after
}

# Example usage
wind_speeds <- c(50, 80, 100, 120, 150)  # Test with multiple wind speeds
for (wind_speed in wind_speeds) {
  predict_rain_and_flood(wind_speed)
}



# run simulations and store the results
wind_speeds <- 50:300  # Wind speeds from 50 to 80
results_list <- list()

for (wind_speed in wind_speeds) {
  samples <- generate_conditional_samples(wind_speed, fit_best@copula)
  results_list[[as.character(wind_speed)]] <- c(
    mean_rainfall = mean(samples$PRCP),
    lower_ci_rainfall = quantile(samples$PRCP, 0.025),
    upper_ci_rainfall = quantile(samples$PRCP, 0.975),
    mean_flood = mean(samples$WaterDepth),
    lower_ci_flood = quantile(samples$WaterDepth, 0.025),
    upper_ci_flood = quantile(samples$WaterDepth, 0.975)
  )
}

# Convert the results to a data frame
results_df <- do.call(rbind, results_list)
results_df <- as.data.frame(results_df)
results_df$wind_speed <- wind_speeds

# Print column names to verify
print(colnames(results_df))

# Now, let's create separate plots for rainfall and flood
library(ggplot2)

# Plot for Rainfall
rainfall_plot <- ggplot(results_df, aes(x = wind_speed)) +
  geom_line(aes(y = mean_rainfall), color = "blue") +
  geom_point(aes(y = mean_rainfall), color = "blue") +
  geom_line(aes(y = `lower_ci_rainfall.2.5%`), linetype = "dashed", color = "blue") +
  geom_line(aes(y = `upper_ci_rainfall.97.5%`), linetype = "dashed", color = "blue") +
  labs(title = "Mean Rainfall vs Wind Speed",
       x = "Wind Speed",
       y = "Rainfall") +
  theme_minimal()


# Plot for Flood Level
flood_plot <- ggplot(results_df, aes(x = wind_speed)) +
  geom_line(aes(y = mean_flood), color = "red") +
  geom_point(aes(y = mean_flood), color = "red") +
  geom_line(aes(y = `lower_ci_flood.2.5%`), linetype = "dashed", color = "red") +
  geom_line(aes(y = `upper_ci_flood.97.5%`), linetype = "dashed", color = "red") +
  labs(title = "Mean Flood Level vs Wind Speed",
       x = "Wind Speed",
       y = "Flood Level") +
  theme_minimal()


# Display the plots
print(rainfall_plot)
print(flood_plot)





# 1. Generate scenarios from the copula
n_scenarios <- 10000  # Number of scenarios to generate
simulated_data <- rCopula(n_scenarios, fit_best@copula)

# 2. Transform back to original scale
# Inverse probability integral transform using the GPD
inverse_transform <- function(u, fit, threshold, max_value) {
  result <- qgpd(u, loc = threshold, scale = fit$estimate["scale"], shape = fit$estimate["shape"])
  return(pmin(result, max_value))  # Ensure values don't exceed a reasonable maximum
}

# Use with realistic maximum values
original_scale_data <- data.frame(
  PRCP = inverse_transform(simulated_data[,1], prcp_fit, selected_threshold_PRCP, max_value = 100),
  WSF5 = inverse_transform(simulated_data[,2], wsf5_fit, selected_threshold_WSF5, max_value = 100000),
  WaterDepth = inverse_transform(simulated_data[,3], water_depth_fit, selected_threshold_average_waterDepth, max_value = 100)
)

# 3. Use for prediction or risk assessment
# For example, calculate joint exceedance probabilities
joint_exceedance <- function(data, thresholds) {
  mean(data$PRCP > thresholds[1] & 
         data$WSF5 > thresholds[2] & 
         data$WaterDepth > thresholds[3])
}

# Example: Probability of exceeding certain thresholds simultaneously
thresholds <- c(2.38, 100, 1.797012)  # Example thresholds for PRCP, WSF5, WaterDepth
prob_exceed <- joint_exceedance(original_scale_data, thresholds)
print(paste("Probability of joint exceedance:", prob_exceed))

# Visualize the simulated data
pairs(original_scale_data, main="Simulated Extreme Events")

# Calculate return levels
calculate_return_level <- function(data, return_period) {
  sorted_data <- sort(data, decreasing = TRUE)
  index <- ceiling(length(data) / return_period)
  return(sorted_data[index])
}

return_period <- 50  # 100-year return level
return_levels <- sapply(original_scale_data, calculate_return_level, return_period = return_period)
print(paste("100-year return levels:", paste(return_levels, collapse = ", ")))


##
mean(predicted_loss_values_glm)*prob_exceed






















generate_conditional_samples <- function(wind_speed, copula, n_samples = 100000) {
  # Print copula information for debugging
  print(paste("Copula class:", class(copula)))
  print("Copula parameters:") 
  print(copula@parameters)
  print(paste("Degrees of freedom:", copula@df.fixed))
  
  # Transform wind speed to uniform scale
  u_wind <- pgpd(wind_speed - selected_threshold_WSF5, 
                 loc = 0, 
                 scale = wsf5_fit$estimate["scale"], 
                 shape = wsf5_fit$estimate["shape"])
  
  # Extract parameters from the t-copula
  rho <- copula@parameters[1]  # Correlation parameter
  df <- copula@df.fixed        # Degrees of freedom
  
  # Generate conditional samples for t-copula
  conditional_samples <- matrix(0, nrow = n_samples, ncol = 3)
  conditional_samples[,2] <- qt(u_wind, df = df)
  
  # Conditional mean and variance
  mu2 <- conditional_samples[,2]
  sigma2 <- sqrt((df + mu2^2) / (df + 1))
  
  # Generate conditional samples for variables 1 and 3
  for (i in c(1,3)) {
    mu <- rho * mu2
    sigma <- sqrt((1 - rho^2) * (df + mu2^2) / (df + 1))
    conditional_samples[,i] <- mu + sigma * rt(n_samples, df = df + 1)
  }
  
  # Transform to uniform scale
  conditional_samples <- pt(conditional_samples, df = df)
  
  # Transform back to original scales
  rain <- qgpd(conditional_samples[,1], 
               loc = selected_threshold_PRCP, 
               scale = prcp_fit$estimate["scale"], 
               shape = prcp_fit$estimate["shape"])
  
  flood <- qgpd(conditional_samples[,3], 
                loc = selected_threshold_average_waterDepth, 
                scale = water_depth_fit$estimate["scale"], 
                shape = water_depth_fit$estimate["shape"])
  
  return(data.frame(PRCP = rain, WaterDepth = flood))
}


predict_rain_and_flood <- function(wind_speed) {
  print(paste("Predicting for wind speed:", wind_speed))
  print("Copula structure:")
  print(str(fit_best@copula))
  
  # Generate conditional samples
  samples <- generate_conditional_samples(wind_speed, fit_best@copula)
  
  # Calculate median and 95% confidence intervals
  results <- data.frame(
    Variable = c("Rainfall", "Flood Level"),
    Mean = c(mean(samples$PRCP), mean(samples$WaterDepth)),
    Median = c(median(samples$PRCP), median(samples$WaterDepth)),
    SD = c(sd(samples$PRCP), sd(samples$WaterDepth)),
    Lower_CI = c(quantile(samples$PRCP, 0.025), quantile(samples$WaterDepth, 0.025)),
    Upper_CI = c(quantile(samples$PRCP, 0.975), quantile(samples$WaterDepth, 0.975))
  )
  
  return(list(results = results, samples = samples))
}


# Example usage
wind_speeds <- c(50, 100, 150, 200)  # Test with multiple wind speeds
for (wind_speed in wind_speeds) {
  predictions <- predict_rain_and_flood(wind_speed)
  print(paste("Predictions for wind speed", wind_speed))
  print(predictions$results)
  
  # Visualize the predictions
  par(mfrow = c(1, 2))
  hist(predictions$samples$PRCP, main = paste("Predicted Rainfall (Wind Speed", wind_speed, ")"), xlab = "Rainfall")
  hist(predictions$samples$WaterDepth, main = paste("Predicted Flood Level (Wind Speed", wind_speed, ")"), xlab = "Flood Level")
}









predict_rain_and_flood <- function(wind_speed) {
  # Generate conditional samples
  samples <- generate_conditional_samples(wind_speed, fit_best@copula)
  
  # Calculate statistics
  results <- data.frame(
    Variable = c("Rainfall", "Flood Level"),
    Mean = c(mean(samples$PRCP), mean(samples$WaterDepth)),
    Median = c(median(samples$PRCP), median(samples$WaterDepth)),
    SD = c(sd(samples$PRCP), sd(samples$WaterDepth)),
    Lower_CI = c(quantile(samples$PRCP, 0.025), quantile(samples$WaterDepth, 0.025)),
    Upper_CI = c(quantile(samples$PRCP, 0.975), quantile(samples$WaterDepth, 0.975))
  )
  
  # Print results in the desired format
  print(paste("Predictions for wind speed", wind_speed))
  print(results)
}

# Example usage
wind_speeds <- c(50, 100, 150)  # Test with multiple wind speeds
for (wind_speed in wind_speeds) {
  predict_rain_and_flood(wind_speed)
}


























### 2D Copula

# Function to extract exceedances
extract_exceedances <- function(data, threshold) {
  exceedances <- data[data > threshold] - threshold
  return(exceedances)
}

# Extract exceedances for each variable
exceedances_PRCP <- extract_exceedances(final_combined_data$PRCP, selected_threshold_PRCP)
exceedances_WSF5 <- extract_exceedances(final_combined_data$WSF5, selected_threshold_WSF5)
exceedances_waterDepth <- extract_exceedances(final_combined_data$average_waterDepth, selected_threshold_average_waterDepth)

# Ensure all vectors have the same length (use the minimum length)
min_length <- min(length(exceedances_PRCP), length(exceedances_WSF5), length(exceedances_waterDepth))
exceedances_PRCP <- exceedances_PRCP[1:min_length]
exceedances_WSF5 <- exceedances_WSF5[1:min_length]
exceedances_waterDepth <- exceedances_waterDepth[1:min_length]

# Transform exceedances to uniform margins using the fitted GPD
transform_to_uniform_gpd <- function(exceedances, fit) {
  pgpd(exceedances, loc = 0, scale = fit$estimate["scale"], shape = fit$estimate["shape"])
}

u_rain_GPD <- transform_to_uniform_gpd(exceedances_PRCP, prcp_fit)
u_wind_GPD <- transform_to_uniform_gpd(exceedances_WSF5, wsf5_fit)
u_flood_GPD <- transform_to_uniform_gpd(exceedances_waterDepth, water_depth_fit)

# Prepare data for 2D copula fitting
u_rain_wind <- cbind(u_rain_GPD, u_wind_GPD)
u_rain_flood <- cbind(u_rain_GPD, u_flood_GPD)
u_wind_flood <- cbind(u_wind_GPD, u_flood_GPD)

# Function to fit 2D copulas
fit_copula_2d <- function(copula, u_data, method = "mpl", start = NULL) {
  tryCatch({
    fitCopula(copula, u_data, method = method, start = start)
  }, error = function(e) {
    message("Error fitting copula: ", e$message)
    NULL
  })
}

# Fit 2D copulas for each pair
fit_gumbel_rain_wind <- fit_copula_2d(gumbelCopula(dim = 2), u_rain_wind)
fit_gumbel_rain_flood <- fit_copula_2d(gumbelCopula(dim = 2), u_rain_flood)
fit_gumbel_wind_flood <- fit_copula_2d(gumbelCopula(dim = 2), u_wind_flood)

fit_t_rain_wind <- fit_copula_2d(tCopula(dim = 2, df.fixed = TRUE), u_rain_wind)
fit_t_rain_flood <- fit_copula_2d(tCopula(dim = 2, df.fixed = TRUE), u_rain_flood)
fit_t_wind_flood <- fit_copula_2d(tCopula(dim = 2, df.fixed = TRUE), u_wind_flood)

fit_clayton_rain_wind <- fit_copula_2d(claytonCopula(dim = 2), u_rain_wind, start = c(0.1))
fit_clayton_rain_flood <- fit_copula_2d(claytonCopula(dim = 2), u_rain_flood, start = c(0.1))
fit_clayton_wind_flood <- fit_copula_2d(claytonCopula(dim = 2), u_wind_flood, start = c(0.1))

fit_frank_rain_wind <- fit_copula_2d(frankCopula(dim = 2), u_rain_wind, start = c(1))
fit_frank_rain_flood <- fit_copula_2d(frankCopula(dim = 2), u_rain_flood, start = c(1))
fit_frank_wind_flood <- fit_copula_2d(frankCopula(dim = 2), u_wind_flood, start = c(1))

# Combine the fitted models into a list for easier management
fit_list <- list(
  Gumbel = list(fit_gumbel_rain_wind, fit_gumbel_rain_flood, fit_gumbel_wind_flood),
  Clayton = list(fit_clayton_rain_wind, fit_clayton_rain_flood, fit_clayton_wind_flood),
  Frank = list(fit_frank_rain_wind, fit_frank_rain_flood, fit_frank_wind_flood),
  t = list(fit_t_rain_wind, fit_t_rain_flood, fit_t_wind_flood)
)

# Function to calculate AIC, BIC, and log-likelihood for each pair
calc_ic_2d <- function(fit_list) {
  ic_results <- data.frame(Copula = character(), Pair = character(), AIC = numeric(), BIC = numeric(), logLik = numeric())
  pairs <- c("Rain vs Wind", "Rain vs Flood", "Wind vs Flood")
  
  for (copula_name in names(fit_list)) {
    for (i in 1:3) {
      fit <- fit_list[[copula_name]][[i]]
      ic <- if (!is.null(fit)) calc_ic(fit) else c(AIC = Inf, BIC = Inf, logLik = -Inf)
      ic_results <- rbind(ic_results, data.frame(Copula = copula_name, Pair = pairs[i], t(ic)))
    }
  }
  
  return(ic_results)
}

# Calculate information criteria for each copula and pair
ic_results_2d <- calc_ic_2d(fit_list)

# Remove rows with Inf values (failed fits)
ic_results_2d <- ic_results_2d[is.finite(ic_results_2d$AIC),]

print(ic_results_2d)

if (nrow(ic_results_2d) > 0) {
  # Select the best copula based on highest log-likelihood for each pair
  best_copulas <- ic_results_2d %>%
    group_by(Pair) %>%
    slice_max(order_by = logLik, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  print(best_copulas)
  
  for (i in 1:nrow(best_copulas)) {
    pair <- best_copulas$Pair[i]
    copula <- best_copulas$Copula[i]
    cat("The best fitting copula for", pair, "(based on log-likelihood) is:", copula, "\n")
    
    # Print details of the best-fitting copula
    fit <- fit_list[[copula]][[match(pair, c("Rain vs Wind", "Rain vs Flood", "Wind vs Flood"))]]
    print(summary(fit))
    
    # Goodness-of-fit test for the best copula
    gof_test <- tryCatch({
      if (copula == "Gumbel") {
        gofCopula(gumbelCopula(dim = 2), u_data, N = 1000, start = c(1))
      } else if (copula == "Clayton") {
        gofCopula(claytonCopula(dim = 2), u_data, N = 1000, start = c(0.1))
      } else if (copula == "Frank") {
        gofCopula(frankCopula(dim = 2), u_data, N = 1000, start = c(1))
      } else if (copula == "t") {
        gofCopula(tCopula(dim = 2, df.fixed = TRUE), u_data, N = 1000, simulation = "pb")
      }
    }, error = function(e) {
      message("Error in goodness-of-fit test: ", e$message)
      NULL
    })
    
    if (!is.null(gof_test)) print(gof_test)
  }
} else {
  cat("No valid copula fits were found.\n")
}

# Calculate and print Kendall's tau for each pair of variables
kendall_tau <- cor(u_data, method = "kendall")
cat("\nKendall's tau:\n")
print(kendall_tau)

# Visualize the dependence structure
par(mfrow = c(1, 3))
plot(u_rain_GPD, u_wind_GPD, main = "Rain vs Wind", xlab = "Rain", ylab = "Wind")
plot(u_rain_GPD, u_flood_GPD, main = "Rain vs Flood", xlab = "Rain", ylab = "Flood")
plot(u_wind_GPD, u_flood_GPD, main = "Wind vs Flood", xlab = "Wind", ylab = "Flood")

library(MASS)  # Add this at the beginning of script with other library() calls

# Additional visualization: Scatter plot matrix
pairs(u_data, main = "Scatter Plot Matrix of Transformed Variables",
      labels = c("Rain", "Wind", "Flood"))

# Contour plots for each pair of variables
par(mfrow = c(1, 3))

# Rain vs Wind
kde_rain_wind <- kde2d(u_rain_GPD, u_wind_GPD)
contour(kde_rain_wind, main = "Rain vs Wind", xlab = "Rain", ylab = "Wind")

# Rain vs Flood
kde_rain_flood <- kde2d(u_rain_GPD, u_flood_GPD)
contour(kde_rain_flood, main = "Rain vs Flood", xlab = "Rain", ylab = "Flood")

# Wind vs Flood
kde_wind_flood <- kde2d(u_wind_GPD, u_flood_GPD)
contour(kde_wind_flood, main = "Wind vs Flood", xlab = "Wind", ylab = "Flood")



### Goodness-of-fit
# Modified loop to calculate goodness-of-fit and extract p-values

p_values <- data.frame(Pair = character(), Copula = character(), P_Value = numeric())

# Mapping pairs to the corresponding 2D uniform data
pair_data_map <- list(
  "Rain vs Wind" = u_rain_wind,
  "Rain vs Flood" = u_rain_flood,
  "Wind vs Flood" = u_wind_flood
)

for (i in 1:nrow(best_copulas)) {
  pair <- best_copulas$Pair[i]
  copula <- best_copulas$Copula[i]
  cat("The best fitting copula for", pair, "(based on log-likelihood) is:", copula, "\n")
  
  # Select the best-fitting copula
  fit <- fit_list[[copula]][[match(pair, c("Rain vs Wind", "Rain vs Flood", "Wind vs Flood"))]]
  print(summary(fit))
  
  # Select the correct 2D data for the pair
  u_data_2d <- pair_data_map[[pair]]
  
  # Goodness-of-fit test for the best copula
  gof_test <- tryCatch({
    if (copula == "Gumbel") {
      gofCopula(gumbelCopula(dim = 2), u_data_2d, N = 1000, method = "Sn")
    } else if (copula == "Clayton") {
      gofCopula(claytonCopula(dim = 2), u_data_2d, N = 1000, method = "Sn", start = c(0.1))
    } else if (copula == "Frank") {
      gofCopula(frankCopula(dim = 2), u_data_2d, N = 1000, method = "Sn", start = c(1))
    } else if (copula == "t") {
      gofCopula(tCopula(dim = 2, df.fixed = TRUE), u_data_2d, N = 1000, method = "Sn", simulation = "pb")
    }
  }, error = function(e) {
    message("Error in goodness-of-fit test: ", e$message)
    NULL
  })
  
  if (!is.null(gof_test)) {
    print(gof_test)
    p_value <- gof_test$p.value
  } else {
    p_value <- NA
  }
  
  # Store the p-value
  p_values <- rbind(p_values, data.frame(Pair = pair, Copula = copula, P_Value = p_value))
}

# Print the p-values
cat("\nP-values for the best-fitting copulas:\n")
print(p_values)











library(copula)


generate_conditional_samples_chain <- function(wind_speed, copula_rw, copula_rf, copula_wf, n_samples = 1000) {
  # Step 1: Wind Speed to Rainfall (Rain vs Wind)
  u_wind <- pgpd(wind_speed - selected_threshold_WSF5, 
                 loc = 0, 
                 scale = wsf5_fit$estimate["scale"], 
                 shape = wsf5_fit$estimate["shape"])
  
  samples_rw <- rCopula(n_samples, copula_rw)
  conditional_rain <- samples_rw[samples_rw[, 2] >= u_wind - 0.05 & samples_rw[, 2] <= u_wind + 0.05, 1]
  
  if (length(conditional_rain) < 10) {
    stop("Not enough conditional samples generated in Rain vs Wind. Consider increasing n_samples or adjusting the tolerance.")
  }
  
  rain_levels <- qgpd(conditional_rain, 
                      loc = selected_threshold_PRCP, 
                      scale = prcp_fit$estimate["scale"], 
                      shape = prcp_fit$estimate["shape"])
  
  # Step 2: Rainfall to Flood Level (Rain vs Flood)
  u_rain <- pgpd(rain_levels - selected_threshold_PRCP, 
                 loc = 0, 
                 scale = prcp_fit$estimate["scale"], 
                 shape = prcp_fit$estimate["shape"])
  
  samples_rf <- rCopula(n_samples, copula_rf)
  
  # Use vectorized operations to compare each u_rain with samples_rf[, 1]
  match_matrix_rf <- outer(samples_rf[, 1], u_rain, function(x, y) x >= y - 0.05 & x <= y + 0.05)
  match_rf <- apply(match_matrix_rf, 1, any)
  conditional_flood <- samples_rf[match_rf, 2]
  
  if (length(conditional_flood) < 10) {
    stop("Not enough conditional samples generated in Rain vs Flood. Consider increasing n_samples or adjusting the tolerance.")
  }
  
  flood_levels_from_rain <- qgpd(conditional_flood, 
                                 loc = selected_threshold_average_waterDepth, 
                                 scale = water_depth_fit$estimate["scale"], 
                                 shape = water_depth_fit$estimate["shape"])
  
  # Step 3: Direct Wind Speed to Flood Level (Wind vs Flood)
  samples_wf <- rCopula(n_samples, copula_wf)
  conditional_flood_wf <- samples_wf[samples_wf[, 1] >= u_wind - 0.05 & samples_wf[, 1] <= u_wind + 0.05, 2]
  
  if (length(conditional_flood_wf) < 10) {
    stop("Not enough conditional samples generated in Wind vs Flood. Consider increasing n_samples or adjusting the tolerance.")
  }
  
  flood_levels_from_wind <- qgpd(conditional_flood_wf, 
                                 loc = selected_threshold_average_waterDepth, 
                                 scale = water_depth_fit$estimate["scale"], 
                                 shape = water_depth_fit$estimate["shape"])
  
  # Trim all results to the minimum length
  min_length <- min(length(rain_levels), length(flood_levels_from_rain), length(flood_levels_from_wind))
  rain_levels <- rain_levels[1:min_length]
  flood_levels_from_rain <- flood_levels_from_rain[1:min_length]
  flood_levels_from_wind <- flood_levels_from_wind[1:min_length]
  
  return(data.frame(WindSpeed = rep(wind_speed, min_length), 
                    PRCP = rain_levels, 
                    FloodFromRain = flood_levels_from_rain, 
                    FloodFromWind = flood_levels_from_wind))
}

# Testing the function for multiple wind speeds
wind_speeds <- c(50, 80, 100, 120, 150)
for (wind_speed in wind_speeds) {
  cat("\n--- Predictions for Wind Speed:", wind_speed, "---\n")
  
  # Use the best-fitted copulas for each pair
  predictions <- generate_conditional_samples_chain(wind_speed, 
                                                    fit_best_list[["Rain vs Wind"]]@copula, 
                                                    fit_best_list[["Rain vs Flood"]]@copula, 
                                                    fit_best_list[["Wind vs Flood"]]@copula)
  
  print(summary(predictions))
}






# Initialize an empty list to store the results
results_list <- list()

# Define the range of wind speeds to simulate
wind_speeds <- 50:300

# Run simulations and store the results
for (wind_speed in wind_speeds) {
  samples <- generate_conditional_samples_chain(wind_speed, 
                                                fit_best_list[["Rain vs Wind"]]@copula, 
                                                fit_best_list[["Rain vs Flood"]]@copula, 
                                                fit_best_list[["Wind vs Flood"]]@copula)
  
  results_list[[as.character(wind_speed)]] <- c(
    mean_rainfall = mean(samples$PRCP),
    lower_ci_rainfall = quantile(samples$PRCP, 0.025),
    upper_ci_rainfall = quantile(samples$PRCP, 0.975),
    mean_flood_rain = mean(samples$FloodFromRain),
    lower_ci_flood_rain = quantile(samples$FloodFromRain, 0.025),
    upper_ci_flood_rain = quantile(samples$FloodFromRain, 0.975),
    mean_flood_wind = mean(samples$FloodFromWind),
    lower_ci_flood_wind = quantile(samples$FloodFromWind, 0.025),
    upper_ci_flood_wind = quantile(samples$FloodFromWind, 0.975)
  )
}

# Convert the results to a data frame
results_df <- do.call(rbind, results_list)
results_df <- as.data.frame(results_df)
results_df$wind_speed <- wind_speeds

# Print column names to verify
print(colnames(results_df))

# Load ggplot2 for plotting
library(ggplot2)

# Plot for Rainfall
rainfall_plot <- ggplot(results_df, aes(x = wind_speed)) +
  geom_line(aes(y = mean_rainfall), color = "blue") +
  geom_point(aes(y = mean_rainfall), color = "blue") +
  geom_line(aes(y = `lower_ci_rainfall.2.5%`), linetype = "dashed", color = "blue") +
  geom_line(aes(y = `upper_ci_rainfall.97.5%`), linetype = "dashed", color = "blue") +
  labs(title = "Mean Rainfall vs Wind Speed",
       x = "Wind Speed",
       y = "Rainfall") +
  theme_minimal()

# Plot for Flood Level based on Rainfall
flood_plot_rain <- ggplot(results_df, aes(x = wind_speed)) +
  geom_line(aes(y = mean_flood_rain), color = "red") +
  geom_point(aes(y = mean_flood_rain), color = "red") +
  geom_line(aes(y = `lower_ci_flood_rain.2.5%`), linetype = "dashed", color = "red") +
  geom_line(aes(y = `upper_ci_flood_rain.97.5%`), linetype = "dashed", color = "red") +
  labs(title = "Mean Flood Level from Rainfall vs Wind Speed",
       x = "Wind Speed",
       y = "Flood Level from Rainfall") +
  theme_minimal()

# Plot for Flood Level based on Wind Speed
flood_plot_wind <- ggplot(results_df, aes(x = wind_speed)) +
  geom_line(aes(y = mean_flood_wind), color = "darkgreen") +
  geom_point(aes(y = mean_flood_wind), color = "darkgreen") +
  geom_line(aes(y = `lower_ci_flood_wind.2.5%`), linetype = "dashed", color = "darkgreen") +
  geom_line(aes(y = `upper_ci_flood_wind.97.5%`), linetype = "dashed", color = "darkgreen") +
  labs(title = "Mean Flood Level from Wind Speed vs Wind Speed",
       x = "Wind Speed",
       y = "Flood Level from Wind Speed") +
  theme_minimal()

# Display the plots
print(rainfall_plot)
print(flood_plot_rain)
print(flood_plot_wind)



# Number of scenarios to generate
n_scenarios <- 10000

# Generate scenarios from the copulas
simulated_data_rw <- rCopula(n_scenarios, fit_best_list[["Rain vs Wind"]]@copula)
simulated_data_rf <- rCopula(n_scenarios, fit_best_list[["Rain vs Flood"]]@copula)
simulated_data_wf <- rCopula(n_scenarios, fit_best_list[["Wind vs Flood"]]@copula)

# Inverse probability integral transform using the GPD
inverse_transform <- function(u, fit, threshold, max_value) {
  result <- qgpd(u, loc = threshold, scale = fit$estimate["scale"], shape = fit$estimate["shape"])
  return(pmin(result, max_value))  # Ensure values don't exceed a reasonable maximum
}

# Transform each simulated pair back to original scale
original_scale_data <- data.frame(
  PRCP = inverse_transform(simulated_data_rw[, 1], prcp_fit, selected_threshold_PRCP, max_value = 100),
  WSF5 = inverse_transform(simulated_data_rw[, 2], wsf5_fit, selected_threshold_WSF5, max_value = 100000),
  WaterDepth_rain = inverse_transform(simulated_data_rf[, 2], water_depth_fit, selected_threshold_average_waterDepth, max_value = 100),
  WaterDepth_wind = inverse_transform(simulated_data_wf[, 2], water_depth_fit, selected_threshold_average_waterDepth, max_value = 100)
)

# Calculate joint exceedance probabilities
joint_exceedance <- function(data, thresholds) {
  mean(data$PRCP > thresholds[1] & 
         data$WSF5 > thresholds[2] & 
         (data$WaterDepth_rain > thresholds[3] | data$WaterDepth_wind > thresholds[3]))
}

# Example thresholds for PRCP, WSF5, and WaterDepth
thresholds <- c(2.38, 80, 1.797012) 

# Calculate the probability of exceeding these thresholds simultaneously
prob_exceed <- joint_exceedance(original_scale_data, thresholds)
print(paste("Probability of joint exceedance:", prob_exceed))

# Visualize the simulated data
pairs(original_scale_data, main="Simulated Extreme Events")

# Calculate return levels
calculate_return_level <- function(data, return_period) {
  sorted_data <- sort(data, decreasing = TRUE)
  index <- ceiling(length(data) / return_period)
  return(sorted_data[index])
}

# Define return period and calculate return levels
return_period <- 50
return_levels <- sapply(original_scale_data, calculate_return_level, return_period = return_period)
print(paste("50-year return levels:", paste(return_levels, collapse = ", ")))

























#####
##### 2-D Copulas
# threashold = 0.8
#install.packages("scatterplot3d")
library(evd)       # For GEV fitting
library(copula)    # For copula modeling
library(VineCopula) # For additional copula functions
library(ggplot2)   # For plotting
library(scatterplot3d) # For 3D scatter plots

# Set the threshold for rainfall
rainfall_threshold <- 0.8

# Filter the data to include only rows with rainfall greater than the threshold
filtered_data <- final_combined_data %>%
  filter(PRCP > rainfall_threshold)

# Step 1: Fit GEV distributions to each variable in the filtered dataset
fit_gev <- function(data) {
  clean_data <- na.omit(data)  # Remove NA values
  
  # Provide initial estimates for location, scale, and shape parameters
  init_vals <- list(loc = mean(clean_data), scale = sd(clean_data), shape = 0.1)
  
  tryCatch({
    fgev(clean_data, start = init_vals, std.err = FALSE)
  }, error = function(e) {
    message("Error fitting GEV: ", e$message)
    return(NULL)
  })
}

gev_rain <- fit_gev(filtered_data$PRCP)
gev_wind <- fit_gev(filtered_data$WSF5)
gev_flood <- fit_gev(filtered_data$average_waterDepth)

# Check if GEV fits were successful
if (is.null(gev_rain) | is.null(gev_wind) | is.null(gev_flood)) {
  stop("GEV fitting failed for one or more variables.")
}

# Step 2: Transform data to uniform margins
transform_to_uniform <- function(data, gev_fit) {
  pgev(data, loc = gev_fit$estimate["loc"], scale = gev_fit$estimate["scale"], shape = gev_fit$estimate["shape"])
}

u_rain <- transform_to_uniform(filtered_data$PRCP, gev_rain)
u_wind <- transform_to_uniform(filtered_data$WSF5, gev_wind)
u_flood <- transform_to_uniform(filtered_data$average_waterDepth, gev_flood)

# Combine transformed data into pairs for pairwise copula fitting
u_data_rain_wind <- cbind(u_rain, u_wind)
u_data_rain_flood <- cbind(u_rain, u_flood)
u_data_wind_flood <- cbind(u_wind, u_flood)

# Step 3: Fit various pairwise copula models
fit_copula_models <- function(data) {
  fit_gumbel <- tryCatch(fitCopula(gumbelCopula(dim = 2), data, method="mpl"), error = function(e) NULL)
  fit_clayton <- tryCatch(fitCopula(claytonCopula(dim = 2), data, method="mpl"), error = function(e) NULL)
  fit_frank <- tryCatch(fitCopula(frankCopula(dim = 2), data, method="mpl"), error = function(e) NULL)
  fit_t <- tryCatch(fitCopula(tCopula(dim = 2, df.fixed = TRUE), data, method="mpl"), error = function(e) NULL)
  fit_gaussian <- tryCatch(fitCopula(normalCopula(dim = 2), data, method="mpl"), error = function(e) NULL)
  
  # Calculate AIC for each copula
  aic_gumbel <- if (!is.null(fit_gumbel)) calc_aic(fit_gumbel) else Inf
  aic_clayton <- if (!is.null(fit_clayton)) calc_aic(fit_clayton) else Inf
  aic_frank <- if (!is.null(fit_frank)) calc_aic(fit_frank) else Inf
  aic_t <- if (!is.null(fit_t)) calc_aic(fit_t) else Inf
  aic_gaussian <- if (!is.null(fit_gaussian)) calc_aic(fit_gaussian) else Inf
  
  # Compare AICs
  aic_results <- data.frame(
    Copula = c("Gumbel", "Clayton", "Frank", "t", "Gaussian"),
    AIC = c(aic_gumbel, aic_clayton, aic_frank, aic_t, aic_gaussian)
  )
  
  best_copula <- aic_results$Copula[which.min(aic_results$AIC)]
  
  list(
    fits = list(gumbel = fit_gumbel, clayton = fit_clayton, frank = fit_frank, t = fit_t, gaussian = fit_gaussian),
    aic_results = aic_results,
    best_copula = best_copula
  )
}

# Fit copula models for each pair
fits_rain_wind <- fit_copula_models(u_data_rain_wind)
fits_rain_flood <- fit_copula_models(u_data_rain_flood)
fits_wind_flood <- fit_copula_models(u_data_wind_flood)

# Print the AIC results and the best copula for each pair
print(fits_rain_wind$aic_results)
cat("Best copula for Rain vs Wind:", fits_rain_wind$best_copula, "\n")

print(fits_rain_flood$aic_results)
cat("Best copula for Rain vs Flood:", fits_rain_flood$best_copula, "\n")

print(fits_wind_flood$aic_results)
cat("Best copula for Wind vs Flood:", fits_wind_flood$best_copula, "\n")

# Optional: Goodness-of-fit test for the best copula for each pair
gof_test_best <- function(best_copula_name, data, fits) {
  if (best_copula_name == "Gumbel") {
    gof_test <- tryCatch(gofCopula(gumbelCopula(dim = 2), data, N = 100), error = function(e) NULL)
  } else if (best_copula_name == "Clayton") {
    gof_test <- tryCatch(gofCopula(claytonCopula(dim = 2), data, N = 100), error = function(e) NULL)
  } else if (best_copula_name == "Frank") {
    gof_test <- tryCatch(gofCopula(frankCopula(dim = 2), data, N = 100), error = function(e) NULL)
  } else if (best_copula_name == "t") {
    gof_test <- tryCatch(gofCopula(tCopula(dim = 2, df.fixed = TRUE), data, N = 100, method = "Kendall"), error = function(e) NULL)
  } else if (best_copula_name == "Gaussian") {
    gof_test <- tryCatch(gofCopula(normalCopula(dim = 2), data, N = 100), error = function(e) NULL)
  }
  gof_test
}

# Conduct goodness-of-fit tests
gof_rain_wind <- gof_test_best(fits_rain_wind$best_copula, u_data_rain_wind, fits_rain_wind$fits)
gof_rain_flood <- gof_test_best(fits_rain_flood$best_copula, u_data_rain_flood, fits_rain_flood$fits)
gof_wind_flood <- gof_test_best(fits_wind_flood$best_copula, u_data_wind_flood, fits_wind_flood$fits)

print(gof_rain_wind)
print(gof_rain_flood)
print(gof_wind_flood)

# Visualize the dependence structure
par(mfrow = c(1, 3))
plot(u_data_rain_wind[,1], u_data_rain_wind[,2], main = "Rain vs Wind")
plot(u_data_rain_flood[,1], u_data_rain_flood[,2], main = "Rain vs Flood")
plot(u_data_wind_flood[,1], u_data_wind_flood[,2], main = "Wind vs Flood")




# Visualize the dependence structure with 3D scatter plots
par(mfrow = c(1, 3))
scatterplot3d(u_rain, u_wind, u_flood, main = "3D Plot: Rain vs Wind vs Flood", xlab = "Rain", ylab = "Wind", zlab = "Flood")
scatterplot3d(u_data_rain_wind[,1], u_data_rain_wind[,2], u_data_wind_flood[,2], main = "3D Plot: Rain vs Wind", xlab = "Rain", ylab = "Wind", zlab = "Flood")
scatterplot3d(u_data_rain_flood[,1], u_data_rain_flood[,2], u_data_wind_flood[,2], main = "3D Plot: Rain vs Flood", xlab = "Rain", ylab = "Flood", zlab = "Wind")
scatterplot3d(u_data_wind_flood[,1], u_data_wind_flood[,2], u_data_rain_flood[,1], main = "3D Plot: Wind vs Flood", xlab = "Wind", ylab = "Flood", zlab = "Rain")












































#######
# Results

#####prediction Using Copulas
# Number of simulations
n_sim <- 1000

# Step 4: Simulate from the copula
simulated_u_data <- rCopula(n_sim, fit_clayton@copula)

# Step 5: Transform back to original margins using the inverse of the fitted marginal distributions
inverse_transform <- function(u, gev_fit) {
  qgev(u, loc = gev_fit$estimate["loc"], scale = gev_fit$estimate["scale"], shape = gev_fit$estimate["shape"])
}

simulated_rain <- inverse_transform(simulated_u_data[, 1], gev_rain)
simulated_wind <- inverse_transform(simulated_u_data[, 2], gev_wind)
simulated_flood <- inverse_transform(simulated_u_data[, 3], gev_flood)

# Combine simulated data into a data frame
simulated_data <- data.frame(Rainfall = simulated_rain, WindSpeed = simulated_wind, FloodLevel = simulated_flood)

# Step 6: Condition on wind speed exceeding a certain threshold
wind_threshold <- 115  # threshold for wind speed
conditioned_data <- simulated_data %>%
  filter(WindSpeed > wind_threshold)

# Analyze the corresponding rainfall and flood levels
summary(conditioned_data$Rainfall)
summary(conditioned_data$FloodLevel)

# Visualize the results
ggplot(conditioned_data, aes(x = Rainfall, y = FloodLevel)) +
  geom_point(alpha = 0.5) +
  labs(title = paste("Rainfall vs Flood Level for Wind Speed >", wind_threshold),
       x = "Rainfall",
       y = "Flood Level") +
  theme_minimal()





# Function to estimate expected values for a given wind speed
estimate_expected_values <- function(wind_speed, simulated_data, tolerance = 0.1) {
  # Find data points close to the given wind speed
  close_points <- simulated_data %>%
    filter(abs(WindSpeed - wind_speed) <= tolerance)
  
  # Calculate expected values
  expected_rainfall <- mean(close_points$Rainfall)
  expected_flood_level <- mean(close_points$FloodLevel)
  
  return(list(rainfall = expected_rainfall, flood_level = expected_flood_level))
}

# Example usage
wind_speed <- 100  # The wind speed interested in
results <- estimate_expected_values(wind_speed, simulated_data)

cat("For wind speed", wind_speed, "m/s:\n")
cat("Expected rainfall:", results$rainfall, "\n")
cat("Expected flood level:", results$flood_level, "\n")





# Define the threshold values for which want to calculate the exceedance probability
rainfall_threshold <- 5  #  threshold for rainfall
wind_threshold <- 70      # threshold for wind speed
flood_threshold <- 1      # threshold for flood level

# Calculate the exceedance probability for the threshold using the fitted GEV distributions
exceedance_probability <- function(threshold, gev_fit) {
  1 - pgev(threshold, loc = gev_fit$estimate["loc"], scale = gev_fit$estimate["scale"], shape = gev_fit$estimate["shape"])
}

# Exceedance probabilities for the defined thresholds
exceed_prob_rain <- exceedance_probability(rainfall_threshold, gev_rain)
exceed_prob_wind <- exceedance_probability(wind_threshold, gev_wind)
exceed_prob_flood <- exceedance_probability(flood_threshold, gev_flood)

cat("Exceedance Probability for Rainfall >", rainfall_threshold, ":", exceed_prob_rain, "\n")
cat("Exceedance Probability for Wind Speed >", wind_threshold, ":", exceed_prob_wind, "\n")
cat("Exceedance Probability for Flood Level >", flood_threshold, ":", exceed_prob_flood, "\n")

# Calculate the annual exceedance probability
annual_exceedance_probability <- function(threshold, gev_fit, n_years = 1) {
  exceed_prob <- exceedance_probability(threshold, gev_fit)
  1 - (1 - exceed_prob)^(1 / n_years)
}

# Annual exceedance probabilities for the defined thresholds
annual_exceed_prob_rain <- annual_exceedance_probability(rainfall_threshold, gev_rain)
annual_exceed_prob_wind <- annual_exceedance_probability(wind_threshold, gev_wind)
annual_exceed_prob_flood <- annual_exceedance_probability(flood_threshold, gev_flood)

cat("Annual Exceedance Probability for Rainfall >", rainfall_threshold, ":", annual_exceed_prob_rain, "\n")
cat("Annual Exceedance Probability for Wind Speed >", wind_threshold, ":", annual_exceed_prob_wind, "\n")
cat("Annual Exceedance Probability for Flood Level >", flood_threshold, ":", annual_exceed_prob_flood, "\n")














































#####


### Quarterly

## Function to extract quarterly maxima
#extract_quarterly_maxima <- function(data, date_column, variable) {
#  data %>%
#    mutate(
#      date = as.Date(get(date_column)),
#      year = format(date, "%Y"),
#      quarter = paste0(year, "-Q", ceiling(as.numeric(format(date, "%m")) / 3))
#    ) %>%
#    group_by(quarter) %>%
#    summarize(max_value = max(get(variable), na.rm = TRUE)) %>%
#    ungroup()
#}

## Extract quarterly maxima for PRCP, WSF5, and average_waterDepth
#quarterly_maxima_PRCP <- extract_quarterly_maxima(final_combined_data, "DATE", "PRCP")
#quarterly_maxima_WSF5 <- extract_quarterly_maxima(final_combined_data, "DATE", "WSF5")
#quarterly_maxima_waterDepth <- extract_quarterly_maxima(final_combined_data, "DATE", "average_waterDepth")

## Fit GEV distribution using fgev
#gev_fit_PRCP <- fgev(quarterly_maxima_PRCP$max_value)
#gev_fit_WSF5 <- fgev(quarterly_maxima_WSF5$max_value)
#gev_fit_waterDepth <- fgev(quarterly_maxima_waterDepth$max_value)

## Print summaries
#print(gev_fit_PRCP)
#print(gev_fit_WSF5)
#print(gev_fit_waterDepth)

## Plot diagnostics for PRCP
#par(mfrow = c(2, 2))
#plot(gev_fit_PRCP)

## Plot diagnostics for WSF5
#par(mfrow = c(2, 2))
#plot(gev_fit_WSF5)

## Plot diagnostics for average_waterDepth
#par(mfrow = c(2, 2))
#plot(gev_fit_waterDepth)













# Plot wind speed (WSF5) over time
plot_wind <- ggplot(final_combined_data, aes(x = DATE, y = WSF5)) +
  geom_line(color = "blue") +
  ggtitle("Wind Speed (WSF5) Over Time") +
  xlab("Date") +
  ylab("Wind Speed (WSF5)") +
  theme_minimal()

# Plot rainfall (PRCP) over time
plot_rainfall <- ggplot(final_combined_data, aes(x = DATE, y = PRCP)) +
  geom_line(color = "green") +
  ggtitle("Rainfall (PRCP) Over Time") +
  xlab("Date") +
  ylab("Rainfall (PRCP)") +
  theme_minimal()

# Plot flood level (average_waterDepth) over time
plot_flood <- ggplot(final_combined_data, aes(x = DATE, y = average_waterDepth)) +
  geom_line(color = "red") +
  ggtitle("Flood Level (average_waterDepth) Over Time") +
  xlab("Date") +
  ylab("Flood Level (average_waterDepth)") +
  theme_minimal()

library(gridExtra)
grid.arrange(plot_wind, plot_rainfall, plot_flood, ncol = 1)







### Threashold = 0
# Load libraries
library(evd)       # For GEV fitting
library(copula)    # For copula modeling
library(VineCopula) # For additional copula functions
library(ggplot2)   # For plotting
library(scatterplot3d) # For 3D scatter plots

# Set the threshold for rainfall
rainfall_threshold <- 0

# Filter the data to include only rows with rainfall greater than the threshold
filtered_data <- final_combined_data %>%
  filter(PRCP > rainfall_threshold)

# Step 1: Fit GEV distributions to each variable in the filtered dataset
fit_gev <- function(data) {
  clean_data <- na.omit(data)  # Remove NA values
  
  # Provide initial estimates for location, scale, and shape parameters
  init_vals <- list(loc = mean(clean_data), scale = sd(clean_data), shape = 0.1)
  
  tryCatch({
    fgev(clean_data, start = init_vals, std.err = FALSE)
  }, error = function(e) {
    message("Error fitting GEV: ", e$message)
    return(NULL)
  })
}

gev_rain <- fit_gev(filtered_data$PRCP)
gev_wind <- fit_gev(filtered_data$WSF5)
gev_flood <- fit_gev(filtered_data$average_waterDepth)

# Check if GEV fits were successful
if (is.null(gev_rain) | is.null(gev_wind) | is.null(gev_flood)) {
  stop("GEV fitting failed for one or more variables.")
}

# Step 2: Transform data to uniform margins
transform_to_uniform <- function(data, gev_fit) {
  pgev(data, loc = gev_fit$estimate["loc"], scale = gev_fit$estimate["scale"], shape = gev_fit$estimate["shape"])
}

u_rain <- transform_to_uniform(filtered_data$PRCP, gev_rain)
u_wind <- transform_to_uniform(filtered_data$WSF5, gev_wind)
u_flood <- transform_to_uniform(filtered_data$average_waterDepth, gev_flood)

# Combine transformed data into pairs for pairwise copula fitting
u_data_rain_wind <- cbind(u_rain, u_wind)
u_data_rain_flood <- cbind(u_rain, u_flood)
u_data_wind_flood <- cbind(u_wind, u_flood)

# Step 3: Fit various pairwise copula models
fit_copula_models <- function(data) {
  fit_gumbel <- tryCatch(fitCopula(gumbelCopula(dim = 2), data, method="mpl", control = list(maxit = 1000)), error = function(e) NULL)
  fit_clayton <- tryCatch(fitCopula(claytonCopula(dim = 2), data, method="mpl", control = list(maxit = 1000)), error = function(e) NULL)
  fit_frank <- tryCatch(fitCopula(frankCopula(dim = 2), data, method="mpl", control = list(maxit = 1000)), error = function(e) NULL)
  fit_t <- tryCatch(fitCopula(tCopula(dim = 2, df.fixed = TRUE), data, method="mpl", control = list(maxit = 1000)), error = function(e) NULL)
  fit_gaussian <- tryCatch(fitCopula(normalCopula(dim = 2), data, method="mpl", control = list(maxit = 1000)), error = function(e) NULL)
  
  # Calculate AIC for each copula
  aic_gumbel <- if (!is.null(fit_gumbel)) calc_aic(fit_gumbel) else Inf
  aic_clayton <- if (!is.null(fit_clayton)) calc_aic(fit_clayton) else Inf
  aic_frank <- if (!is.null(fit_frank)) calc_aic(fit_frank) else Inf
  aic_t <- if (!is.null(fit_t)) calc_aic(fit_t) else Inf
  aic_gaussian <- if (!is.null(fit_gaussian)) calc_aic(fit_gaussian) else Inf
  
  # Compare AICs
  aic_results <- data.frame(
    Copula = c("Gumbel", "Clayton", "Frank", "t", "Gaussian"),
    AIC = c(aic_gumbel, aic_clayton, aic_frank, aic_t, aic_gaussian)
  )
  
  best_copula <- aic_results$Copula[which.min(aic_results$AIC)]
  
  list(
    fits = list(gumbel = fit_gumbel, clayton = fit_clayton, frank = fit_frank, t = fit_t, gaussian = fit_gaussian),
    aic_results = aic_results,
    best_copula = best_copula
  )
}

# Fit copula models for each pair
fits_rain_wind <- fit_copula_models(u_data_rain_wind)
fits_rain_flood <- fit_copula_models(u_data_rain_flood)
fits_wind_flood <- fit_copula_models(u_data_wind_flood)

# Print the AIC results and the best copula for each pair
print(fits_rain_wind$aic_results)
cat("Best copula for Rain vs Wind:", fits_rain_wind$best_copula, "\n")

print(fits_rain_flood$aic_results)
cat("Best copula for Rain vs Flood:", fits_rain_flood$best_copula, "\n")

print(fits_wind_flood$aic_results)
cat("Best copula for Wind vs Flood:", fits_wind_flood$best_copula, "\n")

# Goodness-of-fit test for the best copula for each pair
gof_test_best <- function(best_copula_name, data, fits) {
  if (best_copula_name == "Gumbel") {
    gof_test <- tryCatch(gofCopula(gumbelCopula(dim = 2), data, N = 100), error = function(e) NULL)
  } else if (best_copula_name == "Clayton") {
    gof_test <- tryCatch(gofCopula(claytonCopula(dim = 2), data, N = 100), error = function(e) NULL)
  } else if (best_copula_name == "Frank") {
    gof_test <- tryCatch(gofCopula(frankCopula(dim = 2), data, N = 100), error = function(e) NULL)
  } else if (best_copula_name == "t") {
    gof_test <- tryCatch(gofCopula(tCopula(dim = 2, df.fixed = TRUE), data, N = 100, method = "Kendall"), error = function(e) NULL)
  } else if (best_copula_name == "Gaussian") {
    gof_test <- tryCatch(gofCopula(normalCopula(dim = 2), data, N = 100), error = function(e) NULL)
  }
  gof_test
}

# Conduct goodness-of-fit tests
gof_rain_wind <- gof_test_best(fits_rain_wind$best_copula, u_data_rain_wind, fits_rain_wind$fits)
gof_rain_flood <- gof_test_best(fits_rain_flood$best_copula, u_data_rain_flood, fits_rain_flood$fits)
gof_wind_flood <- gof_test_best(fits_wind_flood$best_copula, u_data_wind_flood, fits_wind_flood$fits)

print(gof_rain_wind)
print(gof_rain_flood)
print(gof_wind_flood)

# Visualize the dependence structure with 3D scatter plots
par(mfrow = c(1, 3))
scatterplot3d(u_rain, u_wind, u_flood, main = "3D Plot: Rain vs Wind vs Flood", xlab = "Rain", ylab = "Wind", zlab = "Flood")
scatterplot3d(u_data_rain_wind[,1], u_data_rain_wind[,2], u_data_wind_flood[,2], main = "3D Plot: Rain vs Wind", xlab = "Rain", ylab = "Wind", zlab = "Flood")
scatterplot3d(u_data_rain_flood[,1], u_data_rain_flood[,2], u_data_wind_flood[,2], main = "3D Plot: Rain vs Flood", xlab = "Rain", ylab = "Flood", zlab = "Wind")
scatterplot3d(u_data_wind_flood[,1], u_data_wind_flood[,2], u_data_rain_flood[,1], main = "3D Plot: Wind vs Flood", xlab = "Wind", ylab = "Flood", zlab = "Rain")

