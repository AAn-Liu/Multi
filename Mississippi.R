
########################################################################################################
####################################### Hazard Model Mississippi #######################################
########################################################################################################

library(dplyr)
library(stringr)
library(tidyr)

Flood_Insurance_Loss <- read.csv("FimaNfipClaims.csv")
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

# Convert waterDepth to numeric 
waterDepth_data$waterDepth <- as.numeric(waterDepth_data$waterDepth)

# Check for any non-numeric values 
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
data <- read.csv("GULFPORT_BILOXI_AIRPORT_MS.csv")

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
summary(final_combined_data)
###### End of data preparation 



###### Basic Summary Statistics

#install.packages("qqplotr")
library(dplyr)
library(tidyr)
library(ggplot2)
library(e1071)
library(qqplotr)


# Define a function to create histograms, skewness, and QQ plots

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
}

par(mfrow = c(1, 2))
# Create plots for PRCP
create_plots(final_combined_data, "PRCP")
mtext("PRCP (MS)", side = 3, line = -1.5, outer = TRUE)

# Create plots for WSF5
create_plots(final_combined_data, "WSF5")
mtext("WSF5 (MS)", side = 3, line = -1.5, outer = TRUE)

# Create plots for average_waterDepth
create_plots(final_combined_data, "average_waterDepth")
mtext("Flood Level (MS)", side = 3, line = -1.5, outer = TRUE)





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
set.seed(0601)

# PRCP
par(mfrow = c(2, 2))
plot(gev_fit_PRCP)
mtext("PRCP (MS)", side = 3, line = -1.5, outer = TRUE)

# WSF5
par(mfrow = c(2, 2))
plot(gev_fit_WSF5)
mtext("WSF5 (MS)", side = 3, line = -1.5, outer = TRUE)

# Average waterDepth
par(mfrow = c(2, 2))
plot(gev_fit_waterDepth)
mtext("Flood Level (MS)", side = 3, line = -1.5, outer = TRUE)







# Return Level
# Define a function to calculate the return level
return_level <- function(loc, scale, shape, return_period) {
  return(loc + (scale / shape) * ((-log(1 - 1/return_period))^(-shape) - 1))
}

# Parameters from GEV fit for PRCP
loc_PRCP <- gev_fit_PRCP$estimate["loc"]
scale_PRCP <- gev_fit_PRCP$estimate["scale"]
shape_PRCP <- gev_fit_PRCP$estimate["shape"]

# Parameters from GEV fit for WSF5
loc_WSF5 <- gev_fit_WSF5$estimate["loc"]
scale_WSF5 <- gev_fit_WSF5$estimate["scale"]
shape_WSF5 <- gev_fit_WSF5$estimate["shape"]

# Parameters from GEV fit for Water Depth
loc_waterDepth <- gev_fit_waterDepth$estimate["loc"]
scale_waterDepth <- gev_fit_waterDepth$estimate["scale"]
shape_waterDepth <- gev_fit_waterDepth$estimate["shape"]

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

library(evd)

# Function to create a profile plot
create_profile_plot <- function(fit, param, main_title) {
  prof <- profile(fit, param)
  plot(prof[[1]][,1], -prof[[1]][,2]/2, type = "l", 
       xlab = param, ylab = "Profile Log-Likelihood", 
       main = main_title)
  abline(h = min(-prof[[1]][,2]/2) + qchisq(0.95, 1)/2, lty = 2)
}

# PRCP Plots
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

create_profile_plot(gev_fit_PRCP, "loc", "Profile Log-Likelihood of Loc")
create_profile_plot(gev_fit_PRCP, "scale", "Profile Log-Likelihood of Scale")
create_profile_plot(gev_fit_PRCP, "shape", "Profile Log-Likelihood of Shape")
mtext("Profile Log-Likelihood Plots for PRCP (MS)", outer = TRUE, cex = 1.2)

# WSF5 Plots
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

create_profile_plot(gev_fit_WSF5, "loc", "Profile Log-Likelihood of Loc")
create_profile_plot(gev_fit_WSF5, "scale", "Profile Log-Likelihood of Scale")
create_profile_plot(gev_fit_WSF5, "shape", "Profile Log-Likelihood of Shape")
mtext("Profile Log-Likelihood Plots for WSF5 (MS)", outer = TRUE, cex = 1.2)

# Water Depth Plots
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

create_profile_plot(gev_fit_waterDepth, "loc", "Profile Log-Likelihood of Loc")
create_profile_plot(gev_fit_waterDepth, "scale", "Profile Log-Likelihood of Scale")
create_profile_plot(gev_fit_waterDepth, "shape", "Profile Log-Likelihood of Shape")
mtext("Profile Log-Likelihood Plots for Water Depth (MS)", outer = TRUE, cex = 1.2)


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
# 4.3496377  1.5794338 -0.3109713 

print(gev_fit_WSF5$estimate)
# loc      scale     shape 
# 51.0760800  6.6287077  0.4208131

print(gev_fit_waterDepth$estimate)
# loc        scale      shape 
# 2.2245845 0.3416247 0.4223146

### End of GEV






####### 
###Peak Over Threshold (POT)
library(ggplot2)
library(gridExtra)
library(evd)
library(grid) 


create_gpd_stability_plots <- function(data, variable_name, state_name, n_thresholds = 50) {
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
    theme(axis.title.x = element_blank()) 
  
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
               top = textGrob(paste("GPD Analysis for", variable_name, "(", state_name, ")"),
                              gp = gpar(fontsize = 16, font = 2)))
}

# For PRCP
create_gpd_stability_plots(final_combined_data, "PRCP", "MS")

# For WSF5
create_gpd_stability_plots(final_combined_data, "WSF5", "MS")

# For average_waterDepth
create_gpd_stability_plots(final_combined_data, "average_waterDepth", "MS")







# Selected thresholds
selected_threshold_PRCP <- 1.3
selected_threshold_WSF5 <- 46
selected_threshold_average_waterDepth <- 1.2



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
               top = textGrob(paste("GPD Fit for", variable_name, "at threshold =", threshold, "(MS)"),
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
#scale   shape  
# 1.01017  0.08633

print(wsf5_fit)
# scale   shape  
# 2.708  0.6704

print(water_depth_fit)
# scale   shape  
# 0.23883  0.09793 



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

# Function to create a profile plot for GPD
create_gpd_profile_plot <- function(fit, param, main_title) {
  prof <- profile(fit, param)
  plot(prof[[1]][,1], -prof[[1]][,2]/2, type = "l", 
       xlab = param, ylab = "Profile Log-Likelihood", 
       main = main_title)
  abline(h = min(-prof[[1]][,2]/2) + qchisq(0.95, 1)/2, lty = 2)
}

# PRCP Plots for GPD (only scale and shape parameters)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

create_gpd_profile_plot(prcp_fit, "scale", "Profile Log-Likelihood of Scale (PRCP)")
create_gpd_profile_plot(prcp_fit, "shape", "Profile Log-Likelihood of Shape (PRCP)")
mtext("Profile Log-Likelihood Plots for PRCP (MS)", outer = TRUE, cex = 1.2)

# WSF5 Plots for GPD
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

create_gpd_profile_plot(wsf5_fit, "scale", "Profile Log-Likelihood of Scale (WSF5)")
create_gpd_profile_plot(wsf5_fit, "shape", "Profile Log-Likelihood of Shape (WSF5)")
mtext("Profile Log-Likelihood Plots for WSF5 (MS)", outer = TRUE, cex = 1.2)

# Water Depth Plots for GPD
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

create_gpd_profile_plot(water_depth_fit, "scale", "Profile Log-Likelihood of Scale (Water Depth)")
create_gpd_profile_plot(water_depth_fit, "shape", "Profile Log-Likelihood of Shape (Water Depth)")
mtext("Profile Log-Likelihood Plots for Water Depth (MS)", outer = TRUE, cex = 1.2)




# 95% of CI (Shape, scale)
library(ismev)

# Function to calculate and print 95% confidence intervals for GPD parameters
calculate_gpd_confidence_intervals <- function(gpd_fit) {
  # Obtain confidence intervals for scale and shape
  conf_intervals <- confint(gpd_fit, level = 0.95)
  
  # Extract confidence intervals for scale and shape
  scale_conf <- conf_intervals["scale",]
  shape_conf <- conf_intervals["shape",]
  
  return(list(
    scale_conf = scale_conf,
    shape_conf = shape_conf
  ))
}

# Calculate and print confidence intervals for PRCP
conf_intervals_PRCP <- calculate_gpd_confidence_intervals(prcp_fit)
cat("95% confidence intervals for PRCP (MS):\n")
print(conf_intervals_PRCP)

# Calculate and print confidence intervals for WSF5
conf_intervals_WSF5 <- calculate_gpd_confidence_intervals(wsf5_fit)
cat("95% confidence intervals for WSF5 (MS):\n")
print(conf_intervals_WSF5)

# Calculate and print confidence intervals for Water Depth
conf_intervals_waterDepth <- calculate_gpd_confidence_intervals(water_depth_fit)
cat("95% confidence intervals for Water Depth (MS):\n")
print(conf_intervals_waterDepth)


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
  fitCopula(claytonCopula(dim = 3), u_data, method="mpl")
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
# Function to calculate AIC, BIC, and log-likelihood
calc_ic <- function(fit, u_data) {
  if (is.null(fit)) return(c(AIC = Inf, BIC = Inf, logLik = NA, Kendall_Tau = NA, Spearman_Rho = NA))
  
  n <- nobs(fit)
  k <- length(fit@estimate)
  logLik <- fit@loglik
  AIC <- -2 * logLik + 2 * k
  BIC <- -2 * logLik + k * log(n)
  
  # Calculate Kendall's Tau and Spearman's Rho
  dependence_measures <- calc_dependence_measures(fit, u_data)
  
  return(c(AIC = AIC, BIC = BIC, logLik = logLik, Kendall_Tau = dependence_measures$Kendall_Tau, Spearman_Rho = dependence_measures$Spearman_Rho))
}

# Function to calculate Kendall's Tau and Spearman's Rho
calc_dependence_measures <- function(fit, u_data) {
  if (is.null(fit)) return(list(Kendall_Tau = NA, Spearman_Rho = NA))
  
  # Calculate Kendall's Tau
  tau_value <- tryCatch({
    tau(fit@copula)
  }, error = function(e) {
    NA
  })
  
  # Calculate Spearman's Rho
  rho_value <- tryCatch({
    rho(fit@copula)
  }, error = function(e) {
    # Manually calculate Spearman's Rho if not available
    cor(u_data, method = "spearman")[1, 2]
  })
  
  return(list(Kendall_Tau = tau_value, Spearman_Rho = rho_value))
}

# Number of observations
n <- nrow(u_data)

# Calculate AIC, BIC, and log-likelihood for each copula
results <- data.frame(
  Copula = c("Gumbel", "Clayton", "Frank", "t"),
  do.call(rbind, lapply(list(fit_gumbel, fit_clayton, fit_frank, fit_t), calc_ic, u_data = u_data))
)

# Remove rows with Inf AIC or BIC (failed fits)
results <- results[is.finite(results$AIC) & is.finite(results$BIC),]

print(results)

if (nrow(results) > 0) {
  # Select the best copula based on lowest AIC
  best_copula <- results$Copula[which.min(results$AIC)]
  cat("The best fitting copula based on AIC is:", best_copula, "\n")
  
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

set.seed(0601)
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

# Function to calculate AIC, BIC, log-likelihood, Kendall's Tau, and Spearman's Rho
calc_ic <- function(fit) {
  if (is.null(fit)) return(c(AIC = Inf, BIC = Inf, logLik = -Inf, Kendall_Tau = NA, Spearman_Rho = NA))
  
  n <- nobs(fit)
  k <- length(fit@estimate)
  logLik <- fit@loglik
  AIC <- -2 * logLik + 2 * k
  BIC <- -2 * logLik + k * log(n)
  
  # Calculate Kendall's Tau and Spearman's Rho from the copula parameters
  kendall_tau <- tryCatch(tau(fit@copula), error = function(e) NA)
  spearman_rho <- tryCatch(rho(fit@copula), error = function(e) NA)
  
  return(c(AIC = AIC, BIC = BIC, logLik = logLik, Kendall_Tau = kendall_tau, Spearman_Rho = spearman_rho))
}

# Calculate information criteria for each copula and include Kendall's Tau and Spearman's Rho
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

# Visualise the dependence structure
library(MASS)  
library(copula)

set.seed(9961) 
# Function to create contour plots for empirical and fitted copula data
plot_empirical_vs_fitted_contours <- function(u_data, fitted_copula, var_names) {
  # Simulate data from the fitted copula
  simulated_data <- rCopula(nrow(u_data), fitted_copula)
  
  # Plotting layout
  par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
  
  # Empirical Contour Plots
  for (i in 1:(ncol(u_data)-1)) {
    for (j in (i+1):ncol(u_data)) {
      empirical_kde <- kde2d(u_data[, i], u_data[, j])
      contour(empirical_kde, main = paste(var_names[i], "vs", var_names[j], "- Empirical"), xlab = var_names[i], ylab = var_names[j])
    }
  }
  
  # Fitted Copula Contour Plots
  for (i in 1:(ncol(u_data)-1)) {
    for (j in (i+1):ncol(u_data)) {
      copula_kde <- kde2d(simulated_data[, i], simulated_data[, j])
      contour(copula_kde, main = paste(var_names[i], "vs", var_names[j], "- Fitted Copula"), xlab = var_names[i], ylab = var_names[j])
    }
  }
  
  # Add a common title
  mtext("Empirical vs Fitted Copula Contour Plots (MS)", outer = TRUE, cex = 1.5)
}

# Define variable names
var_names <- c("Rain", "Wind", "Flood")

# Select the best-fitted copula based on previous results
if (best_copula == "Gumbel") {
  copula_fit <- fit_gumbel
} else if (best_copula == "Clayton") {
  copula_fit <- fit_clayton
} else if (best_copula == "Frank") {
  copula_fit <- fit_frank
} else if (best_copula == "t") {
  copula_fit <- fit_t
}

# Plot empirical vs fitted copula contours
plot_empirical_vs_fitted_contours(u_data, copula_fit@copula, var_names)










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
wind_speeds <- c(50, 100, 150)  # Test with multiple wind speeds
for (wind_speed in wind_speeds) {
  predict_rain_and_flood(wind_speed)
}



# run simulations and store the results
wind_speeds <- 50:150  # Wind speeds from 50 to 150
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
  labs(title = "Mean Rainfall vs Wind Speed (MS)",
       x = "Wind Speed",
       y = "Rainfall") +
  theme_minimal()


# Plot for Flood Level
flood_plot <- ggplot(results_df, aes(x = wind_speed)) +
  geom_line(aes(y = mean_flood), color = "red") +
  geom_point(aes(y = mean_flood), color = "red") +
  geom_line(aes(y = `lower_ci_flood.2.5%`), linetype = "dashed", color = "red") +
  geom_line(aes(y = `upper_ci_flood.97.5%`), linetype = "dashed", color = "red") +
  labs(title = "Mean Flood Level vs Wind Speed (MS)",
       x = "Wind Speed",
       y = "Flood Level") +
  theme_minimal()


# Display the plots
print(rainfall_plot)
print(flood_plot)




set.seed(0601) 
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
thresholds <- c(3, 80, 1.8)  # Example thresholds for PRCP, WSF5, WaterDepth
prob_exceed_MS <- joint_exceedance(original_scale_data, thresholds)
print(paste("Probability of joint exceedance (MS):", prob_exceed_MS))

# Visualize the simulated data
pairs(original_scale_data, main="Simulated Extreme Events (MS)")

# Calculate return levels
calculate_return_level <- function(data, return_period) {
  sorted_data <- sort(data, decreasing = TRUE)
  index <- ceiling(length(data) / return_period)
  return(sorted_data[index])
}

return_period <- 50  # 50-year return level
return_levels <- sapply(original_scale_data, calculate_return_level, return_period = return_period)
print(paste("100-year return levels:", paste(return_levels, collapse = ", ")))





#########################################################################################################
#################################### Vulnerability Model Mississippi ####################################
#########################################################################################################

#library(dplyr)
#library(stringr)
#library(tidyr)


# Read the data
#Flood_Insurance_Loss <- read.csv("D:/MRes_Project_desktop/Multi_/Data/Vulnerability/FimaNfipClaims.csv")

# Filter the dataset for rain-related damages and state MS
filtered_rain_only <- Flood_Insurance_Loss %>% 
  filter(causeOfDamage == "4" ) # & state == "MS"

filtered_rain_only_named_events_clean <- filtered_rain_only[
  !is.na(filtered_rain_only$floodEvent) & filtered_rain_only$floodEvent != "", 
]

# Count non-NA and non-empty string values for each column
valid_counts <- sapply(filtered_rain_only_named_events_clean, function(x) {
  sum(!is.na(x) & x != "")
})

# Print the counts
print(valid_counts)

filtered_dataset <- filtered_rain_only_named_events_clean %>%
  dplyr::select(buildingDamageAmount, 
                basementEnclosureCrawlspaceType, #
                dateOfLoss, 
                elevatedBuildingIndicator, 
                ratedFloodZone, 
                locationOfContents, #
                numberOfFloorsInTheInsuredBuilding, 
                nonProfitIndicator, 
                occupancyType, 
                originalConstructionDate, 
                yearOfLoss = yearOfLoss, 
                floodWaterDuration, #
                floodproofedIndicator, 
                numberOfUnits, 
                stateOwnedIndicator, 
                waterDepth, 
                state) %>%
  dplyr::mutate(dateOfLoss = as.Date(dateOfLoss, format="%Y-%m-%dT%H:%M:%S"), 
                originalConstructionDate = as.Date(originalConstructionDate, format="%Y-%m-%dT%H:%M:%S"))%>%
  drop_na()

filtered_dataset <- filtered_dataset %>%
  mutate(buildingAgeAtLoss = as.numeric(difftime(dateOfLoss, originalConstructionDate, units = "days")) / 365.25)



# Filter the dataset for state MS and buildingDamageAmount > 0
filtered_dataset_MS_non_profit <- filtered_dataset %>% 
  filter(state == "MS", nonProfitIndicator =="0", buildingDamageAmount > 1000, buildingDamageAmount < 100000)

# Find the row with the highest buildingDamageAmount and remove it
filtered_dataset_MS_non_profit <- filtered_dataset_MS_non_profit %>%
  filter(buildingDamageAmount != max(buildingDamageAmount, na.rm = TRUE))

# Read the weather data
weather_data <- read.csv("GULFPORT_BILOXI_AIRPORT_MS.csv")

# Prepare the weather data
weather_data <- weather_data %>%
  mutate(
    DATE = as.Date(DATE, format="%Y-%m-%d"),
    PRCP = as.numeric(PRCP),
    WSF5 = as.numeric(WSF5)
  ) %>%
  dplyr::select(DATE, PRCP, WSF5) %>%
  drop_na()

# Merge the flood insurance data with the weather data
filtered_dataset_MS_non_profit <- filtered_dataset_MS_non_profit %>%
  left_join(weather_data, by = c("dateOfLoss" = "DATE"))

# Remove any rows where the join didn't find a match (i.e., where PRCP or WSF5 is NA)
filtered_dataset_MS_non_profit <- filtered_dataset_MS_non_profit %>%
  drop_na(PRCP, WSF5)

# View the final combined data
head(filtered_dataset_MS_non_profit)

# ensure all categorical variables are factors
filtered_dataset_MS_non_profit <- filtered_dataset_MS_non_profit %>%
  mutate(
    basementEnclosureCrawlspaceType = as.factor(basementEnclosureCrawlspaceType),
    elevatedBuildingIndicator = as.factor(elevatedBuildingIndicator),
    ratedFloodZone = as.factor(ratedFloodZone),
    locationOfContents = as.factor(locationOfContents),
    occupancyType = as.factor(occupancyType),
    floodproofedIndicator = as.factor(floodproofedIndicator),
    stateOwnedIndicator = as.factor(stateOwnedIndicator)
  )

factor_vars <- c("basementEnclosureCrawlspaceType", "elevatedBuildingIndicator", 
                 "ratedFloodZone", "locationOfContents", "occupancyType", 
                 "floodproofedIndicator", "stateOwnedIndicator")

# Load libraries
library(stats)

# forward selection function based on p-values
forward_select_pvalue <- function(data, response, predictors, significance_level = 0.05, family = Gamma(link = "log")) {
  current_formula <- as.formula(paste(response, "~ 1"))
  remaining_predictors <- predictors
  selected_predictors <- c()
  
  while(length(remaining_predictors) > 0) {
    best_predictor <- NULL
    best_p_value <- 1
    
    for(predictor in remaining_predictors) {
      new_formula <- as.formula(paste(response, "~", paste(c(selected_predictors, predictor), collapse = " + ")))
      model <- glm(new_formula, data = data, family = family)
      summary_model <- summary(model)
      p_value <- tail(summary_model$coefficients[,4], 1)
      
      if(p_value < best_p_value) {
        best_predictor <- predictor
        best_p_value <- p_value
      }
    }
    
    if(best_p_value < significance_level) {
      selected_predictors <- c(selected_predictors, best_predictor)
      remaining_predictors <- setdiff(remaining_predictors, best_predictor)
      print(paste("Added", best_predictor, "with p-value", best_p_value))
    } else {
      break
    }
  }
  
  final_formula <- as.formula(paste(response, "~", paste(selected_predictors, collapse = " + ")))
  final_model <- glm(final_formula, data = data, family = family)
  
  return(list(model = final_model, selected_variables = selected_predictors))
}

# Prepare the data (ensure factors have more than one level)
factor_vars <- c("basementEnclosureCrawlspaceType", "elevatedBuildingIndicator", 
                 "ratedFloodZone", "locationOfContents", "occupancyType", 
                 "floodproofedIndicator", "stateOwnedIndicator")

numeric_vars <- c("numberOfFloorsInTheInsuredBuilding", "floodWaterDuration", 
                  "numberOfUnits", "waterDepth", "buildingAgeAtLoss", "PRCP", "WSF5")

all_predictors <- c()

for (var in c(factor_vars, numeric_vars)) {
  if (is.factor(filtered_dataset_MS_non_profit[[var]])) {
    if (length(unique(filtered_dataset_MS_non_profit[[var]])) > 1) {
      all_predictors <- c(all_predictors, var)
    } else {
      print(paste("Removing", var, "as it has only one level"))
    }
  } else {
    all_predictors <- c(all_predictors, var)
  }
}

# Run the custom forward selection
result <- forward_select_pvalue(filtered_dataset_MS_non_profit, "buildingDamageAmount", all_predictors)
head(filtered_dataset_MS_non_profit)
# Print the summary of the final model
summary(result$model)

# Print the selected variables
print(paste("Selected variables:", paste(result$selected_variables, collapse = ", ")))



# Fit the modified model
model_MS_non_profit <- glm(buildingDamageAmount ~ 
                             basementEnclosureCrawlspaceType +
                             elevatedBuildingIndicator +
                             #locationOfContents +
                             #numberOfFloorsInTheInsuredBuilding +
                             #floodWaterDuration +
                             #numberOfUnits +
                             waterDepth +
                             #buildingAgeAtLoss +
                             #occupancyType+
                             PRCP +
                             WSF5, 
                           data = filtered_dataset_MS_non_profit, 
                           family = Gamma(link = "log"))

summary(model_MS_non_profit)






# Loss estimation example
new_data_glm_non_profit <- data.frame(
  basementEnclosureCrawlspaceType = factor(c(1, 0, 0)),
  elevatedBuildingIndicator = factor(c(0, 0, 0)),
  locationOfContents = factor(c(2, 4, 6)),
  numberOfFloorsInTheInsuredBuilding = as.integer(c(2, 3, 2)),
  floodWaterDuration = as.integer(c(1, 1, 1)),
  numberOfUnits = as.integer(c(2, 2, 3)),
  waterDepth =  c(1.8, 1.8, 1.8),
  buildingAgeAtLoss = c(20, 15, 10),
  occupancyType = c(1, 2, 3),
  PRCP = c(3, 3, 3),
  WSF5 = c(80, 80, 80)
)

# Convert categorical variables to factors in new data
new_data_glm_non_profit <- new_data_glm_non_profit %>%
  mutate(
    basementEnclosureCrawlspaceType = factor(basementEnclosureCrawlspaceType, levels = levels(filtered_dataset_MS_non_profit$basementEnclosureCrawlspaceType)),
    elevatedBuildingIndicator = factor(elevatedBuildingIndicator, levels = levels(filtered_dataset_MS_non_profit$elevatedBuildingIndicator)),
    locationOfContents = factor(locationOfContents, levels = levels(filtered_dataset_MS_non_profit$locationOfContents)),
    numberOfFloorsInTheInsuredBuilding = factor(numberOfFloorsInTheInsuredBuilding, levels = levels(filtered_dataset_MS_non_profit$numberOfFloorsInTheInsuredBuilding)),
    occupancyType = factor(occupancyType, levels = levels(filtered_dataset_MS_non_profit$occupancyType))
  )

# Make predictions
predicted_loss_values_glm_non_profit_MS <- predict(model_MS_non_profit, 
                                                   newdata = new_data_glm_non_profit, 
                                                   type = "response")

# Print predictions
print(predicted_loss_values_glm_non_profit_MS)
# 5226.345 10333.926 10333.926 




library(caret)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Prepare the data
model_data <- filtered_dataset_MS_non_profit[, c("buildingDamageAmount", 
                                                 "basementEnclosureCrawlspaceType",
                                                 "elevatedBuildingIndicator",
                                                 #"locationOfContents",
                                                 #"numberOfFloorsInTheInsuredBuilding",
                                                 #"floodWaterDuration",
                                                 #"numberOfUnits",
                                                 "waterDepth",
                                                 "occupancyType",
                                                 "buildingAgeAtLoss")]

# Define the number of folds
# Identify factor variables
factor_vars <- c("basementEnclosureCrawlspaceType")

# Function to ensure all levels are present
ensure_all_levels <- function(data, factor_vars) {
  for (var in factor_vars) {
    all_levels <- unique(data[[var]])
    data[[var]] <- factor(data[[var]], levels = all_levels)
  }
  return(data)
}

# Apply the function to the entire dataset
model_data <- ensure_all_levels(model_data, factor_vars)

# Define the number of folds
k <- 10

# Create folds
folds <- createFolds(model_data$buildingDamageAmount, k = k, list = TRUE, returnTrain = FALSE)

# Function to calculate RMSE for Gamma GLM
rmse_gamma <- function(pred, obs) {
  sqrt(mean((pred - obs)^2))
}

# Perform k-fold cross-validation
cv_results <- lapply(folds, function(test_indices) {
  train_data <- model_data[-test_indices, ]
  test_data <- model_data[test_indices, ]
  
  # Ensure both train and test data have all levels
  train_data <- ensure_all_levels(train_data, factor_vars)
  test_data <- ensure_all_levels(test_data, factor_vars)
  
  # Fit the model on training data
  tryCatch({
    model <- glm(buildingDamageAmount ~ ., 
                 data = train_data, 
                 family = Gamma(link = "log"))
    
    # Make predictions on test data
    predictions <- predict(model, newdata = test_data, type = "response")
    
    # Calculate RMSE
    rmse <- rmse_gamma(predictions, test_data$buildingDamageAmount)
    
    return(rmse)
  }, error = function(e) {
    message("Error in fold: ", e$message)
    return(NA)
  })
})

# Remove any NA results
cv_results <- cv_results[!is.na(cv_results)]

# Calculate mean RMSE across all folds
mean_rmse <- mean(unlist(cv_results))
print(paste("Mean RMSE:", mean_rmse))

# Calculate standard deviation of RMSE
sd_rmse <- sd(unlist(cv_results))
print(paste("Standard Deviation of RMSE:", sd_rmse))

# Print summary of factor levels
for (var in factor_vars) {
  print(paste("Levels in", var, ":"))
  print(levels(model_data[[var]]))
}





mean(predicted_loss_values_glm_non_profit_MS)*prob_exceed_MS