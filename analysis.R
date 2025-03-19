# Assessment Sales Ratio Analysis
# This script analyzes the relationship between property sale prices and assessment values
# to identify potential regressivity in the property tax system.

# Load required libraries
library(tidyverse)
library(scales)
library(modelsummary)
library(kableExtra)
library(readxl)

# ------ Data Loading and Preparation ------
# Load the street data from the vision portal so I have all the assessment values
all_streets <- readRDS("all_streets_data.rds") %>% 
drop_na(appraisal) %>% 
distinct() %>% 
filter(model == "Residential")

# Read the sales data from Excel file
sales_data <- read_excel("sales_data.xlsx")

# Display basic information about the sales data
glimpse(sales_data)

# Summarize the sales data
summary(sales_data)

# Clean up column names and prepare the sales data
# Convert column names to snake_case for consistency with tidyverse style
sales_data <- sales_data %>%
  rename(
    sale_price = `Sale Price`,
    sale_date = `Sale Date`,
    appraised_value = `Current Year Appraisal`,
    assessed_value = `Current Year Assessment`,
    property_class = `Property Class`,
    property_number = `Property Number`,
    street_name = `Street Name`
  ) %>%
  mutate(asr = appraised_value / sale_price) %>%
  # Filter for residential properties (Property Class "R")
  filter(property_class == "R")

# Calculate median sale price
median_sale_price <- median(sales_data$sale_price)
print(paste("Median sale price:", scales::dollar(median_sale_price)))

# Add derived variables
sales_data <- sales_data %>%
  mutate(
    years_since_ref = as.numeric(difftime(as.Date("2024-10-01"), sale_date, units = "days")) / 365,
    sale_price_k = sale_price / 1000,
    sale_price_100k = sale_price / 100000
    # Note: ASR is already calculated in the data
  )

# Calculate price quantiles for analysis
p10 <- quantile(sales_data$sale_price, 0.10)
p90 <- quantile(sales_data$sale_price, 0.90)

# Calculate total Assessment-Sales Ratio (ASR)
total_asr <- sum(sales_data$appraised_value) / sum(sales_data$sale_price)
print(paste("Overall Assessment-Sales Ratio:", round(total_asr, 4)))

# Create filtered datasets for sensitivity analysis
# Dataset excluding high-value properties
sales_data_no_high <- sales_data %>%
  filter(sale_price < 2000000)

# Dataset with middle 80% of sales by price
sales_data_trimmed <- sales_data %>%
  arrange(sale_price) %>%
  slice(round(0.1 * n()):round(0.9 * n()))

# Calculate ASR separately for properties below/above median sale price
asr_below_median <- sales_data %>%
  filter(sale_price <= median_sale_price) %>%
  summarise(asr = sum(appraised_value) / sum(sale_price)) %>%
  pull(asr)

asr_above_median <- sales_data %>%
  filter(sale_price > median_sale_price) %>%
  summarise(asr = sum(appraised_value) / sum(sale_price)) %>%
  pull(asr)

print(paste("ASR for properties below median sale price:", round(asr_below_median, 4)))
print(paste("ASR for properties above median sale price:", round(asr_above_median, 4)))

# ------ Statistical Test: Compare ASR between Below and Above Median Properties ------

# Create datasets for below and above median properties
below_median_data <- sales_data %>%
  filter(sale_price <= median_sale_price)

above_median_data <- sales_data %>%
  filter(sale_price > median_sale_price)

# Perform t-test to compare ASR between below and above median properties
asr_t_test <- t.test(
  below_median_data$asr,
  above_median_data$asr,
  alternative = "two.sided",
  var.equal = FALSE
)

# Print t-test results
cat("\n----- T-Test Results: ASR Below vs Above Median Sale Price -----\n")
cat("ASR Below Median Mean:", round(mean(below_median_data$asr, na.rm = TRUE), 4), "\n")
cat("ASR Above Median Mean:", round(mean(above_median_data$asr, na.rm = TRUE), 4), "\n")
cat("Difference in Means:", round(mean(below_median_data$asr, na.rm = TRUE) - mean(above_median_data$asr, na.rm = TRUE), 4), "\n")
cat("t-statistic:", round(asr_t_test$statistic, 4), "\n")
cat("p-value:", format.pval(asr_t_test$p.value, digits = 4), "\n")
cat("95% Confidence Interval:", paste(round(asr_t_test$conf.int, 4), collapse = " to "), "\n")

# Interpret the results
cat("\nInterpretation: ")
if (asr_t_test$p.value < 0.05) {
  cat("There is a statistically significant difference in ASR between properties below and above the median sale price (p < 0.05).\n")
} else {
  cat("There is no statistically significant difference in ASR between properties below and above the median sale price (p >= 0.05).\n")
}

# ------ Visualization: Assessment Sales Ratio by Sale Price ------

# Plot 1: All data with smooth curve
p1 <- ggplot(sales_data, aes(x = sale_price, y = asr)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = p10, linetype = "dashed", color = "red") +
  geom_vline(xintercept = p90, linetype = "dashed", color = "red") +
  geom_hline(yintercept = total_asr, linetype = "dashed", color = "green") +
  scale_x_continuous(
    labels = scales::dollar_format(),
    breaks = seq(0, max(sales_data$sale_price), by = 100000),
    guide = guide_axis(angle = 90)
  ) +
  scale_y_continuous(
    breaks = seq(0, max(sales_data$asr, na.rm = TRUE), by = 0.05),
    labels = scales::number_format()
  ) +
  labs(
    title = "Assessment Sales Ratio by Sale Price",
    x = "Sale Price", 
    y = "Assessment Sales Ratio (Appraised Value / Sale Price)",
    caption = "Red dashed lines show 10th and 90th percentiles, grey line shows regression line, blue line shows loess curve, green line shows overall ASR"
  ) +
  theme_minimal()

print(p1)

# Plot 2: All data with horizontal lines showing ASR for above/below median properties
p2 <- ggplot(sales_data, aes(x = sale_price, y = asr)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  geom_vline(xintercept = p10, linetype = "dashed", color = "red") +
  geom_vline(xintercept = p90, linetype = "dashed", color = "red") +
  # Add horizontal lines for ASR above/below median
  geom_segment(
    aes(x = 0, xend = median_sale_price, y = asr_below_median, yend = asr_below_median),
    linetype = "dashed", color = "purple"
  ) +
  geom_segment(
    aes(x = median_sale_price, xend = max(sales_data$sale_price), y = asr_above_median, yend = asr_above_median),
    linetype = "dashed", color = "purple"
  ) +
  scale_x_continuous(
    labels = scales::dollar_format(),
    breaks = seq(0, max(sales_data$sale_price), by = 100000),
    guide = guide_axis(angle = 90)
  ) +
  scale_y_continuous(
    breaks = seq(0, max(sales_data$asr, na.rm = TRUE), by = 0.05),
    labels = scales::number_format()
  ) +
  labs(
    title = "Assessment Sales Ratio by Sale Price",
    x = "Sale Price", 
    y = "Assessment Sales Ratio (Appraised Value / Sale Price)",
    caption = "Red dashed lines show 10th and 90th percentiles"
  ) +
  theme_minimal()

print(p2)

# Plot 3: Middle 80% of sales
p3 <- ggplot(sales_data_trimmed, aes(x = sale_price, y = asr)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_segment(
    aes(x = min(sale_price), xend = median_sale_price, y = asr_below_median, yend = asr_below_median),
    linetype = "dashed", color = "purple"
  ) +
  geom_segment(
    aes(x = median_sale_price, xend = max(sale_price), y = asr_above_median, yend = asr_above_median),
    linetype = "dashed", color = "purple"
  ) +
  geom_vline(xintercept = median(sales_data_trimmed$sale_price), linetype = "dashed", color = "orange") +
  scale_x_continuous(
    labels = scales::dollar_format(),
    breaks = seq(0, max(sales_data_trimmed$sale_price), by = 100000),
    guide = guide_axis(angle = 90)
  ) +
  scale_y_continuous(
    breaks = seq(0, max(sales_data_trimmed$asr, na.rm = TRUE), by = 0.05),
    labels = scales::number_format()
  ) +
  labs(
    title = "Assessment Sales Ratio by Sale Price (Middle 80% of Sales)",
    x = "Sale Price",
    y = "Assessment Sales Ratio (Appraised Value / Sale Price)",
    caption = "Orange dashed line shows median sale price. Purple lines show ASR for below median and above median properties."
  ) +
  theme_minimal()

print(p3)

# ------ Tax Impact Analysis ------

# Calculate new mill rate based on 40% grand list growth
new_mill_rate <- 46.65

# Calculate average tax differences based on ASR differences
tax_analysis <- sales_data %>%
  mutate(
    # Calculate actual taxes paid based on appraised value
    actual_tax = assessed_value * (new_mill_rate / 1000),
    # Calculate what taxes should be if ASR was uniform
    fair_tax = sale_price * 0.7 * (new_mill_rate / 1000),
    # Calculate over/under payment
    tax_difference = actual_tax - fair_tax,
    # Calculate over/under payment as proportion of assessed value
    tax_diff_proportion = tax_difference / appraised_value,
    # Calculate effective mill rate (tax per $1000 of market value)
    effective_mill_rate = (actual_tax / sale_price) * 1000
  ) %>%
  group_by(sale_price > median_sale_price) %>%
  summarise(
    n = n(),
    avg_difference = mean(tax_difference),
    avg_proportion = mean(tax_diff_proportion),
    .groups = "drop"
  )

# Calculate deciles of sale price
sales_data <- sales_data %>%
  mutate(
    decile = ntile(sale_price, 10),
    # Calculate effective mill rate (tax per $1000 of market value)
    effective_mill_rate = (assessed_value * (new_mill_rate / 1000) / sale_price) * 1000/0.7
  )

# Calculate average effective mill rate by decile with standard error
mill_rate_by_decile <- sales_data %>%
  group_by(decile) %>%
  summarise(
    n = n(),
    min_sale_price = min(sale_price),
    max_sale_price = max(sale_price),
    avg_sale_price = mean(sale_price),
    avg_effective_mill_rate = mean(effective_mill_rate),
    sd_effective_mill_rate = sd(effective_mill_rate),
    se_effective_mill_rate = sd(effective_mill_rate) / sqrt(n()),
    .groups = "drop"
  ) %>%
  # Create labels with price ranges
  mutate(
    price_range = sprintf("$%s-$%s", 
                         scales::comma(round(min_sale_price)), 
                         scales::comma(round(max_sale_price)))
  )

# Create a point plot of effective mill rate by decile with error bars
p_mill_rate <- ggplot() +
  # Add raw data points in the background with very low alpha
  geom_point(data = sales_data, 
             aes(x = factor(decile), y = effective_mill_rate),
             alpha = 0.05, color = "gray50") +
  # Add summary points and error bars
  geom_point(data = mill_rate_by_decile,
             aes(x = factor(decile), y = avg_effective_mill_rate),
             size = 3, color = "steelblue") +
  geom_errorbar(data = mill_rate_by_decile,
                aes(x = factor(decile),
                    ymin = avg_effective_mill_rate - se_effective_mill_rate, 
                    ymax = avg_effective_mill_rate + se_effective_mill_rate),
                width = 0.2, color = "steelblue") +
  geom_text(data = mill_rate_by_decile,
            aes(x = factor(decile), 
                y = avg_effective_mill_rate,
                label = sprintf("%.1f", avg_effective_mill_rate)), 
            vjust = -1.5, size = 3.5) +
  geom_hline(yintercept = new_mill_rate, linetype = "dashed", color = "red") +
  scale_x_discrete(labels = mill_rate_by_decile$price_range) +
  scale_y_continuous(limits = c(min(mill_rate_by_decile$avg_effective_mill_rate - 
                                       mill_rate_by_decile$se_effective_mill_rate), 
                                max(mill_rate_by_decile$avg_effective_mill_rate + 
                                       mill_rate_by_decile$se_effective_mill_rate) * 1.1)) +
  labs(
    title = "Effective Mill Rate by Sale Price Decile",
    subtitle = paste("Nominal mill rate =", round(new_mill_rate, 2), "shown in red"),
    x = "Sale Price Range",
    y = "Effective Mill Rate (Mills per $1000 of Market Value)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_mill_rate)

# Calculate median appraisal value
median_appraisal <- median(sales_data$appraised_value, na.rm = TRUE)

# Apply average overpayment/underpayment to all properties
all_streets_tax_impact <- all_streets %>%
  mutate(
    is_above_median = appraisal > median_appraisal,
    # Apply the average difference based on whether property is above/below median
    tax_difference = case_when(
      is_above_median ~ tax_analysis$avg_difference[2],
      !is_above_median ~ tax_analysis$avg_difference[1],
      TRUE ~ 0
    )
  ) %>%
  group_by(is_above_median) %>%
  summarise(
    n = n(),
    total_difference = sum(tax_difference, na.rm = TRUE),
    .groups = "drop"
  )

# Update tax_analysis with the new total differences
tax_analysis <- tax_analysis %>%
  mutate(
    total_difference = case_when(
      row_number() == 1 ~ all_streets_tax_impact$total_difference[!all_streets_tax_impact$is_above_median],
      row_number() == 2 ~ all_streets_tax_impact$total_difference[all_streets_tax_impact$is_above_median],
      TRUE ~ NA_real_
    )
  )

# Print tax impact analysis results
cat("\n----- Tax Impact Analysis -----\n")
cat("New mill rate after 40% grand list growth:", round(new_mill_rate, 2), "\n")

cat("\nProperties under median sale price (n=", tax_analysis$n[1], ", N = ", all_streets_tax_impact$n[1], "):\n", 
    "Average overpayment: $", round(tax_analysis$avg_difference[1], 2),
    " (", scales::percent(tax_analysis$avg_proportion[1], accuracy = 0.01), " of appraised value)",
    "\nTotal overpayment: $", round(tax_analysis$total_difference[1], 2), "\n", sep = "")

cat("\nProperties over median sale price (n=", tax_analysis$n[2], ", N = ", all_streets_tax_impact$n[2], "):\n",
    "Average underpayment: $", round(-tax_analysis$avg_difference[2], 2),
    " (", scales::percent(-tax_analysis$avg_proportion[2], accuracy = 0.01), " of appraised value)",
    "\nTotal underpayment: $", round(-tax_analysis$total_difference[2], 2), "\n", sep = "")

# ------ Total Town Revenue at new mill rate ------

# Calculate total revenue based on new mill rate
total_revenue <- sum(all_streets$appraisal, na.rm = TRUE) * (new_mill_rate * 0.7 / 1000)

cat("\n----- Total Town Revenue Analysis -----\n")
cat("Total Town Revenue at new mill rate:", scales::dollar(total_revenue), "\n")

# Calculate proportion of total revenue from tax differences
total_tax_difference <- abs(tax_analysis$total_difference[1]) + abs(tax_analysis$total_difference[2])
tax_diff_proportion <- total_tax_difference / total_revenue

cat("Proportion of total revenue represented by tax differences:", 
    scales::percent(tax_diff_proportion, accuracy = 0.01), "\n")

# ------ Net Subsidy Analysis ------

# Calculate the net subsidy provided by below median households
net_subsidy <- abs(tax_analysis$total_difference[1]) + abs(tax_analysis$total_difference[2])

cat("\n----- Net Subsidy Analysis -----\n")
cat("Net difference (subsidy from below median to above median households): $", 
    scales::dollar(net_subsidy, accuracy = 1), "\n")


# ------ Additional Visualization of ASR by Sale Price ------

# Create plotting function for consistent visualization
plot_asr <- function(data, title_suffix = "") {
  ggplot(data, aes(x = sale_price, y = asr)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_y_continuous(labels = scales::number_format()) +
    labs(
      title = paste0("Assessment Sales Ratio by Sale Price", title_suffix),
      x = "Sale Price",
      y = "Assessment Sales Ratio (Appraised Value / Sale Price)"
    ) +
    theme_minimal()
}

# Generate the three plots with different datasets
p4 <- plot_asr(sales_data, " (All Data)")
p5 <- plot_asr(sales_data_no_high, " (No High Values)")
p6 <- plot_asr(sales_data_trimmed, " (Middle 80% of Sales)")

# Print the plots
print(p4)
print(p5)
print(p6)

# ------ Regression Analysis ------

# 1. Regression of ASR on Sale Price and Years Since Reference Date
cat("\n----- Regression Analysis of ASR on Sale Price -----\n")

# Run three regression models
model1 <- lm(asr ~ sale_price_100k + years_since_ref, data = sales_data)
model2 <- lm(asr ~ sale_price_100k + years_since_ref, data = sales_data_no_high)
model3 <- lm(asr ~ sale_price_100k + years_since_ref, data = sales_data_trimmed)

# Define model names
model_names <- c("All Data", "No High Values", "Middle 80%")

# Create and print the regression table
reg_table1 <- modelsummary(
  list(model1, model2, model3),
  title = "Regression of ASR on Sale Price and Years Since Reference Date",
  stars = TRUE,
  gof_map = c("r.squared"),
  coef_map = c(
    "sale_price_100k" = "Sale Price (hundreds of thousands)",
    "years_since_ref" = "Years Since Reference Date"
  ),
  notes = "Outcome is ASR. Sale price is measured in hundreds of thousands of dollars. First column is all data, second column is data excluding properties over $2 million, third column is middle 80% of sales by price.",
)
reg_table1

# Print interpretation
cat("\nInterpretation of regression results:\n")
cat("- Negative coefficient on Sale Price indicates regressivity\n")
cat("- Coefficient magnitude indicates degree of regressivity\n")
cat("- Years since reference date controls for time trends\n")

# 2. Regression of Appraised Value on Sale Price
cat("\n----- Regression Analysis of Appraised Value on Sale Price -----\n")

# Run three regression models
model4 <- lm(appraised_value ~ sale_price + years_since_ref, data = sales_data)
model5 <- lm(appraised_value ~ sale_price + years_since_ref, data = sales_data_no_high)
model6 <- lm(appraised_value ~ sale_price + years_since_ref, data = sales_data_trimmed)

# Create and print the regression table
reg_table2 <- modelsummary(
  list(model4, model5, model6),
  title = "Regression of Appraised Value on Sale Price and Years Since Reference Date",
  stars = TRUE,
  gof_map = c("r.squared"),
  coef_map = c(
    "sale_price" = "Sale Price",
    "years_since_ref" = "Years Since Reference Date"
  ),
  notes = "Outcome is appraised value. First column is all data, second column is data excluding properties over $2 million, third column is middle 80% of sales by price.",
)
reg_table2

# Print interpretation
cat("\nInterpretation of regression results:\n")
cat("- Negative coefficient on Sale Price indicates regressivity\n")
cat("- Coefficient magnitude indicates degree of regressivity\n")
cat("- Years since reference date controls for time trends\n")

# 3. Regression of log Appraised Value on log Sale Price
cat("\n----- Regression Analysis of log Appraised Value on log Sale Price -----\n")

# Run three regression models
model7 <- lm(log(appraised_value) ~ log(sale_price) + years_since_ref, data = sales_data)
model8 <- lm(log(appraised_value) ~ log(sale_price) + years_since_ref, data = sales_data_no_high)
model9 <- lm(log(appraised_value) ~ log(sale_price) + years_since_ref, data = sales_data_trimmed)

# Create and print the regression table
reg_table3 <- modelsummary(
  list(model7, model8, model9),
  title = "Regression of log Appraised Value on log Sale Price and Years Since Reference Date",
  stars = TRUE,
  gof_map = c("r.squared"),
  coef_map = c(
    "log(sale_price)" = "log(Sale Price)",
    "years_since_ref" = "Years Since Reference Date"
  ),
  notes = "Outcome is log appraised value. First column is all data, second column is data excluding properties over $2 million, third column is middle 80% of sales by price. The interpretation of the coefficient is that a 1 percent increase in sale price as associated with a less than 1 percent increase in appraised value, especially in the middle 80% of sales by price.",
)
reg_table3
# Print overall interpretation
cat("\nOverall Interpretation:\n")
cat("- The negative coefficient on Sale Price in ASR models indicates regressivity\n")
cat("- Lower appraised values relative to sale prices for higher-priced properties\n")
cat("- This regressivity creates inequity in the property tax system\n")
cat("- Lower-valued properties subsidize higher-valued properties\n")


# ------ Sales Ratio by Price Decile Analysis ------

# Prepare the data by getting the minimum and maximum sale price of each decile
graph_data <- sales_data %>%
  mutate(decile = ntile(sale_price, 10)) %>%
  group_by(decile) %>%
  mutate(decile_label = paste(
    scales::dollar(min(sale_price) / 1000, accuracy = 1, suffix = "K"),
    scales::dollar(max(sale_price) / 1000, accuracy = 1, suffix = "K"),
    sep = " - "
  )) %>%
  # Handle the highest decile specially
  mutate(decile_label = ifelse(
    decile == 10, 
    paste0("$", scales::dollar(min(sale_price) / 1000, accuracy = 1, suffix = "K"), "+"), 
    decile_label
  )) %>%
  mutate(decile_label = forcats::fct_reorder(decile_label, decile))

# Calculate summary statistics including error measures
graph_data <- graph_data %>%
  group_by(decile_label) %>%
  summarise(
    n = n(),
    `Median Sales Ratio` = median(asr),
    mean_asr = mean(asr),
    sd_asr = sd(asr),
    se_asr = sd_asr / sqrt(n),
    lower_ci = `Median Sales Ratio` - qt(0.975, n-1) * se_asr,
    upper_ci = `Median Sales Ratio` + qt(0.975, n-1) * se_asr
  )

# Create a plot of sales ratio by decile with error bars
p4 <- ggplot(graph_data, aes(x = decile_label, y = `Median Sales Ratio`)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  labs(
    title = "Median Assessment Sales Ratios by Price Decile",
    subtitle = "Properties grouped by decile of sale price (with 95% confidence intervals)",
    x = "Price Decile Range",
    y = "Assessment Sales Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 6)),
    legend.position = "bottom"
  )

print(p4)

# ------ Lorenz Curve Analysis ------

# Prepare data for Lorenz curve
gini_data <- sales_data %>%
  select(sale_price, appraised_value) %>%
  arrange(sale_price)

sale_price <- gini_data$sale_price
assessed <- gini_data$appraised_value

# Create Lorenz curve data for sale price
lorenz_data_price <- data.frame(
  pct = c(0, cumsum(sale_price) / sum(sale_price)),
  cum_pct = c(0, seq_along(sale_price)) / length(sale_price)
)

# Create Lorenz curve data for assessed value
lorenz_data_assessed <- data.frame(
  pct = c(0, cumsum(assessed) / sum(assessed)),
  cum_pct = c(0, seq_along(assessed)) / length(assessed)
)

# Create Lorenz curve plot
p5 <- ggplot() +
  geom_line(
    data = lorenz_data_price,
    aes(x = cum_pct, y = pct), color = "blue"
  ) +
  geom_line(
    data = lorenz_data_assessed,
    aes(x = cum_pct, y = pct), color = "red"
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "green") +
  geom_text(
    data = data.frame(x = 0.8, y = 0.48, label = "Sale Price"),
    aes(x, y, label = label), color = "blue", vjust = 1
  ) +
  geom_text(
    data = data.frame(x = 0.5, y = 0.45, label = "Assessed Value"),
    aes(x, y, label = label), color = "red", vjust = 1
  ) +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Lorenz Curve for Sales and Assessed Values",
    x = "Percent of Properties",
    y = "Percent of Value"
  ) +
  theme_minimal()

print(p5)


# Calculate Gini coefficients and Kakwani Index

# Calculate the sum of the n elements of the assessed vector
n <- length(assessed)
g_assessed <- sum(assessed * seq_len(n))

# Compute the Gini coefficient based on the previously calculated sum
# and the increasing sum of all elements in the assessed vector
g_assessed <- 2 * g_assessed / sum(assessed) - (n + 1L)

# Normalize the Gini coefficient by dividing it by n.
gini_assessed <- g_assessed / n

# Follow the same process for the sale_price vector
g_sale <- sum(sale_price * seq_len(n))
g_sale <- 2 * g_sale / sum(sale_price) - (n + 1L)
gini_sale <- g_sale / n

# Calculate Modified Kakwani Index (MKI) and Kakwani Index (KI)
MKI <- round(gini_assessed / gini_sale, 4)
KI <- round(gini_assessed - gini_sale, 4)

# Print the results
cat("Gini coefficient for assessed values:", round(gini_assessed, 4), "\n")
cat("Gini coefficient for sale prices:", round(gini_sale, 4), "\n")
cat("Kakwani Index (KI):", KI, "\n")
cat("Modified Kakwani Index (MKI):", MKI, "\n")


# ------ Save Results ------

# Save the main plots
ggsave("asr_vs_sale_price_all.png", p1, width = 10, height = 7)
ggsave("asr_vs_sale_price_median_comparison.png", p2, width = 10, height = 7)
ggsave("asr_vs_sale_price_middle80.png", p3, width = 10, height = 7)

# Save regression results to a text file
sink("regression_results.txt")
cat("----- Regression Analysis Results -----\n\n")
cat("1. Regression of ASR on Sale Price and Years Since Reference Date\n\n")
print(summary(model1))
cat("\n\n")
print(summary(model2))
cat("\n\n")
print(summary(model3))
cat("\n\n")

cat("2. Regression of Appraised Value on Sale Price and Years Since Reference Date\n\n")
print(summary(model4))
cat("\n\n")
print(summary(model5))
cat("\n\n")
print(summary(model6))
cat("\n\n")

cat("3. Regression of log Appraised Value on log Sale Price and Years Since Reference Date\n\n")
print(summary(model7))
cat("\n\n")
print(summary(model8))
cat("\n\n")
print(summary(model9))
sink() 





spring_glen_streets <- c(
  "THORNTON ST",
  "WAKEFIELD ST", 
  "FILBERT ST",
  "ARDMORE ST",
  "ELGIN ST",
  "WOODLAWN ST",
  "INGRAM ST",
  "ELIHU ST",
  "HARMON ST",
  "HAWTHORNE AVE",
  "CENTRAL AVE",
  "PARK AVE",
  "RUSSELL ST",
  "BEVERLY RD",
  "ATHOL PL",
  "WAITE ST",
  "GREENWAY ST",
  "WOODBINE ST",
  "BEDFORD AVE",
  "HUMISTON AVE",
  "WEATON ST",
  "GORDON ST",
  "CORAM ST"
)

sales_data <- sales_data %>%
  filter(street_name %in% spring_glen_streets)

  # Create a subset of the data for the specific property at 120 Thornton
  highlight_120_thornton <- sales_data %>%
    filter(property_number == 120, str_detect(tolower(street_name), "thornton"))
  
  # Plot Sale Price vs. Appraised Value, highlighting 120 Thornton
  ggplot(sales_data, aes(x = sale_price, y = appraised_value)) +
    geom_point(alpha = 0.5) +
    geom_point(data = highlight_120_thornton, color = "red", size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +  # x=y line
    labs(
      title = "Sale Price vs. Appraised Value with 120 Thornton Highlighted",
      x = "Sale Price",
      y = "Appraised Value"
    ) +
    theme_minimal()

    # Calculate ASR for the specific property at 120 Thornton
    highlight_120_thornton_asr <- sales_data %>%
      filter(property_number == 120, str_detect(tolower(street_name), "thornton")) %>%
      mutate(asr = appraised_value / sale_price)

    # Plot Sale Price vs. ASR, highlighting 120 Thornton
    ggplot(sales_data, aes(x = sale_price, y = appraised_value / sale_price)) +
      geom_point(alpha = 0.5) +
      geom_point(data = highlight_120_thornton_asr, color = "red", size = 3) +
      geom_hline(yintercept = 0.95, linetype = "dashed", color = "blue") +  # Overall ASR line
      labs(
        title = "Sale Price vs. Assessment Sales Ratio with 120 Thornton Highlighted",
        x = "Sale Price",
        y = "Assessment Sales Ratio (Appraised Value / Sale Price)"
      ) +
      theme_minimal()

      # Get the appraised value of the specific property at 120 Thornton
      appraised_value_120_thornton <- highlight_120_thornton$appraised_value

      # Filter sales data for properties with similar appraised values
      similar_appraised_properties <- sales_data %>%
        filter(abs(appraised_value - appraised_value_120_thornton) < 50000)  # Adjust the threshold as needed

      # Plot the distribution of sale prices for similar appraised properties
      ggplot(similar_appraised_properties, aes(x = sale_price)) +
        geom_histogram(binwidth = 5000, fill = "lightblue", color = "black", alpha = 0.7) +
        labs(
          title = "Distribution of Sale Prices for Properties with Similar Appraised Values to 120 Thornton",
          x = "Sale Price",
          y = "Count"
        ) +
        theme_minimal()

        average_appraisal_similar <- similar_appraised_properties %>%
          summarise(average_appraised_value = mean(appraised_value, na.rm = TRUE)) %>%
          pull(average_appraised_value)

        print(paste("Average appraisal for properties within $20k of sale price:", 
                    scales::dollar(average_appraisal_similar)))


        # Plot Appraised Value vs. Sale Price for Similar Appraised Properties, highlighting 120 Thornton
        ggplot(similar_appraised_properties, aes(x = appraised_value, y = sale_price)) +
          geom_point(alpha = 0.5) +
          geom_point(data = highlight_120_thornton, color = "red", size = 3) +
          geom_text(data = similar_appraised_properties, aes(label = paste(property_number, street_name)), 
                    vjust = -1, size = 3, check_overlap = TRUE) +  # Print other addresses with property number
          geom_text(aes(label = round(appraised_value / sale_price, 2)), 
                    vjust = 1.5, size = 3, check_overlap = TRUE) +  # Print ASR for each property
          labs(
            title = "Appraised Value vs. Sale Price for Properties with Similar Appraised Values",
            x = "Appraised Value",
            y = "Sale Price"
          ) +
          theme_minimal()
