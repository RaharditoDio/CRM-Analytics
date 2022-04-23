url <- 'https://assets.datacamp.com/production/repositories/2292/datasets/4407050e9b8216249a6d5ff22fd67fd4c44e7301/click_data.csv'
click_data <- read.csv( url )
click_data

# Find oldest and most recent date
min(click_data$visit_date)
max(click_data$visit_date)

# Calculate the mean conversion rate
library(tidyverse)
click_data %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Calculate the mean conversion rate by month of the year
library(lubridate)
click_data %>%
  group_by(month(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Calculate the mean conversion rate by week of the year
click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Calculate the mean conversion rate by day of the week
click_data %>%
  group_by(wday(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Compute conversion rate by week of the year
click_data_sum <- click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Build plot
library(ggplot2)
library(scales)
month_abs <- c( 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
ggplot(click_data_sum, aes(x = `week(visit_date)`,
                           y = conversion_rate)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  scale_x_continuous( breaks = seq( 0,55, by=5 ), labels = month_abs )

# Load powerMediation
library(powerMediation)

# Compute and look at sample size for experiment in August
total_sample_size <- SSizeLogisticBin( p1 = 0.54, #baseline
                                       p2 = 0.64, #our expected guess for the test condition
                                       B = 0.5, #typical val
                                       alpha = 0.05, #typical val
                                       power = 0.8 ) #typical val

res <- paste( 'Total Sample Size:', total_sample_size, 
              '\nSize for each condition:', total_sample_size/2 )
cat( res, sep = '\n' )

# Compute and look at sample size for experiment in August with a 5 percentage point increase
total_sample_size <- SSizeLogisticBin(p1 = 0.54,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
res <- paste( 'Total Sample Size:', total_sample_size, 
              '\nSize for each condition:', total_sample_size/2 )
cat( res, sep = '\n' )

url <- 'https://assets.datacamp.com/production/repositories/2292/datasets/52b52cb1ca28ce10f9a09689325c4d94d889a6da/experiment_data.csv'
experimental_data <- read.csv( url )
View(experimental_data)

# Group and summarize data
experimental_data_sum <- experimental_data %>%
  group_by( visit_date, condition ) %>%
  summarise( conversion_rate = mean( clicked_adopt_today ) )

#experimental_data_sum <- experimental_data %>%
  #group_by( visit_date, condition ) %>%
  #dplyr::summarize( conversion_rate = mean( clicked_adopt_today ) )

# Make plot of conversion rates over time
ggplot( experimental_data_sum,
        aes( x = visit_date,
             y = conversion_rate,
             color = condition,
             group = condition ) ) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = percent)

# Load package for cleaning model results
library(broom)

# View summary of results
experimental_data %>%
  group_by(condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Run logistic regression
experimental_results <- glm(clicked_adopt_today ~ condition,
                          family = "binomial",
                          data = experimental_data) %>%
  tidy()
experimental_results

# Load package for running power analysis
library(powerMediation)

# Run logistic regression power analysis
total_sample_size <- SSizeLogisticBin(p1 = 0.39,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size
