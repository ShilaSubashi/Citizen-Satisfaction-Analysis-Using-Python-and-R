#Question 3
#Task 3.1
# Create vectors
satisfaction <- c(7.75, 4.25, 8.25, 6.00, 6.75, 4.25, 8.25, 4.50)
income <- c(1200, 650, 1400, 580, 900, 700, 1100, 750)

names(satisfaction) <- c('Blloku','Kombinat','Sauk','Laprakë',
                         'Selitë','Kinostudio','Fresku','Don Bosko')
names(income) <- names(satisfaction)

# (a) Logical indexing
#Logical conditions are applied directly to vectors:
high_sat <- satisfaction[satisfaction > 7.0]
low_income <- income[income < 800]
both_conditions <- satisfaction[(satisfaction > 7.0) & (income < 800)]

high_sat
low_income
both_conditions
#No neighbourhood satisfies both high satisfaction (>7.0) and low income (<800 EUR), indicating that higher satisfaction tends to be associated with higher-income areas.

# (b) Value index
value_index <- satisfaction / income * 1000
value_index

highest_value <- names(value_index)[which.max(value_index)]
highest_value

#Laprakë provides the highest satisfaction relative to income, suggesting strong perceived value despite lower income levels.

# (c) Sorting and extremes
sorted_asc <- sort(satisfaction)
sorted_desc <- rev(sort(satisfaction))

highest_neighbourhood <- names(satisfaction)[which.max(satisfaction)]
lowest_neighbourhood <- names(satisfaction)[which.min(satisfaction)]

sorted_asc
sorted_desc
highest_neighbourhood
lowest_neighbourhood

#Sauk and Fresku share the highest values, while Kombinat and Kinostudio have the lowest satisfaction scores.


# (d) Factor classification
satisfaction_level <- ifelse(satisfaction < 5, "Low",
                      ifelse(satisfaction <= 7, "Medium", "High"))

satisfaction_level <- factor(satisfaction_level,
                             levels = c("Low", "Medium", "High"),
                             ordered = TRUE)

table(satisfaction_level)

#The distribution is balanced, with equal representation of high and low satisfaction, and fewer medium-level cases.

#Task 3.2
# Create data frame
neighbourhood_df <- data.frame(
  name = c('Blloku','Kombinat','Sauk','Laprakë',
           'Selitë','Kinostudio','Fresku','Don Bosko'),
  satisfaction = c(7.75, 4.25, 8.25, 6.00,
                   6.75, 4.25, 8.25, 4.50),
  population = c(15000,22000,12000,25000,
                 18000,20000,10000,16000),
  area_km2 = c(1.8, 3.5, 2.2, 4.0, 2.8, 3.0, 1.5, 2.5),
  income = c(1200, 650, 1400, 580, 900, 700, 1100, 750),
  stringsAsFactors = FALSE
)

# (a) Structure and summary
str(neighbourhood_df)
summary(neighbourhood_df)

#str() shows the structure:
#8 observations, 5 variables
#numeric variables: satisfaction, population, area_km2, income
#character variable: name

#summary() provides:
#min, median, mean, max for numeric columns
#quick overview of data distribution

# (b) Density and filtering
neighbourhood_df$density <- neighbourhood_df$population / neighbourhood_df$area_km2

high_density <- neighbourhood_df[neighbourhood_df$density > 6000, ]
high_density

#These areas are more densely populated and may require more infrastructure and services.

# (c) Subset with conditions
subset_df <- subset(neighbourhood_df,
                    satisfaction > 6.0 & income < 1000,
                    select = c(name, satisfaction, income))

subset_df

#Selitë is the only neighbourhood with relatively high satisfaction despite moderate income levels.

# (d) tapply with income group
income_group <- ifelse(neighbourhood_df$income < 800, "Low", "High")

mean_satisfaction <- tapply(neighbourhood_df$satisfaction,
                            income_group,
                            mean)

mean_satisfaction

#Neighbourhoods with higher income levels tend to have significantly higher satisfaction scores, suggesting a positive relationship between income and perceived quality of services.

# (e) lapply on satisfaction
result_list <- lapply(satisfaction, function(x) {
  list(
    original = x,
    classification = ifelse(x < 5, "Low",
                      ifelse(x <= 7, "Medium", "High")),
    squared = x^2
  )
})

result_list

#Neighbourhoods with higher income levels tend to have significantly higher satisfaction scores, suggesting a positive relationship between income and perceived quality of services.

#Conclusion for Question 3
#The analysis highlights that higher-income neighbourhoods generally report higher satisfaction, while areas like Laprakë provide strong value relative to income.