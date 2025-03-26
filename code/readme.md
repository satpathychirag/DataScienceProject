---
title: "Project Proposal"
author: "Team 1"
date: "`03/26/2025"
output: pdf
---

## Project Description

\- **Air Quality Data:** Daily PM2.5 levels from sensors across New York, 2010-2018.

\- **Weather Data:** Hourly temperature, humidity, wind speed, and other meteorological factors, 2010-2018.

This assignment will help you practice skills in **tidyverse, data wrangling, visualization, iteration, and functions**.

## Research Question

### 1. Data Loading & Exploration

#### Load the Data

-   Import the CSV files using `read_csv()` from **tidyverse**. Make sure you download the csv file from Canvas.

#### Explore the Data

-   Use `summary()`, `str()`, and `head()` to understand the datasets.
-   Compute key statistics using `mean()`, `sd()`, `min()`, `max()`, and `median()` for relevant numerical variables.
-   Use `ggplot2` to visualize distributions with histograms or boxplots for key variables you identified.
-   **Hint:** Consider adding a density plot with `geom_density.`

```{r load-data}
library(tidyverse)
library(lubridate)

air_quality <- read_csv("data_NY_Qu_pm2.5.csv")
weather_data <- read_csv("jfk_weather_cleaned.csv")

str(air_quality)
str(weather_data)

summary(air_quality)
summary(weather_data)

view(air_quality)
view(weather_data)


 

```

```{r}
# Your code here

key_statistic <- function(data, cols) {
  
  df_subset <- data |> select(all_of(cols))
  
  stats <- df_subset |>
    summarise(across(everything(), 
                     list(
                       mean = ~ mean(.x, na.rm = TRUE),
                       sd = ~ sd(.x, na.rm = TRUE),
                       min = ~ min(.x, na.rm = TRUE),
                       max = ~ max(.x, na.rm = TRUE),
                       median = ~ median(.x, na.rm = TRUE)
                     )
    )) |>
    pivot_longer(
      cols = everything(),
      names_to = c("variable", ".value"),
      names_sep = "_"
    )
  

  plots <- lapply(cols, function(var) {
    ggplot(df_subset, aes_string(x = var)) +
      geom_histogram(aes(y = ..density..),
                      fill = "yellow", color = "black", alpha = 0.7) +
      geom_density(color = "red",size=1) +
      labs(title = paste("Distribution of", var),
           x = var,
           y = "Value") +
      theme_minimal()
  })
  
  print(stats)
  print(plots)
}

key_statistic(air_quality, c("Daily.Mean.PM2.5.Concentration", "Daily.AQI.Value"))
key_statistic(weather_data,c("HOURLYRelativeHumidity","HOURLYWindSpeed","HOURLYDRYBULBTEMPF"))



```

### 2. Data Cleaning & Merging

#### Time Alignment

1.  Convert timestamps to proper date-time format at a Day level using `mdy()` and `as_date(mdy_hm()` for air_quality and weather_data, respectively.

2.  Aggregate air quality data to daily averages per site using `Daily.Mean.PM2.5.Concentration`. Call this averaged variable `avg_PM2.5`.

3.  Aggregate weather data to daily averages using `HOURLYDRYBULBTEMPF`, `HOURLYRelativeHumidity`, and `HOURLYWindSpeed`. Call these averaged variables `avg_Temp`, `avg_Humidity`, and `avg_WindSpeed`, respectively.

4.  Are there any other relevant variables that might be useful for the analysis? If so, include them in your aggregate dataset.

5.  Merge the daily air quality and weather data on **Date**.

    **Hint:** Use `full_join()` to ensure no data is lost.

6.  Check for missing values in your new `daily_data` (or however you choose to name the new dataset) using `sum(is.na(daily_data))` to count total NAs.

7.  Identify which columns have missing values using `colSums(is.na(daily_data))`.

8.  Remove rows with missing values using `daily_data<- drop_na(daily_data)`.\
    (another option that will give the same result:\
    `daily_data <- daily_data[complete.cases(daily_data),]`).

```{r clean-merge}
# Converting Time Stamp Code
air_quality$date_resolved <- mdy(air_quality$Date)
weather_data$date_resolved <- as_date(mdy_hm(weather_data$DATE))

view(air_quality)


```

```{r}
#Aggregating Air Quality Data 
air_new <- air_quality |>
  group_by(date_resolved) |>  
  summarise(avg_PM2.5 = mean(Daily.Mean.PM2.5.Concentration, na.rm = T),
            avg_AQI= mean(Daily.AQI.Value, na.rm= T))
print(daily_avg)
```

```{r}
# Agrregating Weather Data
weather_new <- weather_data |>
  group_by(date_resolved) |> 
  summarise(
    avg_Temp = mean(HOURLYDRYBULBTEMPF, na.rm = TRUE),
    avg_Humidity = mean(HOURLYRelativeHumidity, na.rm = TRUE),
    avg_WindSpeed = mean(HOURLYWindSpeed, na.rm = TRUE)
  )
print(daily_weather)
```

```{r}
#Merging Air Quality and Weather Data

merged_data <- full_join(air_new,weather_new, by = "date_resolved")
print(merged_data)
```

```{r}
#Total NAs in daily_data

total_missing <- sum(is.na(merged_data))
print(total_missing)
```

```{r}
#Identifying Columns with Missing Values

col_missing <- colSums(is.na(merged_data))
col_missing

#Removing Rows with missing Values
daily_data<- drop_na(merged_data) 


```

```         
```

### 3. Custom Function & Iteration

#### Calculate Yearly Averages

1.  Write a function `calc_yearly_avg()` to compute yearly averages for PM2.5 and weather variables.

    **Hint:** Use `mutate(Year = year(Date))` to extract the year from the date column.

2.  Group the data by year and compute yearly means for `avg_PM2.5`, `avg_Temp`, `avg_Humidity`, and `avg_WindSpeed`.

3.  Store the results in a new dataframe called `yearly_avg_data`.

4.  What do you make of the results of this calculation? Answer below the code.

```{r custom-function}
# Calculating Yearly Averages
calc_yearly_avg <- function(datasetweather) {
  yearly_avg_data <- datasetweather |>
    mutate(Year = year(date_resolved)) |>  
    group_by(Year) |>                 
    summarise(
      avg_PM2.5 = mean(avg_PM2.5, na.rm = TRUE),
      avg_Temp = mean(avg_Temp, na.rm = TRUE),
      avg_Humidity = mean(avg_Humidity, na.rm = TRUE),
      avg_WindSpeed = mean(avg_WindSpeed, na.rm = TRUE)
    )
    
  return(yearly_avg_data)
}

calc_yearly_avg(daily_data)

```

-   **Reflection question**: What does this analysis show?

### 4. Visualization & Analysis

#### Explore the Relationship Between Weather and Air Quality

-   Create a **scatter plot** of temperature, humidity, and wind speed vs. PM2.5 levels.
-   Compute correlations between PM2.5 and each weather variable.
-   **Hint:** Compute correlations using `cor(use = "complete.obs")` as a piped operation.
-   **Hint:** Use `geom_smooth(method = "lm")` to observe trends.

```{r}
ggplot(yearly_avg_data, aes(x=avg_PM2.5, y=avg_Temp)) +
geom_point(color = "steelblue") +
geom_smooth(method = "lm") +
labs(
title = "Temperature vs PM2.5 levels",
x = "PM2.5 level",
y = "Yearly Average Temperature"
) +
theme_minimal()
```

```         
```

```         
```

```         
```

### 5. Your Recommendations

#### Interpret Your Findings

Write a short summary of your insights. Make sure to answer these key questions:

1.  How do temperature, humidity, and wind speed correlate with PM2.5 levels?

2.  What weather conditions seem to worsen air quality?

3.  What interventions (e.g., forecasting-based pollution warnings, public advisories) could be beneficial?
