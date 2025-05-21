# Load data without converting strings to factors
data <- read.csv('/Users/maggiebowen/Documents/GitHub/QuantitativeResearch/FINAL_MERGED_ANSWERS.csv', stringsAsFactors = FALSE)

# ---- T-TESTS ----
run_ttest <- function(gmap_col_index, amap_col_index, label, df) {
  # Extract columns and convert to numeric
  gmap_raw <- as.numeric(trimws(df[[gmap_col_index]]))
  amap_raw <- as.numeric(trimws(df[[amap_col_index]]))
  
  # Remove NAs
  gmap_vals <- na.omit(gmap_raw)
  amap_vals <- na.omit(amap_raw)

  # combine data frame
  ratings <- c(gmap_vals, amap_vals)
  app <- factor(c(rep("Google Maps", length(gmap_vals)), rep("Apple Maps", length(amap_vals))))
  df_long <- data.frame(rating = ratings, app = app)

  # Run independent t-test with confidence interval
  test_result <- t.test(rating ~ app, data = df_long, conf.level = 0.95)

  return(data.frame(
    Question = label,
    p_value = round(test_result$p.value, 4),
    t_statistic = round(test_result$statistic, 3),
    mean_gmap = round(mean(gmap_vals), 3),
    mean_amap = round(mean(amap_vals), 3),
    ci_lower = round(test_result$conf.int[1], 3),
    ci_upper = round(test_result$conf.int[2], 3),
    n_gmap = length(gmap_vals),
    n_amap = length(amap_vals)
  ))
}

# ---- filtered OS for t-tests ----
# only Android and iOS users (exclude "Both")
data_os_filtered <- subset(data, trimws(data[[6]]) %in% c("Android", "iOS (iPhone)"))

run_os_ttest <- function(response_col, label, df) {
  values <- as.numeric(trimws(df[[response_col]]))
  os <- factor(trimws(df[[6]]))  # Column 6 is Operating System
  df_clean <- data.frame(response = values, os = os)
  df_clean <- na.omit(df_clean)

  test_result <- t.test(response ~ os, data = df_clean, conf.level = 0.95)

  return(data.frame(
    Question = label,
    p_value = round(test_result$p.value, 4),
    t_statistic = round(test_result$statistic, 3),
    mean_android = round(mean(df_clean$response[df_clean$os == "Android"]), 3),
    mean_ios = round(mean(df_clean$response[df_clean$os == "iOS (iPhone)"]), 3),
    ci_lower = round(test_result$conf.int[1], 3),
    ci_upper = round(test_result$conf.int[2], 3),
    n_android = sum(df_clean$os == "Android"),
    n_ios = sum(df_clean$os == "iOS (iPhone)")
  ))
}


# ---- t-tests by frequency of public transport usage ----
frequency_levels <- c(
    "I do not use it",
    "Only on weekends",
    "1 to 3 times per month",
    "1 to 3 times per week",
    "4 to 7 times per week"
  )

get_max_frequency <- function(row) {
  freqs <- as.character(row[7:10])
  factor_freqs <- factor(freqs, levels = frequency_levels, ordered = TRUE)
  return(as.character(max(factor_freqs)))
}

# find max freuqnecy for all rows
data$Max_PT_Frequency <- apply(data, 1, get_max_frequency)


ttests_by_frequency <- lapply(frequency_levels, function(freq) {
  subset_df <- subset(data, Max_PT_Frequency == freq)
  
  result <- do.call(rbind, Filter(Negate(is.null), Map(
    function(pair, label) {
      freq_label <- paste0(label, " (", freq, ")")
      
      # Extract cleaned numeric data
      gmap_vals <- as.numeric(trimws(subset_df[[pair[1]]]))
      amap_vals <- as.numeric(trimws(subset_df[[pair[2]]]))
      gmap_vals <- na.omit(gmap_vals)
      amap_vals <- na.omit(amap_vals)
      
      # Only run t-test if both groups have > 1 value because before it was being blocked
      if (length(unique(c(gmap_vals, amap_vals))) < 2 ||
          length(gmap_vals) < 2 || length(amap_vals) < 2) {
        return(NULL)
      }

      return(run_ttest(pair[1], pair[2], freq_label, subset_df))
    },
    ttest_pairs,
    ttest_labels
  )))
  
  return(result)
})


# ---- Mann-Whitney U test ----
run_likert_test <- function(gmap_col_index, amap_col_index, label, df) {
  likert_levels <- c("terrible", "bad", "average", "good", "excellent")

  # Clean and factor responses
  gmap_raw <- tolower(trimws(df[[gmap_col_index]]))
  amap_raw <- tolower(trimws(df[[amap_col_index]]))

  gmap_clean <- factor(gmap_raw[gmap_raw %in% likert_levels], levels = likert_levels, ordered = TRUE)
  amap_clean <- factor(amap_raw[amap_raw %in% likert_levels], levels = likert_levels, ordered = TRUE)

  gmap_numeric <- as.numeric(gmap_clean)
  amap_numeric <- as.numeric(amap_clean)

  # Remove NA values
  gmap_vals <- na.omit(gmap_numeric)
  amap_vals <- na.omit(amap_numeric)

  # Run Mann-Whitney U test
  test_result <- wilcox.test(gmap_vals, amap_vals)

  return(data.frame(
    Question = label,
    p_value = round(test_result$p.value, 4),
    W_statistic = round(test_result$statistic, 3),
    median_gmap = median(gmap_vals),
    median_amap = median(amap_vals),
    n_gmap = length(gmap_vals),
    n_amap = length(amap_vals)
  ))
}

# ---- T-TESTS calls ----
ttest_pairs <- list(
  c(27, 45),  # Visual Design
  c(28, 46),  # Meets Needs
  c(29, 47),  # Public Transport Updates
  c(30, 48),  # Public Transport Coverage
  c(31, 49),  # Time Adjustment
  c(32, 50),  # Find Stops
  c(33, 51),  # Share ETA
  c(34, 52),  # Multiple Stops
  c(35, 53)   # Schedule Match
)

ttest_labels <- c(
  "Visual Design",
  "Meets Needs",
  "Public Transport Updates",
  "Public Transport Coverage",
  "Arrival/Departure Time Adjustment",
  "Find Stops/Stations",
  "Share Route/ETA",
  "Multiple Stops",
  "Schedule Match"
)

os_test_columns <- c(27, 28, 29, 30, 31, 32, 33, 34, 35)
os_test_labels <- c(
  "Visual Design (OS)", "Meets Needs (OS)", "PT Updates (OS)", "PT Coverage (OS)",
  "Arrival/Departure Time (OS)", "Find Stops (OS)", "Share ETA (OS)",
  "Multiple Stops (OS)", "Schedule Match (OS)"
)

os_ttest_results <- do.call(rbind, Map(
  function(col, label) run_os_ttest(col, label, data_os_filtered),
  os_test_columns,
  os_test_labels
))

likert_pairs <- list(
  c(22, 40),  # Public Transport
  c(23, 41),  # Real-Time Info
  c(24, 42),  # Navigation
  c(25, 43),  # Ease of Use
  c(26, 44)   # Offline Access
)

likert_labels <- c(
  "Public Transport",
  "Real-Time Info",
  "Navigation",
  "Ease of Use",
  "Offline Access"
)

# Run t-tests
ttest_results <- do.call(rbind, Map(
  function(pair, label) run_ttest(pair[1], pair[2], label, data),
  ttest_pairs,
  ttest_labels
))

ttests_by_frequency_df <- do.call(rbind, ttests_by_frequency)

likert_results <- do.call(rbind, Map(
  function(pair, label) run_likert_test(pair[1], pair[2], label, data),
  likert_pairs,
  likert_labels
))

print("T-test Results:")
print(ttest_results)

print("OS T-test Results:")
print(os_ttest_results)

print("Mann-Whitney U Test Results for Likert Questions:")
print(likert_results)

# export t-tests to separate CSV
write.csv(ttest_results, "ttests_results.csv", row.names = FALSE)

# export likert results to separate CSV
write.csv(likert_results, "likert_test_results.csv", row.names = FALSE)

# export OS t-tests to separate CSV
write.csv(os_ttest_results, "os_ttest_results.csv", row.names = FALSE)

# export t-tests by frequency to separate CSV
write.csv(ttests_by_frequency_df, "ttests_by_frequency.csv", row.names = FALSE)

# Define frequency levels (optional: reorder them for consistency)
valid_freq_levels <- c(
  "I do not use it",
  "I use it occasionally",
  "A few times per month",
  "1 to 3 times per week",
  "4 to 6 times per week",
  "Every day"
)

# Clean frequency columns
data$Gmaps_Usage_Freq <- factor(trimws(data[[21]]), levels = valid_freq_levels, ordered = TRUE)
data$Amaps_Usage_Freq <- factor(trimws(data[[39]]), levels = valid_freq_levels, ordered = TRUE)

# Column indices for 1–5 scale questions
gmap_cols <- 27:35
amap_cols <- 45:53

labels <- c(
  "Visual Design", "Meets Needs", "PT Updates", "PT Coverage",
  "Arrival/Departure Time", "Find Stops", "Share ETA",
  "Multiple Stops", "Schedule Match"
)

# Function to run ANOVA for one app
run_anova_set <- function(rating_cols, freq_col, app_name) {
  results <- list()
  for (i in seq_along(rating_cols)) {
    col_index <- rating_cols[i]
    label <- labels[i]
    
    ratings <- as.numeric(trimws(data[[col_index]]))
    freq <- data[[freq_col]]
    
    df <- data.frame(rating = ratings, frequency = freq)
    df <- na.omit(df)
    
    if (length(unique(df$frequency)) > 1) {
      model <- aov(rating ~ frequency, data = df)
      p_val <- summary(model)[[1]]["Pr(>F)"][1]
      
      results[[label]] <- data.frame(
        Question = label,
        p_value = round(p_val, 4),
        n = nrow(df)
      )
    }
  }
  return(do.call(rbind, results))
}

# Run for both apps
gmap_anova_results <- run_anova_set(gmap_cols, "Gmaps_Usage_Freq", "Google Maps")
amap_anova_results <- run_anova_set(amap_cols, "Amaps_Usage_Freq", "Apple Maps")

# Print and export
print("Google Maps ANOVA results:")
print(gmap_anova_results)

print("Apple Maps ANOVA results:")
print(amap_anova_results)

write.csv(gmap_anova_results, "anova_gmaps_by_usage_freq.csv", row.names = FALSE)
write.csv(amap_anova_results, "anova_amaps_by_usage_freq.csv", row.names = FALSE)