
# Parameters for case functions -----------------------------------------------------


sample_sizes <- c(72, 150, 250, 350, 450, 550)
lag_amounts <- c(0, 1, 2, 3, 4, 5)

param_data <- data.frame(
  'sample_sizes' = c(sample_sizes),
  'lag_amounts' = c(lag_amounts)
)


# Cross all of them to map over -------------------------------------------

param_map <- param_data %>%
  cross_df()
