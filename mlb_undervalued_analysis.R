# Load libraries
library(tidyverse)
library(ggplot2)

today_date <- Sys.Date()

cat("MLB Undervalued Players Analysis\n")
cat(paste("Generated:", today_date, "\n\n"))

# Create sample player data
player_data <- tibble(
  player_name = c("Mike Trout", "Mookie Betts", "Juan Soto", "Aaron Judge", "Bryce Harper"),
  salary_millions = c(37.1, 30.0, 32.5, 37.5, 26.0),
  home_runs = c(32, 16, 30, 28, 25),
  hits = c(144, 128, 135, 140, 130),
  rbi = c(88, 65, 92, 90, 75)
)

# Calculate metrics
analysis <- player_data %>%
  mutate(
    estimated_war = (home_runs * 1.4 + hits * 0.3) / 100,
    value_score = estimated_war / salary_millions,
    status = case_when(
      value_score > 0.5 ~ "Undervalued",
      value_score > 0.2 ~ "Fair Value",
      TRUE ~ "Overvalued"
    )
  ) %>%
  arrange(desc(value_score))

# Print results
print(analysis)

# Create output folder
dir.create("output", showWarnings = FALSE)

# Save analysis
write.csv(analysis, paste0("output/analysis_", today_date, ".csv"), row.names = FALSE)

cat("\n✓ Analysis saved!\n")
