library(baseballr)
library(rvest)
library(tidyverse)
library(ggplot2)

today_date <- Sys.Date()

cat("======================================\n")
cat("MLB Undervalued Players Analysis\n")
cat(paste("Generated:", today_date, "\n"))
cat("======================================\n\n")

# ============================================================================
# GET 2026 MLB BATTING STATS
# ============================================================================

cat("Step 1: Pulling 2026 MLB batting statistics...\n")

batting_daily <- bref_daily_batter(t1="2026-03-25", t2=as.character(today_date))

batting_stats <- batting_daily %>%
  group_by(Name) %>%
  summarise(
    HR = sum(HR, na.rm = TRUE),
    H = sum(H, na.rm = TRUE),
    AB = sum(AB, na.rm = TRUE),
    RBI = sum(RBI, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(AB >= 50) %>%
  arrange(desc(H))

cat(paste("✓ Retrieved stats for", nrow(batting_stats), "qualified batters\n\n"))

# ============================================================================
# SCRAPE SALARY DATA FROM SPOTRAC
# ============================================================================

cat("Step 2: Scraping salary data from Spotrac...\n")

url <- "https://www.spotrac.com/mlb/contracts/"

# Scrape the page
page <- read_html(url)
tables <- page %>% html_table()

# Get the salary table (first table)
salary_raw <- tables[[1]]

# Clean up the data
salary_data <- salary_raw %>%
  select(Player, `Team\n                    Currently With`, AAV) %>%
  rename(Name = Player, Team = `Team\n                    Currently With`, salary_aav = AAV) %>%
  mutate(
    # Clean team names (remove newlines)
    Team = str_trim(str_replace_all(Team, "\n.*", "")),
    # Convert salary to numeric (remove $ and commas, convert millions)
    salary_millions = as.numeric(str_replace_all(salary_aav, "\\$|,", "")) / 1000000
  ) %>%
  select(Name, Team, salary_millions) %>%
  filter(!is.na(salary_millions))

cat(paste("✓ Retrieved salary data for", nrow(salary_data), "players\n\n"))

# ============================================================================
# MERGE BATTING STATS WITH SALARY DATA
# ============================================================================

cat("Step 3: Merging batting stats with salary data...\n")

analysis <- batting_stats %>%
  left_join(salary_data, by = "Name") %>%
  filter(!is.na(salary_millions)) %>%
  mutate(
    # Calculate estimated WAR
    estimated_war = (HR * 1.4 + H * 0.3) / 100,
    
    # Calculate value score
    value_score = estimated_war / salary_millions,
    
    # Categorize
    status = case_when(
      value_score > 0.5 ~ "Undervalued ⭐",
      value_score > 0.3 ~ "Good Value",
      value_score > 0.15 ~ "Fair Value",
      TRUE ~ "Overvalued"
    )
  ) %>%
  arrange(desc(value_score))

cat(paste("✓ Matched", nrow(analysis), "players with salary data\n\n"))

# ============================================================================
# DISPLAY RESULTS
# ============================================================================

cat("TOP UNDERVALUED PLAYERS - 2026 SEASON\n")
cat("=========================================\n\n")

print(analysis %>% head(25) %>% 
        select(Name, Team, salary_millions, HR, H, estimated_war, value_score, status))

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("\n\nStep 4: Saving results...\n")

dir.create("output", showWarnings = FALSE)

# Save all results
write.csv(analysis, 
          paste0("output/analysis_", today_date, ".csv"),
          row.names = FALSE)

cat("✓ Saved full analysis\n")

# Save top 25 undervalued
write.csv(analysis %>% head(25),
          paste0("output/top_25_undervalued_", today_date, ".csv"),
          row.names = FALSE)

cat("✓ Saved top 25 undervalued\n")

# ============================================================================
# CREATE VISUALIZATIONS
# ============================================================================

cat("Creating visualizations...\n")

# Plot 1: Top 20 by value score
top_20 <- analysis %>% head(20)

plot1 <- ggplot(top_20, aes(x = reorder(Name, value_score), y = value_score, fill = status)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = c(
    "Undervalued ⭐" = "darkgreen",
    "Good Value" = "green",
    "Fair Value" = "orange",
    "Overvalued" = "red"
  )) +
  coord_flip() +
  labs(
    title = "Top 20 Most Undervalued MLB Players - 2026",
    subtitle = paste("Updated:", today_date),
    x = "Player",
    y = "Value Score (WAR / Salary)",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave(paste0("output/undervalued_value_score_", today_date, ".png"), 
       plot1, width = 12, height = 8, dpi = 300)

cat("✓ Saved value score plot\n")

# Plot 2: Salary vs Performance scatter
scatter <- ggplot(analysis, aes(x = salary_millions, y = estimated_war, color = status)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_text(aes(label = Name), size = 2.5, check_overlap = TRUE) +
  scale_color_manual(values = c(
    "Undervalued ⭐" = "darkgreen",
    "Good Value" = "green",
    "Fair Value" = "orange",
    "Overvalued" = "red"
  )) +
  labs(
    title = "Salary vs Performance - All MLB Players",
    subtitle = paste("Updated:", today_date),
    x = "Salary (Millions $)",
    y = "Estimated WAR",
    color = "Status"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggsave(paste0("output/salary_vs_performance_", today_date, ".png"), 
       scatter, width = 14, height = 10, dpi = 300)

cat("✓ Saved salary vs performance plot\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n\n======================================\n")
cat("SUMMARY STATISTICS\n")
cat("======================================\n")
cat("Total qualified batters:", nrow(batting_stats), "\n")
cat("Players with salary data:", nrow(analysis), "\n")
cat("Games analyzed:", n_distinct(batting_daily$Date), "\n")
cat("Date range: 2026-03-25 to", as.character(today_date), "\n")
cat("\nValue Score Distribution:\n")
print(analysis %>% count(status))
cat("\n======================================\n")
cat("Output files saved to: output/\n")
cat("======================================\n\n")



