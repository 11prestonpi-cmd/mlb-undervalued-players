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
# CREATE HTML REPORT
# ============================================================================

cat("Creating HTML report...\n")

top_25 <- analysis %>% head(25)

html_report <- paste(
  "<!DOCTYPE html>",
  "<html>",
  "<head>",
  "<meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<title>MLB Undervalued Players - ", today_date, "</title>",
  "<style>",
  "body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; margin: 0; padding: 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); }",
  ".container { max-width: 1400px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }",
  "h1 { color: #003087; border-bottom: 4px solid #003087; padding-bottom: 15px; margin-bottom: 10px; }",
  ".subtitle { color: #666; font-size: 16px; margin-bottom: 20px; }",
  ".live-badge { background: #ff4444; color: white; padding: 5px 12px; border-radius: 20px; font-weight: bold; font-size: 12px; }",
  "table { width: 100%; border-collapse: collapse; margin: 20px 0; }",
  "th { background: #003087; color: white; padding: 12px; text-align: left; font-weight: bold; }",
  "td { padding: 12px; border-bottom: 1px solid #ddd; }",
  "tr:hover { background: #f5f5f5; }",
  ".undervalued { color: darkgreen; font-weight: bold; }",
  ".good-value { color: green; font-weight: bold; }",
  ".fair-value { color: orange; font-weight: bold; }",
  ".overvalued { color: red; font-weight: bold; }",
  ".info-box { background: #f0f0f0; padding: 15px; border-left: 4px solid #003087; margin: 20px 0; border-radius: 5px; }",
  ".stats-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px; margin: 20px 0; }",
  ".stat-card { background: #f9f9f9; padding: 20px; border-radius: 8px; text-align: center; border: 1px solid #ddd; }",
  ".stat-number { font-size: 28px; font-weight: bold; color: #003087; }",
  ".stat-label { color: #666; font-size: 14px; margin-top: 5px; }",
  "img { max-width: 100%; height: auto; margin: 20px 0; border-radius: 8px; }",
  ".footer { text-align: center; color: #999; font-size: 12px; margin-top: 40px; padding-top: 20px; border-top: 1px solid #ddd; }",
  "</style>",
  "</head>",
  "<body>",
  "<div class='container'>",
  "<h1>⚾ MLB Undervalued Players Analysis <span class='live-badge'>LIVE 2026</span></h1>",
  "<div class='subtitle'>Last Updated: <strong>", format(today_date, "%B %d, %Y"), " at ", format(Sys.time(), "%H:%M:%S"), " UTC</strong></div>",
  "",
  "<div class='info-box'>",
  "<p><strong>How This Works:</strong> This analysis identifies MLB players who are overperforming relative to their salary.</p>",
  "<p><strong>Value Score:</strong> Estimated WAR ÷ Salary (millions). Higher = Better value.</p>",
  "<p><strong>Updates:</strong> Every day at 2 AM UTC via GitHub Actions</p>",
  "</div>",
  "",
  "<div class='stats-grid'>",
  "<div class='stat-card'>",
  "<div class='stat-number'>", nrow(analysis), "</div>",
  "<div class='stat-label'>Total Players</div>",
  "</div>",
  "<div class='stat-card'>",
  "<div class='stat-number'>", nrow(batting_stats), "</div>",
  "<div class='stat-label'>Qualified Batters</div>",
  "</div>",
  "<div class='stat-card'>",
  "<div class='stat-number'>", n_distinct(batting_daily$Date), "</div>",
  "<div class='stat-label'>Games Analyzed</div>",
  "</div>",
  "<div class='stat-card'>",
  "<div class='stat-number'>", as.character(today_date), "</div>",
  "<div class='stat-label'>Current Date</div>",
  "</div>",
  "</div>",
  "",
  "<h2>Top 25 Undervalued Players</h2>",
  "<table>",
  "<thead><tr>",
  "<th>Rank</th>",
  "<th>Player</th>",
  "<th>Team</th>",
  "<th>Salary ($M)</th>",
  "<th>HR</th>",
  "<th>Hits</th>",
  "<th>Est. WAR</th>",
  "<th>Value Score</th>",
  "<th>Status</th>",
  "</tr></thead>",
  "<tbody>",
  paste(apply(cbind(seq(1, nrow(top_25)), top_25), 1, function(row) {
    rank_num <- row[1]
    player_name <- row[3]
    team <- row[4]
    salary <- round(as.numeric(row[5]), 2)
    hr <- row[6]
    hits <- row[7]
    war <- round(as.numeric(row[9]), 2)
    value <- round(as.numeric(row[10]), 3)
    status <- row[11]
    
    status_class <- tolower(gsub(" ⭐", "", status))
    status_class <- gsub(" ", "-", status_class)
    
    paste(
      "<tr>",
      "<td><strong>", rank_num, "</strong></td>",
      "<td>", player_name, "</td>",
      "<td>", team, "</td>",
      "<td>$", salary, "M</td>",
      "<td>", hr, "</td>",
      "<td>", hits, "</td>",
      "<td>", war, "</td>",
      "<td>", value, "</td>",
      "<td class='", status_class, "'>", status, "</td>",
      "</tr>"
    )
  }), collapse = ""),
  "</tbody>",
  "</table>",
  "",
  "<h2>Visualizations</h2>",
  "<p>Click images to view full size</p>",
  "<img src='undervalued_value_score_", today_date, ".png' alt='Value Score Chart'>",
  "<img src='salary_vs_performance_", today_date, ".png' alt='Salary vs Performance'>",
  "",
  "<div class='info-box'>",
  "<h3>Methodology</h3>",
  "<ul>",
  "<li><strong>Data Sources:</strong> Baseball Reference (stats) + Spotrac (salaries)</li>",
  "<li><strong>WAR Estimate:</strong> (HR × 1.4 + Hits × 0.3) ÷ 100</li>",
  "<li><strong>Value Score:</strong> Estimated WAR ÷ Salary (millions)</li>",
  "<li><strong>Minimum AB:</strong> 50+ at-bats for qualification</li>",
  "</ul>",
  "</div>",
  "",
  "<div class='footer'>",
  "<p>Report generated automatically by GitHub Actions | Data sourced from MLB API</p>",
  "<p>Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " UTC</p>",
  "</div>",
  "",
  "</div>",
  "</body>",
  "</html>",
  sep = "\n"
)

writeLines(html_report, "output/index.html")

cat("✓ Saved HTML report\n")


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



