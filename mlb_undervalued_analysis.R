library(baseballr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(scales)

today_date <- Sys.Date()

cat("======================================\n")
cat("MLB Undervalued Players Analysis\n")
cat(paste("Generated:", today_date, "\n"))
cat("======================================\n\n")

# ============================================================================

# ============================================================================
getwd()
setwd("/Users/piercepreston/Desktop/weekly update project mlb")
SPOTRAC_CSV <- "2026 MLB Contracts.csv"

# ============================================================================
# GET 2026 MLB BATTING STATS
# ============================================================================

cat("Step 1: Pulling 2026 MLB batting statistics...\n")

batting_daily <- bref_daily_batter(t1 = "2026-03-25", t2 = as.character(today_date))

batting_stats <- batting_daily %>%
  group_by(Name) %>%
  summarise(
    HR  = sum(HR,  na.rm = TRUE),
    H   = sum(H,   na.rm = TRUE),
    AB  = sum(AB,  na.rm = TRUE),
    RBI = sum(RBI, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(AB >= 50) %>%
  arrange(desc(H))

cat(paste("✓ Retrieved stats for", nrow(batting_stats), "qualified batters\n\n"))

# ============================================================================
# LOAD SALARY DATA FROM SPOTRAC CSV
# ============================================================================

cat("Step 2: Loading salary data from Spotrac CSV...\n")

salary_data <- read_csv(SPOTRAC_CSV, show_col_types = FALSE) %>%
  # The team column has extra whitespace and repeats the abbreviation (e.g. "NYM                  NYM")
  rename(Name = Player) %>%
  rename_with(~ "Team", matches("Team|Currently")) %>%
  mutate(
    Name            = str_trim(Name),
    Team            = str_trim(str_extract(Team, "^[A-Z]+")),
    salary_millions = as.numeric(str_replace_all(AAV, "[^0-9.]", "")) / 1000000
  ) %>%
  select(Name, Team, salary_millions) %>%
  filter(!is.na(salary_millions), salary_millions > 0, Name != "") %>%
  distinct(Name, .keep_all = TRUE)

cat(paste("✓ Loaded salary data for", nrow(salary_data), "players\n\n"))

# ============================================================================
# MERGE BATTING STATS WITH SALARY DATA
# ============================================================================

cat("Step 3: Merging batting stats with salary data...\n")

LEAGUE_MINIMUM_M <- 0.72  # 2026 MLB league minimum in millions

analysis <- batting_stats %>%
  left_join(salary_data, by = "Name") %>%
  mutate(
    salary_source   = if_else(is.na(salary_millions), "league_minimum_default", "spotrac"),
    salary_millions = if_else(is.na(salary_millions), LEAGUE_MINIMUM_M, salary_millions),
    estimated_war   = (HR * 1.4 + H * 0.3) / 100,
    value_score     = estimated_war / salary_millions
  ) %>%
  # Percentile-based cutoffs: even 25/25/25/25 split, recalculates each run
  mutate(
    vs_pct = percent_rank(value_score),
    status = case_when(
      vs_pct >= 0.75 ~ "Undervalued",
      vs_pct >= 0.50 ~ "Good Value",
      vs_pct >= 0.25 ~ "Fair Value",
      TRUE           ~ "Overvalued"
    )
  ) %>%
  arrange(desc(value_score))

n_spotrac <- sum(analysis$salary_source == "spotrac")
n_default <- sum(analysis$salary_source == "league_minimum_default")

cat(paste("✓", nrow(analysis), "total players |",
          n_spotrac, "matched to Spotrac CSV |",
          n_default, "assigned league minimum\n\n"))

# ============================================================================
# DISPLAY RESULTS
# ============================================================================

cat("TOP BEST VALUE PLAYERS - 2026 SEASON\n")
cat("=========================================\n\n")

print(analysis %>%
        head(25) %>%
        select(Name, Team, salary_millions, salary_source, HR, H, estimated_war, value_score, status))

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("\n\nStep 4: Saving results...\n")

dir.create("output", showWarnings = FALSE)

write.csv(analysis,
          paste0("output/analysis_", today_date, ".csv"),
          row.names = FALSE)
cat("✓ Saved full analysis\n")

write.csv(analysis %>% head(25),
          paste0("output/top_25_undervalued_", today_date, ".csv"),
          row.names = FALSE)
cat("✓ Saved top 25 best value\n")

# ============================================================================
# VISUALIZATIONS
# ============================================================================

cat("Creating visualizations...\n")

status_colors <- c(
  "Undervalued" = "#1a7a1a",
  "Good Value"  = "#5cb85c",
  "Fair Value"  = "#f0ad4e",
  "Overvalued"  = "#d9534f"
)

# --- Plot 1: Top 20 Best Value Bar Chart ------------------------------------

top_20 <- analysis %>% head(20)

plot1 <- ggplot(top_20, aes(x = reorder(Name, value_score), y = value_score, fill = status)) +
  geom_col(alpha = 0.85, width = 0.75) +
  geom_text(aes(label = sprintf("%.2f", value_score)),
            hjust = -0.15, size = 3, color = "grey20") +
  scale_fill_manual(values = status_colors, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  coord_flip() +
  labs(
    title    = "Top 20 Best Value Players — 2026",
    subtitle = paste("Value Score = Est. WAR / Salary ($M) · Updated:", today_date),
    x        = NULL,
    y        = "Value Score",
    fill     = "Status"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.text.y   = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_blank()
  )

ggsave(paste0("output/top20_value_score_", today_date, ".png"),
       plot1, width = 12, height = 8, dpi = 300)
cat("✓ Saved value score bar chart\n")

# --- Plot 2: Salary vs Performance Scatter ----------------------------------
# Labels only top 25 by value score + high-salary outliers (top 5% earners)
# to keep the chart readable with a large player pool.

salary_threshold  <- quantile(analysis$salary_millions, 0.95)
label_players     <- analysis %>%
  filter(
    row_number() <= 25 |
    salary_millions >= salary_threshold
  )

scatter <- ggplot(analysis, aes(x = salary_millions, y = estimated_war, color = status)) +
  geom_point(aes(shape = salary_source), size = 2.5, alpha = 0.65) +
  geom_smooth(aes(group = 1),
              method  = "loess",
              formula = y ~ x,
              color   = "grey30",
              fill    = "grey80",
              alpha   = 0.2,
              linewidth = 0.8,
              linetype  = "dashed") +
  geom_text_repel(
    data          = label_players,
    aes(label     = Name),
    size          = 2.8,
    max.overlaps  = 25,
    segment.color = "grey60",
    segment.size  = 0.3,
    box.padding   = 0.3,
    point.padding = 0.2
  ) +
  scale_x_log10(
    labels = dollar_format(suffix = "M", accuracy = 0.1),
    breaks = c(0.72, 1, 2, 5, 10, 20, 40)
  ) +
  scale_color_manual(values = status_colors, drop = FALSE) +
  scale_shape_manual(
    values = c("spotrac" = 16, "league_minimum_default" = 17),
    labels = c("spotrac" = "Spotrac CSV", "league_minimum_default" = "League minimum est.")
  ) +
  labs(
    title    = "Salary vs. Performance — All MLB Qualified Batters, 2026",
    subtitle = paste("Log salary scale · Labels: top 25 by value + top 5% earners · Updated:", today_date),
    x        = "Annual Salary (log scale)",
    y        = "Estimated WAR",
    color    = "Status",
    shape    = "Salary Source"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40", size = 10),
    legend.position = "right"
  )

ggsave(paste0("output/salary_vs_performance_", today_date, ".png"),
       scatter, width = 16, height = 11, dpi = 300)
cat("✓ Saved salary vs performance scatter\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n\n======================================\n")
cat("SUMMARY STATISTICS\n")
cat("======================================\n")
cat("Total qualified batters:", nrow(batting_stats), "\n")
cat("Players in analysis:", nrow(analysis), "\n")
cat("  - Spotrac salary match:", n_spotrac, "\n")
cat("  - League minimum default:", n_default, "\n")
cat("Games analyzed:", n_distinct(batting_daily$Date), "\n")
cat("Date range: 2026-03-25 to", as.character(today_date), "\n")
cat("\nValue Score Distribution (percentile-based, ~25% each):\n")
print(analysis %>% count(status) %>% mutate(pct = round(n / sum(n) * 100, 1)))
cat("\n======================================\n")
cat("Output files saved to: output/\n")
cat("======================================\n\n")

