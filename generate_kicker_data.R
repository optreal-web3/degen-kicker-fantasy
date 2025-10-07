# Install packages if needed (GitHub Actions will handle)
# install.packages(c("nflfastR", "jsonlite"))

library(nflfastR)
library(jsonlite)

# Define season and weeks (up to week 5 as of October 07, 2025)
season <- 2025
weeks <- 1:5

# Function to process kicker stats for a week
process_week <- function(week) {
  # Load play-by-play data
  pbp <- load_pbp(season, weeks = week)
  
  # Filter for field goals and extra points
  fg_idx <- which(!is.na(pbp$kicker_player_name) & pbp$field_goal_result == "made")
  xp_idx <- which(!is.na(pbp$kicker_player_name) & pbp$extra_point_result == "good")
  kicker_plays <- pbp[c(fg_idx, xp_idx), ]
  
  if (nrow(kicker_plays) == 0) {
    return(data.frame(kicker = character(0), team = character(0), fg_made = numeric(0), 
                      total_points = numeric(0), total_yards = numeric(0), longest_fg = numeric(0)))
  }
  
  # Calculate stats
  fg_made <- rep(0, nrow(kicker_plays))
  fg_made[1:length(fg_idx)] <- 1
  fg_points <- ifelse(pbp$field_goal_result[fg_idx] == "made", 
                      ifelse(pbp$yards_to_goal[fg_idx] < 40, 3, ifelse(pbp$yards_to_goal[fg_idx] < 50, 4, 5)), 0)[1:length(fg_idx)]
  xp_points <- rep(0, nrow(kicker_plays))
  xp_points[(length(fg_idx)+1):nrow(kicker_plays)] <- 1
  total_yards <- ifelse(pbp$field_goal_result[fg_idx] == "made", pbp$yards_to_goal[fg_idx], 0)[1:length(fg_idx)]
  longest_fg <- pbp$yards_to_goal[fg_idx]
  
  data_df <- data.frame(
    kicker = kicker_plays$kicker_player_name,
    team = kicker_plays$posteam,
    fg_made = fg_made,
    fg_points = c(fg_points, rep(0, nrow(kicker_plays) - length(fg_idx))),
    xp_points = xp_points,
    total_yards = c(total_yards, rep(0, nrow(kicker_plays) - length(fg_idx))),
    longest_fg = c(longest_fg, rep(0, nrow(kicker_plays) - length(fg_idx)))
  )
  
  # Aggregate
  kicker_stats <- aggregate(
    cbind(fg_made, fg_points, xp_points, total_yards, longest_fg) ~ kicker + team,
    data = data_df,
    FUN = sum,
    na.rm = TRUE
  )
  
  kicker_stats$total_points <- kicker_stats$fg_points + kicker_stats$xp_points
  kicker_stats$longest_fg <- tapply(data_df$longest_fg, list(data_df$kicker, data_df$team), max, na.rm = TRUE)[cbind(kicker_stats$kicker, kicker_stats$team)]
  
  # Select relevant columns
  kicker_stats <- kicker_stats[, c("kicker", "team", "fg_made", "total_points", "total_yards", "longest_fg")]
  
  # Save to JSON
  dir.create("data", showWarnings = FALSE)
  write_json(kicker_stats, paste0("data/week_", week, ".json"), pretty = TRUE)
  
  # Debugging
  print(paste("Week", week, "processed:", nrow(kicker_stats), "kickers"))
  
  return(kicker_stats)
}

# Generate JSON files for each week
for (week in weeks) {
  process_week(week)
}
