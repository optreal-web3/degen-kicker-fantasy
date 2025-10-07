library(shiny)
library(shinyWidgets)
library(dplyr)
library(nflfastR)
library(shinyjs)

# Suppress nflreadr cache warning
options(nflreadr.verbose = FALSE)

# Load team data
team_data <- tryCatch({
  nflfastR::teams_colors_logos
}, error = function(e) {
  data.frame(team_abbr = character(), team_name = character())
})

# Load pbp data with fallback to local CSV if network fails
pbp_data <- tryCatch({
  data <- load_pbp(2025)
  if (nrow(data) == 0 || !all(c("week", "play_type", "kick_distance") %in% names(data))) {
    stop("Invalid 2025 data")
  }
  data
}, error = function(e) {
  message("Failed to load 2025 data, falling back to 2024 or empty data.")
  data <- load_pbp(2024)
  if (nrow(data) == 0 || !all(c("week", "play_type", "kick_distance") %in% names(data))) {
    data.frame() # Empty fallback
  } else {
    data
  }
})

# Scoring functions
calc_kick_points <- function(play_type, result, distance) {
  case_when(
    play_type == "extra_point" & result == "good" ~ 1,
    play_type == "extra_point" & result != "good" & !is.na(result) ~ -1,
    play_type == "field_goal" & result == "made" & distance < 30 ~ 1,
    play_type == "field_goal" & result != "made" & distance < 30 & !is.na(result) ~ -4,
    play_type == "field_goal" & result == "made" & distance < 40 ~ 2,
    play_type == "field_goal" & result != "made" & distance < 40 & !is.na(result) ~ -3,
    play_type == "field_goal" & result == "made" & distance < 50 ~ 3,
    play_type == "field_goal" & result != "made" & distance < 50 & !is.na(result) ~ -2,
    play_type == "field_goal" & result == "made" & distance < 60 ~ 4,
    play_type == "field_goal" & result != "made" & distance < 60 & !is.na(result) ~ -1,
    play_type == "field_goal" & result == "made" & distance >= 60 ~ 5,
    play_type == "field_goal" & result != "made" & distance >= 60 & !is.na(result) ~ 0,
    TRUE ~ 0
  )
}

calc_punt_points <- function(distance, return_yards, inside20, yardline, blocked, touchback) {
  (distance * 0.05) -
    (ifelse(is.na(return_yards), 0, return_yards) * 0.05) +
    (inside20 * 0.5) +
    (ifelse(yardline <= 10 & blocked == 0 & touchback == 0 & is.na(return_yards), 1.5, 0)) -
    (touchback * 5) -
    (blocked * 4)
}

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap"),
    tags$style(HTML("
      body { font-family: 'Roboto', sans-serif; background: #121212; color: #e0e0e0; }
      .header { background: #1e1e1e; padding: 15px; text-align: center; border-bottom: 2px solid #1976d2; margin-bottom: 20px; width: 100vw; }
      .header h1 { color: #ffffff; font-size: 2em; margin: 0 auto; max-width: 1500px; }
      .header h1 span { font-weight: bold; font-style: italic; }
      .container { max-width: 1500px; margin: auto; padding: 20px; }
      .controls { display: flex; align-items: center; margin-bottom: 20px; }
      .how-to-play { background: #2a2a2a; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.3); width: 150px; margin-left: 10px; }
      .how-to-play h4 { margin: 0; color: #bbdefb; cursor: pointer; font-size: 0.9em; }
      .how-to-play h4:hover { color: #90caf9; }
      .how-to-play-content { margin-top: 10px; font-size: 0.9em; }
      .week-selector .selectize-control .selectize-input { background: #2a2a2a; color: #e0e0e0; border: 1px solid #1976d2; border-radius: 5px; padding: 5px; font-size: 0.9em; width: 150px; }
      .leaderboard-panel { background: #2a2a2a; border-radius: 10px; padding: 20px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); margin-bottom: 20px; }
      h3 { color: #bbdefb; font-size: 1.5em; margin-top: 0; text-align: center; }
      table { width: 90%; border-collapse: collapse; background: #333333; margin: auto; }
      th, td { padding: 10px; text-align: center; border-bottom: 1px solid #444; vertical-align: middle; }
      th { background: #1976d2; color: #ffffff; }
      .top-team { font-weight: bold; background: #3f51b5; color: #ffffff; }
      .no-data { color: #ef5350; text-align: center; font-size: 1.2em; }
      .team-logo { width: 40px; height: 40px; vertical-align: middle; margin-right: 10px; }
      .player-section { margin-bottom: 15px; padding: 10px; background: #333333; border-radius: 5px; white-space: normal; word-wrap: break-word; }
      .leaders-header { text-align: center; color: #ffffff; font-size: 1.8em; margin-bottom: 10px; }
      .details-row { display: flex; gap: 20px; }
      .details-col { flex: 1; }
    "))
  ),
  div(class = "header",
      h1(HTML("The Real Fantasy <span>Football</span> Challenge"))
  ),
  div(class = "container",
      div(class = "controls",
          div(class = "week-selector",
              pickerInput("week", "2025 Season", choices = NULL, options = list(style = "btn-primary", width = "150px"))
          ),
          div(class = "how-to-play",
              shinyjs::useShinyjs(),
              h4("How to Play", onclick = "Shiny.setInputValue('toggleHowTo', Math.random())"),
              shinyjs::hidden(
                div(id = "howToContent", class = "how-to-play-content",
                    p("Kickers: PAT (+1 made, -1 missed), FG <30 yards (+1 made, -4 missed), 30-39 (+2 made, -3 missed), 40-49 (+3 made, -2 missed), 50-59 (+4 made, -1 missed), 60+ (+5 made, 0 missed)."),
                    p("Punters: 0.05 points per punt yard, -0.05 per return yard (net yards), +0.5 per punt inside 20, +1.5 inside 10, -5 for touchbacks, -4 for blocks.")
                )
              )
          )
      ),
      div(class = "leaders-header", "Team Leaderboard"),
      div(class = "leaderboard-panel",
          h3("Total Fantasy Points"),
          conditionalPanel(
            condition = "output.hasLeaderboard == 'TRUE'",
            tableOutput("leaderboard")
          ),
          conditionalPanel(
            condition = "output.hasLeaderboard != 'TRUE'",
            div(class = "no-data", "No data available for selected week")
          )
      ),
      div(class = "panel",
          h3("Full Team Details"),
          conditionalPanel(
            condition = "output.hasTeamDetails == 'TRUE'",
            uiOutput("teamDetails")
          ),
          conditionalPanel(
            condition = "output.hasTeamDetails != 'TRUE'",
            div(class = "no-data", "No team details available")
          )
      )
  )
)

# Server
server <- function(input, output, session) {
  # Dynamic week choices
  available_weeks <- reactive({
    if (nrow(pbp_data) == 0) return(1:5) # Fallback if no data
    unique(pbp_data$week)
  })
  
  observe({
    req(available_weeks())
    updatePickerInput(
      session,
      "week",
      choices = paste("Week", sort(available_weeks())),
      selected = paste("Week", min(available_weeks()))
    )
  })

  observeEvent(input$toggleHowTo, {
    shinyjs::toggle("howToContent")
  })

  # Compute stats for selected week
  calc_team_stats <- reactive({
    req(nrow(pbp_data) > 0)
    week_num <- as.integer(gsub("Week ", "", input$week))
    
    # Kicker stats
    fg_data <- pbp_data %>%
      filter(week == week_num & play_type %in% c("field_goal", "extra_point") & !is.na(kick_distance) & kick_distance > 0)
    
    if (nrow(fg_data) == 0) {
      kicker_team_stats <- data.frame()
      kicker_details <- data.frame()
    } else {
      fg_data <- fg_data %>%
        mutate(kick_points = calc_kick_points(play_type, 
                                            ifelse(play_type == "field_goal", field_goal_result, extra_point_result), 
                                            kick_distance))
      
      kicker_team_stats <- fg_data %>%
        group_by(posteam) %>%
        summarise(
          fg_points = sum(kick_points, na.rm = TRUE),
          fg_dist = sum(kick_distance[play_type == "field_goal"], na.rm = TRUE),
          longest_fg = if(sum(play_type == "field_goal") > 0) max(kick_distance[play_type == "field_goal"], na.rm = TRUE) else 0,
          .groups = 'drop'
        ) %>%
        filter(fg_points != 0) %>%
        left_join(team_data %>% select(team_abbr, team_name), by = c("posteam" = "team_abbr")) %>%
        arrange(team_name)
      
      kicker_details <- fg_data %>%
        group_by(posteam, kicker_player_name) %>%
        summarise(
          made_fg = sum(play_type == "field_goal" & field_goal_result == "made"),
          missed_fg = sum(play_type == "field_goal" & field_goal_result != "made" & !is.na(field_goal_result)),
          made_xp = sum(play_type == "extra_point" & extra_point_result == "good"),
          missed_xp = sum(play_type == "extra_point" & extra_point_result != "good"),
          dists = list(as.list(kick_distance[play_type == "field_goal" & field_goal_result == "made"])),
          kick_points = sum(kick_points, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        filter(kick_points != 0) %>%
        left_join(team_data %>% select(team_abbr, team_name), by = c("posteam" = "team_abbr")) %>%
        arrange(team_name)
    }
    
    # Punter stats
    punt_data <- pbp_data %>%
      filter(week == week_num & play_type == "punt" & !is.na(kick_distance) & kick_distance > 0)
    
    if (nrow(punt_data) == 0) {
      punt_team_stats <- data.frame()
      punter_details <- data.frame()
    } else {
      punt_team_stats <- punt_data %>%
        group_by(posteam) %>%
        summarise(
          total_punts = n(),
          punt_yardage = sum(kick_distance),
          longest_punt = max(kick_distance),
          punt_points = sum(calc_punt_points(
            kick_distance, return_yards, punt_inside_twenty, yardline_100, punt_blocked, touchback
          )),
          .groups = 'drop'
        ) %>%
        filter(total_punts > 0) %>%
        left_join(team_data %>% select(team_abbr, team_name), by = c("posteam" = "team_abbr")) %>%
        arrange(team_name)
      
      punter_details <- punt_data %>%
        group_by(posteam, punter_player_name) %>%
        summarise(
          punts = n(),
          dists = list(as.list(kick_distance)),
          punt_points = sum(calc_punt_points(
            kick_distance, return_yards, punt_inside_twenty, yardline_100, punt_blocked, touchback
          )),
          .groups = 'drop'
        ) %>%
        left_join(team_data %>% select(team_abbr, team_name), by = c("posteam" = "team_abbr")) %>%
        arrange(team_name)
    }
    
    # Combine team stats
    team_stats <- full_join(
      kicker_team_stats %>% select(posteam, team_name, fg_points, fg_dist, longest_fg),
      punt_team_stats %>% select(posteam, team_name, punt_points, punt_yardage, longest_punt),
      by = c("posteam", "team_name")
    ) %>%
      mutate(
        fg_points = ifelse(is.na(fg_points), 0, fg_points),
        punt_points = ifelse(is.na(punt_points), 0, punt_points),
        total_points = fg_points + punt_points,
        fg_dist = ifelse(is.na(fg_dist), 0, fg_dist),
        punt_yardage = ifelse(is.na(punt_yardage), 0, punt_yardage),
        longest_fg = ifelse(is.na(longest_fg), 0, longest_fg),
        longest_punt = ifelse(is.na(longest_punt), 0, longest_punt)
      ) %>%
      arrange(desc(total_points), desc(fg_dist), desc(punt_yardage), desc(longest_fg), desc(longest_punt))
    
    list(
      team_stats = team_stats,
      kicker_details = kicker_details,
      punter_details = punter_details
    )
  })
  
  # Reactive flags for data availability
  has_leaderboard <- reactive({
    req(nrow(pbp_data) > 0)
    nrow(calc_team_stats()$team_stats) > 0
  })
  
  has_team_details <- reactive({
    req(nrow(pbp_data) > 0)
    nrow(calc_team_stats()$kicker_details) > 0 || nrow(calc_team_stats()$punter_details) > 0
  })
  
  output$hasLeaderboard <- renderText({
    as.character(has_leaderboard())
  })
  
  output$hasTeamDetails <- renderText({
    as.character(has_team_details())
  })
  
  # Render leaderboard
  output$leaderboard <- renderTable({
    req(has_leaderboard())
    stats <- calc_team_stats()$team_stats
    stats %>%
      mutate(
        rank = row_number(),
        posteam_display = paste0(
          '<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', 
          posteam, 
          '.png" class="team-logo" onerror="this.style.display=\'none\'"> ', 
          posteam
        ),
        total_points = as.integer(total_points),
        fg_dist = as.integer(fg_dist),
        punt_yardage = as.integer(punt_yardage),
        longest_fg = as.integer(longest_fg),
        longest_punt = as.integer(longest_punt),
        row_class = ifelse(rank <= 3, "top-team", "")
      ) %>%
      select(posteam_display, total_points, fg_dist, punt_yardage, longest_fg, longest_punt) %>%
      `colnames<-`(c("Team", "Total PTS", "FG YDS", "Punt YDS", "Long FG", "Long Punt"))
  }, sanitize.text.function = function(x) x, escape = FALSE, classed = TRUE, rowClasses = function(df) df$row_class)
  
  # Function to generate team section
  generate_team_section <- function(team, kicker_details, punter_details) {
    team_kickers <- kicker_details %>% filter(posteam == team)
    team_punters <- punter_details %>% filter(posteam == team)
    team_logo <- tags$img(
      src = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", team, ".png"), 
      class = "team-logo",
      onerror = "this.style.display='none'"
    )
    kicker_content <- if (nrow(team_kickers) > 0) {
      lapply(1:nrow(team_kickers), function(i) {
        player <- team_kickers[i,]
        tags$p(
          sprintf(
            "Kicker %s: %d FG made (%s), %d FG missed, %d XP made, %d XP missed, %d fantasy points",
            player$kicker_player_name,
            player$made_fg,
            paste(player$dists[[1]], collapse = ", "),
            player$missed_fg,
            player$made_xp,
            player$missed_xp,
            as.integer(player$kick_points)
          )
        )
      })
    } else {
      list(tags$p("No kicker data available"))
    }
    punter_content <- if (nrow(team_punters) > 0) {
      lapply(1:nrow(team_punters), function(i) {
        player <- team_punters[i,]
        tags$p(
          sprintf(
            "Punter %s: %d punts (%s yds), %d fantasy points",
            player$punter_player_name,
            player$punts,
            paste(player$dists[[1]], collapse = ", "),
            as.integer(player$punt_points)
          )
        )
      })
    } else {
      list(tags$p("No punter data available"))
    }
    div(
      class = "player-section",
      h4(team_logo, strong(team)),
      kicker_content,
      punter_content
    )
  }
  
  # Render team details
  output$teamDetails <- renderUI({
    req(has_team_details())
    kicker_details <- calc_team_stats()$kicker_details
    punter_details <- calc_team_stats()$punter_details
    teams <- unique(c(kicker_details$posteam, punter_details$posteam))
    teams <- teams[order(team_data$team_name[match(teams, team_data$team_abbr)])]
    mid <- ceiling(length(teams) / 2)
    col1_teams <- teams[1:mid]
    col2_teams <- teams[(mid + 1):length(teams)]
    
    fluidRow(
      column(6, lapply(col1_teams, function(team) generate_team_section(team, kicker_details, punter_details))),
      column(6, lapply(col2_teams, function(team) generate_team_section(team, kicker_details, punter_details)))
    )
  })
}

shinyApp(ui, server)
