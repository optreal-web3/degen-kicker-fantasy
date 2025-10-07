library(shiny)
library(shinyWidgets)
library(dplyr)
library(nflfastR)
library(nflreadr)

# Load players once for IDs
players_df <- load_players()

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap"),
    tags$style(HTML("
      body { font-family: 'Roboto', sans-serif; background: #121212; color: #e0e0e0; }
      .container { max-width: 1200px; margin: auto; padding: 20px; }
      h1 { color: #ffffff; text-align: center; font-size: 2.5em; margin-bottom: 20px; }
      h3 { color: #bbdefb; font-size: 1.5em; margin-top: 20px; }
      .panel { background: #1e1e1e; border-radius: 10px; padding: 20px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); margin-bottom: 20px; }
      .shiny-input-container { margin-bottom: 20px; }
      table { width: 100%; border-collapse: collapse; }
      th, td { padding: 10px; text-align: left; border-bottom: 1px solid #333; }
      th { background: #1976d2; color: #ffffff; }
      pre { background: #212121; padding: 15px; border-radius: 5px; color: #e0e0e0; }
      .week-selector { display: flex; justify-content: center; }
      .no-data { color: #ef5350; text-align: center; font-size: 1.2em; }
      .team-logo { width: 40px; height: 40px; vertical-align: middle; margin-right: 10px; }
      .player-photo { width: 50px; height: 50px; border-radius: 50%; vertical-align: middle; margin-right: 10px; object-fit: cover; }
      .player-section { margin-bottom: 15px; padding: 10px; background: #2a2a2a; border-radius: 5px; }
    "))
  ),
  div(class = "container",
    h1("Degen Kicker Fantasy Stats"),
    div(class = "week-selector",
      pickerInput("week", "Select Week", choices = 1:5, selected = 1, options = list(style = "btn-primary", width = "200px"))
    ),
    div(class = "panel",
      h3("Top 3 Kicker Fantasy Points"),
      conditionalPanel(
        condition = "output.topPoints !== undefined && output.topPoints.length > 0",
        tableOutput("topPoints")
      ),
      conditionalPanel(
        condition = "output.topPoints === undefined || output.topPoints.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Top 3 FG Total Distance"),
      conditionalPanel(
        condition = "output.topDist !== undefined && output.topDist.length > 0",
        tableOutput("topDist")
      ),
      conditionalPanel(
        condition = "output.topDist === undefined || output.topDist.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Top 3 Longest FG"),
      conditionalPanel(
        condition = "output.topLong !== undefined && output.topLong.length > 0",
        tableOutput("topLong")
      ),
      conditionalPanel(
        condition = "output.topLong === undefined || output.topLong.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Kicker Details"),
      conditionalPanel(
        condition = "output.kickerDetails !== undefined",
        uiOutput("kickerDetails")
      ),
      conditionalPanel(
        condition = "output.kickerDetails === undefined",
        div(class = "no-data", "No kicker details available")
      )
    ),
    div(class = "panel",
      h3("Top 3 Total Punts"),
      conditionalPanel(
        condition = "output.topPunts !== undefined && output.topPunts.length > 0",
        tableOutput("topPunts")
      ),
      conditionalPanel(
        condition = "output.topPunts === undefined || output.topPunts.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Top 3 Punt Yardage"),
      conditionalPanel(
        condition = "output.topYardage !== undefined && output.topYardage.length > 0",
        tableOutput("topYardage")
      ),
      conditionalPanel(
        condition = "output.topYardage === undefined || output.topYardage.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Top 3 Longest Punt"),
      conditionalPanel(
        condition = "output.topLongPunt !== undefined && output.topLongPunt.length > 0",
        tableOutput("topLongPunt")
      ),
      conditionalPanel(
        condition = "output.topLongPunt === undefined || output.topLongPunt.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Punter Details"),
      conditionalPanel(
        condition = "output.punterDetails !== undefined",
        uiOutput("punterDetails")
      ),
      conditionalPanel(
        condition = "output.punterDetails === undefined",
        div(class = "no-data", "No punter details available")
      )
    ),
    tags$hr(),
    div(style = "text-align: center; color: #888;", "A QB is only as good as his kicker.")
  )
)

server <- function(input, output) {
  pbp <- reactive({
    load_pbp(2025)
  })

  compute_stats <- reactive({
    week_num <- as.integer(input$week)
    # Kicker stats
    fg_data <- pbp() %>% filter(week == week_num & play_type %in% c("field_goal", "extra_point") & !is.na(kick_distance) & kick_distance > 0)
    if (nrow(fg_data) == 0) {
      kicker_team_stats <- data.frame()
      kicker_details <- data.frame()
    } else {
      fg_made <- fg_data %>% filter(
        (play_type == "field_goal" & field_goal_result == "made") | 
        (play_type == "extra_point" & extra_point_result == "good")
      )
      kicker_team_stats <- fg_made %>%
        group_by(posteam) %>%
        summarise(
          fg_points = sum(ifelse(play_type == "field_goal", 3, 1)),
          total_dist = sum(kick_distance[play_type == "field_goal"], na.rm = TRUE),
          longest = max(kick_distance[play_type == "field_goal"], na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        filter(fg_points > 0)
      
      kicker_details <- fg_data %>%
        left_join(players_df %>% select(display_name, espn_id), by = c("kicker_player_name" = "display_name")) %>%
        group_by(posteam, kicker_player_name, espn_id) %>%
        summarise(
          made_fg = sum(play_type == "field_goal" & field_goal_result == "made"),
          missed_fg = sum(play_type == "field_goal" & field_goal_result != "made" & !is.na(field_goal_result)),
          made_xp = sum(play_type == "extra_point" & extra_point_result == "good"),
          missed_xp = sum(play_type == "extra_point" & extra_point_result != "good"),
          dists = list(kick_distance[play_type == "field_goal" & field_goal_result == "made"]),
          .groups = 'drop'
        ) %>%
        filter(made_fg + made_xp > 0)
    }
    
    # Punter stats
    punt_data <- pbp() %>% filter(week == week_num & play_type == "punt" & !is.na(kick_distance) & kick_distance > 0)
    if (nrow(punt_data) == 0) {
      punt_team_stats <- data.frame()
      punter_details <- data.frame()
    } else {
      punt_team_stats <- punt_data %>%
        group_by(posteam) %>%
        summarise(
          total_punts = n(),
          total_yardage = sum(kick_distance),
          longest_punt = max(kick_distance),
          .groups = 'drop'
        ) %>%
        filter(total_punts > 0)
      
      punter_details <- punt_data %>%
        left_join(players_df %>% select(display_name, espn_id), by = c("punter_player_name" = "display_name")) %>%
        group_by(posteam, punter_player_name, espn_id) %>%
        summarise(
          punts = n(),
          dists = list(kick_distance),
          .groups = 'drop'
        )
    }
    
    list(
      kicker_team_stats = kicker_team_stats, 
      kicker_details = kicker_details,
      punt_team_stats = punt_team_stats,
      punter_details = punter_details
    )
  })

  output$topPoints <- renderTable({
    stats <- compute_stats()$kicker_team_stats
    if (nrow(stats) == 0) return(NULL)
    stats %>%
      arrange(desc(fg_points)) %>%
      head(3) %>%
      mutate(
        posteam_display = paste0(
          '<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', 
          posteam, 
          '.png" class="team-logo" onerror="this.style.display=\'none\'"> ', 
          posteam
        )
      ) %>%
      select(posteam_display, fg_points) %>%
      `colnames<-`(c("Team", "PTS"))
  }, sanitize.text.function = function(x) x, escape = FALSE)

  output$topDist <- renderTable({
    stats <- compute_stats()$kicker_team_stats
    if (nrow(stats) == 0) return(NULL)
    stats %>%
      arrange(desc(total_dist)) %>%
      head(3) %>%
      mutate(
        posteam_display = paste0(
          '<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', 
          posteam, 
          '.png" class="team-logo" onerror="this.style.display=\'none\'"> ', 
          posteam
        )
      ) %>%
      select(posteam_display, total_dist) %>%
      `colnames<-`(c("Team", "YDS"))
  }, sanitize.text.function = function(x) x, escape = FALSE)

  output$topLong <- renderTable({
    stats <- compute_stats()$kicker_team_stats
    if (nrow(stats) == 0) return(NULL)
    stats %>%
      arrange(desc(longest)) %>%
      head(3) %>%
      mutate(
        posteam_display = paste0(
          '<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', 
          posteam, 
          '.png" class="team-logo" onerror="this.style.display=\'none\'"> ', 
          posteam
        )
      ) %>%
      select(posteam_display, longest) %>%
      `colnames<-`(c("Team", "LONG"))
  }, sanitize.text.function = function(x) x, escape = FALSE)

  output$kickerDetails <- renderUI({
    details <- compute_stats()$kicker_details
    if (nrow(details) == 0) return(NULL)
    teams <- unique(details$posteam)
    lapply(teams, function(team) {
      team_players <- details %>% filter(posteam == team)
      team_logo <- tags$img(
        src = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", team, ".png"), 
        class = "team-logo",
        onerror = "this.style.display='none'"
      )
      div(
        class = "player-section",
        h4(team_logo, strong(team)),
        lapply(1:nrow(team_players), function(i) {
          player <- team_players[i,]
          photo_src <- if (!is.na(player$espn_id) && player$espn_id != "") {
            paste0("https://a.espncdn.com/i/headshots/nfl/players/full/", player$espn_id, ".png")
          } else {
            "https://a.espncdn.com/i/headshots/nfl/players/full/default.png"
          }
          tags$p(
            tags$img(src = photo_src, class = "player-photo", onerror = "this.src='https://a.espncdn.com/i/headshots/nfl/players/full/default.png'"),
            sprintf(
              "%s: %d FG made (%s), %d FG missed, %d XP made, %d XP missed",
              player$kicker_player_name,
              player$made_fg,
              paste(player$dists[[1]], collapse = ", "),
              player$missed_fg,
              player$made_xp,
              player$missed_xp
            )
          )
        })
      )
    })
  })

  output$topPunts <- renderTable({
    stats <- compute_stats()$punt_team_stats
    if (nrow(stats) == 0) return(NULL)
    stats %>%
      arrange(desc(total_punts)) %>%
      head(3) %>%
      mutate(
        posteam_display = paste0(
          '<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', 
          posteam, 
          '.png" class="team-logo" onerror="this.style.display=\'none\'"> ', 
          posteam
        )
      ) %>%
      select(posteam_display, total_punts) %>%
      `colnames<-`(c("Team", "PUNTS"))
  }, sanitize.text.function = function(x) x, escape = FALSE)

  output$topYardage <- renderTable({
    stats <- compute_stats()$punt_team_stats
    if (nrow(stats) == 0) return(NULL)
    stats %>%
      arrange(desc(total_yardage)) %>%
      head(3) %>%
      mutate(
        posteam_display = paste0(
          '<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', 
          posteam, 
          '.png" class="team-logo" onerror="this.style.display=\'none\'"> ', 
          posteam
        )
      ) %>%
      select(posteam_display, total_yardage) %>%
      `colnames<-`(c("Team", "YDS"))
  }, sanitize.text.function = function(x) x, escape = FALSE)

  output$topLongPunt <- renderTable({
    stats <- compute_stats()$punt_team_stats
    if (nrow(stats) == 0) return(NULL)
    stats %>%
      arrange(desc(longest_punt)) %>%
      head(3) %>%
      mutate(
        posteam_display = paste0(
          '<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', 
          posteam, 
          '.png" class="team-logo" onerror="this.style.display=\'none\'"> ', 
          posteam
        )
      ) %>%
      select(posteam_display, longest_punt) %>%
      `colnames<-`(c("Team", "LONG"))
  }, sanitize.text.function = function(x) x, escape = FALSE)

  output$punterDetails <- renderUI({
    details <- compute_stats()$punter_details
    if (nrow(details) == 0) return(NULL)
    teams <- unique(details$posteam)
    lapply(teams, function(team) {
      team_players <- details %>% filter(posteam == team)
      team_logo <- tags$img(
        src = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", team, ".png"), 
        class = "team-logo",
        onerror = "this.style.display='none'"
      )
      div(
        class = "player-section",
        h4(team_logo, strong(team)),
        lapply(1:nrow(team_players), function(i) {
          player <- team_players[i,]
          photo_src <- if (!is.na(player$espn_id) && player$espn_id != "") {
            paste0("https://a.espncdn.com/i/headshots/nfl/players/full/", player$espn_id, ".png")
          } else {
            "https://a.espncdn.com/i/headshots/nfl/players/full/default.png"
          }
          tags$p(
            tags$img(src = photo_src, class = "player-photo", onerror = "this.src='https://a.espncdn.com/i/headshots/nfl/players/full/default.png'"),
            sprintf(
              "%s: %d punts (%s yds)",
              player$punter_player_name,
              player$punts,
              paste(player$dists[[1]], collapse = ", ")
            )
          )
        })
      )
    })
  })
}

shinyApp(ui, server)
