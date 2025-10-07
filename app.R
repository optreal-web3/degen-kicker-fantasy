library(shiny)
library(shinyWidgets)
library(dplyr)
library(nflfastR)

# Load team data for city names
team_data <- teams_colors_logos

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap"),
    tags$style(HTML("
      body { font-family: 'Roboto', sans-serif; background: #121212; color: #e0e0e0; }
      .header { background: #1e1e1e; padding: 15px; text-align: center; border-bottom: 2px solid #1976d2; margin-bottom: 20px; }
      .header h1 { color: #ffffff; font-size: 2em; margin: 0; max-width: 1500px; }
      .header h1 span { font-weight: bold; font-style: italic; }
      .container { max-width: 1500px; margin: auto; padding: 20px; }
      .week-selector { display: flex; justify-content: center; margin-bottom: 20px; }
      .panel-row { display: flex; justify-content: space-between; gap: 20px; margin-bottom: 20px; }
      .panel { background: #2a2a2a; border-radius: 10px; padding: 20px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); flex: 1; display: flex; flex-direction: column; justify-content: center; }
      h3 { color: #bbdefb; font-size: 1.5em; margin-top: 0; text-align: center; }
      table { width: 100%; border-collapse: collapse; background: #333333; }
      th, td { padding: 10px; text-align: center; border-bottom: 1px solid #444; }
      th { background: #1976d2; color: #ffffff; }
      .no-data { color: #ef5350; text-align: center; font-size: 1.2em; }
      .team-logo { width: 40px; height: 40px; vertical-align: middle; margin-right: 10px; }
      .player-section { margin-bottom: 15px; padding: 10px; background: #333333; border-radius: 5px; }
      .leaders-header { text-align: center; color: #ffffff; font-size: 1.8em; margin-bottom: 10px; }
    "))
  ),
  div(class = "header",
    h1(HTML("The Real Fantasy <span>Football</span> Challenge"))
  ),
  div(class = "container",
    div(class = "week-selector",
      pickerInput("week", "Select Week", choices = 1:5, selected = 1, options = list(style = "btn-primary", width = "200px"))
    ),
    div(class = "leaders-header", "Kicking Leaders"),
    div(class = "panel-row",
      div(class = "panel",
        h3("Fantasy Points"),
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
        h3("Total Yardage"),
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
        h3("Longest Made"),
        conditionalPanel(
          condition = "output.topLong !== undefined && output.topLong.length > 0",
          tableOutput("topLong")
        ),
        conditionalPanel(
          condition = "output.topLong === undefined || output.topLong.length == 0",
          div(class = "no-data", "No data available for selected week")
        )
      )
    ),
    div(class = "leaders-header", "Punting Leaders"),
    div(class = "panel-row",
      div(class = "panel",
        h3("Fantasy Points"),
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
        h3("Total Yardage"),
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
        h3("Longest Punt"),
        conditionalPanel(
          condition = "output.topLongPunt !== undefined && output.topLongPunt.length > 0",
          tableOutput("topLongPunt")
        ),
        conditionalPanel(
          condition = "output.topLongPunt === undefined || output.topLongPunt.length == 0",
          div(class = "no-data", "No data available for selected week")
        )
      )
    ),
    div(class = "panel",
      h3("Team Special Teams Details"),
      conditionalPanel(
        condition = "output.teamDetails !== undefined",
        uiOutput("teamDetails")
      ),
      conditionalPanel(
        condition = "output.teamDetails === undefined",
        div(class = "no-data", "No team details available")
      )
    )
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
        filter(fg_points > 0) %>%
        left_join(team_data %>% select(team_abbr, team_city), by = c("posteam" = "team_abbr")) %>%
        arrange(team_city)
      
      kicker_details <- fg_data %>%
        group_by(posteam, kicker_player_name) %>%
        summarise(
          made_fg = sum(play_type == "field_goal" & field_goal_result == "made"),
          missed_fg = sum(play_type == "field_goal" & field_goal_result != "made" & !is.na(field_goal_result)),
          made_xp = sum(play_type == "extra_point" & extra_point_result == "good"),
          missed_xp = sum(play_type == "extra_point" & extra_point_result != "good"),
          dists = list(kick_distance[play_type == "field_goal" & field_goal_result == "made"]),
          .groups = 'drop'
        ) %>%
        filter(made_fg + made_xp > 0) %>%
        left_join(team_data %>% select(team_abbr, team_city), by = c("posteam" = "team_abbr")) %>%
        arrange(team_city)
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
          punt_points = sum(
            (kick_distance * 0.05) - 
            (ifelse(is.na(return_yards), 0, return_yards) * 0.05) + 
            (punt_inside_twenty * 0.5) + 
            (ifelse(yardline_100 <= 10 & punt_blocked == 0 & touchback == 0 & is.na(return_yards), 1.5, 0)) - 
            (touchback * 5) - 
            (punt_blocked * 4)
          ),
          .groups = 'drop'
        ) %>%
        filter(total_punts > 0) %>%
        left_join(team_data %>% select(team_abbr, team_city), by = c("posteam" = "team_abbr")) %>%
        arrange(team_city)
      
      punter_details <- punt_data %>%
        group_by(posteam, punter_player_name) %>%
        summarise(
          punts = n(),
          dists = list(kick_distance),
          punt_points = sum(
            (kick_distance * 0.05) - 
            (ifelse(is.na(return_yards), 0, return_yards) * 0.05) + 
            (punt_inside_twenty * 0.5) + 
            (ifelse(yardline_100 <= 10 & punt_blocked == 0 & touchback == 0 & is.na(return_yards), 1.5, 0)) - 
            (touchback * 5) - 
            (punt_blocked * 4)
          ),
          .groups = 'drop'
        ) %>%
        left_join(team_data %>% select(team_abbr, team_city), by = c("posteam" = "team_abbr")) %>%
        arrange(team_city)
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
        ),
        fg_points = as.integer(fg_points)
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
        ),
        total_dist = as.integer(total_dist)
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
        ),
        longest = as.integer(longest)
      ) %>%
      select(posteam_display, longest) %>%
      `colnames<-`(c("Team", "LONG"))
  }, sanitize.text.function = function(x) x, escape = FALSE)

  output$topPunts <- renderTable({
    stats <- compute_stats()$punt_team_stats
    if (nrow(stats) == 0) return(NULL)
    stats %>%
      arrange(desc(punt_points)) %>%
      head(3) %>%
      mutate(
        posteam_display = paste0(
          '<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', 
          posteam, 
          '.png" class="team-logo" onerror="this.style.display=\'none\'"> ', 
          posteam
        ),
        punt_points = as.integer(punt_points)
      ) %>%
      select(posteam_display, punt_points) %>%
      `colnames<-`(c("Team", "PTS"))
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
        ),
        total_yardage = as.integer(total_yardage)
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
        ),
        longest_punt = as.integer(longest_punt)
      ) %>%
      select(posteam_display, longest_punt) %>%
      `colnames<-`(c("Team", "LONG"))
  }, sanitize.text.function = function(x) x, escape = FALSE)

  output$teamDetails <- renderUI({
    kicker_details <- compute_stats()$kicker_details
    punter_details <- compute_stats()$punter_details
    teams <- unique(c(kicker_details$posteam, punter_details$posteam))
    if (length(teams) == 0) return(NULL)
    teams <- teams[order(team_data$team_city[match(teams, team_data$team_abbr)])]
    lapply(teams, function(team) {
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
              "Kicker %s: %d FG made (%s), %d FG missed, %d XP made, %d XP missed",
              player$kicker_player_name,
              player$made_fg,
              paste(player$dists[[1]], collapse = ", "),
              player$missed_fg,
              player$made_xp,
              player$missed_xp
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
    })
  })
}

shinyApp(ui, server)
