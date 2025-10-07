library(shiny)
library(shinyWidgets)
library(dplyr)
library(nflfastR)

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
      .player-photo { width: 50px; height: 50px; border-radius: 50%; vertical-align: middle; margin-right: 10px; }
    "))
  ),
  div(class = "container",
    h1("NFL 2025 Kicker and Punter Stats"),
    div(class = "week-selector",
      pickerInput("week", NULL, choices = 1:5, selected = 1, options = list(`style` = "btn-primary"))
    ),
    div(class = "panel",
      h3("Top 3 Kicker Fantasy Points"),
      conditionalPanel(
        condition = "output.topPoints.length > 0",
        tableOutput("topPoints")
      ),
      conditionalPanel(
        condition = "output.topPoints.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Top 3 FG Total Distance"),
      conditionalPanel(
        condition = "output.topDist.length > 0",
        tableOutput("topDist")
      ),
      conditionalPanel(
        condition = "output.topDist.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Top 3 Longest FG"),
      conditionalPanel(
        condition = "output.topLong.length > 0",
        tableOutput("topLong")
      ),
      conditionalPanel(
        condition = "output.topLong.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Kicker Team Details"),
      conditionalPanel(
        condition = "output.details",
        uiOutput("details")
      ),
      conditionalPanel(
        condition = "!output.details",
        div(class = "no-data", "No team details available for selected week")
      )
    ),
    div(class = "panel",
      h3("Top 3 Total Number of Punts"),
      conditionalPanel(
        condition = "output.topPunts.length > 0",
        tableOutput("topPunts")
      ),
      conditionalPanel(
        condition = "output.topPunts.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Top 3 Total Punt Yardage"),
      conditionalPanel(
        condition = "output.topYardage.length > 0",
        tableOutput("topYardage")
      ),
      conditionalPanel(
        condition = "output.topYardage.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Top 3 Longest Punt"),
      conditionalPanel(
        condition = "output.topLongPunt.length > 0",
        tableOutput("topLongPunt")
      ),
      conditionalPanel(
        condition = "output.topLongPunt.length == 0",
        div(class = "no-data", "No data available for selected week")
      )
    ),
    div(class = "panel",
      h3("Punter Team Details"),
      conditionalPanel(
        condition = "output.puntDetails",
        uiOutput("puntDetails")
      ),
      conditionalPanel(
        condition = "!output.puntDetails",
        div(class = "no-data", "No punter details available for selected week")
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
    fg <- pbp() %>% filter(week == week_num & play_type %in% c("field_goal", "extra_point") & kick_distance > 0)
    if (nrow(fg) == 0) {
      kicker_team_stats <- data.frame()
      kickers <- data.frame()
    } else {
      fg_made <- fg %>% filter((play_type == "field_goal" & field_goal_result == "made") | (play_type == "extra_point" & extra_point_result == "good"))
      kicker_team_stats <- fg_made %>% group_by(posteam) %>% summarise(
        fg_points = sum(ifelse(play_type == "field_goal", 3, 1)),
        total_dist = sum(kick_distance[play_type == "field_goal"]),
        longest = max(kick_distance[play_type == "field_goal"], na.rm = TRUE),
        .groups = 'drop'
      ) %>% filter(fg_points > 0)
      kickers <- fg %>% group_by(posteam, kicker_player_name) %>% summarise(
        made_fg = sum(play_type == "field_goal" & field_goal_result == "made"),
        missed_fg = sum(play_type == "field_goal" & field_goal_result != "made"),
        made_xp = sum(play_type == "extra_point" & extra_point_result == "good"),
        missed_xp = sum(play_type == "extra_point" & extra_point_result != "good"),
        dists = list(kick_distance[play_type == "field_goal" & field_goal_result == "made"]),
        .groups = 'drop'
      )
    }
    
    # Punter stats
    punt <- pbp() %>% filter(week == week_num & play_type == "punt" & kick_distance > 0)
    if (nrow(punt) == 0) {
      punt_team_stats <- data.frame()
      punters <- data.frame()
    } else {
      punt_team_stats <- punt %>% group_by(posteam) %>% summarise(
        total_punts = n(),
        total_yardage = sum(kick_distance),
        longest_punt = max(kick_distance),
        .groups = 'drop'
      ) %>% filter(total_punts > 0)
      punters <- punt %>% group_by(posteam, punter_player_name) %>% summarise(
        punts = n(),
        dists = list(kick_distance),
        .groups = 'drop'
      )
    }
    
    list(
      kicker_team_stats = kicker_team_stats, 
      kickers = kickers,
      punt_team_stats = punt_team_stats,
      punters = punters
    )
  })

  output$topPoints <- renderTable({
    stats <- compute_stats()$kicker_team_stats
    stats %>% arrange(desc(fg_points)) %>% head(3) %>% mutate(
      posteam = paste0('<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', posteam, '.png" class="team-logo"> ', posteam)
    ) %>% select(posteam, fg_points)
  }, sanitize.text.function = identity)

  output$topDist <- renderTable({
    stats <- compute_stats()$kicker_team_stats
    stats %>% arrange(desc(total_dist)) %>% head(3) %>% mutate(
      posteam = paste0('<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', posteam, '.png" class="team-logo"> ', posteam)
    ) %>% select(posteam, total_dist)
  }, sanitize.text.function = identity)

  output$topLong <- renderTable({
    stats <- compute_stats()$kicker_team_stats
    stats %>% arrange(desc(longest)) %>% head(3) %>% mutate(
      posteam = paste0('<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', posteam, '.png" class="team-logo"> ', posteam)
    ) %>% select(posteam, longest)
  }, sanitize.text.function = identity)

  output$details <- renderUI({
    kickers <- compute_stats()$kickers
    if (nrow(kickers) == 0) return(NULL)
    team_list <- lapply(unique(kickers$posteam), function(team) {
      team_kickers <- kickers %>% filter(posteam == team)
      team_content <- lapply(1:nrow(team_kickers), function(i) {
        k <- team_kickers[i,]
        player_id <- k$kicker_player_name
        tags$li(
          tags$img(src = paste0("https://a.espncdn.com/i/headshots/nfl/players/full/", player_id, ".png"), class = "player-photo", onerror = "this.src='https://a.espncdn.com/i/headshots/nfl/players/full/default.png'"),
          sprintf("%s: %d FG made (%s), %d FG missed, %d XP made, %d XP missed", 
                  k$kicker_player_name, k$made_fg, paste(k$dists[[1]], collapse=", "), k$missed_fg, k$made_xp, k$missed_xp)
        )
      })
      list(
        tags$div(
          tags$img(src = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", team, ".png"), class = "team-logo"),
          tags$strong(team),
          tags$ul(team_content)
        )
      )
    })
    tagList(team_list)
  })

  output$topPunts <- renderTable({
    stats <- compute_stats()$punt_team_stats
    stats %>% arrange(desc(total_punts)) %>% head(3) %>% mutate(
      posteam = paste0('<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', posteam, '.png" class="team-logo"> ', posteam)
    ) %>% select(posteam, total_punts)
  }, sanitize.text.function = identity)

  output$topYardage <- renderTable({
    stats <- compute_stats()$punt_team_stats
    stats %>% arrange(desc(total_yardage)) %>% head(3) %>% mutate(
      posteam = paste0('<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', posteam, '.png" class="team-logo"> ', posteam)
    ) %>% select(posteam, total_yardage)
  }, sanitize.text.function = identity)

  output$topLongPunt <- renderTable({
    stats <- compute_stats()$punt_team_stats
    stats %>% arrange(desc(longest_punt)) %>% head(3) %>% mutate(
      posteam = paste0('<img src="https://a.espncdn.com/i/teamlogos/nfl/500/', posteam, '.png" class="team-logo"> ', posteam)
    ) %>% select(posteam, longest_punt)
  }, sanitize.text.function = identity)

  output$puntDetails <- renderUI({
    punters <- compute_stats()$punters
    if (nrow(punters) == 0) return(NULL)
    team_list <- lapply(unique(punters$posteam), function(team) {
      team_punters <- punters %>% filter(posteam == team)
      team_content <- lapply(1:nrow(team_punters), function(i) {
        p <- team_punters[i,]
        player_id <- p$punter_player_name
        tags$li(
          tags$img(src = paste0("https://a.espncdn.com/i/headshots/nfl/players/full/", player_id, ".png"), class = "player-photo", onerror = "this.src='https://a.espncdn.com/i/headshots/nfl/players/full/default.png'"),
          sprintf("%s: %d punts (%s)", 
                  p$punter_player_name, p$punts, paste(p$dists[[1]], collapse=", "))
        )
      })
      list(
        tags$div(
          tags$img(src = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", team, ".png"), class = "team-logo"),
          tags$strong(team),
          tags$ul(team_content)
        )
      )
    })
    tagList(team_list)
  })
}

shinyApp(ui, server)
