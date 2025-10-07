library(shiny)
library(shinyWidgets)
library(dplyr)
library(nflfastR)
library(DT)

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
      .dataTable { background: #212121; border-radius: 5px; overflow: hidden; }
      .dataTable thead th { background: #1976d2; color: #ffffff; font-weight: 500; padding: 12px; border-bottom: 2px solid #1565c0; }
      .dataTable tbody td { padding: 12px; border-bottom: 1px solid #333; }
      .dataTable tbody tr:hover { background: #2c2c2c; }
      .dataTable tbody tr:nth-child(even) { background: #262626; }
      pre { background: #212121; padding: 15px; border-radius: 5px; color: #e0e0e0; }
      .week-selector { display: flex; justify-content: center; }
      .no-data { color: #ef5350; text-align: center; font-size: 1.2em; }
      .dt-buttons { margin-bottom: 10px; }
      .dt-button { background: #1976d2 !important; color: #ffffff !important; border-radius: 5px; padding: 5px 10px; }
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
        DTOutput("topPoints")
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
        DTOutput("topDist")
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
        DTOutput("topLong")
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
        verbatimTextOutput("details")
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
        DTOutput("topPunts")
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
        DTOutput("topYardage")
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
        DTOutput("topLongPunt")
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
        verbatimTextOutput("puntDetails")
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

  output$topPoints <- renderDT({
    stats <- compute_stats()$kicker_team_stats
    datatable(
      stats %>% arrange(desc(fg_points)) %>% head(3) %>% select(posteam, fg_points),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      class = 'dataTable',
      colnames = c("Team", "Fantasy Points")
    )
  })

  output$topDist <- renderDT({
    stats <- compute_stats()$kicker_team_stats
    datatable(
      stats %>% arrange(desc(total_dist)) %>% head(3) %>% select(posteam, total_dist),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      class = 'dataTable',
      colnames = c("Team", "Total Distance (yds)")
    )
  })

  output$topLong <- renderDT({
    stats <- compute_stats()$kicker_team_stats
    datatable(
      stats %>% arrange(desc(longest)) %>% head(3) %>% select(posteam, longest),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      class = 'dataTable',
      colnames = c("Team", "Longest FG (yds)")
    )
  })

  output$details <- renderPrint({
    kickers <- compute_stats()$kickers
    if (nrow(kickers) == 0) return(NULL)
    for (team in unique(kickers$posteam)) {
      cat(team, "\n")
      team_kickers <- kickers %>% filter(posteam == team)
      for (i in 1:nrow(team_kickers)) {
        k <- team_kickers[i,]
        cat(k$kicker_player_name, ": ", k$made_fg, " FG made (", paste(k$dists[[1]], collapse=", "), "), ", k$missed_fg, " FG missed, ", k$made_xp, " XP made, ", k$missed_xp, " XP missed\n")
      }
      cat("\n")
    }
  })

  output$topPunts <- renderDT({
    stats <- compute_stats()$punt_team_stats
    datatable(
      stats %>% arrange(desc(total_punts)) %>% head(3) %>% select(posteam, total_punts),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      class = 'dataTable',
      colnames = c("Team", "Total Punts")
    )
  })

  output$topYardage <- renderDT({
    stats <- compute_stats()$punt_team_stats
    datatable(
      stats %>% arrange(desc(total_yardage)) %>% head(3) %>% select(posteam, total_yardage),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      class = 'dataTable',
      colnames = c("Team", "Total Yardage (yds)")
    )
  })

  output$topLongPunt <- renderDT({
    stats <- compute_stats()$punt_team_stats
    datatable(
      stats %>% arrange(desc(longest_punt)) %>% head(3) %>% select(posteam, longest_punt),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      class = 'dataTable',
      colnames = c("Team", "Longest Punt (yds)")
    )
  })

  output$puntDetails <- renderPrint({
    punters <- compute_stats()$punters
    if (nrow(punters) == 0) return(NULL)
    for (team in unique(punters$posteam)) {
      cat(team, "\n")
      team_punters <- punters %>% filter(posteam == team)
      for (i in 1:nrow(team_punters)) {
        p <- team_punters[i,]
        cat(p$punter_player_name, ": ", p$punts, " punts (", paste(p$dists[[1]], collapse=", "), ")\n")
      }
      cat("\n")
    }
  })
}

shinyApp(ui, server)
