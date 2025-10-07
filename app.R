library(shiny)
library(shinyWidgets)
library(dplyr)
library(nflfastR)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Arial', sans-serif; background: #f4f7fa; color: #333; }
      .container { max-width: 1200px; margin: auto; padding: 20px; }
      h1 { color: #2c3e50; text-align: center; font-size: 2.5em; margin-bottom: 20px; }
      h3 { color: #34495e; font-size: 1.5em; margin-top: 20px; }
      .panel { background: white; border-radius: 10px; padding: 20px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin-bottom: 20px; }
      .shiny-input-container { margin-bottom: 20px; }
      table { width: 100%; border-collapse: collapse; }
      th, td { padding: 10px; text-align: left; border-bottom: 1px solid #ddd; }
      th { background: #3498db; color: white; }
      pre { background: #ecf0f1; padding: 15px; border-radius: 5px; }
      .week-selector { display: flex; justify-content: center; }
      .no-data { color: #e74c3c; text-align: center; font-size: 1.2em; }
    "))
  ),
  div(class = "container",
    h1("NFL 2025 Kicker Stats"),
    div(class = "week-selector",
      pickerInput("week", NULL, choices = 1:5, selected = 1, options = list(`style` = "btn-primary"))
    ),
    div(class = "panel",
      h3("Top 3 FG Fantasy Points"),
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
      h3("Team Details"),
      conditionalPanel(
        condition = "output.details",
        verbatimTextOutput("details")
      ),
      conditionalPanel(
        condition = "!output.details",
        div(class = "no-data", "No team details available for selected week")
      )
    )
  )
)

server <- function(input, output) {
  pbp <- reactive({
    load_pbp(2025)
  })

  compute_stats <- reactive({
    week <- as.integer(input$week)
    fg <- pbp() %>% filter(week == week & play_type == "field_goal" & kick_distance > 0)
    if (nrow(fg) == 0) {
      return(list(team_stats = data.frame(), kickers = data.frame()))
    }
    fg_made <- fg %>% filter(field_goal_result == "made")
    team_stats <- fg_made %>% group_by(posteam) %>% summarise(
      fg_points = n() * 3,
      total_dist = sum(kick_distance),
      longest = max(kick_distance),
      .groups = 'drop'
    ) %>% filter(fg_points > 0)
    kickers <- fg %>% group_by(posteam, kicker_player_name) %>% summarise(
      made = sum(field_goal_result == "made"),
      missed = sum(field_goal_result != "made"),
      dists = list(kick_distance[field_goal_result == "made"]),
      .groups = 'drop'
    )
    list(team_stats = team_stats, kickers = kickers)
  })

  output$topPoints <- renderTable({
    stats <- compute_stats()$team_stats
    stats %>% arrange(desc(fg_points)) %>% head(3) %>% select(posteam, fg_points)
  })

  output$topDist <- renderTable({
    stats <- compute_stats()$team_stats
    stats %>% arrange(desc(total_dist)) %>% head(3) %>% select(posteam, total_dist)
  })

  output$topLong <- renderTable({
    stats <- compute_stats()$team_stats
    stats %>% arrange(desc(longest)) %>% head(3) %>% select(posteam, longest)
  })

  output$details <- renderPrint({
    kickers <- compute_stats()$kickers
    if (nrow(kickers) == 0) return(NULL)
    for (team in unique(kickers$posteam)) {
      cat(team, "\n")
      team_kickers <- kickers %>% filter(posteam == team)
      for (i in 1:nrow(team_kickers)) {
        k <- team_kickers[i,]
        cat(k$kicker_player_name, ": ", k$made, " made (", paste(k$dists[[1]], collapse=", "), "), ", k$missed, " missed\n")
      }
      cat("\n")
    }
  })
}

shinyApp(ui, server)
