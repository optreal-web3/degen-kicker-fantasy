library(shiny)
library(shinyWidgets)
library(dplyr)

# Mock data
mock_data <- data.frame(
  week = c(1,1,1,2,2,3,3,4,4,5),
  posteam = c("NE","KC","DAL","NE","KC","DAL","NE","KC","DAL","NE"),
  kicker_player_name = c("K1","K2","K3","K1","K2","K3","K1","K2","K3","K1"),
  play_type = rep("field_goal", 10),
  kick_distance = c(45,50,30,40,55,35,48,52,33,42),
  field_goal_result = c("made","made","missed","made","missed","made","made","made","made","missed")
)

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
    "))
  ),
  div(class = "container",
    h1("NFL 2025 Kicker Stats Preview"),
    div(class = "week-selector",
      pickerInput("week", NULL, choices = 1:5, selected = 1, options = list(`style` = "btn-primary"))
    ),
    div(class = "panel",
      h3("Top 3 FG Fantasy Points"),
      tableOutput("topPoints")
    ),
    div(class = "panel",
      h3("Top 3 FG Total Distance"),
      tableOutput("topDist")
    ),
    div(class = "panel",
      h3("Top 3 Longest FG"),
      tableOutput("topLong")
    ),
    div(class = "panel",
      h3("Team Details"),
      verbatimTextOutput("details")
    )
  )
)

server <- function(input, output) {
  compute_stats <- reactive({
    week <- as.integer(input$week)
    fg <- mock_data %>% filter(week == week & play_type == "field_goal" & kick_distance > 0)
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
