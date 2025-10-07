library(shiny)
library(shinyWidgets)
library(dplyr)
library(nflfastR)
library(DT)

# Team logo mapping (using NFL team abbreviations and logo URLs from ESPN or similar)
team_logos <- data.frame(
  posteam = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", 
              "DET", "GB", "HOU", "IND", "JAX", "KC", "LAC", "LAR", "LV", "MIA", 
              "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", "SF", "TB", 
              "TEN", "WAS"),
  logo_url = c(
    "https://a.espncdn.com/i/teamlogos/nfl/500/ari.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/atl.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/bal.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/buf.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/car.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/chi.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/cin.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/cle.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/dal.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/den.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/det.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/gb.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/hou.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/ind.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/jax.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/kc.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/lac.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/lar.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/lv.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/mia.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/min.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/ne.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/no.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/nyg.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/nyj.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/phi.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/pit.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/sea.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/sf.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/tb.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/ten.png",
    "https://a.espncdn.com/i/teamlogos/nfl/500/was.png"
  )
)

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
      .team-logo { width: 30px; height: 30px; vertical-align: middle; margin-right: 10px; }
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
      ) %>% filter(fg_points > 0) %>% left_join(team_logos, by = "posteam")
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
      ) %>% filter(total_punts > 0) %>% left_join(team_logos, by = "posteam")
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
      stats %>% arrange(desc(fg_points)) %>% head(3) %>% select(posteam, logo_url, fg_points),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(
            render = JS(
              "function(data, type, row) {",
              "  return type === 'display' && data ? '<img src=\"' + data + '\" class=\"team-logo\" alt=\"Team Logo\" />' : data;",
              "}"
            ),
            targets = 1
          )
        )
      ),
      class = 'dataTable',
      colnames = c("Team", "Logo", "Fantasy Points"),
      escape = FALSE
    )
  })

  output$topDist <- renderDT({
    stats <- compute_stats()$kicker_team_stats
    datatable(
      stats %>% arrange(desc(total_dist)) %>% head(3) %>% select(posteam, logo_url, total_dist),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(
            render = JS(
              "function(data, type, row) {",
              "  return type === 'display' && data ? '<img src=\"' + data + '\" class=\"team-logo\" alt=\"Team Logo\" />' : data;",
              "}"
            ),
            targets = 1
          )
        )
      ),
      class = 'dataTable',
      colnames = c("Team", "Logo", "Total Distance (yds)"),
      escape = FALSE
    )
  })

  output$topLong <- renderDT({
    stats <- compute_stats()$kicker_team_stats
    datatable(
      stats %>% arrange(desc(longest)) %>% head(3) %>% select(posteam, logo_url, longest),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(
            render = JS(
              "function(data, type, row) {",
              "  return type === 'display' && data ? '<img src=\"' + data + '\" class=\"team-logo\" alt=\"Team Logo\" />' : data;",
              "}"
            ),
            targets = 1
          )
        )
      ),
      class = 'dataTable',
      colnames = c("Team", "Logo", "Longest FG (yds)"),
      escape = FALSE
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
      stats %>% arrange(desc(total_punts)) %>% head(3) %>% select(posteam, logo_url, total_punts),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(
            render = JS(
              "function(data, type, row) {",
              "  return type === 'display' && data ? '<img src=\"' + data + '\" class=\"team-logo\" alt=\"Team Logo\" />' : data;",
              "}"
            ),
            targets = 1
          )
        )
      ),
      class = 'dataTable',
      colnames = c("Team", "Logo", "Total Punts"),
      escape = FALSE
    )
  })

  output$topYardage <- renderDT({
    stats <- compute_stats()$punt_team_stats
    datatable(
      stats %>% arrange(desc(total_yardage)) %>% head(3) %>% select(posteam, logo_url, total_yardage),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(
            render = JS(
              "function(data, type, row) {",
              "  return type === 'display' && data ? '<img src=\"' + data + '\" class=\"team-logo\" alt=\"Team Logo\" />' : data;",
              "}"
            ),
            targets = 1
          )
        )
      ),
      class = 'dataTable',
      colnames = c("Team", "Logo", "Total Yardage (yds)"),
      escape = FALSE
    )
  })

  output$topLongPunt <- renderDT({
    stats <- compute_stats()$punt_team_stats
    datatable(
      stats %>% arrange(desc(longest_punt)) %>% head(3) %>% select(posteam, logo_url, longest_punt),
      options = list(
        dom = 't',
        pageLength = 3,
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(
            render = JS(
              "function(data, type, row) {",
              "  return type === 'display' && data ? '<img src=\"' + data + '\" class=\"team-logo\" alt=\"Team Logo\" />' : data;",
              "}"
            ),
            targets = 1
          )
        )
      ),
      class = 'dataTable',
      colnames = c("Team", "Logo", "Longest Punt (yds)"),
      escape = FALSE
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
