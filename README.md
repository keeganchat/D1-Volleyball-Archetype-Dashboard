# D1-Volleyball-Archetype-Dashboard
Division 1 womens volleyball dashboard using shinyr package to compare player archetypes

```{r setup, include=FALSE}
library(shiny)
install.packages("shinydashboard")
library(shinydashboard)
install.packages("tidyverse")
library(tidyverse)
install.packages("DT")
library(DT)
install.packages("plotly")
library(plotly)

knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(out.width = "60%",fig.align='center')
```

```{r}
BASE_URL <- "https://media.githubusercontent.com/media/JeffreyRStevens/ncaavolleyballr/refs/heads/main/data-csv/"

sports   <- "wvb"
divs     <- c(1, 2, 3)
years    <- 2020:2024

build_url <- function(sport, div, year) {
  paste0(BASE_URL, sport, "_playerseason_div", div, "_", year, ".csv")
}
load_data <- function() {
  combos <- expand.grid(sport = sports, div = divs, year = years, stringsAsFactors = FALSE)
  dfs <- lapply(seq_len(nrow(combos)), function(i) {
    url <- build_url(combos$sport[i], combos$div[i], combos$year[i])
    tryCatch({
      df <- read.csv(url, stringsAsFactors = FALSE, check.names = FALSE)
      df$Sport <- ifelse(combos$sport[i] == "wvb", "Women's", "Men's")
      df$Division <- paste0("Div ", combos$div[i])
      df
    }, error = function(e) NULL)
  })
  bind_rows(Filter(Negate(is.null), dfs))
}
```

```{r}
# Position labels
pos_labels <- c(
  "OH"  = "Outside Hitter (OH)",
  "MB"  = "Middle Blocker (MB)",
  "MH"  = "Middle Hitter (MH)",
  "S"   = "Setter (S)",
  "RS"  = "Right Side (RS)",
  "OPP" = "Opposite (OPP)",
  "DS"  = "Defensive Specialist (DS)",
  "L"   = "Libero (L)",
  "LS"  = "Libero/DS (LS)"
)
 
# Stats available per position group
stat_choices <- c(
  "Kills", "Errors", "Total Attacks", "Hit Pct",
  "Assists", "Aces", "Digs",
  "Block Solos", "Block Assists", "PTS"
)
```
#position third axis and color to be a third variable

```{r}
ui <- dashboardPage(
  skin = "blue",
 
  dashboardHeader(title = "NCAA Volleyball Dashboard"),
 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",      tabName = "overview",  icon = icon("volleyball-ball")),
      menuItem("Player Stats",  tabName = "players",   icon = icon("table")),
      menuItem("Position Comp", tabName = "poscomp",   icon = icon("chart-bar")),
      menuItem("Scatter Plot",  tabName = "scatter",   icon = icon("circle-dot"))
    ),
    hr(),
    # Global filters
    selectInput("sport_filter",  "Sport",    choices = c("Both", "Women's", "Men's"), selected = "Women's"),
    checkboxGroupInput("div_filter", "Division",
                       choices  = c("Div 1", "Div 2", "Div 3"),
                       selected = "Div 1"),
    selectInput("season_filter", "Season",
                choices  = rev(paste0(2020:2024, "-", 2021:2025)),
                selected = "2024-2025"),
    hr(),
    tags$small(class = "text-muted", style = "padding-left:10px;",
               "Data: JeffreyRStevens/ncaavolleyballr")
  ),
 
  dashboardBody(
    tags$head(tags$style(HTML("
      .box { border-radius: 8px; }
      .value-box .inner h3 { font-size: 28px; }
      .skin-blue .main-header .logo { font-weight: bold; }
      .position-badge {
        display:inline-block; padding:3px 8px; border-radius:12px;
        color:#fff; font-size:11px; font-weight:600; margin:2px;
      }
    "))),
 
    tabItems(
 
      # ── Overview ────────────────────────────────────────────────────────────
      tabItem("overview",
        fluidRow(
          valueBoxOutput("vb_players",   width = 3),
          valueBoxOutput("vb_teams",     width = 3),
          valueBoxOutput("vb_positions", width = 3),
          valueBoxOutput("vb_avg_pts",   width = 3)
        ),
        fluidRow(
          box(title = "Players by Position", status = "primary", solidHeader = TRUE,
              width = 6, plotlyOutput("pos_dist_plot", height = 340)),
          box(title = "Average Points by Position", status = "info", solidHeader = TRUE,
              width = 6, plotlyOutput("pos_pts_plot",  height = 340))
        ),
        fluidRow(
          box(title = "Top 10 Scorers (PTS)", status = "warning", solidHeader = TRUE,
              width = 12, plotlyOutput("top_scorers_plot", height = 300))
        )
      ),
 
      # ── Player Stats Table ───────────────────────────────────────────────────
      tabItem("players",
        fluidRow(
          box(width = 12, status = "primary",
            fluidRow(
              column(4, checkboxGroupInput("pos_filter", "Filter by Position",
                                          choices  = names(pos_labels),
                                          selected = names(pos_labels),
                                          inline   = TRUE)),
              column(4, selectInput("yr_filter", "Class Year",
                                    choices  = c("All", "Fr", "So", "Jr", "Sr"),
                                    selected = "All")),
              column(4, numericInput("min_sets", "Min Sets Played", value = 10, min = 0))
            )
          )
        ),
        fluidRow(
          box(width = 12, title = "Player Season Statistics",
              status = "primary", solidHeader = TRUE,
              DTOutput("player_table"))
        )
      ),
 
      # ── Position Comparison ──────────────────────────────────────────────────
      tabItem("poscomp",
        fluidRow(
          box(width = 4, status = "primary",
              selectInput("comp_stat", "Statistic to Compare",
                          choices  = stat_choices,
                          selected = "Kills"),
              radioButtons("comp_type", "Summary Type",
                           choices  = c("Mean", "Median", "Total"),
                           selected = "Mean", inline = TRUE),
              checkboxGroupInput("comp_pos", "Positions to Include",
                                 choices  = names(pos_labels),
                                 selected = names(pos_labels))
          ),
          box(width = 8, title = "Position Comparison", status = "info", solidHeader = TRUE,
              plotlyOutput("comp_plot", height = 420))
        ),
        fluidRow(
          box(width = 12, title = "Distribution by Position (Violin)",
              status = "warning", solidHeader = TRUE,
              plotlyOutput("violin_plot", height = 380))
        )
      ),
 
      # ── Scatter Plot ─────────────────────────────────────────────────────────
      tabItem("scatter",
        fluidRow(
          box(width = 3, status = "primary",
              selectInput("sc_x", "X Axis", choices = stat_choices, selected = "Kills"),
              selectInput("sc_y", "Y Axis", choices = stat_choices, selected = "Digs"),
              selectInput("sc_col", "Color By (3rd variable)",
                          choices  = stat_choices,
                          selected = "Assists"),
              tags$small(class = "text-muted",
                         tags$em("Shape = Position (always)")),
              hr(),
              checkboxGroupInput("sc_pos", "Positions",
                                 choices  = names(pos_labels),
                                 selected = names(pos_labels)),
              numericInput("sc_min_sets", "Min Sets", value = 20, min = 0)
          ),
          box(width = 9, title = "Player Scatter Plot", status = "info", solidHeader = TRUE,
              plotlyOutput("scatter_plot", height = 520))
        )
      )
    )
  )
)
 
# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
 
  # Load data once
  raw_data <- reactive({
    withProgress(message = "Loading NCAA volleyball data…", value = 0.1, {
      df <- load_data()
      incProgress(0.8)
      df
    })
  })
 
  # Global filter
  filtered <- reactive({
    df <- raw_data()
    req(nrow(df) > 0)
 
    if (input$sport_filter != "Both") df <- df[df$Sport == input$sport_filter, ]
    if (length(input$div_filter) > 0)  df <- df[df$Division %in% input$div_filter, ]
 
    # Season filter
    if (!is.null(input$season_filter) && input$season_filter != "All") {
      df <- df[df$Season == input$season_filter, ]
    }
 
    # Keep known positions only
    df <- df[df$Pos %in% names(pos_labels), ]
    df
  })
 
  # ── Overview boxes ───────────────────────────────────────────────────────────
  output$vb_players <- renderValueBox({
    valueBox(nrow(filtered()), "Players", icon = icon("users"), color = "blue")
  })
  output$vb_teams <- renderValueBox({
    valueBox(n_distinct(filtered()$Team), "Teams", icon = icon("trophy"), color = "green")
  })
  output$vb_positions <- renderValueBox({
    valueBox(n_distinct(filtered()$Pos), "Positions", icon = icon("layer-group"), color = "orange")
  })
  output$vb_avg_pts <- renderValueBox({
    avg <- round(mean(as.numeric(filtered()$PTS), na.rm = TRUE), 1)
    valueBox(avg, "Avg Points/Player", icon = icon("star"), color = "purple")
  })
 
  # Position distribution
  output$pos_dist_plot <- renderPlotly({
    df <- filtered() %>% count(Pos) %>% arrange(desc(n))
    plot_ly(df, x = ~Pos, y = ~n, type = "bar",
            marker = list(color = "#2c7bb6")) %>%
      layout(xaxis = list(title = "Position"), yaxis = list(title = "# Players"),
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
 
  # Average pts by position
  output$pos_pts_plot <- renderPlotly({
    df <- filtered() %>%
      mutate(PTS = as.numeric(PTS)) %>%
      group_by(Pos) %>%
      summarise(avg_pts = mean(PTS, na.rm = TRUE)) %>%
      arrange(desc(avg_pts))
    plot_ly(df, x = ~reorder(Pos, avg_pts), y = ~avg_pts, type = "bar",
            marker = list(color = "#1a9641")) %>%
      layout(xaxis = list(title = "Position"), yaxis = list(title = "Avg PTS"),
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
 
  # Top scorers
  output$top_scorers_plot <- renderPlotly({
    df <- filtered() %>%
      mutate(PTS = as.numeric(PTS)) %>%
      arrange(desc(PTS)) %>% slice_head(n = 10)
    plot_ly(df, x = ~reorder(Player, PTS), y = ~PTS, type = "bar",
            color = ~Pos,
            text  = ~paste0(Player, "\n", Team, " (", Pos, ")"),
            hoverinfo = "text+y") %>%
      layout(xaxis = list(title = "", tickangle = -30),
             yaxis = list(title = "PTS"),
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
 
  # ── Player table ──────────────────────────────────────────────────────────
  output$player_table <- renderDT({
    df <- filtered()
    if (length(input$pos_filter) > 0) df <- df[df$Pos %in% input$pos_filter, ]
    if (input$yr_filter != "All")     df <- df[df$Yr  == input$yr_filter, ]
    df <- df[!is.na(df$S) & as.numeric(df$S) >= input$min_sets, ]
 
    display_cols <- c("Season", "Player", "Team", "Conference", "Yr", "Pos",
                      "Sport", "Division", "GP", "S",
                      "Kills", "Hit Pct", "Assists", "Aces",
                      "Digs", "Block Solos", "Block Assists", "PTS")
    display_cols <- intersect(display_cols, names(df))
 
    datatable(df[, display_cols],
              filter  = "top",
              options = list(pageLength = 15, scrollX = TRUE,
                             order = list(list(which(display_cols == "PTS") - 1, "desc"))),
              rownames = FALSE) %>%
      formatStyle("Pos",
                  backgroundColor = styleEqual(
                    names(pos_labels),
                    c("#e41a1c","#377eb8","#4daf4a","#984ea3",
                      "#ff7f00","#a65628","#f781bf","#999999","#66c2a5")
                  ),
                  color = "white", fontWeight = "bold", borderRadius = "4px")
  })
 
  # ── Position comparison ────────────────────────────────────────────────────
  output$comp_plot <- renderPlotly({
    df <- filtered()
    if (length(input$comp_pos) == 0) return(NULL)
    df <- df[df$Pos %in% input$comp_pos, ]
    stat_col <- input$comp_stat
    df[[stat_col]] <- as.numeric(df[[stat_col]])
 
    summary_df <- df %>%
      group_by(Pos) %>%
      summarise(val = switch(input$comp_type,
                             Mean   = mean(get(stat_col),   na.rm = TRUE),
                             Median = median(get(stat_col), na.rm = TRUE),
                             Total  = sum(get(stat_col),    na.rm = TRUE)),
                n = n(), .groups = "drop") %>%
      arrange(desc(val))
 
    plot_ly(summary_df, x = ~reorder(Pos, val), y = ~val, type = "bar",
            color = ~Pos, text = ~paste0("n=", n), textposition = "outside") %>%
      layout(title = paste(input$comp_type, stat_col, "by Position"),
             xaxis = list(title = "Position"),
             yaxis = list(title = paste(input$comp_type, stat_col)),
             showlegend = FALSE,
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
 
  output$violin_plot <- renderPlotly({
    df <- filtered()
    if (length(input$comp_pos) == 0) return(NULL)
    df <- df[df$Pos %in% input$comp_pos, ]
    stat_col <- input$comp_stat
    df[[stat_col]] <- as.numeric(df[[stat_col]])
    df <- df[!is.na(df[[stat_col]]), ]
 
    plot_ly(df, x = ~Pos, y = ~get(stat_col), type = "violin",
            color = ~Pos, box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            points = "outliers") %>%
      layout(xaxis = list(title = "Position"),
             yaxis = list(title = stat_col),
             showlegend = FALSE,
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
 
  # ── Scatter plot ───────────────────────────────────────────────────────────
  output$scatter_plot <- renderPlotly({
    df <- filtered()
    if (length(input$sc_pos) == 0) return(NULL)
    df <- df[df$Pos %in% input$sc_pos, ]
    df[[input$sc_x]] <- as.numeric(df[[input$sc_x]])
    df[[input$sc_y]] <- as.numeric(df[[input$sc_y]])
    df <- df[!is.na(df$S) & as.numeric(df$S) >= input$sc_min_sets, ]
    df <- df[!is.na(df[[input$sc_x]]) & !is.na(df[[input$sc_y]]), ]
 
    # Map each position to one of plotly's named symbols
    pos_symbols <- c(
      "OH"  = "circle",
      "MB"  = "square",
      "MH"  = "diamond",
      "S"   = "triangle-up",
      "RS"  = "triangle-down",
      "OPP" = "cross",
      "DS"  = "x",
      "L"   = "star",
      "LS"  = "hexagram"
    )
    df$marker_symbol <- pos_symbols[df$Pos]
 
    # Split into one trace per position so each gets its own symbol in the legend
    col_var  <- input$sc_col
    positions <- unique(df$Pos)
 
    p <- plot_ly(
      type       = "scatter",
      mode       = "markers",
      showlegend = TRUE
    )
 
    for (pos in positions) {
      sub <- df[df$Pos == pos, ]
      p <- add_trace(p,
        data        = sub,
        x           = ~get(input$sc_x),
        y           = ~get(input$sc_y),
        name        = pos,
        legendgroup = pos,
        color       = ~get(col_var),
        text        = ~paste0("<b>", Player, "</b><br>",
                              Team, " | ", Pos, " | ", Yr, "<br>",
                              input$sc_x, ": ", get(input$sc_x), "<br>",
                              input$sc_y, ": ", get(input$sc_y), "<br>",
                              col_var, ": ", get(col_var)),
        hoverinfo   = "text",
        marker      = list(
          symbol  = pos_symbols[[pos]],
          size    = 8,
          opacity = 0.75,
          line    = list(width = 0.5, color = "rgba(255,255,255,0.4)")
        )
      )
    }
 
    p %>% layout(
      xaxis  = list(title = input$sc_x),
      yaxis  = list(title = input$sc_y),
      legend = list(title = list(text = paste0("<b>", col_var, " (color)<br>Shape = Position</b>"))),
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
  })
}
```

```{r}
server <- function(input, output, session) {
 
  # Load data once
  raw_data <- reactive({
    withProgress(message = "Loading NCAA volleyball data…", value = 0.1, {
      df <- load_data()
      incProgress(0.8)
      df
    })
  })
 
  # Global filter
  filtered <- reactive({
    df <- raw_data()
    req(nrow(df) > 0)
 
    if (input$sport_filter != "Both") df <- df[df$Sport == input$sport_filter, ]
    if (length(input$div_filter) > 0)  df <- df[df$Division %in% input$div_filter, ]
 
    # Season filter
    if (!is.null(input$season_filter) && input$season_filter != "All") {
      df <- df[df$Season == input$season_filter, ]
    }
 
    # Keep known positions only
    df <- df[df$Pos %in% names(pos_labels), ]
    df
  })
 
  # ── Overview boxes ───────────────────────────────────────────────────────────
  output$vb_players <- renderValueBox({
    valueBox(nrow(filtered()), "Players", icon = icon("users"), color = "blue")
  })
  output$vb_teams <- renderValueBox({
    valueBox(n_distinct(filtered()$Team), "Teams", icon = icon("trophy"), color = "green")
  })
  output$vb_positions <- renderValueBox({
    valueBox(n_distinct(filtered()$Pos), "Positions", icon = icon("layer-group"), color = "orange")
  })
  output$vb_avg_pts <- renderValueBox({
    avg <- round(mean(as.numeric(filtered()$PTS), na.rm = TRUE), 1)
    valueBox(avg, "Avg Points/Player", icon = icon("star"), color = "purple")
  })
 
  # Position distribution
  output$pos_dist_plot <- renderPlotly({
    df <- filtered() %>% count(Pos) %>% arrange(desc(n))
    plot_ly(df, x = ~Pos, y = ~n, type = "bar",
            marker = list(color = "#2c7bb6")) %>%
      layout(xaxis = list(title = "Position"), yaxis = list(title = "# Players"),
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
 
  # Average pts by position
  output$pos_pts_plot <- renderPlotly({
    df <- filtered() %>%
      mutate(PTS = as.numeric(PTS)) %>%
      group_by(Pos) %>%
      summarise(avg_pts = mean(PTS, na.rm = TRUE)) %>%
      arrange(desc(avg_pts))
    plot_ly(df, x = ~reorder(Pos, avg_pts), y = ~avg_pts, type = "bar",
            marker = list(color = "#1a9641")) %>%
      layout(xaxis = list(title = "Position"), yaxis = list(title = "Avg PTS"),
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
 
  # Top scorers
  output$top_scorers_plot <- renderPlotly({
    df <- filtered() %>%
      mutate(PTS = as.numeric(PTS)) %>%
      arrange(desc(PTS)) %>% slice_head(n = 10)
    plot_ly(df, x = ~reorder(Player, PTS), y = ~PTS, type = "bar",
            color = ~Pos,
            text  = ~paste0(Player, "\n", Team, " (", Pos, ")"),
            hoverinfo = "text+y") %>%
      layout(xaxis = list(title = "", tickangle = -30),
             yaxis = list(title = "PTS"),
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
 
  # ── Player table ──────────────────────────────────────────────────────────
  output$player_table <- renderDT({
    df <- filtered()
    if (length(input$pos_filter) > 0) df <- df[df$Pos %in% input$pos_filter, ]
    if (input$yr_filter != "All")     df <- df[df$Yr  == input$yr_filter, ]
    df <- df[!is.na(df$S) & as.numeric(df$S) >= input$min_sets, ]
 
    display_cols <- c("Season", "Player", "Team", "Conference", "Yr", "Pos",
                      "Sport", "Division", "GP", "S",
                      "Kills", "Hit Pct", "Assists", "Aces",
                      "Digs", "Block Solos", "Block Assists", "PTS")
    display_cols <- intersect(display_cols, names(df))
 
    datatable(df[, display_cols],
              filter  = "top",
              options = list(pageLength = 15, scrollX = TRUE,
                             order = list(list(which(display_cols == "PTS") - 1, "desc"))),
              rownames = FALSE) %>%
      formatStyle("Pos",
                  backgroundColor = styleEqual(
                    names(pos_labels),
                    c("#e41a1c","#377eb8","#4daf4a","#984ea3",
                      "#ff7f00","#a65628","#f781bf","#999999","#66c2a5")
                  ),
                  color = "white", fontWeight = "bold", borderRadius = "4px")
  })
 
  # ── Position comparison ────────────────────────────────────────────────────
  output$comp_plot <- renderPlotly({
    df <- filtered()
    if (length(input$comp_pos) == 0) return(NULL)
    df <- df[df$Pos %in% input$comp_pos, ]
    stat_col <- input$comp_stat
    df[[stat_col]] <- as.numeric(df[[stat_col]])
 
    summary_df <- df %>%
      group_by(Pos) %>%
      summarise(val = switch(input$comp_type,
                             Mean   = mean(get(stat_col),   na.rm = TRUE),
                             Median = median(get(stat_col), na.rm = TRUE),
                             Total  = sum(get(stat_col),    na.rm = TRUE)),
                n = n(), .groups = "drop") %>%
      arrange(desc(val))
 
    plot_ly(summary_df, x = ~reorder(Pos, val), y = ~val, type = "bar",
            color = ~Pos, text = ~paste0("n=", n), textposition = "outside") %>%
      layout(title = paste(input$comp_type, stat_col, "by Position"),
             xaxis = list(title = "Position"),
             yaxis = list(title = paste(input$comp_type, stat_col)),
             showlegend = FALSE,
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
 
  output$violin_plot <- renderPlotly({
    df <- filtered()
    if (length(input$comp_pos) == 0) return(NULL)
    df <- df[df$Pos %in% input$comp_pos, ]
    stat_col <- input$comp_stat
    df[[stat_col]] <- as.numeric(df[[stat_col]])
    df <- df[!is.na(df[[stat_col]]), ]
 
    plot_ly(df, x = ~Pos, y = ~get(stat_col), type = "violin",
            color = ~Pos, box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            points = "outliers") %>%
      layout(xaxis = list(title = "Position"),
             yaxis = list(title = stat_col),
             showlegend = FALSE,
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  })
 
  # ── Scatter plot ───────────────────────────────────────────────────────────
  output$scatter_plot <- renderPlotly({
    df <- filtered()
    if (length(input$sc_pos) == 0) return(NULL)
    df <- df[df$Pos %in% input$sc_pos, ]
    df[[input$sc_x]] <- as.numeric(df[[input$sc_x]])
    df[[input$sc_y]] <- as.numeric(df[[input$sc_y]])
    df <- df[!is.na(df$S) & as.numeric(df$S) >= input$sc_min_sets, ]
    df <- df[!is.na(df[[input$sc_x]]) & !is.na(df[[input$sc_y]]), ]
 
    plot_ly(df,
            x    = ~get(input$sc_x),
            y    = ~get(input$sc_y),
            color = ~get(input$sc_col),
            type  = "scatter", mode = "markers",
            text  = ~paste0("<b>", Player, "</b><br>",
                            Team, " | ", Pos, " | ", Yr, "<br>",
                            input$sc_x, ": ", get(input$sc_x), "<br>",
                            input$sc_y, ": ", get(input$sc_y)),
            hoverinfo = "text",
            marker = list(size = 6, opacity = 0.7)) %>%
      layout(xaxis = list(title = input$sc_x),
             yaxis = list(title = input$sc_y),
             plot_bgcolor  = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)")
  })
}
```

```{r}
shinyApp(ui, server)
```
