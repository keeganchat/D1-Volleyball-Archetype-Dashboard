library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)

# ── Load clean dataset ────────────────────────────────────────────────────────
wvb2 <- read.csv("wvb_clean_v2.csv", stringsAsFactors = FALSE) %>%
  mutate(
    season      = as.factor(season),
    position    = as.factor(position),
    archetype   = as.factor(archetype),
    player_year = as.factor(player_year)
  )

# ── Constants ─────────────────────────────────────────────────────────────────
pos_labels <- c(
  "OH" = "Outside Hitter (OH)",
  "MB" = "Middle Blocker (MB)",
  "RS" = "Right Side (RS)",
  "S"  = "Setter (S)",
  "L"  = "Libero (L)",
  "DS" = "Defensive Specialist (DS)"
)

pos_colors <- c(
  "OH" = "#00d4b4",
  "MB" = "#f5c518",
  "RS" = "#ff5f57",
  "S"  = "#9b5de5",
  "L"  = "#f97316",
  "DS" = "#60a5fa"
)

# Per-position radar variables (from our EDA)
position_vars <- list(
  OH = c("kills_ps", "hit_efficiency_ps", "return_attempts_ps", "digs_ps", 
         "aces_ps", "serve_errors_ps", "blocks_ps", "height_inches"),
  MB = c("kills_ps", "hit_efficiency_ps", "blocks_ps", "dbl_blocks_ps", 
         "digs_ps", "aces_ps", "height_inches"),
  RS = c("kills_ps", "hit_efficiency_ps", "blocks_ps", "dbl_blocks_ps", 
         "digs_ps", "aces_ps", "height_inches"),
  S  = c("assists_ps", "digs_ps", "return_attempts_ps", "aces_ps", 
         "blocks_ps", "kills_ps", "height_inches"),
  L  = c("digs_ps", "return_errors_ps", "aces_ps", 
         "serve_errors_ps", "assists_ps", "height_inches"),
  DS = c("digs_ps", "return_errors_ps", "aces_ps", 
         "serve_errors_ps", "assists_ps", "height_inches"))

var_labels <- c(
  kills_ps           = "Kills/Set",
  hit_efficiency_ps  = "Hit Efficiency",
  return_attempts_ps = "Return Attempts/Set",
  digs_ps            = "Digs/Set",
  aces_ps            = "Aces/Set",
  serve_errors_ps    = "Serve Errors/Set",
  blocks_ps          = "Block Solos/Set",
  dbl_blocks_ps      = "Block Assists/Set",
  assists_ps         = "Assists/Set",
  return_errors_ps   = "Return Errors/Set",
  height_inches      = "Height"
)

# Archetype descriptions
archetype_descriptions <- c(
  "Primary Outside Hitter"          = "High serve-receive responsibility with heavy offensive and defensive involvement. This player is trusted as the primary passer and leads the team in kills, digs, and aces. She is on the court for all rotations.",
  "Secondary Outside Hitter"        = "Lower involvement across all metrics. Fewer return attempts indicate the team is not routing serve receive through her heavily. Likely the O2 in rotation or a role player who may be underutilized at her current program.",
  "Efficient Middle Blocker"        = "Elite termination efficiency and dominant blocking output. When she swings, she converts cleanly. The prototypical high-level middle who contributes on both sides of the net.",
  "Developing Middle Blocker"       = "Below average efficiency and blocking numbers. Not necessarily a poor player — many D1 middles fall here while developing their craft. May have upside if placed in the right system.",
  "Complete Right Side"             = "The most well-rounded RS profile — highest kills and digs simultaneously. A true two-way threat who contributes offensively and defensively. Rare and highly valuable.",
  "Offensive Specialist Right Side" = "High hitting efficiency and solid blocking with lower defensive involvement. Primary value is as a pin attacker who terminates efficiently. Fits systems that want a pure offensive weapon on the right side.",
  "Developmental Right Side"        = "Below average attacking output and efficiency. May be playing a more defensive role than typical for the position, or still developing the attacking skills the RS demands.",
  "5-1 Primary Setter"              = "Runs the entire offense — touching the ball on nearly every offensive play. Minimal passing involvement indicates a 5-1 system where she focuses entirely on distribution. The most common high-level system setter.",
  "6-2 System Setter"               = "Shares setting duties, averaging roughly half the assists of a 5-1 setter. Higher passing involvement confirms she attacks from the front row when rotating forward. Needs to be a credible attacker in addition to a distributor.",
  "Starting Libero"                 = "The defensive anchor — nearly 4 digs per set and the highest assist volume among liberos, indicating frequent out-of-system setting. Trusted as the team's primary passer and defender across all back-row rotations.",
  "Reserve Libero"                  = "Significantly lower involvement across all metrics. Could be a backup rotating behind a starter or a starting-caliber libero who has not yet had the opportunity. Worth examining team context.",
  "Impact Defensive Specialist"     = "High digs and assists — used heavily as a substitution weapon with near-libero level involvement. This player is deployed in multiple rotations and makes a real defensive difference.",
  "Spot Sub Defensive Specialist"   = "Lower involvement — comes in for specific serving or passing situations without accumulating high volume. Could be a serving specialist used in one rotation or a developmental player building her role."
)

# wOV / wDV coefficients (from teammate's model)
compute_wov <- function(kills, errors, aces) {
  0.133197 * kills + -0.063021 * errors + 0.031552 * aces
}
compute_wdv <- function(digs, rerr, bsolo, bassist, berr) {
  -0.009673 * digs + -0.123784 * rerr + 0.027278 * bsolo +
    0.052643 * bassist + -0.011022 * berr
}

# Pre-compute wOV/wDV on dataset
wvb2 <- wvb2 %>%
  mutate(
    wOV = compute_wov(kills_ps,  serve_errors_ps, aces_ps),
    wDV = compute_wdv(digs_ps, return_errors_ps, blocks_ps, dbl_blocks_ps, 0)
  )

# ── K-means helper: assign archetype to custom player ────────────────────────
# We store cluster centers per position for nearest-centroid lookup
position_centers <- lapply(names(position_vars), function(pos) {
  vars <- position_vars[[pos]]
  df <- wvb2 %>%
    filter(position == pos, !is.na(archetype)) %>%
    select(all_of(vars), archetype) %>%
    drop_na()
  df %>%
    group_by(archetype) %>%
    summarise(across(all_of(vars), mean), .groups = "drop")
})
names(position_centers) <- names(position_vars)

assign_archetype <- function(pos, stat_vals) {
  centers <- position_centers[[pos]]
  vars     <- position_vars[[pos]]
  # Only use vars that are available in stat_vals
  use_vars <- intersect(vars, names(stat_vals))
  if (length(use_vars) == 0) return(NA_character_)
  
  dists <- apply(centers[, use_vars, drop = FALSE], 1, function(row) {
    sqrt(sum((row - stat_vals[use_vars])^2, na.rm = TRUE))
  })
  as.character(centers$archetype[which.min(dists)])
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "D1 Volleyball Archetype Dashboard"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",               tabName = "overview", icon = icon("chart-bar")),
      menuItem("Archetype & Radar Chart",tabName = "archetype",icon = icon("bullseye"))
    ),
    
    tags$hr(),
    
    tags$div(style = "padding: 0 12px;",
             tags$p(style = "font-size:12px; font-weight:700; text-transform:uppercase;
                      color:#aaa; margin-bottom:6px;",
                    "Custom Player Input"),
             
             selectInput("cp_position", "Position",
                         choices  = setNames(names(pos_labels), unname(pos_labels)),
                         selected = "OH"),
             
             numericInput("cp_sets", "Sets Played (season)", value = NA, min = 1, step = 1),
             
             tags$p(style = "font-size:11px; color:#aaa; margin: 4px 0 8px;
                      font-style:italic;",
                    "Enter season totals — stats will be normalized per set automatically."),
             
             tags$p(style = "font-size:11px; font-weight:700; color:#00d4b4;
                      margin: 6px 0 4px;",
                    "Offensive"),
             numericInput("cp_kills",   "Kills (season total)",  value = NA, min = 0),
             numericInput("cp_errors",  "Errors (season total)", value = NA, min = 0),
             numericInput("cp_hitpct",  "Hit Efficiency (-1 to 1)",        value = NA, step = 0.001),
             numericInput("cp_assists", "Assists (season total)",value = NA, min = 0),
             numericInput("cp_aces",    "Aces (season total)",   value = NA, min = 0),
             
             tags$p(style = "font-size:11px; font-weight:700; color:#9b5de5;
                      margin: 6px 0 4px;",
                    "Defensive"),
             numericInput("cp_digs",      "Digs (season total)",          value = NA, min = 0),
             numericInput("cp_rerr",      "Return Errors (season total)", value = NA, min = 0),
             numericInput("cp_retatt",    "Return Attempts (season total)",value = NA, min = 0),
             numericInput("cp_bsolo",     "Block Solos (season total)",   value = NA, min = 0),
             numericInput("cp_bassist",   "Block Assists (season total)", value = NA, min = 0),
             numericInput("cp_serveerr",  "Serve Errors (season total)",  value = NA, min = 0),
             
             tags$p(style = "font-size:11px; font-weight:700; color:#aaa; margin: 6px 0 4px;",
                    "Physical"),
             fluidRow(
               column(6, numericInput("cp_height_ft", "Feet",   value = NA, min = 4, max = 7, step = 1)),
               column(6, numericInput("cp_height_in", "Inches", value = NA, min = 0, max = 11, step = 1))
             ),
             tags$br(),
             actionButton("cp_reset", "Reset Inputs",
                          style = "width:100%; font-size:12px;")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ── Tab 1: Overview ────────────────────────────────────────────────────
      tabItem("overview",
              
              fluidRow(
                box(title = "Players by Position", width = 12,
                    solidHeader = TRUE, status = "primary",
                    plotlyOutput("pos_count_plot", height = 280))
              ),
              
              fluidRow(
                box(width = 3, status = "primary",
                    tags$p(style = "font-weight:700; margin-bottom:8px;",
                           "Filter Scatter Plot"),
                    checkboxGroupInput("scatter_pos", "Positions to Show",
                                       choices  = names(pos_labels),
                                       selected = names(pos_labels))
                ),
                box(title = "Weighted Offensive vs Defensive Value",
                    width = 9, solidHeader = TRUE, status = "info",
                    plotlyOutput("scatter_combined", height = 460))
              )
      ),
      
      # ── Tab 2: Archetype & Radar ───────────────────────────────────────────
      tabItem("archetype",
              
              # Archetype assignment row
              fluidRow(
                box(width = 4, solidHeader = TRUE, status = "primary",
                    title = "Assigned Archetype",
                    uiOutput("archetype_label")
                ),
                box(width = 4, solidHeader = TRUE, status = "info",
                    title = "Archetype Description",
                    uiOutput("archetype_desc")
                ),
                box(width = 4, solidHeader = TRUE, status = "warning",
                    title = "Best Player in Archetype",
                    uiOutput("similar_player")
                )
              ),
              
              # Radar chart row
              fluidRow(
                box(width = 3, status = "primary",
                    tags$p(style = "font-weight:700; margin-bottom:8px;",
                           "Compare Against"),
                    radioButtons("radar_filter", NULL,
                                 choices = c(
                                   "All D1 Players"          = "all",
                                   "Players at Position"     = "position",
                                   "Players in Archetype"    = "archetype"
                                 ),
                                 selected = "position"),
                    tags$hr(),
                    tags$p(style = "font-size:11px; color:#aaa;",
                           "Radar shows how the custom player compares to the
                      average of the selected group. Values are min-max
                      scaled (0-1) using the full dataset range.")
                ),
                box(title = "Radar Chart", width = 9,
                    solidHeader = TRUE, status = "info",
                    plotlyOutput("radar_plot", height = 520))
              )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Custom player per-set stats ────────────────────────────────────────────
  custom_ps <- reactive({
    s <- input$cp_sets
    if (is.null(s) || is.na(s) || s <= 0) return(NULL)
    
    safe <- function(x) if (is.null(x) || is.na(x)) 0 else as.numeric(x)
    
    # Height in total inches
    ht_ft <- input$cp_height_ft
    ht_in <- input$cp_height_in
    height <- if (!is.null(ht_ft) && !is.na(ht_ft) && !is.null(ht_in) && !is.na(ht_in))
      ht_ft * 12 + ht_in
    else NA_real_
    
    list(
      kills_ps           = safe(input$cp_kills)    / s,
      hit_efficiency_ps  = safe(input$cp_hitpct),
      assists_ps         = safe(input$cp_assists)  / s,
      aces_ps            = safe(input$cp_aces)     / s,
      serve_errors_ps    = safe(input$cp_serveerr) / s,
      digs_ps            = safe(input$cp_digs)     / s,
      return_attempts_ps = safe(input$cp_retatt)   / s,
      return_errors_ps   = safe(input$cp_rerr)     / s,
      blocks_ps          = safe(input$cp_bsolo)    / s,
      dbl_blocks_ps      = safe(input$cp_bassist)  / s,
      height_inches      = height                       
    )
  })
  
  # Assigned archetype for custom player
  custom_archetype <- reactive({
    ps  <- custom_ps()
    pos <- input$cp_position
    if (is.null(ps)) return(NULL)
    assign_archetype(pos, unlist(ps))
  })
  
  # Reset button
  observeEvent(input$cp_reset, {
    for (id in c("cp_sets","cp_kills","cp_errors","cp_hitpct","cp_assists",
                 "cp_aces","cp_digs","cp_rerr","cp_retatt","cp_bsolo",
                 "cp_bassist","cp_serveerr","cp_height_ft","cp_height_in")) {
      updateNumericInput(session, id, value = NA)
    }
  })
  
  # ── Overview: Players by Position ─────────────────────────────────────────
  output$pos_count_plot <- renderPlotly({
    df <- wvb2 %>% count(position) %>% arrange(desc(n))
    
    max_n <- max(df$n)   # add this line
    
    plot_ly(df,
            x    = ~reorder(position, -n),
            y    = ~n,
            type = "bar",
            marker = list(color = pos_colors[df$position]),
            text = ~n, textposition = "outside",
            hovertemplate = ~paste0("<b>", position, "</b><br>Players: ", n,
                                    "<extra></extra>")) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Number of Player-Seasons",
                          range = c(0, max_n * 1.15)),  # add this
             showlegend = FALSE,
             plot_bgcolor  = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)") %>%
      config(displayModeBar = FALSE)
  })
  # ── Overview: wOV vs wDV scatter ──────────────────────────────────────────
  output$scatter_combined <- renderPlotly({
    df <- wvb2 %>%
      filter(position %in% input$scatter_pos,
             !is.na(wOV), !is.na(wDV))
    
    mean_wov <- mean(df$wOV, na.rm = TRUE)
    mean_wdv <- mean(df$wDV, na.rm = TRUE)
    xr <- range(df$wOV, na.rm = TRUE)
    yr <- range(df$wDV, na.rm = TRUE)
    
    positions <- sort(unique(as.character(df$position)))
    pos_sym <- c(OH="circle", MB="square", RS="diamond",
                 S="triangle-up", L="star", DS="cross")
    
    p <- plot_ly(type = "scatter", mode = "markers")
    
    for (pos in positions) {
      sub <- df %>% filter(position == pos)
      p <- add_trace(p,
                     data        = sub,
                     x           = ~wOV, y = ~wDV,
                     name        = pos,
                     legendgroup = pos,
                     text        = ~paste0("<b>", player_name, "</b><br>",
                                           team_name, " · ", position, " · ", player_year,
                                           "<br>wOV: <b>", round(wOV, 3), "</b>",
                                           "<br>wDV: <b>", round(wDV, 3), "</b>"),
                     hoverinfo   = "text",
                     marker      = list(symbol  = pos_sym[[pos]],
                                        color   = pos_colors[[pos]],
                                        size    = 7, opacity = 0.65,
                                        line    = list(width = 0.4,
                                                       color = "rgba(255,255,255,0.2)"))
      )
    }
    
    # Mean crosshairs
    p <- p %>%
      add_segments(x = xr[1], xend = xr[2],
                   y = mean_wdv, yend = mean_wdv,
                   line = list(color = "rgba(255,95,87,0.5)",
                               width = 1.5, dash = "dash"),
                   showlegend = FALSE, hoverinfo = "none", inherit = FALSE) %>%
      add_segments(x = mean_wov, xend = mean_wov,
                   y = yr[1], yend = yr[2],
                   line = list(color = "rgba(255,95,87,0.5)",
                               width = 1.5, dash = "dash"),
                   showlegend = FALSE, hoverinfo = "none", inherit = FALSE)
    
    # Custom player star
    ps  <- custom_ps()
    if (!is.null(ps)) {
      cust_wov <- compute_wov(ps$kills_ps, ps$serve_errors_ps, ps$aces_ps)
      cust_wdv <- compute_wdv(ps$digs_ps, ps$return_errors_ps,
                              ps$blocks_ps, ps$dbl_blocks_ps, 0)
      p <- add_trace(p,
                     x    = cust_wov, y = cust_wdv,
                     type = "scatter", mode = "markers+text",
                     name = "Custom Player",
                     text = "★ Custom Player", textposition = "top center",
                     hovertemplate = paste0("<b>Custom Player</b><br>",
                                            "wOV: <b>", round(cust_wov, 3), "</b><br>",
                                            "wDV: <b>", round(cust_wdv, 3), "</b>",
                                            "<extra></extra>"),
                     marker = list(symbol = "star", size = 18,
                                   color = "#f5c518", opacity = 1,
                                   line = list(width = 1.5, color = "#000")),
                     inherit = FALSE
      )
    }
    
    p %>% layout(
      xaxis = list(title = "wOV — Weighted Offensive Value per Set"),
      yaxis = list(title = "wDV — Weighted Defensive Value per Set"),
      legend = list(title = list(text = "<b>Position</b>")),
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )%>%
      config(displayModeBar = FALSE)
  })
  
  # ── Archetype label ────────────────────────────────────────────────────────
  output$archetype_label <- renderUI({
    arch <- custom_archetype()
    if (is.null(arch) || is.na(arch)) {
      return(tags$p(style = "color:#aaa; font-style:italic;",
                    "Enter player stats in the sidebar to assign an archetype."))
    }
    tags$div(
      tags$p(style = "font-size:13px; color:#aaa; margin-bottom:4px;",
             "Player Fits:"),
      tags$p(style = "font-size:18px; font-weight:700; color:#f5c518;",
             arch)
    )
  })
  
  # ── Archetype description ──────────────────────────────────────────────────
  output$archetype_desc <- renderUI({
    arch <- custom_archetype()
    if (is.null(arch) || is.na(arch)) {
      return(tags$p(style = "color:#aaa; font-style:italic;",
                    "Archetype description will appear here."))
    }
    desc <- archetype_descriptions[[arch]]
    if (is.null(desc)) desc <- "No description available."
    tags$p(style = "font-size:13px; line-height:1.6;", desc)
  })
  
  # ── Most similar player ────────────────────────────────────────────────────
  output$similar_player <- renderUI({
    ps   <- custom_ps()
    pos  <- input$cp_position
    arch <- custom_archetype()
    if (is.null(ps) || is.na(arch)) {
      return(tags$p(style = "color:#aaa; font-style:italic;",
                    "Best player in archetype will appear here."))
    }
    
    # Variables for this position excluding height
    vars <- position_vars[[pos]]
    vars <- vars[vars != "height_inches"]
    
    # Filter to archetype
    df_arch <- wvb2 %>%
      filter(position == pos, archetype == arch) %>%
      select(player_name, team_name, player_year, season, all_of(vars)) %>%
      drop_na()
    
    if (nrow(df_arch) == 0) {
      return(tags$p("No players found."))
    }
    
    # Compute percentile rank for each variable within the archetype
    df_ranked <- df_arch %>%
      mutate(across(all_of(vars), ~ percent_rank(.), .names = "pct_{.col}"))
    
    # Average percentile across all vars
    pct_cols <- paste0("pct_", vars)
    df_ranked <- df_ranked %>%
      mutate(avg_pct = rowMeans(across(all_of(pct_cols)), na.rm = TRUE))
    
    best <- df_ranked %>%
      slice_max(order_by = avg_pct, n = 1, with_ties = FALSE)
    
    tags$div(
      tags$p(style = "font-size:13px; color:#aaa; margin-bottom:4px;",
             "Best player in this archetype:"),
      tags$p(style = "font-size:17px; font-weight:700; color:#00d4b4;",
             best$player_name),
      tags$p(style = "font-size:13px; color:#ccc;",
             sprintf("A %s from %s (%s, %s)",
                     pos_labels[[pos]],
                     best$team_name,
                     best$player_year,
                     best$season)),
      tags$p(style = "font-size:12px; color:#aaa;",
             sprintf("Avg percentile rank within archetype: %d%%",
                     round(best$avg_pct * 100)))
    )
  })
  
  # ── Radar chart ────────────────────────────────────────────────────────────
  output$radar_plot <- renderPlotly({
    ps   <- custom_ps()
    pos  <- input$cp_position
    vars <- position_vars[[pos]]
    
    # Determine comparison group (same logic as before)
    comp_df <- switch(input$radar_filter,
                      "all"       = wvb2 %>% filter(!is.na(archetype)),
                      "position"  = wvb2 %>% filter(position == pos, !is.na(archetype)),
                      "archetype" = {
                        arch <- custom_archetype()
                        if (is.null(arch) || is.na(arch))
                          wvb2 %>% filter(position == pos, !is.na(archetype))
                        else
                          wvb2 %>% filter(position == pos, archetype == arch, !is.na(archetype))
                      }
    )
    
    # Scale against the comparison group's 5th-95th percentile range
    scale_vs_group <- function(vals) {
      sapply(seq_along(vars), function(i) {
        lo  <- quantile(comp_df[[vars[i]]], 0.05, na.rm = TRUE)
        hi  <- quantile(comp_df[[vars[i]]], 1.0, na.rm = TRUE)
        if (hi == lo) 0.5
        else max(0, min(1, (vals[i] - lo) / (hi - lo)))
      })
    }
    
    theta_labels <- var_labels[vars]
    
    group_label <- switch(input$radar_filter,
                          "all"       = "All D1 Players",
                          "position"  = pos_labels[[pos]],
                          "archetype" = {
                            arch <- custom_archetype()
                            if (is.null(arch) || is.na(arch)) pos_labels[[pos]] else arch
                          }
    )
    
    p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
    
    if (!is.null(ps)) {
      ps_vals <- sapply(vars, function(v) {
        val <- ps[[v]]
        if (is.null(val) || is.na(val)) 0 else val
      })
      ps_scaled <- scale_vs_group(ps_vals)
      
      p <- add_trace(p,
                     r         = c(ps_scaled, ps_scaled[1]),
                     theta     = c(theta_labels, theta_labels[1]),
                     name      = paste0("Custom Player vs ", group_label),
                     line      = list(color = "#f5c518", width = 2.5),
                     marker    = list(color = "#f5c518", size = 8,
                                      line = list(color = "#000", width = 1)),
                     fillcolor = "rgba(245,197,24,0.15)"
      )
    } else {
      # No stats entered yet — show empty plot with a message
      p <- add_trace(p,
                     r     = rep(0, length(vars) + 1),
                     theta = c(theta_labels, theta_labels[1]),
                     name  = "No data entered",
                     line  = list(color = "rgba(0,0,0,0)"),
                     fillcolor = "rgba(0,0,0,0)"
      )
    }
    
    p %>% layout(
      polar = list(
        bgcolor = "rgba(0,0,0,0)",
        radialaxis = list(
          visible  = TRUE,
          range    = c(0, 1),
          tickvals = c(0, 0.25, 0.5, 0.75, 1),
          ticktext = c("0%", "25%", "50%", "75%", "100%")
        ),
        angularaxis = list(tickfont = list(size = 12))
      ),
      legend = list(title = list(text = "<b>Comparison</b>")),
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      margin        = list(t = 30, b = 30, l = 30, r = 30)
    ) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
