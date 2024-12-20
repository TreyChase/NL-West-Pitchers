library(tidyverse)
library(shiny)
library(mlbplotR)

source(
  knitr::purl("Functions.qmd", output=tempfile(), quiet=TRUE)
)

## Load team logos from mlbplotR package
teams_colors_logos = load_mlb_teams() |> 
  filter(!team_abbr %in% c("AL", "NL", "MLB"))

## Values for dropdown menus 
teams = sort(c("SF", "SD", "LAD", "COL", "AZ"))
years = sort(unique(nl_west$game_year))

ui = fluidPage(
  titlePanel("NL West Pitching Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select Team:", choices = teams, selected = "SF"),
      selectInput("year", "Select Year:", choices = years, selected = 2024),
      uiOutput("player_selector"), 
      actionButton("get_dashboard", "Get Dashboard"),
      plotOutput("team_logo", height = "150px")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Percentile Profile", plotOutput("percentile_plot")),
        tabPanel("Location Profile", plotOutput("location_plot")),
        tabPanel("Velocity Profile", plotOutput("velocity_plot")),
        tabPanel("Movement Profile", plotOutput("movement_plot")),
        tabPanel("Pitch Usage Table", tableOutput("table"))
      )
    )
  )
)

server = function(input, output, session) {
  
  ## Reactive to update player dropdown based on selected team and year
  players = reactive({
    nl_west |>
      filter(game_year == input$year & 
               ((home_team == input$team & inning_topbot == "Top") | 
                  (away_team == input$team & inning_topbot == "Bot"))) |>
      distinct(player_name) |>
      pull(player_name) |>
      sort()
  })
  
  ## Percentile dataframe
  
  percentile_df <- nl_west |>
    select(
      game_year,
      player_name,
      pitch_name,
      release_speed,
      launch_speed,
      woba_value,
      bb_type, 
      events
    ) |>
    summarise(
      avg_exit_velo = mean(launch_speed, na.rm = TRUE),
      woba_against = mean(woba_value, na.rm = TRUE),
      gb_rate = sum(bb_type == "ground_ball", na.rm = TRUE) / sum(!is.na(bb_type)),
      fb_rate = sum(bb_type == "fly_ball", na.rm = TRUE) / sum(!is.na(bb_type)),
      ld_rate = sum(bb_type == "line_drive", na.rm = TRUE) / sum(!is.na(bb_type)),
      k_rate = sum(events == "strikeout", na.rm = TRUE) / sum(!is.na(events)),
      bb_rate = sum(events == "walk", na.rm = TRUE) / sum(!is.na(events)),
      pitch_count = n(),
      .by = c(game_year, player_name)
    ) |>
    ungroup() |>
    filter(pitch_count >= 100) |> #getting qualified pitchers
    group_by(game_year) |>
    mutate(
      percentile_avg_exit_velo = (1 - round(percent_rank(avg_exit_velo), 2)) * 100,
      percentile_woba_against = (1 - round(percent_rank(woba_against), 2)) * 100,
      percentile_gb_rate = round(percent_rank(gb_rate), 2) * 100,
      percentile_fb_rate = round(percent_rank(fb_rate), 2) * 100,
      percentile_ld_rate = round(percent_rank(ld_rate), 2) * 100,
      percentile_k_rate = round(percent_rank(k_rate), 2) * 100,
      percentile_bb_rate = (1 - round(percent_rank(bb_rate), 2)) * 100 #inverse relationship, the lower the bb rate the better
    ) |>
    #want integer values for the plot
    ungroup()
  
  
  ## Keep track of the current selected player
  observeEvent(players(), {
    current_player = isolate(input$player) # Get the current selected player
    player_choices = players() # Update the list of players
    
    ## Check if the current player is in the new list of players
    if (!is.null(current_player) && current_player %in% player_choices) {
      updateSelectInput(session, "player", choices = player_choices, selected = current_player)
    } else {
      # Default to the first player in the list if the current player is not available
      updateSelectInput(session, "player", choices = player_choices, selected = player_choices[1])
    }
  })
  
  ## Render the player dropdown dynamically
  output$player_selector = renderUI({
    selectInput("player", "Select Player", choices = players(), selected = input$player)
  })
  
  ## Create a reactive value for selected team; default is SF
  selected_team = reactiveVal(teams[1])
  
  ## Update selected team value when the user selects a team
  observeEvent(input$team, {
    selected_team(input$team)
  })
  
  ## Render the team logo associated with the user's current team selection
  output$team_logo = renderPlot({
    team = selected_team()
    
    ## Filter the selected team
    filtered_team = teams_colors_logos |> 
      filter(team_abbr == team)
    
    ## Create a ggplot with the team logo using geom_mlb_logos from the mlbplotR package
    ggplot(filtered_team, aes(x = 1, y = 1)) +
      geom_mlb_logos(aes(team_abbr = team_abbr), width = 0.3) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0),
            plot.background = element_rect(fill = "#f7f7f7", color = NA))
  })
  
  ## Create separate eventReactives to produce the plots/table when the button is clicked
  percentile_data = eventReactive(input$get_dashboard, {
    percentile_profile(percentile_df, input$player, input$year)
  })
  
  location_data = eventReactive(input$get_dashboard, {
    location_profile(nl_west, input$player, input$year)
  })
  
  velocity_data = eventReactive(input$get_dashboard, {
    velocity_profile(nl_west, input$player, input$year)
  })
  
  movement_data = eventReactive(input$get_dashboard, {
    movement_profile(nl_west, input$player, input$year)
  })
  
  usage_table_data = eventReactive(input$get_dashboard, {
    pitch_usage_table(nl_west, input$player, input$year)
  })
  
  ## Render each plot in its own tab
  
  output$percentile_plot = renderPlot({
    plot = percentile_data()
    if (is.null(plot)) {
      validate(
        need(FALSE, "No valid pitch data available for the selected player.")
      )
    }
    plot
  })
  
  
  output$location_plot = renderPlot({
    plot = location_data()
    if (is.null(plot)) {
      validate(
        need(FALSE, "No valid pitch data available for the selected player.")
      )
    }
    plot
  })
  
  output$velocity_plot = renderPlot({
    plot = velocity_data()
    if (is.null(plot)) {
      validate(
        need(FALSE, "No valid pitch data available for the selected player.")
      )
    }
    plot
  })
  
  output$movement_plot = renderPlot({
    plot = movement_data()
    if (is.null(plot)) {
      validate(
        need(FALSE, "No valid pitch data available for the selected player.")
      )
    }
    plot
  })
  
  ## Render the table in its own tab
  output$table = renderTable({
    table = usage_table_data()
    if (is.null(table)) {
      validate(
        need(FALSE, "No valid pitch data available for the selected player.")
      )
    }
    table
  })
}

shinyApp(ui = ui, server = server)
