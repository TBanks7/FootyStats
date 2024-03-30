library(shiny)
library(shinydashboard)
library(worldfootballR)
library(ggplot2)
library(dplyr)

# Function to fetch player stats from fbref.com
current_season_player <- load_fb_big5_advanced_season_stats(season_end_year = 2024, stat_type = "standard", team_or_player = "player")

# Extract numeric columns from current_season_player
numeric_columns <- sapply(current_season_player, is.numeric)
numeric_column_names <- names(current_season_player)[numeric_columns]



# UI
ui <- dashboardPage(
  skin = c("black"),
  dashboardHeader(
    title = div("FootyStats", align = "center")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Player Comparison", tabName = "player_comparison"),
      menuItem("Player Plot Builder", tabName = "player_plot_builder"),
      menuItem("Teams", tabName = "teams")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .center-cards {
          display: flex;
          justify-content: center;
        }
      "))
    ),
    tabItems(
      # Player Comparison tab
      tabItem(tabName = "player_comparison",
              uiOutput("player_cards"),
              div(actionButton("add_card", "Add Player Card"), align = "center"),
              br(),
              fluidRow(
                column(width = 12,
                       box(
                         title = "OverView",
                         width = NULL,
                         status = "success",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         tableOutput("overveiw_table")
                       ),
                       box(
                         title = "Shooting",
                         width = NULL,
                         status = "success",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         tableOutput("shooting_table")
                       )
                )
              ),
              
              
              # uiOutput("player_results")
              
      ),
      # Player Plot Builder tab
      tabItem(tabName = "player_plot_builder",
              uiOutput("player_plot"),
              # Add content for Player Plot Builder tab
      ),
      # Teams tab
      tabItem(tabName = "teams",
              # uiOutput(paste0("player_cards"))
              # Add content for Teams tab
      )
    )
  )
)







# Server logic
server <- function(input, output, session) {
  # Update player choices based on current_season_player data
  observe({
    updateSelectizeInput(session, "player1", choices = current_season_player$Player, selected = input$player1)
    updateSelectizeInput(session, "player2", choices = current_season_player$Player, selected = input$player2)
    updateSelectizeInput(session, "player3", choices = current_season_player$Player, selected = input$player3)
    updateSelectizeInput(session, "player4", choices = current_season_player$Player, selected = input$player4)
  })
  
  # Initial number of cards
  cards <- reactiveVal(1)
  
  # Store selected players
  selected_players <- reactiveValues(player1 = "", player2 = "", player3 = "", player4 = "")
  
  # Render player cards
  output$player_cards <- renderUI({
    fluidRow(
      class = "center-cards",  # Apply custom CSS class
      lapply(seq_len(cards()), function(i) {
        column(width = 3,
               box(
                 selectInput(paste0("player", i), paste0("Select Player ", i, ": "),
                             choices = c("", current_season_player$Player),
                             selected = selected_players[[paste0("player", i)]]),
                 width = 100,
                 title = paste0("Player ", i),
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 uiOutput(paste0("player_card_", i
                 ))
               )
        )
      }),
    )
  })
  
  # Render player Plot
  output$player_plot <- renderUI({
    fluidRow(
      column(width = 12,
             plotOutput("plot"),
             fluidRow(
               column(width = 6, selectInput("x_axis", "X-Axis:", choices = numeric_column_names)),
               column(width = 6, selectInput("y_axis", "Y-Axis:", choices = numeric_column_names))
               ),
      )
    )
  })
  
  # Render individual player card
  observe({
    lapply(seq_len(cards()), function(i) {
      output[[paste0("player_card_", i)]] <- renderUI({
        render_player_card(i)
      })
    })
  })
  
  # Function to render player card
  render_player_card <- function(player_index) {
    selected_player <- input[[paste0("player", player_index)]]
    if (selected_player != "") {
      player <- isolate(current_season_player[current_season_player$Player == selected_player, ])
      player <- head(player, 1)
      tagList(
        tags$div(
          tags$img(src = paste0("https://fbref.com/req/202302030/images/headshots/",substr(player$Url, 30, 37),"_2022.jpg") , height = 100, width = 100),
          tags$p(paste("Name:", player$Player)),
          tags$p(paste("Age:", substr(player$Age, 1, 2) )),
          tags$p(paste("Current Team:", player$Squad)),
          tags$p(paste("Nation:", player$Nation))
        )
      )
    }
  }
  
  # Render Comparison Results
  output$player_results <- renderUI({
    titles <- c("Overview", "shooting", "passing", "passing_types", "gca", "defense", "possession", "playing_time", "misc", "keepers", "keepers_adv")
    statuses <- sample(c("primary", "success", "info", "warning", "danger"), length(titles), replace = TRUE)
    fluidRow(
      lapply(seq_along(titles), function(i) {
        column(width = 12,
               box(
                 title = titles[i],
                 width = NULL,
                 status = statuses[i],
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 tableOutput("player_table")
               )
        )
      })
    )
  })
  
  # Render Overveiw table
  output$overveiw_table <- renderTable({
    # Create a matrix with 5 rows and dynamic number of columns based on the current number of cards
    matrix("Content", nrow = 5, ncol = cards() + 1, dimnames = list(NULL, c("Column 1", paste("Player", 1:cards()))))
  })
  
  # Render Shooting table
  output$shooting_table <- renderTable({
    # Create a matrix with 5 rows and dynamic number of columns based on the current number of cards
    matrix("Content", nrow = 5, ncol = cards() + 1, dimnames = list(NULL, c("Column 1", paste("Player", 1:cards()))))
  })
  
  # Add more player cards
  observeEvent(input$add_card, {
    if (cards() < 4) {
      cards(cards() + 1)
    } else {
      showModal(modalDialog(
        title = "Maximum Players Reached",
        "You can compare up to 4 players.",
        easyClose = TRUE
      ))
    }
  })
  
  # Update selected players
  observe({
    selected_players$player1 <- input$player1
    selected_players$player2 <- input$player2
    selected_players$player3 <- input$player3
    selected_players$player4 <- input$player4
  })
  
  # Reactive expression for filtered data based on selected x-axis and y-axis
  filtered_data <- reactive({
    print(input$x_axis)
    print(input$y_axis)
    req(input$x_axis, input$y_axis)
    current_season_player[, c("Player", input$x_axis, input$y_axis, "Pos")]  # Include "Pos" column
  })
  
  # Render plot
  output$plot <- renderPlot({
    x_range <- range(filtered_data()[[input$x_axis]], na.rm = TRUE)
    y_range <- range(filtered_data()[[input$y_axis]], na.rm = TRUE)
    
    x_nudge <- diff(x_range) * 0.02  # Adjust the multiplier as needed
    y_nudge <- diff(y_range) * 0.04  # Adjust the multiplier as needed
    
    ggplot(filtered_data(), aes_string(x = input$x_axis, y = input$y_axis, color="Pos")) +
      geom_point(size=3) +
      geom_text(
        aes(label = Player), 
        nudge_x = x_nudge, nudge_y = y_nudge, 
        check_overlap = TRUE
      ) +
      labs(x = input$x_axis, y = input$y_axis) +
      theme_minimal()
  })
  
  
  
  # Plot using ggplot
  # output$plot <- renderPlot({
  #   x_range <- range(current_season_player$xG_Expected, na.rm = TRUE)
  #   y_range <- range(current_season_player$PrgC_Progression, na.rm = TRUE)
  #   
  #   x_nudge <- diff(x_range) * 0.02  # Adjust the multiplier as needed
  #   y_nudge <- diff(y_range) * 0.04  # Adjust the multiplier as needed
  #   
  #   print(x_range)
  #   
  #   
  #   ggplot(current_season_player, aes(x = xG_Expected, y = PrgC_Progression, color=substr(Pos, 1, 2))) +
  #     geom_point(size=3) +
  #     geom_text(
  #       aes(label = Player), 
  #       nudge_x = x_nudge, nudge_y = y_nudge,
  #       check_overlap = T
  #     ) +
  #     labs(x = "xG", y = "Progressive Carries") +
  #     theme_minimal()
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
