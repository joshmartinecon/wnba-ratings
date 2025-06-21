library(shiny)
library(DT)
library(dplyr)

##### Load Data #####
setwd("C:/Users/jmart/OneDrive/Desktop/GitHub/wnba-ratings/Data")
players <- read.csv("WNBA_Ratings_and_Rotations.csv", stringsAsFactors = FALSE)
team_ratings <- read.csv("WNBA_Team_Ratings.csv", stringsAsFactors = FALSE)
last_updated <- readRDS("last_updated.RDS")

##### UI #####
ui <- navbarPage(
  title    = div("WNBA Explorer", style = "margin-left:15px;"),
  selected = "All Players",
  
  header = tags$head(
    tags$style(HTML("\n      /* Arrow styling for Δ Minutes */\n      .arrow-zero   { color: gray;        font-weight: bold; font-size: 18px; }\n      .arrow-up-1   { color: forestgreen; font-weight: bold; font-size: 18px; }\n      .arrow-up-2   { color: forestgreen; font-weight: bold; font-size: 18px; }\n      .arrow-up-3   { color: forestgreen; font-weight: bold; font-size: 18px; }\n      .arrow-down-1 { color: crimson;     font-weight: bold; font-size: 18px; }\n      .arrow-down-2 { color: crimson;     font-weight: bold; font-size: 18px; }\n      .arrow-down-3 { color: crimson;     font-weight: bold; font-size: 18px; }\n    "))
  ),
  
  # ── All Players (default tab) ───────────────────────────────────
  tabPanel(
    "All Players",
    fluidPage(
      fluidRow(
        column(3,
               wellPanel(
                 tags$div("Created by ", tags$a("Josh Martin", href = "https://joshmartinecon.github.io/")),
                 tags$div(paste("Updated:", format(last_updated, "%Y-%m-%d %H:%M:%S"))),
                 tags$div(tags$a("Source Code & Data", href = "https://github.com/joshmartinecon/wnba-ratings")),
                 tags$div("+ = injured",)
               ),
               wellPanel(
                 selectInput(
                   "team_filter", "Select Team:",
                   choices  = c("All", sort(unique(players$team))),
                   selected = "All"
                 )
               )
        ),
        column(6, div(style = "max-width: 800px;", DTOutput("all_table")))
      )
    )
  ),
  
  # ── Team Ratings ────────────────────────────────────────────────
  tabPanel(
    "Team Ratings",
    fluidPage(
      fluidRow(
        column(3, wellPanel(
          tags$div("Created by ",
                   tags$a("Josh Martin", href = "https://joshmartinecon.github.io/")),
          tags$div(paste("Updated:", format(last_updated, "%Y-%m-%d %H:%M:%S"))),
          tags$div(tags$a("Source Code & Data", href = "https://github.com/joshmartinecon/wnba-ratings")),
          tags$div("+ = injured, * = optimal")
        )),
        column(6, div(style = "max-width: 800px;", DTOutput("ratings_table")))
      )
    )
  ),
  
  # ── Team Rotations ──────────────────────────────────────────────
  tabPanel(
    "Team Rotations",
    fluidPage(
      fluidRow(
        column(3,
               wellPanel(
                 tags$div("Created by ",
                          tags$a("Josh Martin", href = "https://joshmartinecon.github.io/")),
                 tags$div(paste("Updated:", format(last_updated, "%Y-%m-%d %H:%M:%S"))),
                 tags$div(tags$a("Source Code & Data", href = "https://github.com/joshmartinecon/wnba-ratings")),
                 tags$div("+ = injured, * = optimal",)
               ),
               wellPanel(
                 selectInput(
                   "team_select", "Select Team:",
                   choices  = c("ATL","CHI","CON","DAL","GSV","IND","LAS","LVA","MIN","NYL","PHO","SEA","WAS"),
                   selected = "ATL"
                 )
               )
        ),
        column(6, DTOutput("team_table"))
      )
    )
  )
)

##### SERVER #####
server <- function(input, output, session) {
  
  # Reactive player data with ranking -------------------------------------------
  ranked_players <- reactive({
    players %>%
      arrange(desc(rating)) %>%
      mutate(Rank = row_number())
  })
  
  # 1) All Players ---------------------------------------------------------------
  output$all_table <- renderDT({
    df <- ranked_players()
    if (input$team_filter != "All") {
      df <- df %>% filter(team == input$team_filter)
    }
    
    df %>%
      select(Rank, Player = player, Team = team, Rating = rating) %>%
      datatable(
        colnames = c("", "Player", "Team", "Rating"),
        options = list(
          paging       = FALSE,
          autoWidth    = TRUE,
          lengthChange = FALSE,
          info = FALSE
        ),
        width    = "100%",
        rownames = FALSE
      )
  })
  
  # 2) Team Rotations ------------------------------------------------------------
  output$team_table <- renderDT({
    players %>%
      filter(team == input$team_select) %>%
      mutate(min_symbol = case_when(
        min_diff >= 15  ~ "<span class='arrow-up-3'>&uarr;&uarr;&uarr;</span>",
        min_diff >= 10  ~ "<span class='arrow-up-2'>&uarr;&uarr;</span>",
        min_diff >= 5   ~ "<span class='arrow-up-1'>&uarr;</span>",
        min_diff <= -15 ~ "<span class='arrow-down-3'>&darr;&darr;&darr;</span>",
        min_diff <= -10 ~ "<span class='arrow-down-2'>&darr;&darr;</span>",
        min_diff <= -5  ~ "<span class='arrow-down-1'>&darr;</span>",
        TRUE            ~ "<span class='arrow-zero'>&ndash;</span>"
      )) %>%
      select(
        Player      = player,
        Rating      = rating,
        `MPG`       = mp_g,
        `MPG*`      = mp_g_star,
        `Δ Minutes` = min_symbol
      ) %>%
      arrange(desc(`MPG*`)) %>%
      datatable(
        options = list(
          pageLength   = 15,
          autoWidth    = TRUE,
          lengthChange = FALSE,
          info = FALSE
        ),
        escape   = FALSE,
        width    = "100%",
        rownames = FALSE
      )
  })
  
  # 3) Team Ratings --------------------------------------------------------------
  output$ratings_table <- renderDT({
    team_ratings %>%
      arrange(desc(rating)) %>%
      select(Team = team, Rating = rating, 'Rating*' = rating_star) %>%
      datatable(
        options = list(
          pageLength   = 15,
          autoWidth    = TRUE,
          lengthChange = FALSE,
          info = FALSE
        ),
        width    = "100%",
        rownames = FALSE
      )
  })
}

##### Run App #####
shinyApp(ui, server)
