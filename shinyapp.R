library(shiny)
library(DT)
library(dplyr)

##### Load Data #####
players <- read.csv("https://raw.githubusercontent.com/joshmartinecon/wnba-ratings/refs/heads/main/WNBA_Ratings_and_Rotations.csv", stringsAsFactors = FALSE)
team_ratings <- read.csv("https://raw.githubusercontent.com/joshmartinecon/wnba-ratings/refs/heads/main/WNBA_Team_Ratings.csv", stringsAsFactors = FALSE)
last_updated <- read.csv("https://raw.githubusercontent.com/joshmartinecon/wnba-ratings/refs/heads/main/last_updated.csv")
# players <- read.csv("WNBA_Ratings_and_Rotations.csv", stringsAsFactors = FALSE)
# team_ratings <- read.csv("WNBA_Team_Ratings.csv", stringsAsFactors = FALSE)
# last_updated <- read.csv("last_updated.csv")
last_updated <- do.call(cbind, strsplit(last_updated[1,2], " "))[1]

##### UI #####
ui <- navbarPage(
  title = div("WNBA Explorer", style = "margin-left:15px;"),
  selected = "All Players",
  
  header = tags$head(
    tags$style(HTML("
      .arrow-zero   { color: gray;        font-weight: bold; font-size: 18px; }
      .arrow-up-1   { color: forestgreen; font-weight: bold; font-size: 18px; }
      .arrow-up-2   { color: forestgreen; font-weight: bold; font-size: 18px; }
      .arrow-up-3   { color: forestgreen; font-weight: bold; font-size: 18px; }
      .arrow-down-1 { color: crimson;     font-weight: bold; font-size: 18px; }
      .arrow-down-2 { color: crimson;     font-weight: bold; font-size: 18px; }
      .arrow-down-3 { color: crimson;     font-weight: bold; font-size: 18px; }
    "))
  ),
  
  tabPanel("All Players",
           fluidPage(
             fluidRow(
               column(3,
                      wellPanel(
                        tags$div("Created by ", tags$a("Josh Martin", href = "https://joshmartinecon.github.io/")),
                        tags$div(paste("Updated:", last_updated)),
                        tags$div(tags$a("Source Code & Data", href = "https://github.com/joshmartinecon/wnba-ratings")),
                        tags$div("+ = injured")
                      ),
                      wellPanel(
                        selectInput("team_filter", "Select Team:", choices = c("All", sort(unique(players$team))), selected = "All")
                      )
               ),
               column(6, div(style = "max-width: 800px;", DTOutput("all_table")))
             )
           )
  ),
  
  tabPanel("Team Ratings",
           fluidPage(
             fluidRow(
               column(3, wellPanel(
                 tags$div("Created by ", tags$a("Josh Martin", href = "https://joshmartinecon.github.io/")),
                 tags$div(paste("Updated:", last_updated)),
                 tags$div(tags$a("Source Code & Data", href = "https://github.com/joshmartinecon/wnba-ratings")),
                 tags$div("+ = injured")
               )),
               column(6, div(style = "max-width: 800px;", DTOutput("ratings_table")))
             )
           )
  ),
  
  tabPanel("Team Rotations",
           fluidPage(
             fluidRow(
               column(3,
                      wellPanel(
                        tags$div("Created by ", tags$a("Josh Martin", href = "https://joshmartinecon.github.io/")),
                        tags$div(paste("Updated:", last_updated)),
                        tags$div(tags$a("Source Code & Data", href = "https://github.com/joshmartinecon/wnba-ratings")),
                        tags$div("+ = injured")
                      ),
                      wellPanel(
                        selectInput("team_select", "Select Team:", choices = c("ATL","CHI","CON","DAL","GSV","IND","LAS","LVA","MIN","NYL","PHO","SEA","WAS"), selected = "ATL")
                      )
               ),
               column(6, DTOutput("team_table"))
             )
           )
  )
)

##### SERVER #####
server <- function(input, output, session) {
  
  ranked_players <- reactive({
    players %>%
      arrange(desc(rating)) %>%
      mutate(Rank = row_number())
  })
  
  output$all_table <- renderDT({
    df <- ranked_players()
    if (input$team_filter != "All") {
      df <- df %>% filter(team == input$team_filter)
    }
    df %>%
      select(Rank, Player = player, Team = team, Rating = rating) %>%
      datatable(
        colnames = c("", "Player", "Team", "Rating"),
        options = list(paging = FALSE, autoWidth = TRUE, lengthChange = FALSE, info = FALSE),
        width = "100%",
        rownames = FALSE
      )
  })
  
  output$ratings_table <- renderDT({
    team_ratings %>%
      arrange(desc(rating)) %>%
      select(
        Team             = team,
        `Current Rating` = rating,
        `Full Strength`  = strength,
        Rotation         = rotation_rating
      ) %>%
      DT::datatable(
        options = list(
          paging = FALSE,
          autoWidth = TRUE,
          lengthChange = FALSE,
          info = FALSE
        ),
        width = "100%",
        rownames = FALSE
      )
  })
  
  output$team_table <- renderDT({
    df <- players %>% 
      filter(team == input$team_select) %>%
      arrange(desc(rating)) %>%
      mutate(
        delta = case_when(
          min_diff >= 15  ~ "<span style='color:forestgreen;'>&#8593;&#8593;&#8593;</span>",  # ↑↑↑
          min_diff >= 10  ~ "<span style='color:forestgreen;'>&#8593;&#8593;</span>",         # ↑↑
          min_diff >  5   ~ "<span style='color:forestgreen;'>&#8593;</span>",                # ↑
          min_diff <= -15 ~ "<span style='color:crimson;'>&#8595;&#8595;&#8595;</span>",      # ↓↓↓
          min_diff <= -10 ~ "<span style='color:crimson;'>&#8595;&#8595;</span>",             # ↓↓
          min_diff <  -5  ~ "<span style='color:crimson;'>&#8595;</span>",                    # ↓
          TRUE            ~ "<span style='color:gray;'>-</span>"
        )
      ) %>%
      select(
        Player = player,
        `MP/G` = mp_g,
        `Target MP/G` = mp_g_star,
        `Minutes Change` = delta,
        Rating = rating
      )
    
    datatable(
      df, 
      escape = FALSE,
      options = list(paging = FALSE, autoWidth = TRUE, lengthChange = FALSE, info = FALSE),
      width = "100%",
      rownames = FALSE
    )
  })
}

##### Run App #####
shinyApp(ui, server)