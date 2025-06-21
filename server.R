library(shiny)
library(DT)
library(dplyr)

##### Load Data #####
# setwd("C:/Users/jmart/OneDrive/Desktop/GitHub/wnba-ratings")
players <- read.csv("WNBA_Ratings_and_Rotations.csv", stringsAsFactors = FALSE)
team_ratings <- read.csv("WNBA_Team_Ratings.csv", stringsAsFactors = FALSE)
last_updated <- readRDS("last_updated.RDS")

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
        `NB Minutes` = min_symbol
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