library(shiny)
library(DT)
library(dplyr)

##### Load Data #####
# setwd("C:/Users/jmart/OneDrive/Desktop/GitHub/wnba-ratings")
players <- read.csv("https://raw.githubusercontent.com/joshmartinecon/wnba-ratings/refs/heads/main/WNBA_Ratings_and_Rotations.csv", stringsAsFactors = FALSE)
team_ratings <- read.csv("https://raw.githubusercontent.com/joshmartinecon/wnba-ratings/refs/heads/main/WNBA_Team_Ratings.csv", stringsAsFactors = FALSE)
last_updated <- read.csv("https://raw.githubusercontent.com/joshmartinecon/wnba-ratings/refs/heads/main/last_updated.csv")
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
    
    table.dataTable th, table.dataTable td {
      white-space: nowrap;     /* Prevent text wrapping */
      width: auto !important;  /* Allow column to shrink to fit */
    }
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