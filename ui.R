library(shiny)
library(DT)
library(dplyr)

##### Load Data #####
# setwd("C:/Users/jmart/OneDrive/Desktop/GitHub/wnba-ratings")
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