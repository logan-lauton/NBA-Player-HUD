##Logan Lauton
##app.R - NBA Player HUD App v1.0
##this is the app.R for the first iteration of my NBA Player HUD R Shiny app that I will
##be producing. This file holds the main foundation of the apps appearance and brain.

##clear your working directory--------------------------------------------------------------------------
rm(list = ls())

##load libraries----------------------------------------------------------------------------------------
library(bslib)
library(dplyr)
library(DT)
library(extrafont)
library(ggridges)
library(gridExtra)
library(scales)
library(shiny)
library(shinythemes)
library(tidyverse)

##loading in data---------------------------------------------------------------------------------------
ptot <- read.csv("~\\NBA Player Stats(1950 - 2022).csv")
psal <- read.csv("~\\NBA Salaries(1990-2023).csv")
tpay <- read.csv("~\\NBA Payroll(1990-2023).csv")

#assign variable----------------------------------------------------------------------------------------
latest_season <- 2022

# Define UI for application-----------------------------------------------------------------------------
ui <- navbarPage(
  theme = shinytheme('flatly'),
  id = "nba_player_hud",
  imageOutput('kiwi'),
  
  #Main Tab-----------------------------------------------------------------------------------------------
  tabPanel("Welcome",
           fluidRow(column(
             12,
             wellPanel(style = "background-color: #fff00; border-color: #2c3e50;",
                       includeMarkdown("~\\Test.md"))
           ))),
  
  #Player Database----------------------------------------------------------------------------------------
  tabPanel("NBA Player Database",
           fluidRow(column(
             7,
             wellPanel(
               style = "background-color: #fff; border-color: #1D4289; height: 775px;",
               fluidRow(style = "margin-top: 25px; margin-bottom: -10px;",
                        column(
                          10,
                          column(2, style = 'margin-top: 7px;', align = 'right', p("From:")),
                          fluidRow(column(
                            8,
                            selectInput(
                              inputId = 'banana',
                              label = NULL,
                              choices = sort(unique(ptot$Season), decreasing = T),
                              selected = latest_season
                            )
                          )),
                          DT::dataTableOutput("strawberry")
                        ))
             )
           ))),
  # Footer -------------------------------
  hr(style = "border-color: #cbcbcb;"),
  fluidRow(column(
    9,
    p(
      'All of the data used to generate this app were obtained from ',
      tags$a(
        href = "https://www.basketball-reference.com/",
        'basketball-reference.com',
        target = '_blank'
      ),
      'and',
      tags$a(href = "https://hoopshype.com/salaries/players/", 'hoopshype.com', target = '_blank'),
      '.',
      style = "font-size: 85%"
    ),
    p(
      "App created by Logan Lauton in Spring of 2023",
      HTML("&bull;"),
      "Find Logan on Github:",
      tags$a(
        href = "https://github.com/logan-lauton",
        tags$i(class = 'fa fa-github', style = 'color:#5000a5'),
        target = '_blank'
      ),
      style = "font-size: 85%"
    )
  ),
  column(
    3,
    align = "right",
    conditionalPanel(
      condition = "input.masters_golf == 'Scoring Averages' | input.masters_golf == 'Player Pages'",
      p(
        tags$em(
          'Check the box below to change the colour scheme of the distributions to one that is more easily read by those with color blindness.',
          style = "font-size: 70%; font-family:Helvetica"
        )
      ),
      checkboxInput("col_blind", label = "Colourblind?", value = F)
    )
  )),
  windowTitle = "NBA Player HUD"
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$kiwi <- renderImage({
    list(
      src = "~//basketball-cover.png",
      alt = "basketball",
      height = 81,
      width = 81,
      style = "float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px"
    )
  })
  
  
  
  output$strawberry <- renderDataTable({
    datatable(
      dplyr::filter(ptot, Season %in% c(input$banana)) %>%
        select(Player, Pos, Age, Tm, BLK, AST, TRB, STL, FG. , FT.),
      options = list(
        dom = 't',
        pageLength = 9,
        paging = F,
        scrollY = '550px'
      )
    )
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
