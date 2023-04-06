##Logan Lauton
##app.R - NBA Player HUD App v1.1
##this is the app.R for the first iteration of my NBA Player HUD R Shiny app that I will
##be producing. This file holds the main foundation of the apps appearance and brain.

##clear your working directory--------------------------------------------------------------------------
rm(list = ls())

##load libraries----------------------------------------------------------------------------------------
library(bslib)
library(data.table)
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
           fluidRow(
             column(
               width = 7,
               wellPanel(
                 style = "background-color: #fff; border-color: #1D4289; height: 775px;",
                 fluidRow(
                   style = "margin-top: 25px; margin-bottom: -10px;",
                   column(
                     width = 12,
                     selectInput(
                       inputId = "banana",
                       label = "Season",
                       choices = sort(unique(ptot$Season), decreasing = T),
                       selected = latest_season
                     ),
                     selectInput(
                       inputId = 'apple',
                       label = "Team",
                       choices = NULL,
                       selected = NULL
                     )
                   ),
                   column(width = 12,
                          DTOutput("strawberry"))
                 )
               )
             ),
             column(
               width = 5,
               wellPanel(style = "background-color: #fff; border-color: #1D4289; height: 500px; margin-left: 25px;",
                         plotOutput("starfruit"))
             )
           )),
  
  
  
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
  ),),
  windowTitle = "NBA Player HUD"
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$kiwi <- renderImage({
    list(
      src = "~//basketball-cover.png",
      alt = "basketball",
      height = 81,
      width = 81,
      style = "float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px"
    )
  }, deleteFile = FALSE)
  
  team_choices <- reactive({
    ptot %>%
      filter(Season == input$banana) %>%
      distinct(TeamName, .keep_all = FALSE) %>%
      arrange(TeamName)
  })
  
  observeEvent(input$banana, {
    updateSelectInput(session,
                      "apple",
                      choices = team_choices(),
                      selected = NULL)
  })
  
  output$strawberry <- renderDataTable({
    df <-
      dplyr::filter(ptot, Season %in% c(input$banana), TeamName == input$apple)
    df <- dplyr::filter(df,
                        Season %in% c(input$banana),
                        TeamName %in% c(input$apple)) %>%
      select(Player, Pos, Age, TeamName, BLK, AST, TRB, STL, PTS, FG. , FT.) %>%
      mutate(FG. = FG. * 100, FT. = FT. * 100) %>%
      arrange(Player)
    setnames(
      df,
      c(
        "Player",
        "Position",
        "Age",
        "Team",
        "BLK",
        "AST",
        "TRB",
        "STL",
        "PTS",
        "FG%",
        "FT%"
      )
    )
    
    datatable(
      df,
      options = list(
        dom = 't',
        pageLength = 12,
        paging = F,
        scrollY = '520px',
        scrollX = FALSE,
        autoWidth = TRUE,
        rowCallback = JS(
          "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
    if (iDisplayIndex === 0) {
      $(nRow).addClass('selected');
      Shiny.onInputChange('guava', aData[1]);
    }
    $(nRow).on('click', function() {
      $('tr.selected').removeClass('selected');
      $(this).addClass('selected');
      Shiny.onInputChange('guava', aData[1]);
    });
  }"
        )
      )
    )
  })
  
  
  
  output$starfruit <- renderPlot({
    selected_player <- input$guava
    
    starfruit_plt <-
      dplyr::filter(ptot, Player == selected_player) %>%
      group_by(Player, Season) %>%
      summarise(
        BLK = sum(BLK),
        AST = sum(AST),
        TRB = sum(TRB),
        STL = sum(STL),
        PTS = sum(PTS),
        FG. = sum(FG.),
        FT. = sum(FT.),
        G   = G
      )
    gplt <- ggplot(starfruit_plt, aes(x = Season))
    
    if (length(unique(starfruit_plt$Season)) > 2) {
      gplt +
        geom_line(aes(y = (BLK / G), col = 'Blocks')) +
        geom_line(aes(y = (AST / G), col = 'Assists')) +
        geom_line(aes(y = (TRB / G), col = 'Rebounds')) +
        geom_line(aes(y = (STL / G), col = 'Steals')) +
        geom_line(aes(y = (PTS / G), col = 'Points')) +
        theme_minimal() +
        ggtitle(paste(selected_player, "'s Per Game Statistics", sep = "")) +
        theme(plot.title = element_text(hjust = 0.4)) +
        theme(
          axis.line = element_line(color = 'lightgrey'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
        ) +
        scale_color_manual(
          name = "Measured Stat",
          values = c(
            "Blocks" = '#fde725',
            "Assists" = '#5ec962',
            "Rebounds" = '#21918c',
            "Steals" = '#3b528b',
            "Points" = '#440154'
          )
        ) +
        scale_x_continuous(breaks = seq(
          min(starfruit_plt$Season),
          max(starfruit_plt$Season),
          by = 1
        ),
        expand = c(0, 0)) +
        ylab("Per Game Totals")
    }
    else{
      gplt +
        theme_minimal() +
        ggtitle(paste(selected_player, "'s Statistics", sep = "")) +
        theme(plot.title = element_text(hjust = 0.4)) +
        theme(
          axis.line = element_line(color = 'lightgrey'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
        ) +
        geom_blank() +
        scale_x_continuous(breaks = seq(
          min(starfruit_plt$Season),
          max(starfruit_plt$Season),
          by = 1
        ),
        expand = c(0, 0)) +
        ylab("Total")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
