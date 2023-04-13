##Logan Lauton
##app.R - NBA Player HUD App v1.4
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

ptot <- readRDS("/cloud/project/ptot_20_data.rds")
psal <- readRDS("/cloud/project/psal_20_data.rds")
tpay <- readRDS("/cloud/project/tpay_20_data.rds")
pbox <- readRDS("/cloud/project/pbox_20_data.rds")

#assign variable----------------------------------------------------------------------------------------
latest_season     <- 2022
latest_season_box <- 2023

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
                       includeMarkdown("/cloud/project/Test.md"))
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
               wellPanel(
                 style = "background-color: #fff; border-color: #1D4289; height: 775px; margin-left: 25px;",
                 plotOutput("starfruit", height = "355px"),
                 plotOutput("mango", height = '355px')
               )
             )
           )),
  
  #Box Score Tab---------------------------------------------------
  tabPanel("Box Score Tab",
           fluidRow(column(
             width = 12,
             wellPanel(
               style = "background-color: #fff; border-color: #1D4289; height: 775px;",
               fluidRow(
                 style = "margin-top: 25px; margin-bottom: -10px;",
                 column(
                   width = 12,
                   selectInput(
                     inputId = "blueberry",
                     label = "Season",
                     choices = sort(unique(pbox$Season), decreasing = T),
                     selected = latest_season_box
                   ),
                   selectInput(
                     inputId = 'raspberry',
                     label = "Team",
                     choices = NULL,
                     selected = NULL
                   )
                 ),
                 column(width = 12,
                        DTOutput("grape"))
               )
             )
           ))),
  #Forecasting Tab--------------------------------------------------------------
  tabPanel("Forecasting Tab",
           fluidRow(column(
             width = 7,
             wellPanel(
               style = "background-color: #fff; border-color: #1D4289; height: 775px;",
               fluidRow(
                 style = "margin-top: 25px; margin-bottom: -10px;",
                 column(
                   width = 3,
                   textInput("text", h3("Player Search"), 
                             value = "Enter Player..."))   
                 )
               )
             )
           ))
)
  




# Define server logic required to draw a histogram=====================================================
server <- function(input, output, session) {
  output$kiwi <- renderImage({
    list(
      src = "/cloud/project/basketball-cover.png",
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
      dplyr::filter(ptot, Season %in% input$banana, TeamName == input$apple)
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
      extensions = c('Scroller', 'ColReorder'),
      options = list(
        dom = 't',
        server = TRUE,
        processing = TRUE,
        pageLength = 12,
        paging = TRUE,
        scrollY = '520px',
        scrollX = TRUE,
        autoWidth = TRUE,
        colReorder = TRUE,
        deferRender = TRUE,
        scroller = TRUE,
        scrollCollapse = TRUE,
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
        BLK = sum(BLK * G) / sum(G),
        AST = sum(AST * G) / sum(G),
        TRB = sum(TRB * G) / sum(G),
        STL = sum(STL * G) / sum(G),
        PTS = sum(PTS * G) / sum(G),
        FG. = sum(FG. * G) / sum(G),
        FT. = sum(FT. * G) / sum(G),
        G   = sum(G)
      )
    
    gplt <- ggplot(starfruit_plt, aes(x = Season))
    
    if (length(unique(starfruit_plt$Season)) >= 2) {
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
          panel.border = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank()
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
          by = 2
        ),
        expand = c(0, 0)) +
        ylab("Per Game Totals")
    }
    else{
      gplt +
        geom_point(aes(y = (BLK / G), col = 'Blocks')) +
        geom_text(aes(
          y = (BLK / G),
          label = round(BLK / G, 1),
          col = 'Blocks'
        ),
        nudge_y = -0.1,
        size = 3.5) +
        
        geom_point(aes(y = (AST / G), col = 'Assists')) +
        geom_text(aes(
          y = (AST / G),
          label = round(AST / G, 1),
          col = 'Assists'
        ),
        nudge_y = 0.1,
        size = 3.5) +
        
        geom_point(aes(y = (TRB / G), col = 'Rebounds')) +
        geom_text(
          aes(
            y = (TRB / G),
            label = round(TRB / G, 1),
            col = 'Rebounds'
          ),
          nudge_y = -0.1,
          size = 3.5
        ) +
        
        geom_point(aes(y = (STL / G), col = 'Steals')) +
        geom_text(aes(
          y = (STL / G),
          label = round(STL / G, 1),
          col = 'Steals'
        ),
        nudge_y = 0.1,
        size = 3.5) +
        
        geom_point(aes(y = (PTS / G), col = 'Points')) +
        geom_text(aes(
          y = (PTS / G),
          label = round(PTS / G, 1),
          col = 'Points'
        ),
        nudge_y = -0.1,
        size = 3.5) +
        
        theme_minimal() +
        ggtitle(paste(selected_player, "'s Statistics", sep = "")) +
        theme(plot.title = element_text(hjust = 0.4)) +
        theme(
          axis.line = element_line(color = 'lightgrey'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
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
        ylab("Total")
      
    }
  })
  
  output$mango <- renderPlot({
    selected_player <- input$guava
    
    mango_plt <-
      dplyr::filter(ptot, Player == selected_player) %>%
      group_by(Player, Season) %>%
      summarise(
        BLK = sum(BLK * G) / sum(G),
        AST = sum(AST * G) / sum(G),
        TRB = sum(TRB * G) / sum(G),
        STL = sum(STL * G) / sum(G),
        PTS = sum(PTS * G) / sum(G),
        FG. = sum(FG. * G) / sum(G),
        FT. = sum(FT. * G) / sum(G),
        G   = sum(G)
      )
    
    gplt <- ggplot(mango_plt, aes(x = Season))
    
    if (length(unique(mango_plt$Season)) >= 2) {
      gplt +
        geom_line(aes(y = (FG. * 100), col = 'FG%')) +
        geom_line(aes(y = (FT. * 100), col = 'FT%')) +
        theme_minimal() +
        ggtitle(paste(selected_player, "'s %'s", sep = "")) +
        theme(plot.title = element_text(hjust = 0.4)) +
        theme(
          axis.line = element_line(color = 'lightgrey'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
        ) +
        scale_color_manual(name = "Measured %",
                           values = c("FG%" = '#7ad151',
                                      "FT%" = '#481c6e')) +
        scale_x_continuous(breaks = seq(min(mango_plt$Season),
                                        max(mango_plt$Season),
                                        by = 2),
                           expand = c(0, 0)) +
        ylab("%")
    }
    else{
      gplt +
        geom_point(aes(y = (FG. * 100), col = 'FG%')) +
        geom_text(aes(
          y = (FG. * 100),
          label = round(FG. * 100, 1),
          col = 'FG%'
        ),
        nudge_y = 1,
        size = 3.5) +
        
        geom_point(aes(y = (FT. * 100), col = 'FT%')) +
        geom_text(aes(
          y = (FT. * 100),
          label = round(FT. * 100, 1),
          col = 'FT%'
        ),
        nudge_y = 1,
        size = 3.5) +
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
        scale_color_manual(name = "Measured %",
                           values = c("FG%" = '#7ad151',
                                      "FT%" = '#481c6e')) +
        scale_x_continuous(breaks = seq(min(mango_plt$Season),
                                        max(mango_plt$Season),
                                        by = 1),
                           expand = c(0, 0)) +
        ylab("%")
    }
  })
  
  
  team_choices_1 <- reactive({
    pbox %>%
      filter(Season == input$blueberry) %>%
      distinct(TeamName, .keep_all = FALSE) %>%
      arrange(TeamName)
  })
  
  observeEvent(input$blueberry, {
    updateSelectInput(session,
                      "raspberry",
                      choices = team_choices_1(),
                      selected = NULL)
  })
  
  output$grape <- DT::renderDataTable({
    # create a data frame with only the data for the selected season
    df1 <-
      dplyr::filter(pbox,
                    Season %in% input$blueberry,
                    TeamName == input$raspberry)
    
    df1 <- dplyr::filter(df1,
                         Season %in% c(input$blueberry),
                         TeamName %in% c(input$raspberry)) %>%
      select(
        PLAYER_NAME,
        TeamName,
        GAME_DATE,
        MATCHUP,
        WL,
        MIN,
        FGM,
        FGA,
        FG_PCT,
        FG3M ,
        FG3A,
        FG3_PCT,
        FTM,
        FTA,
        FT_PCT,
        REB,
        AST,
        STL,
        BLK,
        TOV,
        PF,
        PTS,
        PLUS_MINUS
      ) %>%
      mutate(
        FG_PCT = FG_PCT * 100,
        FG3_PCT = FG3_PCT * 100,
        FT_PCT = FT_PCT * 100
      ) %>%
      arrange(GAME_DATE, PLAYER_NAME)
    
    # set the column names to match the names in your UI code
    setnames(
      df1,
      c(
        "Player",
        "Team",
        "Date",
        "Matchup",
        "W/L",
        "MIN",
        "FGM",
        "FGA",
        "FG%",
        "3PM",
        "3PA",
        "3P%",
        "FTM",
        "FTA",
        "FT%",
        "REB",
        "AST",
        "STL",
        "BLK",
        "TOV",
        "PF",
        "PTS",
        "+/-"
      )
    )
    
    # configure the DT::datatable to use server-side processing
    DT::datatable(
      df1,
      extensions = c('Scroller', 'ColReorder'),
      options = list(
        dom = 't',
        server = TRUE,
        processing = TRUE,
        pageLength = 12,
        paging = TRUE,
        scrollY = '520px',
        scrollX = TRUE,
        autoWidth = TRUE,
        colReorder = TRUE,
        deferRender = TRUE,
        scroller = TRUE,
        scrollCollapse = TRUE
      ),
      rownames = FALSE,
      selection = 'none'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
