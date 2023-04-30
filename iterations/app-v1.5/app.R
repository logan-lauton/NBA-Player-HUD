##Logan Lauton
##app.R - NBA Player HUD App v1.5
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
library(plotly) 
library(scales)
library(shiny)
library(shinythemes)
library(tidyverse)

##loading in data---------------------------------------------------------------------------------------

ptot <- readRDS("/cloud/project/ptot_20_data.rds")
inj  <- readRDS("/cloud/project/inj_20_data.rds" )

#assign variable----------------------------------------------------------------------------------------
latest_season     <- 2023
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
                 plotlyOutput("starfruit", height = "355px"),
                 plotlyOutput("mango", height = '355px')
                
               )
             )
           )),
  #Comparison Tab--------------------------------------------------------------
  tabPanel("Player Comparison",
           fluidRow(column(
             width = 7,
             wellPanel(
               style = "background-color: #fff; border-color: #1D4289; height: 775px;",
               fluidRow(
                 style = "margin-top: 25px; margin-bottom: -10px;",
                 column(
                   width = 3,
                   textInput("text", h3("Player 1"), 
                             value = "Enter Player...")),   
                 column(
                   width = 3,
                   textInput("text", h3("Player 2"), 
                             value = "Enter Player..."))
                 )   
             )
               )
             )
           ))

  




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
    df <- ptot %>%
      group_by(Player) %>% 
      filter(n()>1) %>%
      arrange(Player)
    
    df <-
      dplyr::filter(df, Season %in% input$banana, TeamName == input$apple)
    
    df <- 
      dplyr::filter(df,
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
        autoHeight = TRUE,
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
  

  output$starfruit <- renderPlotly({
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

    avg_plt <- 
      dplyr::filter(ptot, Player == selected_player)
    
    avg_plt <- 
      dplyr::filter(ptot, Pos == avg_plt$Pos, Season >= min(avg_plt$Season)) %>%
      group_by(Pos, Season) %>%
      summarise(
        BLK = sum(BLK),
        AST = sum(AST),
        TRB = sum(TRB),
        STL = sum(STL),
        PTS = sum(PTS),
        FG. = sum(FG.),
        FT. = sum(FT.),
        G   = sum(G)
      )
      
      

    inj_dat<-
      dplyr::filter(inj, Relinquished == selected_player)

    inj_dat <- 
      inj_dat %>%
      group_by(Season) %>% 
      count(Relinquished)
    
    inj_dat <- 
      inj_dat %>%
      rename(IL = n)
    

    gplt <- ggplot(starfruit_plt, aes(x = Season))
    
      gplt +
        geom_line(aes(y = (BLK / G), col = 'Players Blocks')) +
        geom_line(data = avg_plt, aes(y = (BLK / G), col = 'Position Avg Blocks'),
                  alpha = 0.2)+
        geom_line(aes(y = (AST / G), col = 'Players Assists')) +
        geom_line(data = avg_plt, aes(y = (AST / G), col = 'Position Avg Assists'),
                  alpha = 0.2)+
        geom_line(aes(y = (TRB / G), col = 'Players Rebounds')) +
        geom_line(data = avg_plt, aes(y = (TRB / G), col = 'Position Avg Rebounds'),
                  alpha = 0.2)+
        geom_line(aes(y = (STL / G), col = 'Players Steals')) +
        geom_line(data = avg_plt, aes(y = (STL / G), col = 'Position Avg Steals'),
                  alpha = 0.2)+
        geom_line(aes(y = (PTS / G), col = 'Players Points')) +
        geom_line(data = avg_plt, aes(y = (PTS / G), col = 'Position Avg Points'),
                  alpha = 0.2)+
        geom_col(data = inj_dat,
                   aes(x=Season, y=IL),
                    fill = '#8b0000',
                    alpha = 0.15)+
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
            "Players Blocks" = '#fde725',
            "Players Assists" = '#5ec962',
            "Players Rebounds" = '#21918c',
            "Players Steals" = '#3b528b',
            "Players Points" = '#440154',
            "Position Avg Blocks" = '#fde725',
            "Position Avg Assists" = '#5ec962',
            "Position Avg Rebounds" = '#21918c',
            "Position Avg Steals" = '#3b528b',
            "Position Avg Points" = '#440154'
          )
        ) +
        scale_x_continuous(breaks = seq(
          min(starfruit_plt$Season),
          max(starfruit_plt$Season),
          by = 2
        ),
        expand = c(0, 0)) +
        ylab("Per Game Totals")
  })
  
  output$mango <- renderPlotly({
    selected_player <- input$guava
    
    avg_plt <- 
      dplyr::filter(ptot, Player == selected_player)
    
    avg_plt <- 
      dplyr::filter(ptot, Pos == avg_plt$Pos, Season >= min(avg_plt$Season)) %>%
      group_by(Pos, Season) %>%
      summarise(
        BLK = sum(BLK),
        AST = sum(AST),
        TRB = sum(TRB),
        STL = sum(STL),
        PTS = sum(PTS),
        FGM = sum(FG),
        FGA = sum(FGA),
        FTM = sum(FT),
        FTA = sum(FTA),
        G   = sum(G)
      )
    
    inj_dat<-
      dplyr::filter(inj, Relinquished == selected_player)
    
    inj_dat <- 
      inj_dat %>%
      group_by(Season) %>% 
      count(Relinquished)
    
    inj_dat <- 
      inj_dat %>%
      rename(IL = n)
    
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
    
      gplt +
        geom_line(aes(y = (FG. * 100), col = 'Player FG%')) +
        geom_line(data = avg_plt, aes(y = ((FGM/FGA) * 100), col = 'Position Avg FG%'),
                 alpha = 0.2)+
        geom_line(aes(y = (FT. * 100), col = 'Player FT%')) +
        geom_line(data = avg_plt, aes(y = ((FTM/FTA) * 100), col = 'Position Avg FT%'),
                  alpha = 0.2)+
        geom_col(data = inj_dat,
                 aes(x=Season, y=IL),
                 fill = '#8b0000',
                 alpha = 0.15)+
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
                           values = c("Player FG%" = '#7ad151',
                                      "Player FT%" = '#481c6e',
                                      "Position Avg FG%" = '#7ad151',
                                      "Position Avg FT%" = '#481c6e')) +
        scale_x_continuous(breaks = seq(min(mango_plt$Season),
                                        max(mango_plt$Season),
                                        by = 2),
                           expand = c(0, 0)) +
        ylab("%")
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
