## app.R ##
source(paste0(here::here(),"/00_setup.R"))

model_run_list <- list.files(paste0(here(), '/Logs/'))
model_run_list <- model_run_list[model_run_list != 'Archive']

folder <- paste0(here(), '/Logs/season_2020_diff_60_20211309_2136/')

ui <- dashboardPage(
  dashboardHeader(title = 'Brownlow Medal prediction'),
  dashboardSidebar(
    selectInput('logSelector', 'Select model run:', model_run_list, selected = 'season_2020_diff_60_20211309_2136')
    # sidebarMenu(
    #   menuItem('By match', tabName = 'by_match_tab'),
    #   menuItem('By player', tabName = 'by_player_tab'),
    #   menuItem('By team', tabName = 'by_team_tab')
    # )
  ),
  dashboardBody(
    # tabItems(
    #   tabItem(
    #     tabName = 'by_match_tab',
    #     fluidRow(
    #       box(DTOutput('by_match'), width = 12)
    #     )
    #   ),
      tabItem(
        tabName = 'by_player_tab',
        fluidRow(
          box(plotlyOutput('by_player'), width = 12)
        )
      )
      # tabItem(
      #   tabName = 'by_team_tab',
      #   fluidRow(
      #     box(DTOutput('by_team'), width = 12)
      #   )
      # )
    )
  )

server <- function(input, output) {
  # file_list <- list.files(paste0(here(), '/logs'))
  # 
  # season_list <- file_list %>% 
  #   str_extract('(?<=season_)[0-9]{4}')
  # season_list <- unique(season_list[!is.na(season_list)])
  # 
  # top_20_diff_list <- file_list %>% 
  #   str_extract('(?<=diff_)[0-9]{2,4}')
  # top_20_diff_list <- unique(top_20_diff_list[!is.na(top_20_diff_list)])
  # 
  # time_run_list <- file_list %>% 
  #   str_extract('[0-9]{8}_[0-9]{4}')
  # time_run_list <- unique(time_run_list[!is.na(time_run_list)]) %>% 
  #   as.Date('%Y%d%m_%H%M') %>% 
  #   format('%Y-%m-%d %H:%M:%S')
    
  # Hard code the model files that we want to visualise
  # folder <- reactive({paste0(here(), '/Logs/', input$logSelector, '/')})
  
  
  
  by_match <- reactive({
    fread(paste0(here(), '/Logs/', input$logSelector, '/', 'votes_by_match.csv')) %>% 
      arrange(match_round)
  })
  by_player <- reactive({fread(paste0(here(), '/Logs/', input$logSelector, '/', 'votes_by_player.csv'))})
  by_team <- reactive({fread(paste0(here(), '/Logs/', input$logSelector, '/', 'votes_by_team.csv'))})
  
  # output$by_match <- renderDT({
  #   by_match() %>% 
  #     select(
  #       `Player name` = player_name, 
  #       `Brownlow votes` = brownlow_votes, 
  #       `Predicted votes` = predicted_votes
  #     ),
  #   options = list(
  #     scrollX = T
  #   ),
  #   rownames = F
  # })
  
  output$by_player <- renderPlotly({
    top_10_players_df <- by_match() %>% 
      group_by(player_name) %>% 
      summarise(predicted_votes = sum(predicted_votes), actual_votes = sum(brownlow_votes)) %>% 
      arrange(desc(predicted_votes)) %>% 
      head(10)
    
    top_10_players <- top_10_players_df %>% 
      select(player_name) %>% 
      pull()
    
    p <- by_match() %>% 
      filter(player_name %in% top_10_players) %>% 
      mutate(
        player_name = factor(player_name, levels = rev(top_10_players))
      ) %>% 
      select(
        Player = player_name, 
        `Predicted votes` = predicted_votes, 
        `Actual votes` = brownlow_votes, 
        Round = match_round
      ) %>% 
      ggplot(aes(x = Player, y = `Predicted votes`, fill = Round)) + 
      geom_bar(position = 'stack', stat = 'identity') +
      coord_flip() +
      ylab('Predicted votes by round') + 
      xlab('Top 10 players') +
      ggtitle('Top 10 player votes by round') + 
      scale_fill_gradient(name = 'Round') + 
      geom_text(
        aes(label = stat(y), group = Player), 
        stat = 'summary', fun = sum, nudge_y = 1
      ) +
      theme_classic()
    
    plotly_p <- ggplotly(p) %>% 
      config(displayModeBar = F)
    
    font_style <- list(
      family = 'arial'
    )
    
    plotly_p %>% 
      layout(font = font_style)
  })
  
  # output$by_player <- renderDT(
  #   by_player,
  #   options = list(
  #     scrollX = T
  #   ),
  #   rownames = F
  # )
  
  # output$by_team <- renderDT({
  #   by_team(),
  #   options = list(
  #     scrollX = T
  #   ),
  #   rownames = F
  # })
  
}

shinyApp(ui, server)