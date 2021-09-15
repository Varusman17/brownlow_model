## app.R ##
source(paste0(here(),"/00_setup.R"))

ui <- dashboardPage(
  dashboardHeader(title = 'Brownlow Medal prediction'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('By match', tabName = 'by_match_tab'),
      menuItem('By player', tabName = 'by_player_tab'),
      menuItem('By team', tabName = 'by_team_tab')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'by_match_tab',
        fluidRow(
          box(DTOutput('by_match'), width = 12)
        )
      ),
      tabItem(
        tabName = 'by_player_tab',
        fluidRow(
          box(DTOutput('by_player'), width = 12)
        )
      ),
      tabItem(
        tabName = 'by_team_tab',
        fluidRow(
          box(DTOutput('by_team'), width = 12)
        )
      )
    )
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
  folder <- paste0(here(), '/Logs/season_2020_diff_60_20211309_2136/')
  
  by_match <- read_csv(paste0(folder, 'votes_by_match.csv'))
  by_player <- read_csv(paste0(folder, 'votes_by_player.csv'))
  by_team <- read_csv(paste0(folder, 'votes_by_team.csv'))
  
  output$by_match <- renderDT(
    by_match %>% 
      select(
        `Player name` = player_name, 
        `Brownlow votes` = brownlow_votes, 
        `Predicted votes` = predicted_votes
      ),
    options = list(
      scrollX = T
    ),
    rownames = F
  )
  
  output$by_player <- renderDT(
    by_player,
    options = list(
      scrollX = T
    ),
    rownames = F
  )
  
  output$by_team <- renderDT(
    by_team,
    options = list(
      scrollX = T
    ),
    rownames = F
  )
  
}

shinyApp(ui, server)