library(shinydashboard)
library(shiny)
library(tidyverse)
library(grid)
library(gridExtra)
library(readxl)
library(rapportools)

source("preprocess.r", local=FALSE)
question_wave_map <- generateQ_Wave_Map()


# Setup environments and variables
dat = as_tibble(read.csv('Wave1-8_A-E.csv'))
request_lookup = read_excel('Combined_Request_Lookup.xlsx', skip=1)
dict_tracking = read_excel('COVID_Data_Dictionaries_tracking.xlsx')
data_dictionary = read_excel('Combined_Data_Dictionary.xlsx', skip=1)
dummy_waves <- colnames(data_dictionary %>% select(8:22))

# Remove outlier for age:
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

dat$DemC1 = remove_outliers(dat$DemC1)

# For testing

score_variables = c('Disgust_COVID_raw', 'Disgust_core_zscore', 'Disgust_animal_zscore', 'Disgust_contam_zscore', 'Disgust_total_zscore', 'EMS_Black_raw', 'IMS_Black_raw', 'EMS_Chinese_raw', 'Fear_COVID_raw', 
                    'Human_Egal_total_score', 'NIH_TB_Emot_Support_uncorrTscore', 'NIH_TB_Loneliness_uncorrTscore',
                    'PANAS_Pos_zscore', 'PANAS_Neg_zscore', 'PSS_Total', 'RT_Black', 'RT_White', 'RT_Hispanic', 
                    'RT_Asian', 'RT_Chinese', 'RT_European', 'City_Restriction_raw', 'City_Punishment_raw', 
                    'City_Reactance_raw', 'City_Research_raw', 'City_Stimulus_raw', 'City_Information_raw', 
                    'State_Restriction_raw', 'State_Punishment_raw', 'State_Reactance_raw', 'State_Research_raw', 
                    'State_Stimulus_raw', 'State_Information_raw', 'Federal_Restriction_raw', 'Federal_Punishment_raw', 
                    'Federal_Reactance_raw', 'Federal_Research_raw', 'Federal_Stimulus_raw', 'Federal_Information_raw', 
                    'STAI_State_raw', 'STAI_Trait_raw', 'VSA_raw_total')

# -----------------------------------------------------------------------------
# Dashboard UI
# -----------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "COVID-Dynamic"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("crosshairs")),
      menuItem("Score Correlation", tabName = "corr", icon = icon("database")),
      menuItem("COVID19 Cases Tracking", tabName = "case_track", icon = icon("fire")))),

  dashboardBody(
    tabItems(
      
      # Data Category and Visualization ----------------------------------------------
      tabItem('data',
        fluidRow(
          box(width=8, title= 'Data Category', solidHeader = TRUE,
              selectInput('category', 'Category', question_wave_map$measure)
          ),
          box(width=3, title= 'Wave', solidHeader = TRUE,
              selectInput('wave','Wave', dummy_waves)),
          box(width=12, title='Question', solidHeader = TRUE,
              selectInput('question','Question', NULL),
          box(width= 12,title='Data Distribution', solidHeader = TRUE,
              plotOutput('plot', height=600)
          )
        )),
      
      # Correlation 
      tabItem('corr')
      ))
  
  ))
  

# -----------------------------------------------------------------------------
# Dashboard server code
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

 observe({
   
   measure <- input$category
   if(is.null(measure))
     x <- character(0)
   
   wave <- unlist(question_wave_map$wave[question_wave_map$measure == measure])
   q <- unlist(question_wave_map$questions[question_wave_map$measure == measure])

   updateSelectInput(session, 'wave', choices=wave, selected=NULL)
   updateSelectInput(session, 'question', choices=q, selected=NULL)
   
 })
  
  
  output$plot <- renderPlot(
    { ggplot(data=dat, aes(x=AnxS1_1)) + geom_bar() }
  )
  
  
}

shinyApp(ui, server)

