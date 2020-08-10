library(shinydashboard)
library(shiny)
library(tidyverse)
library(grid)
library(gridExtra)
library(readxl)
library(rapportools)

source("preprocess.r", local=FALSE)

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
          box(width= 12, title='Raw Data Plot 1', solidHeader = TRUE,
            box(width=8, solidHeader = TRUE,
                selectInput('category', 'Category', cats),
                selectInput('question','Question', NULL)
            ),
            box(width=4, solidHeader = TRUE,
                selectInput('wave','Wave', dummy_waves)
            ),
            box(width= 12, solidHeader = TRUE,
                plotOutput('plot', height=600)
            )
          )
          )
        ),
      
      # Correlation 
      tabItem('corr')
      ))
  
  )
  

# -----------------------------------------------------------------------------
# Dashboard server code
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

 observe({
   
   measure <- measures[cats==input$category]
   
   wave <- unlist(findWaves(measure))
   wave <- c(wave, 'Prolific')
   wave <- c(wave, 'Conte')
   wave <- c(wave, 'All')
   
   qs <- unlist(grabQuestions(measure))
   updateSelectInput(session, 'wave', choices=wave, selected=NULL)
   updateSelectInput(session, 'question', choices=qs, selected=NULL)
   
 })
  
  
  output$plot <- renderPlot({ 
    variable <- response_rows[response_rows$ElementDescription==input$question, 'unified_variable_names']
    title <- response_rows[response_rows$ElementDescription==input$question, 'Text']
    title <- paste0(strwrap(title, 70), sep="", collapse="\n")
    
    measure <- measures[cats==input$category]
    
    if (input$wave == 'All'){
      wave <- str_remove(unlist(findWaves(measure)), 'Wave ')
      subset_dat <- dat[dat$wave %in% wave,]
    } else if (input$wave == 'Prolific') {
      wave <- str_remove(unlist(findWaves(measure)), 'Wave ')
      wave <- wave[!is.na(as.numeric(wave))]
      subset_dat <- dat[dat$wave %in% wave,]
    } else if (input$wave == 'Conte') {
      wave <- str_remove(unlist(findWaves(measure)), 'Wave ')
      wave <- wave[is.na(as.numeric(wave))]
      subset_dat <- dat[dat$wave %in% wave,]
    } else {
      wave <- str_remove(input$wave, 'Wave ')
      subset_dat <- dat[dat$wave==wave,]
    }
    
    ase_x = paste('factor(', variable, ')')
    (ggplot(data=subset_dat, aes_string(ase_x, fill=ase_x)) + geom_bar() 
           + geom_text(stat='count', aes(label=..count..), vjust=-1)
           + labs(title = title,
                  x =variable, y = "Count", fill = variable))
  }
)
  
  
}

shinyApp(ui, server)

