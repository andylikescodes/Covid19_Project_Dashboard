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
            box(width=8, height = 350, background= 'purple', solidHeader = TRUE,
                selectInput('category', 'Category', cats),
                selectInput('question','Question', NULL),
                selectInput('wave','Wave', dummy_waves)
            ),
            box(width=4, height = 350, background= 'olive', solidHeader = TRUE,
                selectInput('dem', 'Demographics', demographic_varnames),
                selectInput('wave_dem','Wave', dummy_waves),
            ),
            box(width= 12, solidHeader = TRUE,
                plotOutput('plot', height=600)
            ),
            box(width=12, solidHeader = TRUE,
                plotOutput('plot_dem', height = 600))
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
  })
  
  
  output$plot_dem <- renderPlot({
    dem <- unlist(response_rows[response_rows$ElementDescription == input$dem, 'unified_variable_names'])
    dem_data <- unlist(dat[dem])
    text_type <- unlist(request_lookup[request_lookup$unified_variable_names == dem, 'Text'])
    
    
    if(is.na(text_type)){
      graph_type <- NA
    }
    else{
     graph_type <- unlist(val_types[val_types$`unique(demo_data$Text)` == text_type, 'values'])
     graph_type <- graph_type[!is.na(graph_type)]
    }
    
    
    if(is.na(graph_type)){
      # This error is giving trouble, evaluates to TRUE by gives warning + no plot
      ggplot(dat, aes(dem_data)) + geom_bar() 
    }
    if (graph_type == 'Likert'){
      data <- as.factor(dem_data)
      ggplot(dat, aes(data)) + geom_bar() + coord_flip()
      
    }
    else if (graph_type == 'Numeric Factor'){
      data <- as.factor(dem_data)
      ggplot(dat, aes(data)) + geom_bar() + coord_flip()
    }
    else if (graph_type == 'Categorical Factor'){
      data <- as.factor(dem_data)
      ggplot(dat, aes(data)) + geom_bar() + coord_flip()
    }
    else if (graph_type == 'Binary'){
      data <- as.factor(dem_data)
      ggplot(dat, aes(data)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust= -1)
    }
    else if (graph_type == 'Y/N/O'){
      data <- as.factor(dem_data)
      ggplot(dat, aes(data)) + geom_bar() + coord_flip()
    }
    else{
     plot(1, type='n', xlab='', ylab='')
     title("No Data Available")
    }

  })
  
}

shinyApp(ui, server)

