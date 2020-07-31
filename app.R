library(shinydashboard)
library(shiny)
library(tidyverse)
library(grid)
library(gridExtra)
library(readxl)


# Setup environments and variables
dat = as_tibble(read.csv('Wave1-8_A-E.csv'))
request_lookup = read_excel('Combined_Request_Lookup.xlsx', skip=1)
dict_tracking = read_excel('COVID_Data_Dictionaries_tracking.xlsx')
data_dictionary = read_excel('Combined_Data_Dictionary.xlsx', skip=1)


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
                    'STAI_State_raw', 'STAI_Trait_raw', 'VSA_raw_total');




# -----------------------------------------------------------------------------
# Dashboard UI
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "COVID-Dynamic"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "demo", icon = icon("crosshairs")),
      menuItem("Score Correlation", tabName = "corr", icon = icon("database")),
      menuItem("COVID19 Cases Tracking", tabName = "case_track", icon = icon("fire"))
    )
  ),
  dashboardBody(
    tabItems(
      # Score Correlation ------------------------------------------------------
      tabItem(tabName = "demo",
              fluidRow(
                box(width = 12, title = "State Distribution",
                    plotOutput("state_plot", height = 500)
                )
              ),
              fluidRow(
                box(width = 6, title = "Race",
                    plotOutput("race_plot", height = 500)),
                box(width = 6, title = "Age",
                    plotOutput("age_plot", height = 500))
                )
      ),
      # Score Correlation Visualization ------------------------------------------------------
      tabItem(tabName = "corr",
              fluidRow(
                infoBoxOutput("info_corr_coef"),
                infoBoxOutput("info_state"),
                infoBoxOutput("info_count")
              ),
              fluidRow(
                box(width = 8,
                    plotOutput('corr_plot')
                ),
                box(width = 4, title = "Select Variables", status = "warning", solidHeader = TRUE,
                    selectInput('state', 'State', unique(dat$DemW3)),
                    #selectInput('wave', 'Wave', unique(dat$wave)),
                    selectInput('var1', 'Variable 1', score_variables),
                    selectInput('var2', 'Variable 2', score_variables)
                )
              )
      )
      # COVID19 Cases Tracking -------------x-------------------------------------------------
      # tabItem(tabName = "case_track",
      #         fluidRow(box(width = 12,
      #                      plotOutput("vo2", height = "150px"),
      #                      plotOutput("vco2", height = "150px"),
      #                      plotOutput("activity", height = "150px")
      #         )),
      #         fluidRow(
      #           box(width = 6, plotOutput("vo2_vco2", height = "300px")),
      #           box(width = 6, plotOutput("activity_vo2", height = "300px"))
      #         ),
      #         fluidRow(
      #           box(width = 6, plotOutput("vo2_hist", height = "200px"))
      #         )
      # )
    )
  )
)


# -----------------------------------------------------------------------------
# Dashboard server code
# -----------------------------------------------------------------------------
server <- function(input, output) {
  
  # Demographics
  
  output$state_plot <- renderPlot({
    ggplot(data = dat, aes(x=factor(DemW3))) + geom_bar(stat = "count")
  })
  
  
  output$race_plot <- renderPlot({
    ggplot(data = dat, aes(x=factor(DemC9))) + geom_bar(stat = "count")
  })
  
  output$age_plot <- renderPlot({
    ggplot(data = dat, aes(x=DemC1)) + geom_histogram()
  })
  
  
  # The correlation plot --------------------------------
  var1 <- reactive({
    dat[(dat$DemW3==input$state), input$var1]
  })
  
  var2 <- reactive({
    dat[(dat$DemW3==input$state), input$var2]
  })
  
  new_var <- reactive({
    var1 = var1()
    var2 = var2()
    var1 = var1 %>% rename(var1=1)
    var2 = var2 %>% rename(var2=1)
    new_var = drop_na(tibble(var1, var2))
    return(new_var)
  })
  
  output$info_corr_coef <- renderInfoBox({
    
    new_var = new_var()
    
    corr = round(cor(new_var$var1, new_var$var2), 2)
    print(corr)
    infoBox(
      "Correlation Coefficient",
      color = "green",
      icon = icon("bicycle"),
      value = corr
    )
  })
  
  output$info_state <- renderInfoBox({
    
    new_var = new_var()
    
    infoBox(
      "State",
      icon = icon("compass"),
      value = input$state
    )
  })
  
  output$corr_plot <- renderPlot({
    # TODO: The correlation plot. Comment for now and implement later
    new_var = new_var()
    ggplot(data = new_var, aes(x=var1, y=var2)) + geom_point() + geom_smooth(method=lm)
  })
  
  
}


shinyApp(ui, server)
