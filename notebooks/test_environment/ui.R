library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput('file',
                label = h3('File input'),
                multiple = F,
                accept = c(".tsv",
                           ".csv")),
      selectInput('barChart_input',
                  'markers',
                  choices = 'none',
                  multiple = T,
                  selectize = T),
      radioButtons('radio_donut', 
                   'Select Doghnut Chart',
                   choices = c("% of subset",
                               "% of whole")),
      radioButtons('radio_bar',
                   'Select bar chart',
                   choices = c("% of subset",
                               "% of whole" )),
      selectInput('y_input',
                  'y_axis',
                  choices = 'none',
                  multiple = F,
                  selectize = T),
      selectInput('x_input',
                  'x_axis',
                  choices = 'none',
                  multiple = F,
                  selectize = T),
      radioButtons('log_trans',
                   'Log Transformation',
                   choices = c('yes', 
                               'no')),
      radioButtons('intensity_choices',
                   'Intensity Values',
                   choices = c('all',
                               'positive',
                               'double positive',
                               'positive (no double positive)',
                               'negative'))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("cellMap", height = "100vh")
    )
  )
))
