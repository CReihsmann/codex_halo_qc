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
                                    "% of whole" ))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("barChart"),
            plotOutput("doughnutChart")
        )
    )
))