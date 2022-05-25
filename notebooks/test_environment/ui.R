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
                        selectize = T)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("barChart"),
            plotlyOutput("cellMap")
        )
    )
))