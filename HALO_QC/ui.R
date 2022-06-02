library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    dashboardPage(
        dashboardHeader(title = 'HALO Object Analysis'),
        
        dashboardSidebar(width = 300,
                         sidebarMenu(
                             menuItem('Data Upload', 
                                      tabName = 'file_input',
                                      fileInput('file',
                                                label = h3('File input'),
                                                multiple = F,
                                                accept = c(".tsv",
                                                           ".csv"))),
                             menuItem('Marker Comparisons', 
                                      tabName = 'comparisons'),
                             menuItem('Intensity Comparison',
                                      tabName = 'intensities'),
                             menuItem('Tissue Representation',
                                      tabName = 'cell_map'),
                             menuItem('Data Tables',
                                      tabName = 'datatable')
                         )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = 'comparisons',
                        fluidRow(column(3,
                                        radioButtons('subset_pie',
                                                     'Pie Chart',
                                                     choices = c('% of subset',
                                                                 '% of whole')),
                                        radioButtons('subset_bar',
                                                     'Bar Chart',
                                                     choices = c('% of subset',
                                                                 '% of whole')),
                                        selectInput('barChart_input',
                                                    'markers',
                                                    choices = 'none',
                                                    multiple = T,
                                                    selectize = T)),
                                 column(9,
                                        plotlyOutput('doughnutChart'))),
                        fluidRow(12, plotOutput('barChart'))),
                tabItem(tabName = 'datatable',
                        tabBox(width = 12,
                               title = "Dataset",
                               id = 'dataset',
                               tabPanel("Classifications",
                                        dataTableOutput('classification_dataset')),
                               tabPanel('Intensities',
                                        dataTableOutput('intensity_dataset')),
                               tabPanel('Full Dataset',
                                        dataTableOutput('full_dataset')))
                )
            )
            )
        )
))
    