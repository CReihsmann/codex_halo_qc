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
                                tabName = 'comparisons',
                                radioButtons('subset_bar',
                                             'Bar Chart',
                                             choices = c('% of subset',
                                                         '% of whole')),
                                radioButtons('subset_pie',
                                             'Pie Chart',
                                             choices = c('% of subset',
                                                         '% of whole'))),
                       menuItem('Intensity Comparison',
                                tabName = 'intensities'),
                       menuItem('Tissue Representation',
                                'cell_map'),
                       menuItem('Data Tables',
                                'datatable')
                     )
    ),
    dashboardBody(
      tabItem(tabName = 'file_input',
              tabBox(width = 12,
                title = "Dataset",
                     id = 'dataset',
                     tabPanel("Classifications",
                              dataTableOutput('classification_dataset')),
                     tabPanel('Intensities',
                              dataTableOutput('intensity_dataset')),
                     tabPanel('Full Dataset',
                              dataTableOutput('full_dataset'))
              )
      )
      tabItem(tabName = 'comparisons',
              plotOutput('barChart'))
    )
  ))
)