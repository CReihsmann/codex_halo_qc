#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
                             menuItem('Bar Charts', 
                                      tabName = 'bar_donut',
                                      radioButtons('subset_or_whole',
                                                   'Subset or Whole Tissue',
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
            
        )
    ))
)
