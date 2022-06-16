library(shinydashboard)

custom_dark_grey <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Arial"
    ,appFontColor = "rgb(205,205,205)"
    ,primaryFontColor = "rgb(255,255,255)"
    ,infoFontColor = "rgb(255,255,255)"
    ,successFontColor = "rgb(255,255,255)"
    ,warningFontColor = "rgb(255,255,255)"
    ,dangerFontColor = "rgb(255,255,255)"
    ,bodyBackColor = "rgb(45,55,65)"
    
    ### header
    ,logoBackColor = "rgb(70,80,90)"
    
    ,headerButtonBackColor = "rgb(70,80,90)"
    ,headerButtonIconColor = "rgb(25,35,45)"
    ,headerButtonBackColorHover = "rgb(40,50,60)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"
    
    ,headerBackColor = "rgb(70,80,90)"
    ,headerBoxShadowColor = ""
    ,headerBoxShadowSize = "0px 0px 0px"
    
    ### sidebar
    ,sidebarBackColor = "rgb(52,62,72)"
    ,sidebarPadding = 0
    
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0
    
    ,sidebarShadowRadius = ""
    ,sidebarShadowColor = "0px 0px 0px"
    
    ,sidebarUserTextColor = "rgb(205,205,205)"
    
    ,sidebarSearchBackColor = "rgb(45,55,65)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(45,55,65)"
    
    ,sidebarTabTextColor = "rgb(205,205,205)"
    ,sidebarTabTextSize = 14
    ,sidebarTabBorderStyle = "none"
    ,sidebarTabBorderColor = "none"
    ,sidebarTabBorderWidth = 0
    
    ,sidebarTabBackColorSelected = "rgb(70,80,90)"
    ,sidebarTabTextColorSelected = "rgb(255,255,255)"
    ,sidebarTabRadiusSelected = "5px"
    
    ,sidebarTabBackColorHover = "rgb(55,65,75)"
    ,sidebarTabTextColorHover = "rgb(255,255,255)"
    ,sidebarTabBorderStyleHover = "none"
    ,sidebarTabBorderColorHover = "none"
    ,sidebarTabBorderWidthHover = 0
    ,sidebarTabRadiusHover = "5px"
    
    ### boxes
    ,boxBackColor = "rgb(52,62,72)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 0px 0px"
    ,boxShadowColor = ""
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(52,62,72)"
    ,boxPrimaryColor = "rgb(200,200,200)"
    ,boxInfoColor = "rgb(80,95,105)"
    ,boxSuccessColor = "rgb(102,158,52)"
    ,boxWarningColor = "rgb(240,80,210)"
    ,boxDangerColor = "rgb(240,80,80)"
    
    ,tabBoxTabColor = "rgb(52,62,72)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(205,205,205)"
    ,tabBoxTabTextColorSelected = "rgb(205,205,205)"
    ,tabBoxBackColor = "rgb(52,62,72)"
    ,tabBoxHighlightColor = "rgb(70,80,90)"
    ,tabBoxBorderRadius = 5
    
    ### inputs
    ,buttonBackColor = "rgb(230,230,230)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(50,50,50)"
    ,buttonBorderRadius = 5
    
    ,buttonBackColorHover = "rgb(180,180,180)"
    ,buttonTextColorHover = "rgb(50,50,50)"
    ,buttonBorderColorHover = "rgb(50,50,50)"
    
    ,textboxBackColor = "rgb(68,80,90)"
    ,textboxBorderColor = "rgb(76,90,103)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(80,90,100)"
    ,textboxBorderColorSelect = "rgb(255,255,255)"
    
    ### tables
    ,tableBackColor = "rgb(52,62,72)"
    ,tableBorderColor = "rgb(70,80,90)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
    
)

# Define UI for application that draws a histogram
shinyUI(fluidPage(useShinyjs(),
                  dashboardPage(
                      dashboardHeader(title = 'HORNS'),
                      
                      dashboardSidebar(width = 300,
                                       sidebarMenu(
                                           menuItem(strong('Data Select'), 
                                                    tabName = 'file_input',
                                                    fileInput('file',
                                                              label = h4(strong('File input')),
                                                              multiple = F,
                                                              accept = c(".tsv",
                                                                         ".csv")),
                                                    selectInput('file_select',
                                                                label = h4(strong('Select file')),
                                                                choices = c('example.csv')),
                                                    actionButton('update_file',
                                                                 'Parse Data')),
                                           menuItem(strong('Instructions'),
                                                    tabName = 'instructions'),
                                           menuItem(strong('Marker Comparisons'), 
                                                    tabName = 'comparisons'),
                                           menuItem(strong('Intensity Comparisons'),
                                                    tabName = 'intensities'),
                                           menuItem(strong('Tissue Mapping'),
                                                    tabName = 'cell_map'),
                                           menuItem(strong('Data Tables'),
                                                    tabName = 'datatable'),
                                           menuItem(strong('About'),
                                                    tabName = 'about')
                                       )
                      ),
                      dashboardBody(custom_dark_grey,
                                    tabItems(
                                        tabItem(tabName = 'instructions',
                                                box(width = 12,
                                                    style='overflow-x: scroll;height:700px;overflow-y: scroll;',
                                                    includeMarkdown("instructions.md"))
                                        ),
                                        tabItem(tabName = 'comparisons',
                                                fluidRow(column(12,
                                                                p(strong("Choose a dataset and press the Parse button."),  " Select target markers to compare
                                                  against each other. Multiple markers can be chosen. To delete marker press the backspace button. To update
                                                        graphs press the ", strong("Update"), " button",
                                                                  style = 'text-align:justify;color:white;padding:15px;border-radius:10px'),
                                                                br())),
                                                fluidRow(column(3, 
                                                                selectizeInput('barChart_input',
                                                                               'Markers',
                                                                               choices = NULL,
                                                                               multiple = T,
                                                                               options = list(
                                                                                   placeholder = 'Upload dataset',
                                                                                   onInitialize = I('function() { this.setValue(""); }')
                                                                               ))),
                                                         column(3,
                                                                actionButton('comp_reset',
                                                                             'Update'))),
                                                tabBox(width = 12,
                                                       title = 'Marker Compositions',
                                                       id = 'bar_pie',
                                                       tabPanel('Bar Chart',
                                                                div(style = 'overflow-y:scroll;height:500px;',
                                                                    fluidRow(column(3,
                                                                                    radioButtons('subset_bar',
                                                                                                 h4(strong('Bar Chart')),
                                                                                                 choices = c('% of subset',
                                                                                                             '% of whole'))),
                                                                             column(9,
                                                                                    plotlyOutput('barChart'))))),
                                                       tabPanel('Pie Chart',
                                                                div(style = 'overflow-y:scroll;height:500px;',
                                                                    fluidRow(column(3,
                                                                                    radioButtons('subset_pie',
                                                                                                 h4(strong('Pie Chart')),
                                                                                                 choices = c('% of subset',
                                                                                                             '% of whole'))),
                                                                             column(9,
                                                                                    plotlyOutput('doughnutChart'))))))),
                                        tabItem(tabName = 'datatable',
                                                fluidRow(column(12,
                                                                p(strong("Choose a dataset and press the Parse button."),  " The resulting datatables are parsed from the file and 
                                                  are downloadable as CSVs."),
                                                                style = 'text-align:justify;color:white;padding:15px;border-radius:10px')),
                                                br(),
                                                tabBox(width = 12,
                                                       title = "Dataset",
                                                       id = 'dataset',
                                                       tabPanel("Classifications",
                                                                div(style = 'overflow-y:scroll;height:600px;',
                                                                    dataTableOutput('classification_dataset'),
                                                                    downloadButton('download_classification_cols', 'Download'))),
                                                       tabPanel('Intensities',
                                                                div(style = 'overflow-y:scroll;height:600px;',
                                                                    dataTableOutput('intensity_dataset'),
                                                                    downloadButton('download_intensity_cols', 'Download'))),
                                                       tabPanel('Double Positives',
                                                                div(style = 'overflow-y:scroll;height:625px;',
                                                                    h3("Double Positive Sums"),
                                                                    dataTableOutput('double_positive_data'),
                                                                    downloadButton('download_dp_total', 'Download'),
                                                                    br(),
                                                                    br(),
                                                                    h3("Double Positive % (whole tissue)"),
                                                                    dataTableOutput('double_positive_percentages'),
                                                                    downloadButton('download_dp_perc', 'Download'))),
                                                       tabPanel('Full Dataset',
                                                                div(style = 'overflow-y:scroll;height:600px;',
                                                                    dataTableOutput('full_dataset'))))),
                                        tabItem(tabName = 'intensities',
                                                fluidRow(column(12,
                                                                p(strong("Choose a dataset and press the Parse button."),  " Select individual markers for the 
                                                x and y axes to plot intensity values against each other. Make sure to choose", strong("two different markers"),
                                                                  " To choose a different markers, press backspace, choose different markers, and press ", strong("Update"), ".",
                                                                  style = 'text-align:justify;color:white;padding:15px;border-radius:10px'),
                                                                br())),
                                                fluidRow(column(2,
                                                                actionButton('intensity_reset',
                                                                             'Update'))),
                                                fluidRow(column(2,
                                                                selectizeInput('y_input',
                                                                               strong('y-axis'),
                                                                               choices = NULL,
                                                                               multiple = F,
                                                                               options = list(
                                                                                   placeholder = 'Upload dataset',
                                                                                   onInitialize = I('function() { this.setValue(""); }')))),
                                                         column(10,
                                                                plotlyOutput('intensityChart'))),
                                                fluidRow(column(2),
                                                         column(10,
                                                                selectizeInput('x_input',
                                                                               strong('x-axis'),
                                                                               choices = NULL,
                                                                               multiple = F,
                                                                               options = list(
                                                                                   placeholder = 'Upload dataset',
                                                                                   onInitialize = I('function() { this.setValue(""); }'))))),
                                                fluidRow(column(3,
                                                                radioButtons('intensity_choices',
                                                                             'Intensity Values',
                                                                             choices = c('all',
                                                                                         'positive',
                                                                                         'double positive',
                                                                                         'positive (no double positive)',
                                                                                         'negative'))),
                                                         column(3,
                                                                radioButtons('log_trans',
                                                                             'Log Transformation',
                                                                             choices = c('yes', 
                                                                                         'no'))))
                                        ),
                                        tabItem(tabName = 'cell_map',
                                                tabBox(width = 12,
                                                       tabPanel('Two Markers',
                                                                div(style = 'overflow-y:scroll;height:700px;',
                                                                    fluidRow(column(12,
                                                                                    p(strong("Choose a dataset and press the Parse button."),
                                                                                      " Select individual markers for Marker 1 and Marker 2. To change markers, press backspace, choose new markers, and press ", strong("Update"), "."),
                                                                                    style = 'text-align:justify;color:white;padding:15px;border-radius:10px'),
                                                                             br()),
                                                                    fluidRow(column(3,
                                                                                    selectizeInput('marker_1',
                                                                                                   'Marker 1',
                                                                                                   choices = NULL,
                                                                                                   multiple = F,
                                                                                                   options = list(
                                                                                                       placeholder = 'Upload dataset',
                                                                                                       onInitialize = I('function() { this.setValue(""); }')))),
                                                                             column(3,
                                                                                    selectizeInput('marker_2',
                                                                                                   'Marker 2',
                                                                                                   choices = NULL,
                                                                                                   multiple = F,
                                                                                                   options = list(
                                                                                                       placeholder = 'Upload dataset',
                                                                                                       onInitialize = I('function() { this.setValue(""); }')))),
                                                                             column(3,
                                                                                    actionButton('map_update',
                                                                                                 'Update'))),
                                                                    tags$style(type = "text/css", "#cellMap {height: calc(100vh - 10px) !important;}"),
                                                                    fillPage(plotlyOutput("cellMap", height="100%", width="100%")))),
                                                       tabPanel('One Marker',
                                                                div(style = 'overflow-y:scroll;height:700px;',
                                                                    fluidRow(column(12,
                                                                                    p(strong("Parse the selected data under the Data tab"),  " and then select markers. The resulting graph is a 
                                                                    representation of the tissue mapping locations of what cells are positive for the 
                                                                      selected marker"),
                                                                                    style = 'text-align:justify;color:white;padding:15px;border-radius:10px'),
                                                                             br()),
                                                                    selectInput('marker_ind',
                                                                                'Marker',
                                                                                choices = NULL,
                                                                                multiple = F,
                                                                                selectize = T),
                                                                    tags$style(type = "text/css", "#cellMap_ind {height: calc(100vh - 10px) !important;}"),
                                                                    fillPage(plotlyOutput("cellMap_ind", height="100%", width="100%")))))
                                        ),
                                        tabItem(tabName = 'about',
                                                tabBox(width = 12,
                                                       title = 'About',
                                                       id = 'about_box',
                                                       tabPanel('Purpose',
                                                                div(style = 'overflow-y:scroll;height:700px;',
                                                                    includeMarkdown("about.md"),
                                                                    style = 'text-align:justify;color:white;padding:15px;border-radius:10px')),
                                                       tabPanel('Contact',
                                                                div(style = 'overflow-y:scroll;height:700px;',
                                                                    includeMarkdown("contact.md")))))
                                    )
                      )
                  )
))