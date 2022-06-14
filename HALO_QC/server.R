library(shiny)

options(shiny.maxRequestSize=500*1024^2) 

shinyServer(function(input, output, session) {
    
    source('reactives.R', local = T)
    source('conditions.R', local = T)
    source('tables.R', local = T)
    source('plots.R', local = T)
    source('downloads.R', local = T)
    
})