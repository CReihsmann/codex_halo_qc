library(tidyverse)
library(shiny)
library(httr)
library(rjson)
library(plotly)
library(dashboardthemes)
library(markdown)
library(DT)
library(shinyjs)
library(ggplot2)
library(viridis)

example_data = read_csv('test.csv')

inferno <- read_csv('viridis_inferno.csv')
comp_colors <- inferno$inferno


