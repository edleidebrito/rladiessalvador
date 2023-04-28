################################################################################
#################################### PACOTE ####################################
################################################################################

### Script elaborado por Michelle Passos - Estatística/UFBA; Mestranda em Matemática 
### com área de concentração em Estatística na UFBA para o meetup do R-LadiesSalvador
### intitulado "Dando adeus ao Shiny App padrão: themes e shinyWidgets" - Abril/2022

## install.packages("C:/Users/chell/Downloads/shinydashboard_0.7.2.tar.gz", repos = NULL, type = "source")

library(shiny)
library(shinydashboard)
library(shinyDashboardThemes)
library(dashboardthemes)
library(plotly)
library(highcharter)

################################################################################
#################################### PARTE 1 ###################################
################################################################################

# INTERFACE BÁSICA

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)


server <- function(input, output) { }


shinyApp(ui, server)
